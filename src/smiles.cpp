#include "native.h"
#include <tuple>

// A deliberately closed SMILES subset. Every atom must be accounted for in the
// resulting glycan; unsupported chemistry is never silently stripped off.
namespace gp {
namespace {
struct Atom {
  char element;
  int stereo = 0, hydrogen = -1;
  VI neighbors, orders;
};
struct Molecule {
  std::vector<Atom> atoms;
  void bond(int a, int b, int order) {
    if (a == b ||
        std::find(atoms[a].neighbors.begin(), atoms[a].neighbors.end(), b) !=
            atoms[a].neighbors.end())
      fail("Invalid or duplicate SMILES bond");
    atoms[a].neighbors.push_back(b);
    atoms[a].orders.push_back(order);
    atoms[b].neighbors.push_back(a);
    atoms[b].orders.push_back(order);
  }
  int order(int a, int b) const {
    const auto &v = atoms[a];
    auto i = std::find(v.neighbors.begin(), v.neighbors.end(), b);
    return i == v.neighbors.end() ? 0 : v.orders[i - v.neighbors.begin()];
  }
};
Molecule read_smiles(const S &s) {
  if (s.empty())
    fail("Empty SMILES");
  Molecule m;
  VI branches;
  std::map<int, std::tuple<int, int, int>> rings;
  int previous = -1, pending = 0;
  bool need_atom = true;
  for (size_t i = 0; i < s.size();) {
    char c = s[i];
    if (c == '(') {
      if (previous < 0 || pending || need_atom)
        fail("Invalid SMILES branch");
      branches.push_back(previous);
      need_atom = true;
      ++i;
      continue;
    }
    if (c == ')') {
      if (branches.empty() || pending || need_atom)
        fail("Invalid SMILES branch");
      previous = branches.back();
      branches.pop_back();
      ++i;
      continue;
    }
    if (c == '-' || c == '=' || c == '#') {
      if (previous < 0 || pending)
        fail("Invalid SMILES bond");
      pending = c == '-' ? 1 : c == '=' ? 2 : 3;
      ++i;
      continue;
    }
    if (std::isdigit(static_cast<unsigned char>(c)) || c == '%') {
      if (previous < 0 || need_atom)
        fail("Invalid SMILES ring");
      int label;
      if (c == '%') {
        if (i + 2 >= s.size() ||
            !std::isdigit(static_cast<unsigned char>(s[i + 1])) ||
            !std::isdigit(static_cast<unsigned char>(s[i + 2])))
          fail("Invalid SMILES ring label");
        label = (s[i + 1] - '0') * 10 + s[i + 2] - '0';
        i += 3;
      } else {
        label = c - '0';
        ++i;
      }
      auto found = rings.find(label);
      if (found == rings.end()) {
        int slot = m.atoms[previous].neighbors.size();
        m.atoms[previous].neighbors.push_back(-2);
        m.atoms[previous].orders.push_back(0);
        rings[label] = std::make_tuple(previous, slot, pending);
      } else {
        int first = std::get<0>(found->second),
            slot = std::get<1>(found->second),
            first_order = std::get<2>(found->second);
        if (first == previous ||
            (first_order && pending && first_order != pending) ||
            m.order(first, previous))
          fail("Invalid SMILES ring closure");
        int order = pending ? pending : first_order ? first_order : 1;
        m.atoms[first].neighbors[slot] = previous;
        m.atoms[first].orders[slot] = order;
        m.atoms[previous].neighbors.push_back(first);
        m.atoms[previous].orders.push_back(order);
        rings.erase(found);
      }
      pending = 0;
      continue;
    }
    Atom a;
    if (c == '[') {
      size_t end = s.find(']', i);
      if (end == S::npos)
        fail("Unclosed SMILES atom");
      S token = s.substr(i + 1, end - i - 1);
      auto match_atom = match(token, "^([CNOSP])(@@?)?(H[0-3]?)?$");
      if (match_atom.empty())
        fail("Unsupported bracket atom (charge, isotope, or stereochemical "
             "class)");
      a.element = match_atom[1][0];
      a.stereo = match_atom[2] == "@" ? 1 : match_atom[2] == "@@" ? -1 : 0;
      a.hydrogen = match_atom[3].empty()       ? 0
                   : match_atom[3].size() == 1 ? 1
                                               : match_atom[3][1] - '0';
      i = end + 1;
    } else {
      if (S("CNOSP").find(c) == S::npos)
        fail("Unsupported SMILES atom or syntax");
      a.element = c;
      ++i;
    }
    if (m.atoms.size() >= 10000)
      fail("SMILES atom limit exceeded");
    int here = m.atoms.size();
    m.atoms.push_back(a);
    if (previous >= 0)
      m.bond(previous, here, pending ? pending : 1);
    previous = here;
    pending = 0;
    need_atom = false;
  }
  if (pending || need_atom || !branches.empty() || !rings.empty())
    fail("Incomplete SMILES");
  for (auto &a : m.atoms) {
    int valence = 0;
    for (int order : a.orders)
      valence += order;
    int expected = a.element == 'C'   ? 4
                   : a.element == 'O' ? 2
                   : a.element == 'N' ? 3
                   : a.element == 'P' ? 5
                                      : 6;
    if (a.hydrogen < 0)
      a.hydrogen = expected - valence;
    if (a.hydrogen < 0 || valence + a.hydrogen != expected)
      fail("Invalid or unsupported SMILES valence");
    if (a.stereo && (a.element != 'C' || a.neighbors.size() + a.hydrogen != 4 ||
                     a.hydrogen > 1))
      fail("Invalid tetrahedral SMILES atom");
  }
  return m;
}
// Return orientation relative to an explicitly specified neighbor order, not
// the spelling's @/@@ token. Ring-opening placeholders preserve SMILES order.
int orientation(const Molecule &m, int atom, VI target) {
  const auto &a = m.atoms[atom];
  if (!a.stereo)
    return 0;
  VI written = a.neighbors;
  if (a.hydrogen == 1) {
    if (atom == 0)
      written.insert(written.begin(), -1);
    else
      written.push_back(-1);
  }
  if (written.size() != 4 || target.size() != 4)
    fail("Invalid tetrahedral neighbors");
  VI permutation;
  for (int n : target) {
    auto i = std::find(written.begin(), written.end(), n);
    if (i == written.end())
      fail("Unsupported tetrahedral topology");
    permutation.push_back(i - written.begin());
  }
  int sign = a.stereo;
  for (int i = 0; i < 4; ++i)
    for (int j = i + 1; j < 4; ++j)
      if (permutation[i] > permutation[j])
        sign = -sign;
  return sign;
}
struct Sugar {
  VI chain;
  int ring, start, end;
  S descriptor;
  bool reversible = false;
};
struct Link {
  int a, b, pa, pb;
};
S encode_smiles(const S &input) {
  Molecule m = read_smiles(input);
  int n = m.atoms.size();
  VI seen(n, 0), owner(n, -1), position(n, -1), used(n, 0);
  std::vector<Sugar> sugars;
  for (int first = 0; first < n; ++first) {
    if (seen[first] || m.atoms[first].element != 'C')
      continue;
    VI component, stack{first};
    seen[first] = 1;
    while (!stack.empty()) {
      int a = stack.back();
      stack.pop_back();
      component.push_back(a);
      for (int b : m.atoms[a].neighbors)
        if (m.atoms[b].element == 'C' && !seen[b]) {
          seen[b] = 1;
          stack.push_back(b);
        }
    }
    if (component.size() < 5 || component.size() > 9)
      continue;
    VI ends;
    bool path = true;
    for (int a : component) {
      int count = 0;
      for (int b : m.atoms[a].neighbors)
        if (m.atoms[b].element == 'C') {
          ++count;
          if (m.order(a, b) != 1)
            path = false;
        }
      if (count == 1)
        ends.push_back(a);
      if (count > 2)
        path = false;
    }
    if (!path || ends.size() != 2)
      continue;
    VI chain{ends[0]};
    while (chain.size() < component.size()) {
      int next = -1;
      for (int b : m.atoms[chain.back()].neighbors)
        if (m.atoms[b].element == 'C' &&
            (chain.size() == 1 || b != chain[chain.size() - 2]))
          next = b;
      if (next < 0)
        fail("Invalid sugar chain");
      chain.push_back(next);
    }
    std::vector<Sugar> candidates;
    for (int direction = 0; direction < 2; ++direction) {
      if (direction)
        std::reverse(chain.begin(), chain.end());
      for (int start = 0; start <= 1; ++start)
        for (int ring : m.atoms[chain[start]].neighbors) {
          if (m.atoms[ring].element != 'O' ||
              m.atoms[ring].neighbors.size() != 2 ||
              m.order(chain[start], ring) != 1)
            continue;
          int other = m.atoms[ring].neighbors[0] == chain[start]
                          ? m.atoms[ring].neighbors[1]
                          : m.atoms[ring].neighbors[0];
          auto e = std::find(chain.begin(), chain.end(), other);
          if (e == chain.end())
            continue;
          int end = e - chain.begin();
          if (end - start != 3 && end - start != 4)
            continue;
          int hetero = 0;
          for (int b : m.atoms[chain[start]].neighbors)
            if (m.atoms[b].element == 'O' || m.atoms[b].element == 'N')
              ++hetero;
          if (hetero != 2)
            continue;
          candidates.push_back({chain, ring, start, end, ""});
        }
    }
    if (candidates.empty()) {
      // Acyclic polyols have no anomeric center. Require every carbon to
      // carry one O/N substituent; this excludes arbitrary alkyl aglycones.
      bool polyol = true;
      int deoxy_ends = 0;
      for (size_t p = 0; p < chain.size(); ++p) {
        int a = chain[p], count = 0;
        for (int b : m.atoms[a].neighbors)
          if (m.atoms[b].element != 'C') {
            if ((m.atoms[b].element != 'O' && m.atoms[b].element != 'N') ||
                m.order(a, b) != 1)
              polyol = false;
            // An ether between carbons of the same chain is not an alditol.
            for (int c : m.atoms[b].neighbors)
              if (c != a &&
                  std::find(chain.begin(), chain.end(), c) != chain.end())
                polyol = false;
            ++count;
          }
        bool terminal = p == 0 || p + 1 == chain.size();
        if (terminal && count == 0 && m.atoms[a].hydrogen == 3)
          ++deoxy_ends;
        else if (count != 1 || m.atoms[a].hydrogen != (terminal ? 2 : 1))
          polyol = false;
      }
      if (polyol && deoxy_ends <= 1) {
        // A polyol can be numbered from either end. Prefer the low amino
        // locant, then established alditol parent names; never atom indices.
        auto score = [&](const VI &cc) {
          VI amino, attachments;
          S skeleton;
          for (size_t p = 0; p < cc.size(); ++p) {
            int a = cc[p], h = -1;
            for (int b : m.atoms[a].neighbors)
              if (m.atoms[b].element != 'C')
                h = b;
            if (h < 0) {
              skeleton += 'm';
              continue;
            }
            if (m.atoms[h].element == 'N')
              amino.push_back(p + 1);
            if (m.atoms[h].neighbors.size() > 1)
              attachments.push_back(p + 1);
            if (p > 0 && p + 1 < cc.size()) {
              int sign = orientation(m, a, {cc[p - 1], cc[p + 1], h, -1});
              skeleton += sign == 0 ? 'x' : sign == 1 ? '1' : '2';
            } else
              skeleton += 'h';
          }
          if (amino.empty())
            amino.push_back(100);
          const VS preferred = {"h2122h", "h2112h", "h1122h",
                                "h212h",  "hxxxxh", "hxxxh"};
          auto it = std::find(preferred.begin(), preferred.end(), skeleton);
          int rank = it - preferred.begin();
          return std::make_tuple(amino, rank, skeleton, attachments);
        };
        VI reverse = chain;
        std::reverse(reverse.begin(), reverse.end());
        bool reversible = score(reverse) == score(chain);
        if (score(reverse) < score(chain))
          chain = reverse;
        candidates.push_back({chain, -1, -1, -1, "", reversible});
      }
    }
    if (candidates.size() != 1)
      continue;
    Sugar sugar = candidates[0];
    int id = sugars.size();
    for (size_t p = 0; p < sugar.chain.size(); ++p) {
      int a = sugar.chain[p];
      owner[a] = id;
      position[a] = p + 1;
      used[a] = 1;
    }
    if (sugar.ring >= 0)
      used[sugar.ring] = 1;
    sugars.push_back(sugar);
  }
  if (sugars.empty() || sugars.size() > 52)
    fail("No supported sugar backbone (or too many residues)");
  std::vector<Link> links;
  // All glycosidic bridges must be ordinary O bridges between two backbones.
  for (int a = 0; a < n; ++a)
    if (m.atoms[a].element == 'O' && m.atoms[a].neighbors.size() == 2) {
      int b = m.atoms[a].neighbors[0], c = m.atoms[a].neighbors[1];
      if (owner[b] >= 0 && owner[c] >= 0 && owner[b] != owner[c]) {
        if (m.order(a, b) != 1 || m.order(a, c) != 1)
          fail("Unsupported glycosidic bond");
        links.push_back({owner[b], owner[c], position[b], position[c]});
        used[a] = 1;
      }
    }
  auto claim = [&](int atom) {
    if (used[atom])
      fail("Overlapping substituents");
    used[atom] = 1;
  };
  // Recognize closed, neutral substituent graphs; unclaimed atoms fail below.
  std::function<S(int, int)> substituent = [&](int hetero, int carbon) -> S {
    const auto &a = m.atoms[hetero];
    if (a.element != 'O' && a.element != 'N')
      fail("Unsupported substituent attachment");
    if (m.order(hetero, carbon) != 1)
      fail("Unsupported substituent bond");
    if (used[hetero])
      return ""; // ring or a previously recognized glycosidic bridge
    claim(hetero);
    VI rest;
    for (int b : a.neighbors)
      if (b != carbon)
        rest.push_back(b);
    if (rest.empty())
      return a.element == 'N' ? "N" : "";
    if (rest.size() != 1)
      fail("Unsupported branched substituent");
    int center = rest[0];
    const auto &c = m.atoms[center];
    if (c.element == 'C') {
      if (c.neighbors.size() == 1 && c.hydrogen == 3 &&
          m.order(hetero, center) == 1) {
        claim(center);
        return S(1, a.element) + "C";
      }
      VI oxy, tail;
      for (int b : c.neighbors)
        if (b != hetero) {
          if (m.atoms[b].element == 'O' && m.order(center, b) == 2)
            oxy.push_back(b);
          else
            tail.push_back(b);
        }
      if (oxy.size() != 1 || tail.size() != 1 || c.hydrogen != 0 ||
          m.atoms[oxy[0]].neighbors.size() != 1 ||
          m.atoms[tail[0]].element != 'C' || m.order(center, tail[0]) != 1)
        fail("Unsupported acyl substituent");
      int t = tail[0];
      claim(center);
      claim(oxy[0]);
      claim(t);
      if (m.atoms[t].neighbors.size() == 1 && m.atoms[t].hydrogen == 3)
        return S(1, a.element) + "CC/3=O";
      if (m.atoms[t].neighbors.size() == 2 && m.atoms[t].hydrogen == 2) {
        int o = m.atoms[t].neighbors[0] == center ? m.atoms[t].neighbors[1]
                                                  : m.atoms[t].neighbors[0];
        if (m.atoms[o].element == 'O' && m.atoms[o].neighbors.size() == 1 &&
            m.order(t, o) == 1) {
          claim(o);
          return S(1, a.element) + "CCO/3=O";
        }
      }
      fail("Unsupported acyl side chain");
    }
    if (c.element == 'S' || c.element == 'P') {
      int singles = 0, doubles = 0;
      claim(center);
      for (int b : c.neighbors)
        if (b != hetero) {
          if (m.atoms[b].element != 'O' || m.atoms[b].neighbors.size() != 1)
            fail("Unsupported sulfate/phosphate bridge");
          if (m.order(center, b) == 1)
            ++singles;
          else if (m.order(center, b) == 2)
            ++doubles;
          else
            fail("Unsupported bond order");
          claim(b);
        }
      if (c.element == 'S' && singles == 1 && doubles == 2)
        return S(1, a.element) + "SO/3=O/3=O";
      if (c.element == 'P' && singles == 2 && doubles == 1)
        return S(1, a.element) + "PO/3O/3=O";
    }
    fail("Unsupported substituent");
    return "";
  };
  for (auto &s : sugars) {
    S skeleton;
    VS mods;
    int anomer_sign = 0, reference_sign = 0;
    for (size_t p = 0; p < s.chain.size(); ++p) {
      int a = s.chain[p];
      const auto &atom = m.atoms[a];
      VI hetero;
      for (int b : atom.neighbors)
        if (owner[b] != owner[a])
          hetero.push_back(b);
      if (static_cast<int>(p) == s.start) {
        VI external;
        for (int b : hetero)
          if (b != s.ring)
            external.push_back(b);
        if (external.size() != 1)
          fail("Unsupported anomeric center");
        int h = external[0];
        if (p == 0)
          anomer_sign = orientation(m, a, {s.ring, s.chain[p + 1], h, -1});
        else
          anomer_sign =
              orientation(m, a, {s.chain[p - 1], s.chain[p + 1], s.ring, h});
        skeleton += 'a';
        S mod = substituent(h, a);
        if (!mod.empty())
          fail("Modified reducing/anomeric center unsupported");
        continue;
      }
      bool terminal = p == 0 || p + 1 == s.chain.size();
      if (terminal && hetero.size() == 2) {
        int oxo = -1, attachment = -1;
        for (int b : hetero) {
          if (m.atoms[b].element == 'O' && m.order(a, b) == 2 &&
              m.atoms[b].neighbors.size() == 1)
            oxo = b;
          else if (m.order(a, b) == 1 &&
                   (m.atoms[b].element == 'O' || m.atoms[b].element == 'N'))
            attachment = b;
        }
        if (oxo < 0 || attachment < 0 || atom.stereo)
          fail("Unsupported carboxyl group");
        claim(oxo);
        S mod = substituent(attachment, a);
        if (!mod.empty())
          mods.push_back("_" + std::to_string(p + 1) + "*" + mod);
        skeleton += 'A';
        continue;
      }
      if (hetero.empty()) {
        if (atom.stereo)
          fail("Stereochemistry on achiral carbon");
        skeleton += terminal ? 'm' : 'd';
        continue;
      }
      if (hetero.size() != 1 || m.order(a, hetero[0]) != 1)
        fail("Unsupported sugar carbon substitution");
      int h = hetero[0];
      if (terminal) {
        if (atom.stereo || atom.hydrogen != 2)
          fail("Unsupported terminal carbon");
        skeleton += 'h';
      } else {
        int sign = orientation(m, a, {s.chain[p - 1], s.chain[p + 1], h, -1});
        skeleton += sign == 0 ? 'x' : sign == 1 ? '1' : '2';
        reference_sign =
            sign; // highest-numbered configurational center, not ring closure
      }
      if (h != s.ring) {
        S mod = substituent(h, a);
        if (!mod.empty())
          mods.push_back("_" + std::to_string(p + 1) + "*" + mod);
      }
    }
    char anomer = 'x';
    if (anomer_sign && reference_sign)
      anomer = anomer_sign == reference_sign ? 'a' : 'b';
    s.descriptor = s.start < 0
                       ? skeleton + join(mods)
                       : skeleton + "-" + std::to_string(s.start + 1) + anomer +
                             "_" + std::to_string(s.start + 1) + "-" +
                             std::to_string(s.end + 1) + join(mods);
  }
  if (std::find(used.begin(), used.end(), 0) != used.end())
    fail("Non-glycan or unsupported atoms remain");
  if (links.size() + 1 != sugars.size())
    fail("Only connected tree glycans are supported");
  // A non-reducing glycosidic bridge has two donors. Its direction is not
  // chemical information: choose it from the two labeled sides, never from
  // the SMILES traversal order. Other bridges must have exactly one donor.
  std::function<S(int, int, std::set<int>)> signature =
      [&](int node, int parent, std::set<int> visited) -> S {
    if (!visited.insert(node).second)
      fail("Cyclic residue graph unsupported");
    VS children;
    for (const auto &e : links) {
      int other = e.a == node ? e.b : e.b == node ? e.a : -1;
      if (other < 0 || other == parent)
        continue;
      int here = e.a == node ? e.pa : e.pb, there = e.a == node ? e.pb : e.pa;
      children.push_back(std::to_string(here) + ":" + std::to_string(there) +
                         ":" + signature(other, node, visited));
    }
    std::sort(children.begin(), children.end());
    return sugars[node].descriptor + "{" + join(children, ";") + "}";
  };
  // Equal local scores do not resolve a symmetric polyol decorated by
  // different glycans at reflected positions. Compare the complete labeled
  // sides (and reflected modification positions) before choosing numbering.
  for (size_t node = 0; node < sugars.size(); ++node) {
    auto &s = sugars[node];
    if (!s.reversible)
      continue;
    auto pieces = split(s.descriptor, '_');
    VS reflected;
    for (size_t j = 1; j < pieces.size(); ++j) {
      auto star = pieces[j].find('*');
      if (star == S::npos)
        fail("Invalid alditol modification");
      int p = std::stoi(pieces[j].substr(0, star));
      reflected.push_back(std::to_string(s.chain.size() + 1 - p) +
                          pieces[j].substr(star));
    }
    std::sort(reflected.begin(), reflected.end());
    S reverse_descriptor =
        pieces[0] + (reflected.empty() ? "" : "_" + join(reflected, "_"));
    VS forward, backward;
    for (const auto &e : links) {
      int other = e.a == static_cast<int>(node)   ? e.b
                  : e.b == static_cast<int>(node) ? e.a
                                                  : -1;
      if (other < 0)
        continue;
      int here = e.a == static_cast<int>(node) ? e.pa : e.pb,
          there = e.a == static_cast<int>(node) ? e.pb : e.pa;
      S side = ":" + std::to_string(there) + ":" + signature(other, node, {});
      forward.push_back(std::to_string(here) + side);
      backward.push_back(std::to_string(s.chain.size() + 1 - here) + side);
    }
    std::sort(forward.begin(), forward.end());
    std::sort(backward.begin(), backward.end());
    if (std::make_pair(reverse_descriptor, backward) <
        std::make_pair(s.descriptor, forward)) {
      s.descriptor = reverse_descriptor;
      std::reverse(s.chain.begin(), s.chain.end());
      for (auto &e : links) {
        if (e.a == static_cast<int>(node))
          e.pa = s.chain.size() + 1 - e.pa;
        if (e.b == static_cast<int>(node))
          e.pb = s.chain.size() + 1 - e.pb;
      }
    }
  }
  for (auto &e : links) {
    bool a = e.pa == sugars[e.a].start + 1, b = e.pb == sugars[e.b].start + 1;
    if (!a && !b) {
      if (sugars[e.a].start < 0 || sugars[e.b].start < 0)
        fail("Non-root alditol cannot be represented without information loss");
      fail("Non-anomeric sugar bridge unsupported");
    }
    if (a && b && signature(e.b, e.a, {}) < signature(e.a, e.b, {})) {
      std::swap(e.a, e.b);
      std::swap(e.pa, e.pb);
    }
  }
  VS descriptors, indices, edges;
  for (size_t i = 0; i < sugars.size(); ++i) {
    descriptors.push_back("[" + sugars[i].descriptor + "]");
    indices.push_back(std::to_string(i + 1));
  }
  auto letter = [](int i) { return S(1, i < 26 ? 'a' + i : 'A' + i - 26); };
  for (const auto &e : links)
    edges.push_back(letter(e.a) + std::to_string(e.pa) + "-" + letter(e.b) +
                    std::to_string(e.pb));
  return "WURCS=2.0/" + std::to_string(sugars.size()) + "," +
         std::to_string(sugars.size()) + "," + std::to_string(links.size()) +
         "/" + join(descriptors) + "/" + join(indices, "-") + "/" +
         join(edges, "_");
}
} // namespace
S smiles_to_wurcs(const S &x) { return encode_smiles(x); }
Record smiles_parse(const S &x, const Vocab &v) {
  return wurcs_parse(smiles_to_wurcs(x), v);
}
} // namespace gp
