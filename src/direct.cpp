#include "native.h"
namespace gp {
static Record pglyco(const S &x) {
  Record r;
  r.anomer = "??";
  VS ms = tokens(x, "[^/(), ]+");
  Pairs map = {{"H", "Hex"},   {"N", "HexNAc"}, {"F", "dHex"}, {"A", "NeuAc"},
               {"G", "NeuGc"}, {"aH", "HexN"},  {"pH", "Hex"}};
  for (auto &m : ms)
    r.add(lookup(map, m, m), m == "pH" ? "?P" : "");
  VI stack;
  int n = 0;
  for (char c : x) {
    if (c == '(') {
      ++n;
      if (!stack.empty())
        r.edge(stack.back(), n,
               "?"
               "?-?");
      stack.push_back(n);
    } else if (c == ')') {
      if (stack.empty())
        fail("Unbalanced pGlyco branch");
      stack.pop_back();
    }
  }
  if (n != (int)ms.size() || n == 0 || (n == 1 && x.find(')') == S::npos))
    fail("Invalid pGlyco structure");
  return r;
}
static void strucgp_node(const S &x, Record &r, int parent, int depth = 0) {
  if (x.empty())
    return;
  if (depth > 1024)
    fail("StrucGP nesting limit");
  if (x.size() < 2)
    fail("Invalid StrucGP node");
  char start = x[0];
  if (parent && !std::isalpha(static_cast<unsigned char>(start)))
    fail("Invalid StrucGP depth marker");
  auto n = std::count(x.begin(), x.end(), start);
  if (parent && n > 1) {
    S pattern =
        S(1, start) + ".*?" + S(1, std::tolower(static_cast<unsigned char>(start)));
    for (const auto &branch : tokens(x, pattern))
      strucgp_node(branch, r, parent, depth + 1);
    return;
  }
  S m = lookup(
      {{"1", "Hex"}, {"2", "HexNAc"}, {"3", "NeuAc"}, {"4", "NeuGc"}, {"5", "dHex"}},
      x.substr(1, 1));
  if (m.empty())
    fail("Unknown StrucGP residue");
  int id = r.add(m);
  if (parent)
    r.edge(parent, id,
           "?"
           "?-?");
  strucgp_node(x.size() > 3 ? x.substr(2, x.size() - 3) : "", r, id, depth + 1);
}
struct LinResidue {
  S kind, mono, sub, anomer;
  bool alditol = false;
};
static LinResidue lin_residue(S x, const Vocab &v) {
  if (x == "1(R)-carboxyethoxy")
    return {"implicit", "", "", ""};
  S subname = lookup({{"S", "S"},
                      {"SO3H", "S"},
                      {"P", "P"},
                      {"Me", "Me"},
                      {"Ac", "Ac"},
                      {"NAc", "NAc"},
                      {"NGc", "NGc"}},
                     x);
  if (!subname.empty())
    return {"sub", "", subname, ""};
  S a = "?";
  auto m = match(x, "^([ab?])-?(.*)(?:\\n)?$");
  if (!m.empty()) {
    a = m[1];
    x = m[2];
  } else {
    m = match(x, "^(6-deoxy-)([ab?])-?(.*)(?:\\n)?$");
    if (!m.empty()) {
      a = m[2];
      x = m[1] + m[3];
    }
  }
  bool alditol = ends(x, "-ol");
  x = sub(sub(sub(sub(sub(x, "^(?:keto|aldehydo)-"), "-onic$"), "^LDMan", "LDman"),
              "^DDMan", "DDman"),
          "-ol$");
  S stem, mono;
  for (auto &p : linucs_table)
    if (starts(x, p.first) && p.first.size() > stem.size()) {
      stem = p.first;
      mono = p.second;
    }
  if (stem.empty())
    fail("Unknown LINUCS residue");
  x = x.substr(stem.size());
  VS subs;
  S ringless = mono;
  for (auto &p : v.furanose)
    if (p.second == mono)
      ringless = p.first;
  if (starts(ringless, "D-") || starts(ringless, "L-"))
    ringless = ringless.substr(2);
  if (ends(ringless, "N") && starts(x, "S")) {
    subs.push_back("2S");
    x.erase(0, 1);
  }
  if (starts(x, "NAc")) {
    subs.push_back("2NAc");
    x.erase(0, 3);
  } else if (starts(x, "NGc")) {
    subs.push_back("2NGc");
    x.erase(0, 3);
  } else if (starts(x, "N")) {
    subs.push_back("2N");
    x.erase(0, 1);
  }
  const S &pat = v.cached("linucs-subs", [&] {
    return "^([0-9]+|\\?)(" + alternatives(longest(v.subs)) + ")";
  });
  while (!x.empty()) {
    m = match(x, pat);
    if (m.empty())
      fail("Invalid LINUCS substituent");
    subs.push_back(m[1] + m[2]);
    x.erase(0, m[0].size());
  }
  return {"mono", mono, join(subs, ","), a + v.pos(mono), alditol};
}
static S bracket(const S &x, size_t &p) {
  if (p >= x.size() || x[p] != '[')
    fail("Expected LINUCS bracket");
  auto e = x.find(']', p);
  if (e == S::npos)
    fail("Unclosed LINUCS bracket");
  S s = x.substr(p + 1, e - p - 1);
  p = e + 1;
  return s;
}
static void lin_node(const S &x, size_t &p, Record &r, int parent, const Vocab &v,
                     int depth = 0) {
  if (depth > 1024)
    fail("LINUCS nesting limit");
  S l = bracket(x, p), s = bracket(x, p);
  auto link = match(l, "^\\(([0-9]+|\\?)\\+([0-9]+|\\?)\\)$");
  if ((!parent && !l.empty()) || (parent && link.empty()))
    fail("Invalid LINUCS linkage");
  auto res = lin_residue(s, v);
  if (p >= x.size() || x[p++] != '{')
    fail("Expected LINUCS child list");
  if (res.kind != "mono") {
    if (!parent || p >= x.size() || x[p] != '}')
      fail("Invalid LINUCS substituent node");
    if (res.kind == "sub") {
      VS subs = split(r.sub[parent - 1], ',', false);
      subs.push_back(link[1] + res.sub);
      r.sub[parent - 1] = join(subs, ",");
    }
    ++p;
    return;
  }
  if (parent && res.alditol)
    fail("Non-root LINUCS alditol");
  int id = r.add(res.mono, res.sub);
  if (parent)
    r.edge(parent, id, res.anomer.substr(0, 1) + link[2] + "-" + link[1]);
  else {
    r.anomer = res.anomer;
    r.alditol = res.alditol;
  }
  while (p < x.size() && x[p] != '}')
    lin_node(x, p, r, id, v, depth + 1);
  if (p >= x.size())
    fail("Unclosed LINUCS children");
  ++p;
}
struct KNode {
  S kind, mono, sub;
  int idx = 0;
};
struct KEnd {
  int id;
  S a, pos;
};
static KEnd kend(S x) {
  auto m = match(x, "^([0-9]+)(?::(.*))?$");
  if (m.empty())
    fail("Invalid KCF endpoint");
  S l = m[2], a;
  if (!l.empty() && has(l, "^[abAB?]")) {
    a = S(1, std::tolower((unsigned char)l[0]));
    l.erase(0, 1);
  }
  return {std::stoi(m[1]), a, replace(l, "|", "/")};
}
static KNode knode(const S &x, const Vocab &v) {
  VS cand = {x};
  bool config = x.size() > 1 && (x[0] == 'D' || x[0] == 'L') && x[1] != '-';
  if (config)
    cand.push_back(x.substr(1));
  for (size_t i = 0; i < cand.size(); ++i) {
    S m;
    for (auto &n : v.sorted_monos())
      if (starts(cand[i], n)) {
        m = n;
        break;
      }
    if (m.empty())
      continue;
    S rest = cand[i].substr(m.size());
    VS subs;
    while (!rest.empty()) {
      auto t = match(rest, "^([0-9]+|\\?)(Ac|Me|Pyr|Py|S|P|N)");
      if (t.empty())
        break;
      subs.push_back(t[1] + (t[2] == "Py" ? "Pyr" : t[2]));
      rest.erase(0, t[0].size());
    }
    if (rest.empty())
      return {"mono", i ? v.config(m, x.substr(0, 1)) : m, sort_subs(subs)};
  }
  if (contains(VS{"S", "P", "Ac", "Me", "N"}, x))
    return {"sub", "", x};
  if (contains(VS{"LipidA", "R", "Asn", "Ser/Thr", "Ser", "P-Dol", "PP-Dol", "PP-Und",
                  "PE", "Cer", "myo-Ino", "Ino(acyl)-P", "Ino-P", "Ino", "Sph", "*"},
               x))
    return {"aglycon", "", ""};
  fail("Unknown KCF node");
  return {};
}
static Record kcf(const S &x, const Vocab &v) {
  Record r;
  std::map<int, KNode> nodes;
  std::vector<std::pair<KEnd, KEnd>> edges;
  S section;
  bool seen = false;
  for (auto line : split(x, '\n', false)) {
    line = trim(line);
    if (line.empty())
      continue;
    if (line == "///")
      break;
    auto h = match(line, "^(NODE|EDGE)\\s+[0-9]+");
    if (!h.empty()) {
      section = h[1];
      seen |= section == "NODE";
      continue;
    }
    VS p = tokens(line, "\\S+");
    if (section == "NODE") {
      if (p.size() < 2 || !has(p[0], "^[0-9]+$"))
        continue;
      auto n = knode(p[1], v);
      if (n.kind == "mono")
        n.idx = r.add(n.mono, n.sub);
      nodes[std::stoi(p[0])] = n;
    } else if (section == "EDGE") {
      if (p.size() < 3 || !has(p[0], "^[0-9]+$"))
        fail("Malformed KCF edge");
      edges.push_back({kend(p[1]), kend(p[2])});
    }
  }
  if (!seen || r.mono.empty())
    fail("Missing KCF nodes");
  std::map<int, S> rootanomer;
  for (auto &e : edges) {
    auto l = e.first, q = e.second;
    if (!nodes.count(l.id) || !nodes.count(q.id))
      fail("Missing KCF endpoint");
    auto a = nodes[l.id], b = nodes[q.id];
    if (a.kind == "mono" && b.kind == "mono")
      r.edge(b.idx, a.idx,
             (l.a == "a" || l.a == "b" ? l.a : "?") + (l.pos.empty() ? "?" : l.pos) +
                 "-" + (q.pos.empty() ? "?" : q.pos));
    else if ((a.kind == "mono" && b.kind == "aglycon") ||
             (a.kind == "aglycon" && b.kind == "mono")) {
      auto n = a.kind == "mono" ? a : b;
      auto end = a.kind == "mono" ? l : q;
      rootanomer[n.idx] = (end.a == "a" || end.a == "b" ? end.a : "?") +
                          (end.pos.empty() ? v.pos(n.mono) : end.pos);
    } else if ((a.kind == "sub" && b.kind == "mono") ||
               (a.kind == "mono" && b.kind == "sub")) {
      int id = a.kind == "mono" ? a.idx : b.idx;
      S s = a.kind == "sub" ? a.sub : b.sub;
      S pos = a.kind == "mono" ? l.pos : q.pos;
      auto subs = split(r.sub[id - 1], ',', false);
      subs.push_back((pos.empty() ? "?" : pos) + s);
      r.sub[id - 1] = sort_subs(subs);
    } else
      fail("Unsupported KCF edge");
  }
  auto roots = r.roots();
  int root = roots.empty() ? 1 : roots[0];
  r.anomer = rootanomer.count(root) ? rootanomer[root] : "?" + v.pos(r.mono[root - 1]);
  return r;
}
Record parse_direct(const S &x, const S &format, const Vocab &v) {
  if (format == "pglyco")
    return pglyco(x);
  if (format == "strucgp") {
    Record r;
    r.anomer = "??";
    strucgp_node(x, r, 0);
    return r;
  }
  if (format == "linucs") {
    Record r;
    size_t p = 0;
    lin_node(x, p, r, 0, v);
    if (p != x.size())
      fail("Trailing LINUCS content");
    return r;
  }
  if (format == "kcf")
    return kcf(x, v);
  if (format == "wurcs")
    return wurcs_parse(x, v);
  if (format == "glycoct")
    return glycoct_parse(x, v);
  fail("Unknown parser");
  return {};
}
} // namespace gp
