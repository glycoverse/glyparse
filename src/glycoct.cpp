#include "native.h"
namespace gp {
struct CResidue {
  int id;
  S kind, content, anomer, position;
  bool alditol;
};
struct CEdge {
  int from, to;
  S parent, child;
};
struct CBlock {
  std::vector<CResidue> residues;
  std::vector<CEdge> edges;
  VI parents;
  S parent, child;
};
static S bounds(const S &s) {
  auto m = match(s, "-((?:[0-9]+|x):(?:[0-9]+|x))");
  return m.empty() ? "" : m[1];
}
static S core(const S &s) { return sub(s, "-(?:[0-9]+|x):(?:[0-9]+|x)"); }
static bool cyclic(const S &b) {
  auto m = match(b, "^([0-9]+):([0-9]+)$");
  if (m.empty())
    return false;
  // Preserve the reference's permissive numeric ring-family comparison.
  // Its R expression subtracts (start %in% c(3, 4)) from end before testing.
  int start = std::stoi(m[1]), end = std::stoi(m[2]);
  return end != (start == 3 || start == 4 ? 1 : 0);
}
static bool same_bounds(const S &a, const S &b) {
  if (a.empty() || b.empty())
    return a == b;
  return a == b || a.find('x') != S::npos || b.find('x') != S::npos ||
         (cyclic(a) && cyclic(b) && split(a, ':')[0] == split(b, ':')[0]);
}
static bool compatible(const S &s, const CTMapping &m) {
  return core(s) == m.core && (s.find("|1:aldi") != S::npos) == m.alditol &&
         same_bounds(bounds(s), m.bounds);
}
static S preserve_ring(S mono, const S &s, const Vocab &v) {
  auto m = match(bounds(s), "^([0-9]+):([0-9]+)$");
  return !m.empty() && std::stoi(m[2]) - std::stoi(m[1]) == 3 ? v.furan(mono) : mono;
}
static bool generic_hex(const S &s) {
  return has(s, "^HEX-(?:[0-9]+|x):(?:[0-9]+|x)(?:\\|6:d)?$");
}
static S base_mono(const S &s, const Vocab &v) {
  bool aldi = s.find("|1:aldi") != S::npos;
  if (bounds(s) == "0:0" && !aldi)
    fail("Open-chain GlycoCT residues without an alditol modification are not "
         "representable.");
  const auto &table = aldi ? glycoct_alditol_entries : glycoct_entries;
  for (auto &m : table)
    if (m.subs.empty() && compatible(s, m))
      return m.name;
  if (aldi)
    return "Unk";
  if (has(s, "^HEX-(?:[0-9]+|x):(?:[0-9]+|x)\\|6:d$"))
    return "dHex";
  if (has(s, "^HEX-(?:[0-9]+|x):(?:[0-9]+|x)\\|6:a$"))
    return "HexA";
  if (generic_hex(s))
    return "Hex";
  if (has(s, "^PEN-(?:[0-9]+|x):(?:[0-9]+|x)$"))
    return "Pen";
  auto p = split(s, '-');
  if (p.size() > 1) {
    for (auto &t : Pairs{{"dglc", "Glc"},
                         {"dgal", "Gal"},
                         {"dman", "Man"},
                         {"dxyl", "Xyl"},
                         {"glc", "Glc"},
                         {"gal", "Gal"},
                         {"man", "Man"},
                         {"xyl", "Xyl"}})
      if (p[0].find(t.first) != S::npos)
        return t.second;
    if (has(s, "dglc.*6:a"))
      return "GlcA";
    if (p[1] == "HEX")
      return "Hex";
    if (p[1] == "PEN")
      return "Pen";
  }
  return "Unk";
}
static S cabbrev(const S &s) {
  return lookup({{"sulfate", "S"},
                 {"n-acetyl", "NAc"},
                 {"n-glycolyl", "NGc"},
                 {"acetyl", "Ac"},
                 {"methyl", "Me"},
                 {"amino", "N"},
                 {"phosphate", "P"},
                 {"phospho-ethanolamine", "PEtn"},
                 {"diphospho-ethanolamine", "PPEtn"}},
                s, s);
}
static S position(const S &s) { return s == "-1" ? "?" : replace(s, "|", "/"); }
static S cformat(const VS &tokens) {
  VS out;
  for (auto &t : tokens) {
    auto p = split(t, '\r');
    if (p.size() != 3)
      fail("Invalid GlycoCT substituent token");
    out.push_back(position(p[1]) + cabbrev(p[0]));
  }
  auto rank = [](const S &s) {
    auto m = match(s, "^([?0-9/]+)");
    if (m.empty() || m[1] == "?")
      return 999;
    int value = 999;
    for (auto &p : split(m[1], '/'))
      value = std::min(value, std::stoi(p));
    return value;
  };
  std::stable_sort(out.begin(), out.end(), [&](const S &a, const S &b) {
    int x = rank(a), y = rank(b);
    return x == y ? a < b : x < y;
  });
  return join(out, ",");
}
static bool subtract(VS &x, const VS &y) {
  for (auto &s : y) {
    auto i = std::find(x.begin(), x.end(), s);
    if (i == x.end())
      return false;
    x.erase(i);
  }
  return true;
}
static S exact_mono(const S &s, const VS &subs, const std::vector<CTMapping> &table) {
  VS sorted = subs;
  std::sort(sorted.begin(), sorted.end());
  for (auto &m : table)
    if (compatible(s, m) && sorted == m.subs)
      return m.name;
  return "";
}
static std::pair<S, S> composite(const S &s, VS tokens, const Vocab &v) {
  bool aldi = s.find("|1:aldi") != S::npos;
  S exact = aldi ? exact_mono(s, tokens, glycoct_alditol_entries) : "";
  if (exact.empty())
    exact = exact_mono(s, tokens, glycoct_entries);
  if (!exact.empty())
    return {preserve_ring(exact, s, v), ""};
  auto at = [&](const S &name, const S &pos) {
    for (auto &t : tokens) {
      auto p = split(t, '\r');
      if (p[0] == name && p[1] == pos)
        return true;
    }
    return false;
  };
  auto exclude = [&](const S &name) {
    VS out;
    for (auto &t : tokens)
      if (split(t, '\r')[0] != name)
        out.push_back(t);
    return out;
  };
  if (at("n-sulfate", "2")) {
    S mono = preserve_ring(base_mono(s, v), s, v);
    if (mono != "Unk") {
      S extra = cformat(exclude("n-sulfate"));
      return {mono == "Hex" ? "HexN" : mono + "N",
              "2S" + (extra.empty() ? "" : "," + extra)};
    }
  }
  bool dhex = has(s, "^HEX-(?:[0-9]+|x):(?:[0-9]+|x)\\|6:d$");
  if (generic_hex(s) && !dhex && at("amino", "2")) {
    VS remaining = tokens;
    for (auto i = remaining.begin(); i != remaining.end(); ++i) {
      auto p = split(*i, '\r');
      if (p[0] == "amino" && p[1] == "2") {
        remaining.erase(i);
        break;
      }
    }
    return {"HexN", cformat(remaining)};
  }
  if (generic_hex(s) && at("n-acetyl", "2"))
    return {dhex ? "dHexNAc" : "HexNAc", cformat(exclude("n-acetyl"))};
  if (s == "NON-2:6|1:a|2:keto|3:d" && (at("n-acetyl", "5") != at("n-glycolyl", "5"))) {
    bool ac = at("n-acetyl", "5");
    return {ac ? "Neu5Ac" : "Neu5Gc", cformat(exclude(ac ? "n-acetyl" : "n-glycolyl"))};
  }
  const auto &table = aldi ? glycoct_alditol_entries : glycoct_entries;
  S best;
  VS extras;
  size_t size = 0;
  for (auto &m : table) {
    VS extra = tokens;
    if (m.subs.size() > size && m.subs.size() < tokens.size() && compatible(s, m) &&
        subtract(extra, m.subs)) {
      best = m.name;
      extras = extra;
      size = m.subs.size();
    }
  }
  if (!best.empty())
    return {preserve_ring(best, s, v), cformat(extras)};
  return {preserve_ring(base_mono(s, v), s, v), cformat(tokens)};
}
static CBlock block(const VS &lines, bool und) {
  CBlock b;
  S section;
  int rescount = 0, lincount = 0, pc = 0, lc = 0;
  for (auto &s : lines) {
    if (s == "RES") {
      section = s;
      ++rescount;
      continue;
    }
    if (s == "LIN") {
      section = s;
      ++lincount;
      continue;
    }
    if (und && starts(s, "ParentIDs:")) {
      ++pc;
      for (auto &p : split(s.substr(10), '|')) {
        if (!has(p, "^[0-9]+$"))
          fail("Invalid UND parent IDs");
        b.parents.push_back(std::stoi(p));
      }
      continue;
    }
    if (und && has(s, "^SubtreeLinkageID[0-9]+:")) {
      ++lc;
      auto m = match(
          sub(s, "^SubtreeLinkageID[0-9]+:"),
          "^[a-z]?\\((-?[0-9]+(?:\\|[0-9]+)*)\\+(-?[0-9]+(?:\\|[0-9]+)*)\\)[a-z]?$");
      if (m.empty() || m[2].find('|') != S::npos)
        fail("Invalid UND attachment");
      b.parent = m[1];
      b.child = m[2];
      continue;
    }
    if (section == "RES") {
      auto m = match(s, "^([^:]*)([bs]):(.*)$");
      if (m.empty()) {
        if (s.find(':') != S::npos)
          fail("Invalid GlycoCT residue");
        continue;
      }
      auto id = match(m[1], "[0-9]+");
      // A header without an ID cannot participate in a glycan component.
      if (id.empty())
        continue;
      S content = m[3], a, pos;
      bool aldi = false;
      if (m[2] == "b") {
        auto t = match(content, "^([abxo])");
        if (!t.empty()) {
          a = t[1];
          if (content.size() > 1 && content[1] == '-')
            content.erase(0, 2);
        }
        auto bounds_match = match(content, "-([0-9]+|x):");
        if (!bounds_match.empty())
          pos = bounds_match[1];
        aldi = content.find("|1:aldi") != S::npos;
      }
      CResidue r{std::stoi(id[0]), m[2] == "b" ? "mono" : "sub", content, a, pos, aldi};
      bool existing = false;
      for (auto &old : b.residues)
        if (old.id == r.id) {
          old = r;
          existing = true;
          break;
        }
      if (!existing)
        b.residues.push_back(r);
    } else if (section == "LIN") {
      auto m = match(s, "^[0-9]+:([0-9]+)[do]?\\((-?[0-9]+(?:\\|[0-9]+)*)\\+(-?[0-9]+(?"
                        ":\\|[0-9]+)*)\\)([0-9]+)[dn]?");
      if (!m.empty())
        b.edges.push_back({std::stoi(m[1]), std::stoi(m[4]), m[2], m[3]});
    }
  }
  if (rescount != 1 || lincount > 1)
    fail("Invalid GlycoCT sections");
  if (und && (pc != 1 || lc != 1 || b.parents.empty()))
    fail("Invalid UND metadata");
  return b;
}
struct CData {
  Record record;
  VI ids;
  std::vector<CResidue> mono;
};
static CData assemble(const CBlock &b, const Vocab &v) {
  CData d;
  std::map<int, CResidue> residues;
  for (auto &r : b.residues)
    residues[r.id] = r;
  for (auto &r : b.residues) {
    if (r.kind != "mono")
      continue;
    VS subs;
    for (auto &e : b.edges)
      if (e.from == r.id && residues.count(e.to) && residues[e.to].kind == "sub")
        subs.push_back(residues[e.to].content + "\r" + e.parent + "\r" + e.child);
    std::pair<S, S> m;
    if (subs.empty())
      m = {preserve_ring(base_mono(r.content, v), r.content, v), ""};
    else
      m = composite(r.content, subs, v);
    d.record.add(m.first, m.second);
    d.ids.push_back(r.id);
    d.mono.push_back(r);
  }
  if (d.ids.empty())
    fail("No GlycoCT monosaccharides");
  std::map<int, int> idx;
  for (size_t i = 0; i < d.ids.size(); ++i)
    idx[d.ids[i]] = i + 1;
  std::vector<CEdge> edges;
  for (auto &e : b.edges)
    if (idx.count(e.from) && idx.count(e.to))
      edges.push_back(e);
  int root = 0;
  for (size_t i = 0; i < d.ids.size(); ++i) {
    bool target = false;
    for (auto &e : edges)
      target |= e.to == d.ids[i];
    if (!target) {
      root = i;
      break;
    }
  }
  // Resolve the same symmetric Man-alditol orientation as the reference parser.
  if (d.mono[root].alditol && d.record.mono[root] == "Man" &&
      d.record.sub[root].empty()) {
    VI ps;
    bool unknown = false;
    for (auto &e : edges)
      if (e.from == d.ids[root])
        for (auto &p : split(e.parent, '|')) {
          if (!has(p, "^-?[0-9]+$")) {
            unknown = true;
            break;
          }
          int n = std::stoi(p);
          if (n >= 1 && n <= 6)
            ps.push_back(n);
        }
    if (!unknown && !ps.empty()) {
      VI reflected;
      for (int p : ps)
        reflected.push_back(7 - p);
      std::sort(ps.rbegin(), ps.rend());
      std::sort(reflected.rbegin(), reflected.rend());
      if (reflected > ps)
        for (auto &e : edges)
          if (e.from == d.ids[root]) {
            VS pos = split(e.parent, '|');
            for (auto &p : pos) {
              int n = std::stoi(p);
              if (n >= 1 && n <= 6)
                p = std::to_string(7 - n);
            }
            e.parent = join(pos, "|");
          }
    }
  }
  for (auto &e : edges) {
    const auto &r = d.mono[idx[e.to] - 1];
    d.record.edge(idx[e.from], idx[e.to],
                  (r.anomer == "x" || r.anomer == "o" ? "?" : r.anomer) +
                      position(e.child) + "-" + position(e.parent));
  }
  const auto &r = d.mono[root];
  S pos = r.position;
  if (r.alditol)
    pos = v.source_anomers.at(d.record.mono[root]);
  else if (pos.empty())
    pos = v.pos(d.record.mono[root]);
  else if (pos == "x")
    pos = v.anomers.count(d.record.mono[root]) ? "?" : v.pos(d.record.mono[root]);
  d.record.anomer =
      (r.alditol || r.anomer.empty() || r.anomer == "x" || r.anomer == "o" ? "?"
                                                                           : r.anomer) +
      pos;
  d.record.alditol = r.alditol;
  for (size_t i = 0; i < d.mono.size(); ++i)
    if ((int)i != root && d.mono[i].alditol)
      d.record.warn = true;
  return d;
}
Record glycoct_parse(const S &x, const Vocab &v) {
  VS lines;
  for (auto &s : split(x, '\n')) {
    S t = trim(s);
    if (!t.empty())
      lines.push_back(t);
  }
  if (lines.size() == 1 && has(lines[0], "^RES\\s+"))
    lines = tokens(lines[0], "\\S+");
  VS main;
  std::vector<VS> unds;
  bool in = false;
  for (auto &s : lines) {
    if (s == "UND") {
      in = true;
      continue;
    }
    if (!in)
      main.push_back(s);
    else if (has(s, "^UND[0-9]+:"))
      unds.push_back({s});
    else if (!unds.empty())
      unds.back().push_back(s);
  }
  if (in && unds.empty())
    fail("Missing UND block");
  auto md = assemble(block(main, false), v);
  Record r = md.record;
  std::map<int, int> mainids;
  for (size_t i = 0; i < md.ids.size(); ++i)
    mainids[md.ids[i]] = i + 1;
  struct Pending {
    CBlock block;
    CData data;
    int offset;
  };
  std::vector<Pending> parts;
  std::vector<CBlock> subs;
  for (auto &u : unds) {
    auto b = block(u, true);
    if (b.residues.size() == 1 && b.residues[0].kind == "sub") {
      subs.push_back(b);
      continue;
    }
    auto d = assemble(b, v);
    int offset = r.mono.size();
    for (size_t i = 0; i < d.record.mono.size(); ++i)
      r.add(d.record.mono[i], d.record.sub[i]);
    for (size_t i = 0; i < d.record.linkage.size(); ++i)
      r.edge(offset + d.record.edges[2 * i], offset + d.record.edges[2 * i + 1],
             d.record.linkage[i]);
    for (auto &a : d.mono)
      r.warn |= a.alditol;
    parts.push_back({b, d, offset});
  }
  auto parents = [&](const VI &ids) {
    VI p;
    for (int id : ids) {
      if (!mainids.count(id))
        fail("Unknown GlycoCT UND parent");
      p.push_back(mainids[id]);
    }
    return unique(p);
  };
  auto occ = occupied(md.record), carbon = occupied(md.record, true);
  for (auto &p : parts) {
    auto roots = p.data.record.roots();
    if (roots.size() != 1)
      fail("Invalid UND subtree root");
    int root = roots[0];
    auto &a = p.data.mono[root - 1];
    S l = (a.anomer == "x" || a.anomer == "o" ? "?" : a.anomer) +
          position(p.block.child) + "-" + position(p.block.parent);
    VI nodes = seq(p.data.record.mono.size());
    for (int &i : nodes)
      i += p.offset;
    VI domain = normalize_parents(parents(p.block.parents),
                                  diff(seq(r.mono.size()), nodes), l, occ);
    r.parts.push_back({p.offset + root, nodes, l, domain});
  }
  for (auto &b : subs) {
    S pos = position(b.parent);
    if (pos.find('?') != S::npos)
      pos = "?";
    r.floats.push_back(normalize_sub(parents(b.parents), seq(r.mono.size()),
                                     pos + cabbrev(b.residues[0].content), carbon));
  }
  return r;
}

Rcpp::RObject glycoct_aux(const S &op, Rcpp::List a, const Vocab &v) {
  if (op == "base")
    return Rcpp::wrap(base_mono(Rcpp::as<S>(a["x"]), v));
  if (op == "signature") {
    S x = Rcpp::as<S>(a["x"]), b = bounds(x);
    return Rcpp::List::create(Rcpp::Named("mono") = x, Rcpp::Named("core") = core(x),
                              Rcpp::Named("bounds") =
                                  b.empty() ? Rcpp::CharacterVector::create(NA_STRING)
                                            : Rcpp::CharacterVector::create(b),
                              Rcpp::Named("wildcard_bounds") = b.find('x') != S::npos,
                              Rcpp::Named("alditol") = x.find("|1:aldi") != S::npos);
  }
  if (op == "exact") {
    Rcpp::List sig = a["signature"];
    VS subs;
    if (sig.containsElementNamed("substituents"))
      subs = Rcpp::as<VS>(sig["substituents"]);
    else {
      VS names = Rcpp::as<VS>(sig["subs"]), links = Rcpp::as<VS>(sig["linkages"]);
      if (names.size() != links.size())
        return Rcpp::RObject(R_NilValue);
      for (size_t i = 0; i < names.size(); ++i)
        subs.push_back(names[i] + "\r" + replace(links[i], "+", "\r"));
    }
    S result = exact_mono(Rcpp::as<S>(sig["mono"]), subs, glycoct_entries);
    return result.empty() ? Rcpp::RObject(R_NilValue)
                          : Rcpp::RObject(Rcpp::wrap(result));
  }
  if (op == "und") {
    VS lines = Rcpp::as<VS>(a["lines"]);
    for (auto &s : lines)
      if (has(s, "^SubtreeLinkageID[0-9]+:")) {
        S attach = sub(s, "^SubtreeLinkageID[0-9]+:");
        auto m = match(attach, "\\+([^)]*)");
        if (!m.empty() && m[1].find('|') != S::npos)
          fail("GlycoCT UND linkages with alternative donor positions are not "
               "supported: \"" +
               attach + "\"");
      }
    auto b = block(lines, true);
    return Rcpp::List::create(Rcpp::Named("parent_ids") = b.parents,
                              Rcpp::Named("parent_pos") = b.parent,
                              Rcpp::Named("child_pos") = b.child);
  }
  fail("Unknown GlycoCT operation");
  return Rcpp::List();
}
} // namespace gp
