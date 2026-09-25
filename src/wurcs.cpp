#include "native.h"
namespace gp {
static WResidue parse_residue(const S &source, const Vocab &v) {
  S x = sub(sub(source, "(-1[abx]_1)-4", "$1-5"), "(-2[abx]_2)-5", "$1-6");
  bool furan = has(source, "-(?:1[abx]_1-4|2[abx]_2-5)(?:_|$)");
  S mono, pattern, anomer;
  int family = 0;
  for (auto table : {&wurcs_table, &wurcs_unknown_table, &wurcs_alditol_table,
                     &wurcs_ambiguous_table}) {
    for (auto &p : *table)
      if (has(x, p.second)) {
        mono = p.first;
        pattern = p.second;
        break;
      }
    if (!mono.empty())
      break;
    ++family;
  }
  if (mono.empty())
    fail("Unable to parse residue: \"" + source + "\"");
  bool alditol = family == 2;
  if (family < 2) {
    auto a = match(source, "-([0-9]+)([abx])_");
    if (a.empty())
      fail("Missing WURCS anomer");
    anomer = (a[2] == "x" ? "?" : a[2]) + a[1];
  } else if (alditol)
    anomer = "?" + v.source_anomers.at(mono);
  else
    anomer = contains(VS{"Hex", "HexNAc", "HexN"}, mono) ? "?" + v.pos(mono) : "??";
  S identity = (starts(mono, "D-") || starts(mono, "L-")) ? mono.substr(2) : mono, code;
  if (contains(VS{"Neu5Ac", "Neu5Gc", "Neu"}, identity) &&
      ((!alditol && starts(x, "Aad")) || starts(x, "AUd"))) {
    auto backbone = match(pattern, "^\\^([[:alnum:]]+)");
    if (backbone.empty())
      fail("Invalid WURCS backbone");
    S pat = "^" + backbone[1] + (starts(x, "Aad") ? "-2[abx]_2-(?:6|\\?)" : "");
    code = sub(x, pat);
    code = sub(code, identity == "Neu5Ac"   ? "_5\\*NCC/3=O"
                     : identity == "Neu5Gc" ? "_5\\*NCCO/3=O"
                                            : "_5\\*N(?!CC(O)?/3=O)");
  } else {
    code = sub(x, pattern);
    if (contains(VS{"NeuAc", "NeuGc", "gNeu"}, mono))
      code = sub(code, mono == "NeuAc"   ? "_5\\*NCC/3=O"
                       : mono == "NeuGc" ? "_5\\*NCCO/3=O"
                                         : "_5\\*N(?!CC(O)?/3=O)");
  }
  auto ns = match(source, "_([0-9]+|\\?)\\*NSO/3=O/3=O");
  if (!ns.empty() && starts(code, "SO/3=O/3=O"))
    code = sub(code, "^SO/3=O/3=O", "_" + ns[1] + "*OSO/3=O/3=O");
  VS subs;
  for (auto &s : split(code, '_', false)) {
    bool found = false;
    for (auto &p : wurcs_sub_table) {
      auto m = match("_" + s, "^_((?:[0-9]+(?:\\|[0-9]+)*)|\\?)\\*" + p.second + "$");
      if (!m.empty()) {
        subs.push_back(replace(m[1], "|", "/") + p.first);
        found = true;
        break;
      }
    }
    if (!found)
      fail("Unable to parse substituent: \"_" + s + "\"");
  }
  return {furan ? v.furan(mono) : mono, anomer, join(subs, ","), alditol};
}
// Cache descriptors within one batch, never across vocabulary instances.
static WResidue wres(const S &source, const Vocab &v) {
  auto found = v.wurcs_cache.find(source);
  if (found != v.wurcs_cache.end())
    return found->second;
  auto residue = parse_residue(source, v);
  v.wurcs_cache.emplace(source, residue);
  return residue;
}
static int letter(char c) {
  if (c >= 'a' && c <= 'z')
    return c - 'a' + 1;
  if (c >= 'A' && c <= 'Z')
    return c - 'A' + 27;
  fail("Invalid WURCS residue ID");
  return 0;
}
static std::pair<int, S> endpoint(const S &s, int n) {
  if (s.empty())
    fail("Empty WURCS endpoint");
  int id = letter(s[0]);
  if (id > n)
    fail("Unknown WURCS endpoint");
  return {id, s.substr(1)};
}
static bool donor(const S &s, const std::vector<WResidue> &res) {
  auto alts = split(s, '|');
  auto e = endpoint(alts[0], res.size());
  if (res[e.first - 1].alditol)
    return false;
  S pos = res[e.first - 1].anomer.substr(1);
  bool allunknown = true, matched = false;
  for (auto &a : alts) {
    auto p = endpoint(a, res.size()).second;
    matched |= pos != "?" && p == pos;
    allunknown &= p == "?";
  }
  return allunknown || matched;
}
static std::pair<int, S> parallel(const S &s, int n) {
  auto alts = split(s, '|');
  VS ps;
  for (auto &a : alts)
    ps.push_back(endpoint(a, n).second);
  return {endpoint(alts[0], n).first, contains(ps, S("?")) ? "?" : join(ps, "/")};
}
static std::pair<VI, VS> candidates(const S &s, int n) {
  VI parents;
  std::map<int, VS> positions;
  for (auto &a : split(s, '|')) {
    auto e = endpoint(a, n);
    parents.push_back(e.first);
    positions[e.first].push_back(e.second);
  }
  VS common;
  bool first = true;
  for (auto &p : positions) {
    auto ps = unique(p.second);
    std::sort(ps.begin(), ps.end());
    if (!first && common != ps)
      fail("Parent-specific WURCS positions unsupported");
    first = false;
    common = ps;
  }
  return {unique(parents), common};
}
static S collapse(const VS &ps) {
  if (contains(ps, S("?")) || contains(ps, S("-1")))
    return "?";
  return join(unique(ps), "/");
}
Record wurcs_parse(const S &x, const Vocab &v) {
  auto m = match(x, "^WURCS=2\\.0/[0-9]+,[0-9]+,[0-9]+/((?:\\[.*?\\])+)/"
                    "((?:[0-9]+-)*[0-9]+)(?:/(.*))?");
  if (m.empty())
    fail("Invalid WURCS");
  std::vector<WResidue> uniq;
  for (auto &a : tokens(m[1], "\\[.*?\\]"))
    uniq.push_back(wres(a.substr(1, a.size() - 2), v));
  std::vector<WResidue> residues;
  Record r;
  for (auto &s : split(m[2], '-')) {
    int i = std::stoi(s);
    if (i < 1 || i > (int)uniq.size())
      fail("Invalid WURCS residue sequence");
    auto a = uniq[i - 1];
    residues.push_back(a);
    r.add(a.mono, a.sub);
  }
  struct FP {
    int root;
    S donor;
    VI parents;
    VS positions;
  };
  std::vector<FP> floats;
  std::vector<FloatingSub> subs;
  for (auto l : split(m[3], '_', false)) {
    if (l.find('}') == S::npos) {
      auto e = split(l, '-');
      if (e.size() != 2)
        fail("Invalid WURCS linkage");
      bool left = donor(e[0], residues), right = donor(e[1], residues);
      if ((left && !right) || (!left && !right && e[1].find('|') != S::npos))
        std::swap(e[0], e[1]);
      auto a = parallel(e[0], residues.size()), b = parallel(e[1], residues.size());
      r.edge(a.first, b.first,
             residues[b.first - 1].anomer.substr(0, 1) + b.second + "-" + a.second);
      continue;
    }
    auto b = split(l, '}');
    if (b.size() != 2)
      fail("Invalid floating WURCS linkage");
    if (starts(b[1], "*")) {
      auto c = candidates(b[0], residues.size());
      S code = b[1].substr(1);
      if (code == "NSO/3=O/3=O")
        code = "OSO/3=O/3=O";
      S name;
      for (auto &p : wurcs_sub_table)
        if (has(code, "^(?:" + p.second + ")$")) {
          name = p.first;
          break;
        }
      if (name.empty())
        fail("Unknown floating WURCS substituent");
      subs.push_back({collapse(c.second) + name, c.first});
    } else {
      if (!b[1].empty())
        fail("Invalid floating WURCS suffix");
      auto e = split(b[0], '-');
      if (e.size() != 2)
        fail("Invalid floating WURCS linkage");
      auto child = endpoint(e[0], residues.size());
      auto c = candidates(e[1], residues.size());
      floats.push_back({child.first, child.second, c.first, c.second});
    }
  }
  VI comps = seq(r.mono.size());
  std::function<int(int)> root = [&](int i) {
    while (comps[i - 1] != i)
      i = comps[i - 1];
    return i;
  };
  for (size_t i = 0; i < r.edges.size(); i += 2)
    comps[root(r.edges[i + 1]) - 1] = root(r.edges[i]);
  auto occ = occupied(r);
  VI floatingroots;
  for (auto &f : floats) {
    VI nodes;
    int comp = root(f.root);
    for (int i : seq(r.mono.size()))
      if (root(i) == comp)
        nodes.push_back(i);
    auto parents = diff(f.parents, nodes);
    if (parents.empty())
      fail("No external WURCS candidate");
    S link = residues[f.root - 1].anomer.substr(0, 1) + f.donor + "-" +
             join(f.positions, "/");
    parents = normalize_parents(parents, diff(seq(r.mono.size()), nodes), link, occ);
    r.parts.push_back({f.root, nodes, link, parents});
    floatingroots.push_back(f.root);
  }
  auto carbon = occupied(r, true);
  for (auto &s : subs)
    r.floats.push_back(
        normalize_sub(s.parents, seq(r.mono.size()), s.substituent, carbon));
  auto roots = diff(r.roots(), floatingroots);
  if (roots.size() != 1)
    fail("Missing main WURCS root");
  int core = roots[0] - 1;
  r.anomer = residues[core].anomer;
  r.alditol = residues[core].alditol;
  for (size_t i = 0; i < residues.size(); ++i)
    if ((int)i != core && residues[i].alditol)
      r.warn = true;
  return r;
}

Rcpp::RObject wurcs_aux(const S &op, Rcpp::List a, const Vocab &v) {
  S x = Rcpp::as<S>(a["x"]);
  if (op == "residue") {
    auto r = wres(x, v);
    return Rcpp::CharacterVector::create(Rcpp::Named("mono") = r.mono,
                                         Rcpp::Named("anomer") = r.anomer,
                                         Rcpp::Named("sub") = r.sub);
  }
  if (op == "letter")
    return Rcpp::wrap(letter(x.at(0)));
  if (op == "invert_pattern") {
    auto m = match(x, "^\\^([[:alnum:]]+)");
    if (m.empty())
      fail("Invalid WURCS pattern");
    S b = m[1];
    for (char &c : b) {
      if (c == '1')
        c = '2';
      else if (c == '2')
        c = '1';
    }
    return Rcpp::wrap(sub(x, "^\\^" + m[1], "^" + b));
  }
  if (op == "linkage") {
    auto e = split(x, '-');
    if (e.size() != 2)
      fail("Invalid WURCS linkage");
    bool swap = e[1].find('|') != S::npos;
    int n = 52;
    if (a.containsElementNamed("anomers") && !Rf_isNull(a["anomers"])) {
      VS anomers = Rcpp::as<VS>(a["anomers"]);
      Rcpp::LogicalVector alditols(a["alditols"]);
      std::vector<WResidue> res;
      for (size_t i = 0; i < anomers.size(); ++i)
        res.push_back(
            {"", anomers[i], "", alditols.size() > (int)i && alditols[i] == TRUE});
      n = res.size();
      bool l = donor(e[0], res), r = donor(e[1], res);
      swap = (l && !r) || (!l && !r && swap);
    }
    if (swap)
      std::swap(e[0], e[1]);
    auto p = parallel(e[0], n), q = parallel(e[1], n);
    return Rcpp::List::create(Rcpp::Named("from") = p.first,
                              Rcpp::Named("to") = q.first,
                              Rcpp::Named("linkage") = q.second + "-" + p.second);
  }
  if (op == "floating") {
    auto b = split(x, '}');
    if (b.size() != 2)
      fail("Invalid floating WURCS linkage");
    if (starts(b[1], "*")) {
      auto c = candidates(b[0], 52);
      S code = b[1].substr(1);
      if (code == "NSO/3=O/3=O")
        code = "OSO/3=O/3=O";
      S name;
      for (auto &p : wurcs_sub_table)
        if (has(code, "^(?:" + p.second + ")$")) {
          name = p.first;
          break;
        }
      if (name.empty())
        fail("Unknown floating WURCS substituent");
      return Rcpp::List::create(
          Rcpp::Named("type") = "substituent",
          Rcpp::Named("metadata") =
              Rcpp::List::create(Rcpp::Named("substituent") = collapse(c.second) + name,
                                 Rcpp::Named("parents") = c.first));
    }
    auto es = split(b[0], '-');
    if (es.size() != 2 || !b[1].empty())
      fail("Invalid floating WURCS linkage");
    auto child = endpoint(es[0], 52);
    auto c = candidates(es[1], 52);
    return Rcpp::List::create(Rcpp::Named("type") = "part",
                              Rcpp::Named("metadata") = Rcpp::List::create(
                                  Rcpp::Named("root") = child.first,
                                  Rcpp::Named("child_position") = child.second,
                                  Rcpp::Named("parent_positions") = c.second,
                                  Rcpp::Named("parents") = c.first));
  }
  fail("Unknown WURCS operation");
  return Rcpp::List();
}
} // namespace gp
