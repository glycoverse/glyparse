#include "native.h"
namespace gp {
struct GNode {
  S kind, mono, sub, anomer, ring, pos;
  std::vector<GNode> children;
};
static GNode gnode(const S &token, const Vocab &v) {
  auto m = match(token, "^([abo?][1-9N?])?([DL]-)?([A-Za-z0-9_#=.]+)(?:,([?opf]))?$");
  if (m.empty())
    fail("Invalid GlycoWorkbench residue");
  S s = m[3], subname = s == "NS" ? "S" : (contains(v.subs, s) ? s : "");
  S kind = "mono";
  if (m[1].empty() && m[2].empty() && m[4].empty()) {
    if (s == "U")
      kind = "ulosonic";
    else if (!subname.empty())
      kind = "sub";
    else if (s == "m")
      kind = "deoxy";
  }
  if (starts(m[1], "o"))
    fail("Open-chain GlycoWorkbench residue");
  S mono;
  if (kind == "mono") {
    mono = s == "NeuAc" ? "Neu5Ac" : s == "NeuGc" ? "Neu5Gc" : s;
    if (m[4] == "f")
      mono = v.furan(mono);
    mono = v.config(mono, m[2].substr(0, m[2].empty() ? 0 : 1));
    if (!contains(v.monos, mono))
      fail("Unknown GlycoWorkbench residue");
  }
  return {kind, mono, subname, replace(m[1], "N", "?"), m[4], "", {}};
}
static GNode linked(const S &, size_t &, const Vocab &, int);
static std::vector<GNode> children(const S &x, size_t &p, const Vocab &v, int depth) {
  int n = 0;
  while (p < x.size() && x[p] == '(') {
    ++n;
    ++p;
  }
  std::vector<GNode> out;
  for (int i = 0; i < n; ++i) {
    out.push_back(linked(x, p, v, depth + 1));
    if (p >= x.size() || x[p++] != ')')
      fail("Unclosed GlycoWorkbench branch");
  }
  if (x.compare(p, 2, "--") == 0)
    out.push_back(linked(x, p, v, depth + 1));
  return out;
}
static GNode linked(const S &x, size_t &p, const Vocab &v, int depth) {
  if (depth > 1024)
    fail("GlycoWorkbench nesting limit");
  auto m =
      match(x.substr(p), "^--((?:(?:[1-9N?]/)*[1-9N?]=[1-9N?],)*(?:[1-9N?]/)*[1-9N?])");
  if (m.empty() || has(m[1], "[,=]"))
    fail("Invalid or multi-bond GlycoWorkbench linkage");
  p += m[0].size();
  size_t end = p;
  while (end < x.size() && x[end] != '(' && x[end] != ')' &&
         x.compare(end, 2, "--") != 0)
    ++end;
  auto node = gnode(x.substr(p, end - p), v);
  p = end;
  node.pos = replace(m[1], "N", "?");
  node.children = children(x, p, v, depth);
  return node;
}
static S gformat(const GNode &, const Vocab &, bool = false, bool = false);
static S childformat(const GNode &n, const Vocab &v) {
  if (n.kind != "mono")
    fail("Unexpected GlycoWorkbench substituent");
  return gformat(n, v) + "(" + (n.anomer.empty() ? "?" + v.pos(n.mono) : n.anomer) +
         "-" + n.pos + ")";
}
static S gformat(const GNode &n, const Vocab &v, bool root, bool alditol) {
  if (n.kind != "mono" || (n.ring == "o" && !(root && alditol)))
    fail("Invalid GlycoWorkbench root");
  S mono = n.mono, subs;
  VS arms;
  int deoxy = 0, ulo = 0;
  for (auto &c : n.children) {
    if (c.kind == "sub") {
      if (!c.children.empty())
        fail("Nonterminal substituent");
      subs += c.pos + c.sub;
    } else if (c.kind == "deoxy") {
      if (++deoxy > 1 || !c.children.empty() || mono != "Hex" || c.pos != "6")
        fail("Unsupported deoxy modification");
      mono = "dHex";
    } else if (c.kind == "ulosonic") {
      if (++ulo > 1 || !c.children.empty() || (mono != "Fru" && mono != "Kdn") ||
          c.pos != "2")
        fail("Unsupported ulosonic modification");
      alditol = alditol || mono == "Fru";
    } else
      arms.push_back(childformat(c, v));
  }
  S prefix;
  for (size_t i = 0; i < arms.size(); ++i)
    prefix += i ? "[" + arms[i] + "]" : arms[i];
  S out = prefix + mono + subs;
  if (!root)
    return out;
  if (alditol)
    return out + "-ol(?" + v.pos(mono) + "-";
  return out + (n.anomer.empty() ? "" : "(" + n.anomer + "-");
}
static void collect(const GNode &n, std::vector<const GNode *> &out) {
  if (n.kind != "mono")
    return;
  for (auto &c : n.children)
    collect(c, out);
  out.push_back(&n);
}
S gwb_convert(const S &source, const Vocab &v) {
  S x = source.substr(0, source.find('$'));
  S marker = starts(x, "freeEnd") ? "freeEnd" : starts(x, "redEnd") ? "redEnd" : "";
  if (marker.empty())
    fail("Missing GlycoWorkbench reducing marker");
  auto b = x.find('}');
  S main = x.substr(0, b);
  size_t p = marker.size();
  auto tree = linked(main, p, v, 0);
  if (p != main.size())
    fail("Trailing GlycoWorkbench content");
  S out = gformat(tree, v, true, marker == "redEnd");
  if (b == S::npos)
    return out;
  p = b + 1;
  auto floating = children(x, p, v, 0);
  if (floating.empty() || p != x.size())
    fail("Invalid uncertain antenna container");
  int size = 0, parts = 0;
  for (auto &n : floating) {
    std::vector<const GNode *> ns;
    collect(n, ns);
    size += ns.size();
    parts += !ns.empty();
  }
  std::vector<const GNode *> ns;
  collect(tree, ns);
  S prefix;
  for (auto &n : floating) {
    VI parents;
    VS positions = split(n.pos, '/');
    for (size_t i = 0; i < ns.size(); ++i) {
      VS occ;
      for (auto &c : ns[i]->children) {
        auto ps = split(c.pos, '/');
        if (ps.size() == 1 && ps[0] != "?")
          occ.push_back(ps[0]);
      }
      bool ok = contains(positions, S("?"));
      for (auto &pos : positions)
        ok |= !contains(occ, pos);
      if (ok)
        parents.push_back(size + i + 1);
    }
    if (parents.empty())
      fail("No feasible uncertain antenna parent");
    bool explicitp =
        parents.size() != ns.size() || (n.kind == "mono" ? parts > 1 : parts > 0);
    S token;
    if (n.kind == "sub") {
      if (!n.children.empty())
        fail("Nonterminal floating substituent");
      token = n.pos + n.sub;
    } else if (n.kind == "mono")
      token = childformat(n, v);
    else
      fail("Invalid floating modification");
    if (explicitp) {
      VS ids;
      for (auto id : parents)
        ids.push_back(std::to_string(id));
      token += "|" + join(ids, ",");
    }
    prefix += "{" + token + "}";
  }
  return prefix + out;
}
} // namespace gp
