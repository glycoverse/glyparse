#include "native.h"
namespace gp {
static GNode decode_gnode(const S &token, const Vocab &v) {
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
static GNode gnode(const S &token, const Vocab &v) {
  auto found = v.gwb_cache.find(token);
  if (found != v.gwb_cache.end())
    return found->second;
  return v.gwb_cache.emplace(token, decode_gnode(token, v)).first->second;
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
  auto position = [](char c) { return (c >= '1' && c <= '9') || c == 'N' || c == '?'; };
  if (x.compare(p, 2, "--") != 0 || p + 2 >= x.size() || !position(x[p + 2]))
    fail("Invalid or multi-bond GlycoWorkbench linkage");
  p += 2;
  size_t start = p++;
  while (p + 1 < x.size() && x[p] == '/' && position(x[p + 1])) p += 2;
  if (p < x.size() && (x[p] == '=' || x[p] == ','))
    fail("Invalid or multi-bond GlycoWorkbench linkage");
  S attachment = replace(x.substr(start, p - start), "N", "?");
  size_t end = p;
  while (end < x.size() && x[end] != '(' && x[end] != ')' &&
         x.compare(end, 2, "--") != 0)
    ++end;
  auto node = gnode(x.substr(p, end - p), v);
  p = end;
  node.pos = attachment;
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
static void gformat_into(const GNode &n, const Vocab &v, bool root, bool alditol, S& out) {
  if (n.kind != "mono" || (n.ring == "o" && !(root && alditol)))
    fail("Invalid GlycoWorkbench root");
  S mono = n.mono, subs;
  std::vector<const GNode*> arms;
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
      arms.push_back(&c);
  }
  for (size_t i = 0; i < arms.size(); ++i) {
    const auto& child = *arms[i];
    if (i) out += "[";
    gformat_into(child, v, false, false, out);
    out += "(" + (child.anomer.empty() ? "?" + v.pos(child.mono) : child.anomer) + "-" + child.pos + ")";
    if (i) out += "]";
  }
  out += mono + subs;
  if (!root) return;
  if (alditol) out += "-ol(?" + v.pos(mono) + "-";
  else if (!n.anomer.empty()) out += "(" + n.anomer + "-";
}
static S gformat(const GNode& n, const Vocab& v, bool root, bool alditol) {
  S out;
  out.reserve(128);
  gformat_into(n, v, root, alditol, out);
  return out;
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
    int positions = 0;
    bool unknown = n.pos.find('?') != S::npos;
    for (char c : n.pos) if (c >= '1' && c <= '9') positions |= 1 << (c-'0');
    for (size_t i = 0; i < ns.size(); ++i) {
      int occupied = 0;
      for (const auto& c : ns[i]->children)
        if (c.pos.size() == 1 && c.pos[0] >= '1' && c.pos[0] <= '9') occupied |= 1 << (c.pos[0]-'0');
      if (unknown || (positions & ~occupied)) parents.push_back(size+i+1);
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
