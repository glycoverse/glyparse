#include "native.h"
namespace gp {
static S alias(S x) {
  x = replace(x, "Neu5,9Ac2", "Neu5Ac9Ac");
  x = replace(x, "NeuAc", "Neu5Ac");
  return replace(x, "NeuGc", "Neu5Gc");
}
static S terminal_pattern(const Vocab &v) {
  return "(" + alternatives(longest(v.monos)) +
         ")((?:[0-9?](?:" + alternatives(longest(v.subs)) + "))*)(-ol)?([ab?])?$";
}
static S compact(S x, const Vocab &v) {
  x = alias(x);
  S p = "\\(([0-9?]*(?:" + alternatives(longest(v.subs)) + "))\\)(" +
        alternatives(longest(v.monos)) + ")";
  for (;;) {
    S y = rewrite(x, p, [](const VS &m) {
      return m[2] + (std::isalpha((unsigned char)m[1][0]) ? "?" : "") + m[1];
    });
    if (x == y)
      break;
    x = y;
  }
  x = replace(replace(x, "(", "["), ")", "]");
  x = replace(x, "|", "/");
  x = rewrite(x, "([ab?])([0-9?])-([0-9?](?:/[0-9?])*)",
              [](const VS &m) { return "(" + m[1] + m[2] + "-" + m[3] + ")"; });
  if (ends(x, "+aldi")) {
    x.resize(x.size() - 5);
    x = rewrite(x, terminal_pattern(v),
                [](const VS &m) { return m[1] + m[2] + "-ol"; });
  }
  return rewrite(x, terminal_pattern(v), [&](const VS &m) {
    return m[1] + m[2] + m[3] + "(" + (m[4].empty() ? "?" : m[4]) + v.pos(m[1]) + "-";
  });
}
static S short_iupac(const S &x, const Vocab &v) {
  S mono = "(?:" + alternatives(v.monos) +
           ")(?:(?:[0-9]+(?:/[0-9]+)*|\\?)(?:" + alternatives(v.subs) + "))*";
  S residue = "(" + mono + ")([ab?])-?([0-9]+(?:/[0-9]+)*|\\?)";
  S terminal = "(" + mono + ")([ab?])-$";
  auto last = match(x, terminal);
  if (last.empty())
    fail("Invalid IUPAC-short terminal");
  VS ts = tokens(x, residue + "|\\(|\\)");
  if (join(ts) + last[0] != x)
    fail("Invalid IUPAC-short tokens");
  S out;
  for (auto &t : ts) {
    if (t == "(")
      out += "[";
    else if (t == ")")
      out += "]";
    else {
      auto m = match(t, residue);
      out += m[1] + "(" + m[2] + v.pos(m[1]) + "-" + m[3] + ")";
    }
  }
  return out + last[1] + "(" + last[2] + v.pos(last[1]) + "-";
}
static S extended(S x, const Vocab &v) {
  x = replace(replace(x, "alpha", "\xCE\xB1"), "beta", "\xCE\xB2");
  x = replace(x, "\xE2\x86\x92", "->");
  S greek = "(\xCE\xB1|\xCE\xB2|\\?)";
  S pos = "(?:[0-9]+|\\?)(?:/(?:[0-9]+|\\?))*";
  S regular = "-?" + greek + "-([DL?])-([[:alnum:]?]+?)-\\((" + pos + ")->(?:(" + pos +
              ")\\))?";
  S hep = "-?([DL])-gro-" + greek + "-D-manHepp([[:alnum:]?]*?)-\\((" + pos +
          ")->(?:(" + pos + ")\\))?";
  VS ts = tokens(x, hep + "|" + regular + "|\\?-(?:HexNAc|HexN|Hex)$|\\[|\\]");
  S out;
  for (S t : ts) {
    if (t == "[" || t == "]") {
      out += t;
      continue;
    }
    if (has(t, "^\\?-(HexNAc|HexN|Hex)$")) {
      out += t.substr(2) + "(?1-";
      continue;
    }
    auto h = match(t, hep);
    if (!h.empty())
      t = h[2] + "-D-" + h[1] + "DmanHep" + h[3] + "-(" + h[4] + "->" +
          (h[5].empty() ? "" : h[5] + ")");
    auto m = match(t, regular);
    if (m.empty())
      fail("Invalid extended residue");
    S key, val;
    for (auto &p : extended_table)
      if (m[3].find(p.first) != S::npos && p.second.size() > val.size()) {
        key = p.first;
        val = p.second;
      }
    if (key.empty())
      fail("Unknown extended residue");
    S mono = m[3];
    mono.replace(mono.find(key), key.size(), val);
    mono = v.config(mono, m[2]);
    out += mono + "(" +
           (m[1] == "\xCE\xB1"   ? "a"
            : m[1] == "\xCE\xB2" ? "b"
                                 : "?") +
           m[4] + "-" + (m[5].empty() ? "" : m[5] + ")");
  }
  return out;
}
static S glycam(const S &x) {
  S mod = "\\[[0-9?][A-Za-z]+(?:,[0-9?][A-Za-z]+)*\\]";
  S residue =
      "([A-Za-z0-9]+?)(?:" + mod + ")*([ab?])([0-9?])-([0-9?]|[A-Za-z][A-Za-z0-9]*)";
  VS ts = tokens(x, residue + "|\\[|\\]");
  if (ts.empty() || join(ts) != x)
    fail("Invalid GlyCAM tokens");
  S out;
  for (auto &t : ts) {
    if (t == "[" || t == "]") {
      out += t;
      continue;
    }
    auto m = match(t, "^" + residue + "$");
    S mono = lookup(glycam_table, m[1]);
    if (mono.empty())
      fail("Unknown GlyCAM residue");
    S mods;
    for (auto a : tokens(t, mod))
      for (auto b : split(a.substr(1, a.size() - 2), ','))
        mods += sub(b, "A$", "Ac");
    out += mono + mods + "(" + m[2] + m[3] + "-" +
           (has(m[4], "^[A-Za-z]") ? "" : m[4] + ")");
  }
  return out;
}
static S linear(S x, const Vocab &v) {
  Pairs maps = {{"Glc", "G"},  {"Gal", "A"},     {"GlcNAc", "GN"}, {"GalNAc", "AN"},
                {"Man", "M"},  {"Neu5Ac", "NN"}, {"Neu", "N"},     {"Kdn", "K"},
                {"Kdo", "W"},  {"GalA", "L"},    {"Ido", "I"},     {"Rha", "H"},
                {"Fuc", "F"},  {"Xyl", "X"},     {"Rib", "B"},     {"Ara", "R"},
                {"GlcA", "U"}, {"All", "O"},     {"Api", "P"},     {"Fru", "E"}};
  auto base = maps;
  for (auto &p : base)
    if (v.furan(p.first) != p.first)
      maps.push_back({v.furan(p.first), p.second + "^"});
  x = replace(replace(x, "[", "_"), "]", "_");
  x = replace(replace(x, "(", "["), ")", "]");
  x = rewrite(x, "([ab?])([0-9]+(?:/[0-9]+)*|\\?)",
              [](const VS &m) { return "(" + m[1] + "1-" + m[2] + ")"; });
  if (x.empty())
    fail("Empty Linear Code");
  S red = x.substr(x.size() - 1);
  x.resize(x.size() - 1);
  x += "(" + red + "1-";
  for (auto &p : maps)
    x = rewrite(x, "(^|[^[:alnum:]?])" + escape(p.second) + "(?=[_(])",
                [&](const VS &m) { return m[1] + p.first; });
  for (auto &p : maps)
    x = rewrite(x, "(^|[^[:alnum:]])(" + escape(p.first) + ")(_.*?_)?\\(([ab?])1-",
                [&](const VS &m) {
                  return m[1] + m[2] + m[3] + "(" + m[4] + v.pos(p.first) + "-";
                });
  for (auto &p : Pairs{{"NAc", "N"}, {"Me", "ME"}, {"Ac", "T"}, {"P", "P"}, {"S", "S"}})
    x = rewrite(x, "_([0-9]+|\\?)" + p.second + "_",
                [&](const VS &m) { return m[1] + p.first; });
  return x;
}
S convert(const S &x, const S &format, const Vocab &v) {
  if (format == "iupac_short")
    return short_iupac(x, v);
  if (format == "iupac_extended")
    return extended(x, v);
  if (format == "iupac_compact")
    return compact(x, v);
  if (format == "glycam_iupac")
    return glycam(x);
  if (format == "linear_code")
    return linear(x, v);
  if (format == "gwb")
    return gwb_convert(x, v);
  if (format == "iupac_condensed")
    return x;
  fail("Unknown converter");
  return "";
}
S detect(const S &x, const Vocab &v) {
  if (starts(x, "freeEnd") || starts(x, "redEnd"))
    return "parse_gwb";
  if (x.find("ENTRY") != S::npos)
    return "parse_kcf";
  if (x.find("RES") != S::npos)
    return "parse_glycoct";
  if (x.find("WURCS") != S::npos)
    return "parse_wurcs";
  if (has(x, "^\\([HNAGFSap]"))
    return "parse_pglyco_struc";
  if (starts(x, "A") && ends(x, "a"))
    return "parse_strucgp_struc";
  if (ends(x, "-OH"))
    return "parse_glycam_iupac";
  if (ends(x, "-ol"))
    return "parse_iupac_condensed";
  if (x.find("\xE2\x86\x92") != S::npos || x.find("->") != S::npos ||
      has(x, "alpha|beta"))
    return "parse_iupac_extended";
  if (has(x, "\\w+\\([ab?][0-9?]-"))
    return "parse_iupac_condensed";
  if (has(x, "^\\[\\]\\[[^\\]]+\\]\\{"))
    return "parse_linucs";
  if (ends(x, "+aldi") || has(x, "([ab?])([0-9?])-([0-9?](?:/[0-9?])*)") ||
      has(alias(x), terminal_pattern(v)))
    return "parse_iupac_compact";
  if (ends(x, "-"))
    return "parse_iupac_short";
  return "parse_linear_code";
}
} // namespace gp
