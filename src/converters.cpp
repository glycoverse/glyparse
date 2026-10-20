#include "native.h"
namespace gp {
static S alias(S x) {
  x = replace(x, "Neu5,9Ac2", "Neu5Ac9Ac");
  x = replace(x, "NeuAc", "Neu5Ac");
  return replace(x, "NeuGc", "Neu5Gc");
}
static const S &terminal_pattern(const Vocab &v) {
  return v.cached("compact-terminal", [&] {
    return "(" + alternatives(v.sorted_monos()) +
           ")((?:[0-9?](?:" + alternatives(longest(v.subs)) + "))*)(-ol)?([ab?])?$";
  });
}
static S terminal_suffix(const S &x) {
  // No symbol in the terminal vocabulary can cross these delimiters.
  auto boundary = x.find_last_of("()[]");
  return boundary == S::npos ? x : x.substr(boundary + 1);
}
static S compact(S x, const Vocab &v) {
  x = alias(x);
  const S &p = v.cached("compact-modifier", [&] {
    return "\\(([0-9?]*(?:" + alternatives(longest(v.subs)) + "))\\)(" +
           alternatives(v.sorted_monos()) + ")";
  });
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
  // A terminal residue cannot cross a branch or linkage delimiter.
  // Restrict the large vocabulary expression to the reducing-end suffix.
  size_t terminal_start = x.find_last_of(")]");
  terminal_start = terminal_start == S::npos ? 0 : terminal_start + 1;
  S prefix = x.substr(0, terminal_start);
  x = x.substr(terminal_start);
  if (ends(x, "+aldi")) {
    x.resize(x.size() - 5);
    x = rewrite(x, terminal_pattern(v),
                [](const VS &m) { return m[1] + m[2] + "-ol"; });
  }
  return prefix + rewrite(x, terminal_pattern(v), [&](const VS &m) {
    return m[1] + m[2] + m[3] + "(" + (m[4].empty() ? "?" : m[4]) + v.pos(m[1]) + "-";
  });
}
// Common unmodified short notation has an unambiguous linear tokenization.
// Modified or unusual tokens retain the general vocabulary-based converter.
static S simple_short(const S &x, const Vocab &v) {
  S out;
  size_t p = 0;
  while (p < x.size()) {
    if (x[p] == '(' || x[p] == ')') {
      out += x[p++] == '(' ? '[' : ']';
      continue;
    }
    size_t a = p;
    for (; a + 1 < x.size(); ++a)
      if ((x[a] == 'a' || x[a] == 'b' || x[a] == '?') &&
          (x[a + 1] == '-' || x[a + 1] == '?' ||
           (x[a + 1] >= '0' && x[a + 1] <= '9'))) break;
    if (a + 1 == x.size() || a == x.size()) return "";
    S mono = x.substr(p, a - p);
    if (!v.source_anomers.count(mono)) return "";
    p = a + 1;
    if (x[p] == '-') {
      if (++p == x.size()) return out + mono + "(" + x[a] + v.pos(mono) + "-";
    }
    size_t begin = p;
    if (p < x.size() && x[p] == '?') ++p;
    else {
      if (p == x.size() || x[p] < '0' || x[p] > '9') return "";
      while (p < x.size() && x[p] >= '0' && x[p] <= '9') ++p;
      while (p < x.size() && x[p] == '/') {
        ++p;
        if (p == x.size() || x[p] < '0' || x[p] > '9') return "";
        while (p < x.size() && x[p] >= '0' && x[p] <= '9') ++p;
      }
    }
    out += mono + "(" + x[a] + v.pos(mono) + "-" + x.substr(begin, p - begin) + ")";
  }
  return "";
}
static S short_iupac(const S &x, const Vocab &v) {
  S simple = simple_short(x, v);
  if (!simple.empty()) return simple;
  const S &mono = v.cached("short-mono", [&] {
    return "(?:" + alternatives(v.monos) +
           ")(?:(?:[0-9]+(?:/[0-9]+)*|\\?)(?:" + alternatives(v.subs) + "))*";
  });
  const S &residue = v.cached("short-residue", [&] {
    return "(" + mono + ")([ab?])-?([0-9]+(?:/[0-9]+)*|\\?)";
  });
  const S &terminal =
      v.cached("short-terminal", [&] { return "(" + mono + ")([ab?])-$"; });
  auto last = match(x, terminal);
  if (last.empty())
    fail("Invalid IUPAC-short terminal");
  const S &token_pattern =
      v.cached("short-tokens", [&] { return residue + "|\\(|\\)"; });
  VS ts = tokens(x, token_pattern);
  if (join(ts) + last[0] != x)
    fail("Invalid IUPAC-short tokens");
  S out;
  for (auto &t : ts) {
    if (t == "(")
      out += "[";
    else if (t == ")")
      out += "]";
    else {
      auto key = std::make_pair(S("short"), t);
      auto cached = v.converted_tokens.find(key);
      if (cached == v.converted_tokens.end()) {
        auto m = match(t, residue);
        cached = v.converted_tokens.emplace(key, m[1] + "(" + m[2] + v.pos(m[1]) + "-" + m[3] + ")").first;
      }
      out += cached->second;
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
    auto cache_key = std::make_pair(S("extended"), t);
    auto cached = v.converted_tokens.find(cache_key);
    if (cached != v.converted_tokens.end()) {
      out += cached->second;
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
    S converted = mono + "(" +
           (m[1] == "\xCE\xB1"   ? "a"
            : m[1] == "\xCE\xB2" ? "b"
                                 : "?") +
           m[4] + "-" + (m[5].empty() ? "" : m[5] + ")");
    out += converted;
    v.converted_tokens.emplace(cache_key, converted);
  }
  return out;
}
static S glycam(const S &x, const Vocab &v) {
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
    auto cache_key = std::make_pair(S("glycam"), t);
    auto cached = v.converted_tokens.find(cache_key);
    if (cached != v.converted_tokens.end()) {
      out += cached->second;
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
    S converted = mono + mods + "(" + m[2] + m[3] + "-" +
           (has(m[4], "^[A-Za-z]") ? "" : m[4] + ")");
    out += converted;
    v.converted_tokens.emplace(cache_key, converted);
  }
  return out;
}

// Fast normalization for plain Linear Code. Return a failure sentinel for the
// R converter to handle modified residues and less common syntax unchanged.
static S simple_linear(const S &x, const Vocab &v) {
  static const std::map<S, S> monos = {
    {"G", "Glc"}, {"A", "Gal"}, {"GN", "GlcNAc"}, {"AN", "GalNAc"},
    {"M", "Man"}, {"NN", "Neu5Ac"}, {"N", "Neu"}, {"K", "Kdn"},
    {"W", "Kdo"}, {"L", "GalA"}, {"I", "Ido"}, {"H", "Rha"},
    {"F", "Fuc"}, {"X", "Xyl"}, {"B", "Rib"}, {"R", "Ara"},
    {"U", "GlcA"}, {"O", "All"}, {"P", "Api"}, {"E", "Fru"}
  };
  S out;
  size_t p = 0;
  while (p < x.size()) {
    if (x[p] == '(' || x[p] == ')') {
      out += x[p++] == '(' ? '[' : ']';
      continue;
    }
    size_t begin = p;
    while (p < x.size() && x[p] >= 'A' && x[p] <= 'Z') ++p;
    auto found = monos.find(x.substr(begin, p - begin));
    if (found == monos.end()) fail("Use general Linear Code converter");
    S mono = found->second;
    if (p < x.size() && x[p] == '^') {
      mono = v.furan(mono);
      if (mono == found->second) fail("Use general Linear Code converter");
      ++p;
    }
    if (p == x.size() || (x[p] != 'a' && x[p] != 'b' && x[p] != '?'))
      fail("Use general Linear Code converter");
    char anomer = x[p++];
    if (p == x.size()) return out + mono + "(" + anomer + v.pos(mono) + "-";
    begin = p;
    if (x[p] == '?') ++p;
    else {
      if (x[p] < '0' || x[p] > '9') fail("Use general Linear Code converter");
      while (p < x.size() && x[p] >= '0' && x[p] <= '9') ++p;
      while (p < x.size() && x[p] == '/') {
        ++p;
        if (p == x.size() || x[p] < '0' || x[p] > '9') fail("Use general Linear Code converter");
        while (p < x.size() && x[p] >= '0' && x[p] <= '9') ++p;
      }
    }
    out += mono + "(" + anomer + v.pos(mono) + "-" + x.substr(begin, p - begin) + ")";
  }
  fail("Use general Linear Code converter");
  return "";
}

S convert(const S &x, const S &format, const Vocab &v) {
  if (format == "linear_code")
    return simple_linear(x, v);
  if (format == "iupac_short")
    return short_iupac(x, v);
  if (format == "iupac_extended")
    return extended(x, v);
  if (format == "iupac_compact")
    return compact(x, v);
  if (format == "glycam_iupac")
    return glycam(x, v);
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
      (!ends(x, "-") && has(alias(terminal_suffix(x)), terminal_pattern(v))))
    return "parse_iupac_compact";
  if (ends(x, "-"))
    return "parse_iupac_short";
  return "parse_linear_code";
}
} // namespace gp
