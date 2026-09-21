#ifndef GLYPARSE_NATIVE_H
#define GLYPARSE_NATIVE_H
#include "native-tables.h"
#include <Rcpp.h>
#include <algorithm>
#include <cctype>
#include <functional>
#include <map>
#include <regex>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>
namespace gp {
using S = std::string;
using VS = std::vector<S>;
using VI = std::vector<int>;
inline void fail(const S &s) { throw std::runtime_error(s); }
inline bool starts(const S &s, const S &p) { return s.compare(0, p.size(), p) == 0; }
inline bool ends(const S &s, const S &p) {
  return s.size() >= p.size() && s.compare(s.size() - p.size(), p.size(), p) == 0;
}
inline const std::regex &rx(const S &p) {
  static std::map<S, std::regex> cache;
  auto i = cache.find(p);
  if (i == cache.end())
    i = cache.emplace(p, std::regex(p)).first;
  return i->second;
}
inline bool has(const S &s, const S &p) { return std::regex_search(s, rx(p)); }
inline VS match(const S &s, const S &p) {
  std::smatch m;
  if (!std::regex_search(s, m, rx(p)))
    return {};
  VS v;
  for (auto a : m)
    v.push_back(a.str());
  return v;
}
inline S sub(const S &s, const S &p, const S &r = "") {
  return std::regex_replace(s, rx(p), r, std::regex_constants::format_first_only);
}
inline S gsub(const S &s, const S &p, const S &r = "") {
  return std::regex_replace(s, rx(p), r);
}
inline S replace(S s, const S &a, const S &b) {
  size_t p = 0;
  while ((p = s.find(a, p)) != S::npos) {
    s.replace(p, a.size(), b);
    p += b.size();
  }
  return s;
}
inline S trim(const S &s) { return sub(sub(s, "^\\s+"), "\\s+$"); }
inline VS split(const S &s, char d, bool keep = true) {
  VS out;
  size_t a = 0, b;
  do {
    b = s.find(d, a);
    S t = s.substr(a, b == S::npos ? b : b - a);
    if (keep || !t.empty())
      out.push_back(t);
    a = b + 1;
  } while (b != S::npos);
  return out;
}
inline S join(const VS &x, const S &sep = "") {
  S s;
  for (size_t i = 0; i < x.size(); ++i) {
    if (i)
      s += sep;
    s += x[i];
  }
  return s;
}
inline VS tokens(const S &s, const S &p) {
  VS out;
  for (std::sregex_iterator i(s.begin(), s.end(), rx(p)), e; i != e; ++i)
    out.push_back(i->str());
  return out;
}
inline S rewrite(const S &s, const S &p, const std::function<S(const VS &)> &f) {
  S out;
  size_t pos = 0;
  for (std::sregex_iterator i(s.begin(), s.end(), rx(p)), e; i != e; ++i) {
    out += s.substr(pos, i->position() - pos);
    VS m;
    for (auto a : *i)
      m.push_back(a.str());
    out += f(m);
    pos = i->position() + i->length();
  }
  return out + s.substr(pos);
}
inline S escape(const S &s) { return gsub(s, R"([.^$|()\[\]{}*+?\\])", R"(\$&)"); }
inline VS longest(VS x) {
  std::stable_sort(x.begin(), x.end(),
                   [](const S &a, const S &b) { return a.size() > b.size(); });
  return x;
}
inline S alternatives(VS x) {
  for (auto &s : x)
    s = escape(s);
  return join(x, "|");
}
template <class T> inline std::vector<T> unique(std::vector<T> x) {
  std::vector<T> y;
  for (auto a : x)
    if (std::find(y.begin(), y.end(), a) == y.end())
      y.push_back(a);
  return y;
}
template <class T> inline bool contains(const std::vector<T> &x, const T &v) {
  return std::find(x.begin(), x.end(), v) != x.end();
}
inline VI seq(int n) {
  VI x;
  for (int i = 1; i <= n; ++i)
    x.push_back(i);
  return x;
}
inline VI diff(const VI &a, const VI &b) {
  VI x;
  for (int v : a)
    if (!contains(b, v))
      x.push_back(v);
  return x;
}
inline bool seteq(VI a, VI b) {
  std::sort(a.begin(), a.end());
  std::sort(b.begin(), b.end());
  return a == b;
}
inline S lookup(const Pairs &t, const S &k, const S &fallback = "") {
  for (auto &p : t)
    if (p.first == k)
      return p.second;
  return fallback;
}
struct WResidue {
  S mono, anomer, sub;
  bool alditol = false;
};
struct Vocab {
  mutable std::map<S, WResidue> wurcs_cache;
  std::map<S, S> source_anomers;
  VS monos, subs;
  std::map<S, S> anomers, furanose, unusual;
  explicit Vocab(Rcpp::List v) {
    monos = Rcpp::as<VS>(v["mono"]);
    subs = Rcpp::as<VS>(v["sub"]);
    VS concrete = Rcpp::as<VS>(v["concrete"]), pos = Rcpp::as<VS>(v["anomer"]);
    VS source_pos = Rcpp::as<VS>(v["source_anomer"]);
    if (monos.size() != source_pos.size() || concrete.size() != pos.size())
      fail("Mismatched native vocabulary columns");
    for (size_t i = 0; i < monos.size(); ++i)
      source_anomers[monos[i]] = source_pos[i];
    for (size_t i = 0; i < concrete.size(); ++i) {
      const S &m = concrete[i];
      anomers[m] = pos[i];
      if (m.size() > 2 && (starts(m, "D-") || starts(m, "L-")) &&
          contains(concrete, m.substr(2)))
        unusual[m.substr(2)] = m;
      auto p = m.find('f');
      if (p != S::npos) {
        S base = m;
        base.erase(p, 1);
        if (contains(concrete, base))
          furanose[base] = m;
      }
    }
  }
  S pos(const S &m) const {
    auto i = anomers.find(m);
    return i == anomers.end() ? "1" : i->second;
  }
  S furan(const S &m) const {
    auto i = furanose.find(m);
    return i == furanose.end() ? m : i->second;
  }
  S config(const S &m, const S &c) const {
    S best;
    for (auto &p : unusual)
      if (starts(m, p.first) && p.first.size() > best.size())
        best = p.first;
    if (best.empty() || c.empty() || unusual.at(best).substr(0, 1) != c)
      return m;
    return unusual.at(best) + m.substr(best.size());
  }
};
struct Part {
  int root;
  VI nodes;
  S linkage;
  VI parents;
};
struct FloatingSub {
  S substituent;
  VI parents;
};
struct Record {
  VS mono, sub, linkage;
  VI edges;
  S anomer = "?1";
  bool alditol = false, warn = false;
  std::vector<Part> parts;
  std::vector<FloatingSub> floats;
  int add(S m, S s = "") {
    mono.push_back(m);
    sub.push_back(s);
    return mono.size();
  }
  void edge(int p, int c, S l) {
    edges.push_back(p);
    edges.push_back(c);
    linkage.push_back(l);
  }
  VI roots() const {
    VI children;
    for (size_t i = 1; i < edges.size(); i += 2)
      children.push_back(edges[i]);
    return diff(seq(mono.size()), children);
  }
  Rcpp::List list() const {
    Rcpp::List a = Rcpp::List::create(
        Rcpp::Named("mono") = mono, Rcpp::Named("sub") = sub,
        Rcpp::Named("edges") = edges, Rcpp::Named("linkage") = linkage,
        Rcpp::Named("anomer") = anomer, Rcpp::Named("alditol") = alditol);
    if (!parts.empty()) {
      Rcpp::List ps;
      for (auto &p : parts)
        ps.push_back(Rcpp::List::create(
            Rcpp::Named("root") = p.root, Rcpp::Named("nodes") = p.nodes,
            Rcpp::Named("linkage") = p.linkage, Rcpp::Named("parents") = p.parents));
      a["floating_parts"] = ps;
    }
    if (!floats.empty()) {
      Rcpp::List fs;
      for (auto &p : floats)
        fs.push_back(Rcpp::List::create(Rcpp::Named("substituent") = p.substituent,
                                        Rcpp::Named("parents") = p.parents));
      a["floating_substituents"] = fs;
    }
    return a;
  }
};
inline S sort_subs(VS v, bool dedup = true) {
  if (dedup)
    v = unique(v);
  std::stable_sort(v.begin(), v.end(), [](const S &a, const S &b) {
    auto ma = match(a, "^([0-9]+|\\?)(.*)"), mb = match(b, "^([0-9]+|\\?)(.*)");
    if (ma.empty() || mb.empty())
      return a < b;
    int pa = ma[1] == "?" ? 999 : std::stoi(ma[1]),
        pb = mb[1] == "?" ? 999 : std::stoi(mb[1]);
    return pa == pb ? ma[2] < mb[2] : pa < pb;
  });
  return join(v, ",");
}
using Slots = std::set<std::pair<int, S>>;
Slots occupied(const Record &, bool carbon = false, int count = -1);
VI normalize_parents(VI, const VI &, const S &, const Slots &);
FloatingSub normalize_sub(VI, const VI &, S, const Slots &);
S convert(const S &, const S &, const Vocab &);
S detect(const S &, const Vocab &);
Record parse_direct(const S &, const S &, const Vocab &);
Record wurcs_parse(const S &, const Vocab &);
Record glycoct_parse(const S &, const Vocab &);
S gwb_convert(const S &, const Vocab &);
Rcpp::RObject wurcs_aux(const S &, Rcpp::List, const Vocab &);
Rcpp::RObject glycoct_aux(const S &, Rcpp::List, const Vocab &);
} // namespace gp
#endif
