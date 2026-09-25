#include "native.h"
namespace gp {
Slots occupied(const Record &r, bool carbon, int count) {
  if (count < 0)
    count = r.mono.size();
  Slots slots;
  for (size_t i = 0; i < r.linkage.size(); ++i) {
    auto p = split(r.linkage[i].substr(r.linkage[i].find('-') + 1), '/');
    if (p.size() == 1 && p[0] != "?" && r.edges[2 * i] <= count &&
        r.edges[2 * i + 1] <= count)
      slots.insert({r.edges[2 * i], p[0]});
  }
  if (carbon)
    for (int i = 0; i < count; ++i)
      for (auto &s : split(r.sub[i], ',', false)) {
        auto m = match(s, "^([?0-9/]+)");
        if (!m.empty() && m[1] != "?" && m[1].find('/') == S::npos)
          slots.insert({i + 1, m[1]});
      }
  return slots;
}
VI normalize_parents(const VI &parents, const VI &implicit, const S &link,
                     const Slots &slots) {
  if (seteq(parents, implicit))
    return {};
  auto ps = split(link.substr(link.find('-') + 1), '/');
  if (contains(ps, S("?")))
    return parents;
  VI out;
  for (int p : parents) {
    bool ok = false;
    for (auto &s : ps)
      ok |= !slots.count({p, s});
    if (ok)
      out.push_back(p);
  }
  if (out.empty())
    fail("No feasible parent after excluding occupied acceptor positions");
  return out;
}
FloatingSub normalize_sub(const VI &parents, const VI &implicit, S subtoken,
                          const Slots &slots) {
  auto m = match(subtoken, "^([?0-9/]+)");
  if (m.empty() || m[1] == "?")
    return {subtoken, seteq(parents, implicit) ? VI{} : parents};
  auto ps = split(m[1], '/');
  VI good;
  VS common;
  for (int p : parents) {
    VS feasible;
    for (auto &s : ps)
      if (!slots.count({p, s}))
        feasible.push_back(s);
    feasible = unique(feasible);
    std::sort(feasible.begin(), feasible.end());
    if (feasible.empty())
      continue;
    if (!common.empty() && common != feasible)
      fail("Unrepresentable parent-position combinations");
    common = feasible;
    good.push_back(p);
  }
  if (good.empty())
    fail("No feasible parent after excluding occupied carbon positions");
  subtoken = join(common, "/") + subtoken.substr(m[1].size());
  return {subtoken, seteq(good, implicit) ? VI{} : good};
}
} // namespace gp
