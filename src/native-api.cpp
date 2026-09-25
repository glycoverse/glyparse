#include "native.h"
// [[Rcpp::export]]
Rcpp::CharacterVector native_convert_cpp(Rcpp::CharacterVector x, const std::string &format,
                                         Rcpp::List vocabulary) {
  gp::Vocab v(vocabulary);
  Rcpp::CharacterVector out(x.size(), NA_STRING);
  for (R_xlen_t i = 0; i < x.size(); ++i) {
    if (i % 128 == 0)
      Rcpp::checkUserInterrupt();
    if (x[i] == NA_STRING)
      continue;
    try {
      std::string s = Rcpp::as<std::string>(x[i]);
      out[i] = format == "detect" ? gp::detect(s, v) : gp::convert(s, format, v);
    } catch (const std::exception &) {
      // Keep failed detection in a group so auto_parse preserves its position.
      if (format == "detect")
        out[i] = "parse_linear_code";
    }
  }
  out.attr("names") = x.attr("names");
  return out;
}
// [[Rcpp::export]]
Rcpp::List native_records_cpp(Rcpp::CharacterVector x, const std::string &format,
                              Rcpp::List vocabulary) {
  gp::Vocab v(vocabulary);
  Rcpp::List out(x.size());
  Rcpp::CharacterVector reasons(x.size(), NA_STRING);
  Rcpp::LogicalVector warnings(x.size());
  for (R_xlen_t i = 0; i < x.size(); ++i) {
    if (i % 128 == 0)
      Rcpp::checkUserInterrupt();
    if (x[i] == NA_STRING)
      continue;
    try {
      auto r = gp::parse_direct(Rcpp::as<std::string>(x[i]), format, v);
      out[i] = r.list();
      warnings[i] = r.warn;
    } catch (const std::exception &e) {
      reasons[i] = e.what();
    }
  }
  out.attr("names") = x.attr("names");
  out.attr("reasons") = reasons;
  out.attr("warnings") = warnings;
  out.attr("residue_parses") = static_cast<int>(v.wurcs_cache.size());
  return out;
}
// [[Rcpp::export]]
Rcpp::RObject native_aux_cpp(const std::string &op, Rcpp::List args, Rcpp::List vocabulary) {
  using namespace gp;
  Vocab v(vocabulary);
  if (starts(op, "wurcs_"))
    return wurcs_aux(op.substr(6), args, v);
  if (starts(op, "ct_"))
    return glycoct_aux(op.substr(3), args, v);
  if (op == "map") {
    S name = Rcpp::as<S>(args["name"]);
    Pairs pairs;
    if (name == "extended")
      pairs = extended_table;
    else if (name == "glycam")
      pairs = glycam_table;
    else if (name == "linucs")
      pairs = linucs_table;
    else if (name == "wurcs")
      pairs = wurcs_table;
    else if (name == "wurcs_unknown")
      pairs = wurcs_unknown_table;
    else if (name == "wurcs_alditol")
      pairs = wurcs_alditol_table;
    else if (name == "wurcs_ambiguous")
      pairs = wurcs_ambiguous_table;
    else {
      VS concrete = Rcpp::as<VS>(vocabulary["concrete"]);
      for (auto &m : concrete) {
        if (name == "unusual" && (starts(m, "D-") || starts(m, "L-")) &&
            v.unusual.count(m.substr(2)))
          pairs.push_back({m.substr(2), m});
        if (name == "furanose") {
          for (auto &p : v.furanose)
            if (p.second == m)
              pairs.push_back(p);
        }
      }
    }
    VS names, values;
    for (auto &p : pairs) {
      names.push_back(p.first);
      values.push_back(p.second);
    }
    Rcpp::CharacterVector out = Rcpp::wrap(values);
    out.attr("names") = names;
    return out;
  }
  if (op == "furanose" || op == "ringless" || op == "configuration" || op == "anomer" ||
      op == "invert") {
    Rcpp::CharacterVector x = args["x"];
    Rcpp::CharacterVector configs = args.containsElementNamed("configuration")
                                        ? Rcpp::CharacterVector(args["configuration"])
                                        : Rcpp::CharacterVector();
    Rcpp::CharacterVector out(x.size(), NA_STRING);
    for (R_xlen_t i = 0; i < x.size(); ++i) {
      if (x[i] == NA_STRING)
        continue;
      S s = Rcpp::as<S>(x[i]);
      if (op == "furanose")
        s = v.furan(s);
      else if (op == "ringless") {
        for (auto &p : v.furanose)
          if (p.second == s) {
            s = p.first;
            break;
          }
      } else if (op == "configuration") {
        if (configs.size() && configs[i % configs.size()] != NA_STRING)
          s = v.config(s, Rcpp::as<S>(configs[i % configs.size()]));
      } else if (op == "anomer")
        s = v.pos(s);
      else
        for (char &c : s) {
          if (c == 'D')
            c = 'L';
          else if (c == 'L')
            c = 'D';
          else if (c == 'd')
            c = 'l';
          else if (c == 'l')
            c = 'd';
        }
      out[i] = s;
    }
    return out;
  }
  if (op == "floating_sub" || op == "floating_parents") {
    VI parents = Rcpp::as<VI>(args["parents"]),
       implicit = Rcpp::as<VI>(args["implicit"]);
    Slots slots;
    for (auto &s : Rcpp::as<VS>(args["occupied"])) {
      auto p = split(s, '\r');
      if (p.size() == 2)
        slots.insert({std::stoi(p[0]), p[1]});
    }
    S context = Rcpp::as<S>(args["context"]);
    try {
      if (op == "floating_sub") {
        auto f =
            normalize_sub(parents, implicit, Rcpp::as<S>(args["substituent"]), slots);
        return Rcpp::List::create(Rcpp::Named("substituent") = f.substituent,
                                  Rcpp::Named("parents") = f.parents);
      }
      return Rcpp::wrap(
          normalize_parents(parents, implicit, Rcpp::as<S>(args["linkage"]), slots));
    } catch (const std::exception &e) {
      S reason = e.what();
      if (reason == "Unrepresentable parent-position combinations")
        fail("Can't represent the feasible parent-position combinations for a " +
             context + " after excluding occupied carbon positions.");
      fail("No feasible parent remains for a " + context +
           " after excluding occupied " +
           (op == "floating_sub" ? "carbon" : "acceptor") + " positions.");
    }
  }
  fail("Unknown native helper");
  return Rcpp::List();
}
