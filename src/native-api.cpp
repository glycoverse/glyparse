#include "native.h"
namespace {
// Workers only handle owned C++ strings/records. All R reads, allocations,
// callbacks and interrupt checks stay on the calling thread.
template <class Parse>
std::vector<gp::Vocab> parse_batch(const std::vector<std::string>& input, const std::vector<bool>& missing,
                 const gp::Vocab& vocabulary, int threads, Parse parse) {
  threads = std::max(1, std::min(threads, 64));
  if (input.size() < 256) threads = 1;
  std::vector<gp::Vocab> vocabularies(threads, vocabulary);
  std::atomic<size_t> next{0};
  std::atomic<bool> stop{false};
  struct Workers {
    std::vector<std::thread> workers;
    std::atomic<bool>& stop;
    ~Workers() { stop = true; for (auto& worker : workers) if (worker.joinable()) worker.join(); }
  } workers{{}, stop};
  auto run = [&](int worker) {
    while (!stop) {
      size_t begin = next.fetch_add(128);
      if (begin >= input.size()) break;
      if (!worker) Rcpp::checkUserInterrupt();
      for (size_t i = begin; i < std::min(begin+128, input.size()); ++i)
        if (!missing[i]) parse(i, vocabularies[worker]);
    }
  };
  for (int i = 1; i < threads; ++i) {
    try { workers.workers.emplace_back(run, i); }
    catch (const std::system_error&) { break; } // Continue on available workers.
  }
  run(0);
  for (auto& worker : workers.workers) worker.join();
  return vocabularies;
}
class RecordWriter {
  Rcpp::CharacterVector fields;
  Rcpp::LogicalVector reduced, ordinary;
  std::map<std::string, Rcpp::CharacterVector> anomers;
  std::map<int, Rcpp::CharacterVector> empty_subs;
 public:
  RecordWriter() : fields(Rcpp::CharacterVector::create("mono", "sub", "edges",
      "linkage", "anomer", "alditol")), reduced(Rcpp::LogicalVector::create(true)),
      ordinary(Rcpp::LogicalVector::create(false)) {}
  Rcpp::List write(const gp::Record& r) {
    if (!r.parts.empty() || !r.floats.empty()) return r.list();
    Rcpp::List out(6);
    Rf_setAttrib(out, R_NamesSymbol, fields);
    SEXP mono = Rf_allocVector(STRSXP, r.mono.size()); SET_VECTOR_ELT(out, 0, mono);
    for (size_t i = 0; i < r.mono.size(); ++i) SET_STRING_ELT(mono, i, Rf_mkChar(r.mono[i].c_str()));
    bool empty = std::all_of(r.sub.begin(), r.sub.end(), [](const gp::S& s){ return s.empty(); });
    if (empty) {
      auto found = empty_subs.find(r.sub.size());
      if (found == empty_subs.end()) found = empty_subs.emplace(r.sub.size(),
        Rcpp::CharacterVector(r.sub.size(), "")).first;
      SET_VECTOR_ELT(out, 1, found->second);
    } else {
      SEXP sub = Rf_allocVector(STRSXP, r.sub.size()); SET_VECTOR_ELT(out, 1, sub);
      for (size_t i = 0; i < r.sub.size(); ++i) SET_STRING_ELT(sub, i, Rf_mkChar(r.sub[i].c_str()));
    }
    SEXP edges = Rf_allocVector(INTSXP, r.edges.size()); SET_VECTOR_ELT(out, 2, edges);
    std::copy(r.edges.begin(), r.edges.end(), INTEGER(edges));
    SEXP links = Rf_allocVector(STRSXP, r.linkage.size()); SET_VECTOR_ELT(out, 3, links);
    for (size_t i = 0; i < r.linkage.size(); ++i) SET_STRING_ELT(links, i, Rf_mkChar(r.linkage[i].c_str()));
    auto anomer = anomers.find(r.anomer);
    if (anomer == anomers.end()) anomer = anomers.emplace(r.anomer,
      Rcpp::CharacterVector::create(r.anomer)).first;
    SET_VECTOR_ELT(out, 4, anomer->second);
    SET_VECTOR_ELT(out, 5, r.alditol ? static_cast<SEXP>(reduced) : static_cast<SEXP>(ordinary));
    return out;
  }
};
void copy_inputs(Rcpp::CharacterVector x, std::vector<std::string>& input,
                 std::vector<bool>& missing) {
  input.resize(x.size()); missing.resize(x.size());
  for (R_xlen_t i = 0; i < x.size(); ++i) {
    missing[i] = STRING_ELT(x,i) == NA_STRING;
    if (!missing[i]) input[i] = CHAR(STRING_ELT(x,i));
  }
}
}
// [[Rcpp::export]]
Rcpp::CharacterVector native_convert_cpp(Rcpp::CharacterVector x, const std::string &format,
                                         Rcpp::List vocabulary, int threads = 2) {
  gp::Vocab v(vocabulary);
  std::vector<std::string> input, converted(x.size());
  std::vector<bool> missing;
  std::vector<unsigned char> valid(x.size(), 0);
  copy_inputs(x, input, missing);
  parse_batch(input, missing, v, threads, [&](size_t i, gp::Vocab& vocab) {
    try {
      converted[i] = format == "detect" ? gp::detect(input[i], vocab) : gp::convert(input[i], format, vocab);
      valid[i] = 1;
    } catch (const std::exception&) {
      if (format == "detect") { converted[i] = "parse_linear_code"; valid[i] = 1; }
    }
  });
  Rcpp::CharacterVector out(x.size(), NA_STRING);
  for (R_xlen_t i = 0; i < x.size(); ++i) if (valid[i]) out[i] = converted[i];
  out.attr("names") = x.attr("names");
  return out;
}
// [[Rcpp::export]]
Rcpp::List native_records_cpp(Rcpp::CharacterVector x, const std::string &format,
                              Rcpp::List vocabulary, int threads = 2, bool constructible_only = false) {
  gp::Vocab v(vocabulary);
  std::vector<std::string> input, failures(x.size());
  std::vector<bool> missing;
  std::vector<gp::Record> records(x.size());
  std::vector<unsigned char> valid(x.size(), 0);
  // Retain descriptor-count diagnostics independently of work scheduling.
  std::set<gp::S> descriptors;
  copy_inputs(x, input, missing);
  auto vocabularies = parse_batch(input, missing, v, threads, [&](size_t i, gp::Vocab& vocab) {
    try { records[i] = gp::parse_direct(input[i], format, vocab); valid[i] = 1; }
    catch (const std::exception& e) { failures[i] = e.what(); }
  });
  for (const auto& vocab : vocabularies)
    for (const auto& entry : vocab.wurcs_cache) descriptors.insert(entry.first);
  Rcpp::List out(x.size());
  Rcpp::CharacterVector reasons(x.size(), NA_STRING);
  Rcpp::LogicalVector warnings(x.size());
  RecordWriter writer;
  for (R_xlen_t i = 0; i < x.size(); ++i) {
    if (i % 128 == 0) Rcpp::checkUserInterrupt();
    if (valid[i]) {
      const auto& record = records[i];
      warnings[i] = record.warn;
      // A single out-tree must have n-1 edges. No floating component can
      // repair this when none is declared. Avoid materializing records that
      // the mandatory graph validator would necessarily reject.
      bool impossible = record.parts.empty() &&
        record.edges.size()/2 + 1 != record.mono.size();
      if (!constructible_only || !impossible) out[i] = writer.write(record);
    }
    else if (!missing[i]) reasons[i] = failures[i];
  }
  out.attr("names") = x.attr("names");
  out.attr("reasons") = reasons;
  out.attr("warnings") = warnings;
  out.attr("residue_parses") = static_cast<int>(descriptors.size());
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
