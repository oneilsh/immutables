#include <Rcpp.h>
#include <algorithm>

using namespace Rcpp;

namespace {

struct ReprotectSEXP {
  SEXP value;
  PROTECT_INDEX index;

  explicit ReprotectSEXP(SEXP init) : value(R_NilValue) {
    PROTECT_WITH_INDEX(value = init, &index);
  }

  ReprotectSEXP(const ReprotectSEXP&) = delete;
  ReprotectSEXP& operator=(const ReprotectSEXP&) = delete;

  ~ReprotectSEXP() {
    UNPROTECT(1);
  }

  SEXP get() const {
    return value;
  }

  SEXP set(SEXP next) {
    REPROTECT(value = next, index);
    return value;
  }
};

SEXP class_sym = Rf_install("class");
SEXP monoids_sym = Rf_install("monoids");
SEXP measures_sym = Rf_install("measures");
SEXP ft_name_sym = Rf_install("ft_name");

bool has_class(SEXP x, const char* cls) {
  return Rf_inherits(x, cls);
}

bool is_structural_node_cpp(SEXP x) {
  return has_class(x, "FingerTree") || has_class(x, "Digit") || has_class(x, "Node");
}

bool has_name_attr(SEXP x) {
  SEXP nm = Rf_getAttrib(x, ft_name_sym);
  return !Rf_isNull(nm) && XLENGTH(nm) > 0;
}

bool name_equals(SEXP x, const std::string& target) {
  SEXP nm = Rf_getAttrib(x, ft_name_sym);
  if(Rf_isNull(nm) || XLENGTH(nm) == 0) {
    return false;
  }
  return as<std::string>(STRING_ELT(nm, 0)) == target;
}

double child_size(SEXP x) {
  if(has_class(x, "FingerTree") || has_class(x, "Digit") || has_class(x, "Node")) {
    List ms = Rf_getAttrib(x, measures_sym);
    return as<double>(ms[".size"]);
  }
  return 1.0;
}

int child_named_count(SEXP x) {
  if(has_class(x, "FingerTree") || has_class(x, "Digit") || has_class(x, "Node")) {
    List ms = Rf_getAttrib(x, measures_sym);
    return as<int>(ms[".named_count"]);
  }
  return has_name_attr(x) ? 1 : 0;
}

int find_name_position_impl(SEXP x, const std::string& target, int offset) {
  if(!is_structural_node_cpp(x)) {
    return name_equals(x, target) ? (offset + 1) : -1;
  }

  if(has_class(x, "Empty")) {
    return -1;
  }

  if(has_class(x, "Single")) {
    List s(x);
    return find_name_position_impl(s[0], target, offset);
  }

  if(has_class(x, "Deep")) {
    List d(x);
    SEXP prefix = d["prefix"];
    SEXP middle = d["middle"];
    SEXP suffix = d["suffix"];

    int p = find_name_position_impl(prefix, target, offset);
    if(p >= 0) {
      return p;
    }
    offset += static_cast<int>(child_size(prefix));

    p = find_name_position_impl(middle, target, offset);
    if(p >= 0) {
      return p;
    }
    offset += static_cast<int>(child_size(middle));

    return find_name_position_impl(suffix, target, offset);
  }

  List xs(x);
  for(int i = 0; i < xs.size(); ++i) {
    SEXP el = xs[i];
    int p = find_name_position_impl(el, target, offset);
    if(p >= 0) {
      return p;
    }
    offset += static_cast<int>(child_size(el));
  }
  return -1;
}

SEXP get_by_index_impl(SEXP x, int idx) {
  if(idx <= 0) {
    stop("Only positive integer indices are supported.");
  }

  if(!is_structural_node_cpp(x)) {
    if(idx == 1) {
      return x;
    }
    stop("Index out of bounds.");
  }

  if(has_class(x, "Empty")) {
    stop("Index out of bounds.");
  }

  if(has_class(x, "Single")) {
    List s(x);
    return get_by_index_impl(s[0], idx);
  }

  if(has_class(x, "Deep")) {
    List d(x);
    SEXP prefix = d["prefix"];
    SEXP middle = d["middle"];
    SEXP suffix = d["suffix"];

    const int npr = static_cast<int>(child_size(prefix));
    if(idx <= npr) {
      return get_by_index_impl(prefix, idx);
    }
    idx -= npr;

    const int nm = static_cast<int>(child_size(middle));
    if(idx <= nm) {
      return get_by_index_impl(middle, idx);
    }
    idx -= nm;

    return get_by_index_impl(suffix, idx);
  }

  List xs(x);
  for(int i = 0; i < xs.size(); ++i) {
    SEXP el = xs[i];
    const int n = static_cast<int>(child_size(el));
    if(idx <= n) {
      return get_by_index_impl(el, idx);
    }
    idx -= n;
  }

  stop("Index out of bounds.");
}

void collect_names_impl(SEXP x, CharacterVector& out, int& pos) {
  if(!is_structural_node_cpp(x)) {
    SEXP nm = Rf_getAttrib(x, ft_name_sym);
    if(Rf_isNull(nm) || XLENGTH(nm) == 0) {
      out[pos++] = NA_STRING;
      return;
    }
    out[pos++] = STRING_ELT(nm, 0);
    return;
  }

  if(has_class(x, "Empty")) {
    return;
  }

  if(has_class(x, "Single")) {
    List s(x);
    collect_names_impl(s[0], out, pos);
    return;
  }

  if(has_class(x, "Deep")) {
    List d(x);
    collect_names_impl(d["prefix"], out, pos);
    collect_names_impl(d["middle"], out, pos);
    collect_names_impl(d["suffix"], out, pos);
    return;
  }

  List xs(x);
  for(int i = 0; i < xs.size(); ++i) {
    collect_names_impl(xs[i], out, pos);
  }
}

void collect_leaves_impl(SEXP x, std::vector<SEXP>& out) {
  if(!is_structural_node_cpp(x)) {
    out.push_back(x);
    return;
  }

  if(has_class(x, "Empty")) {
    return;
  }

  if(has_class(x, "Single")) {
    List s(x);
    collect_leaves_impl(s[0], out);
    return;
  }

  if(has_class(x, "Deep")) {
    List d(x);
    collect_leaves_impl(d["prefix"], out);
    collect_leaves_impl(d["middle"], out);
    collect_leaves_impl(d["suffix"], out);
    return;
  }

  List xs(x);
  for(int i = 0; i < xs.size(); ++i) {
    collect_leaves_impl(xs[i], out);
  }
}

SEXP oms_entry_key(SEXP entry) {
  if(TYPEOF(entry) != VECSXP || XLENGTH(entry) < 2) {
    stop("ordered_multiset entries must be list(item, key, seq_id).");
  }
  List e(entry);
  SEXP key = e[1];
  if(Rf_isNull(key)) {
    stop("ordered_multiset entry is missing `key`.");
  }
  return key;
}

int oms_compare_keys(SEXP a, SEXP b, const std::string& key_type) {
  if(key_type == "numeric") {
    const double da = as<double>(a);
    const double db = as<double>(b);
    if(da < db) return -1;
    if(da > db) return 1;
    return 0;
  }
  if(key_type == "character") {
    const std::string sa = as<std::string>(a);
    const std::string sb = as<std::string>(b);
    if(sa < sb) return -1;
    if(sa > sb) return 1;
    return 0;
  }
  if(key_type == "logical") {
    const int ia = as<bool>(a) ? 1 : 0;
    const int ib = as<bool>(b) ? 1 : 0;
    if(ia < ib) return -1;
    if(ia > ib) return 1;
    return 0;
  }
  stop("Unsupported ordered_multiset key type.");
}

int oms_compare_entry_keys(SEXP ea, SEXP eb, const std::string& key_type) {
  return oms_compare_keys(oms_entry_key(ea), oms_entry_key(eb), key_type);
}

int oms_group_end(const std::vector<SEXP>& entries, int start_idx, const std::string& key_type) {
  const int n = static_cast<int>(entries.size());
  SEXP key0 = oms_entry_key(entries[start_idx]);
  int j = start_idx;
  while(j < n && oms_compare_keys(oms_entry_key(entries[j]), key0, key_type) == 0) {
    ++j;
  }
  return j - 1;
}

void oms_append_range(
  const std::vector<SEXP>& src,
  int start_idx,
  int end_idx_incl,
  std::vector<SEXP>& out
) {
  if(start_idx > end_idx_incl) {
    return;
  }
  for(int i = start_idx; i <= end_idx_incl; ++i) {
    out.push_back(src[i]);
  }
}

List measures_from_children(const List& children, const List& monoids) {
  CharacterVector nms = monoids.names();
  List out(nms.size());
  out.attr("names") = nms;
  for(int i = 0; i < nms.size(); ++i) {
    std::string nm = as<std::string>(nms[i]);
    if(nm == ".size") {
      double s = 0.0;
      for(int j = 0; j < children.size(); ++j) {
        s += child_size(children[j]);
      }
      out[i] = s;
      continue;
    }
    if(nm == ".named_count") {
      int c = 0;
      for(int j = 0; j < children.size(); ++j) {
        c += child_named_count(children[j]);
      }
      out[i] = c;
      continue;
    }

    List r = as<List>(monoids[i]);
    Function f = as<Function>(r["f"]);
    Function measure = as<Function>(r["measure"]);
    ReprotectSEXP acc(static_cast<SEXP>(r["i"]));
    for(int j = 0; j < children.size(); ++j) {
      SEXP ch = children[j];
      SEXP mv = R_NilValue;
      if(has_class(ch, "FingerTree") || has_class(ch, "Digit") || has_class(ch, "Node")) {
        List cms = Rf_getAttrib(ch, measures_sym);
        if(!Rf_isNull(cms) && i < cms.size() && !Rf_isNull(cms[i])) {
          mv = cms[i];
        }
      }
      if(Rf_isNull(mv)) {
        mv = measure(ch);
      }
      acc.set(f(acc.get(), mv));
    }
    out[i] = acc.get();
  }
  return out;
}

List measures_empty(const List& monoids) {
  CharacterVector nms = monoids.names();
  List out(nms.size());
  out.attr("names") = nms;
  for(int i = 0; i < nms.size(); ++i) {
    std::string nm = as<std::string>(nms[i]);
    if(nm == ".size") out[i] = 0.0;
    else if(nm == ".named_count") out[i] = 0;
    else {
      List r = as<List>(monoids[i]);
      out[i] = r["i"];
    }
  }
  return out;
}

SEXP make_empty(const List& monoids) {
  List e = List::create(R_NilValue);
  e.attr("class") = CharacterVector::create("Empty", "FingerTree", "list");
  e.attr("monoids") = monoids;
  e.attr("measures") = measures_empty(monoids);
  return e;
}

SEXP make_single(SEXP el, const List& monoids) {
  List s = List::create(el);
  s.attr("class") = CharacterVector::create("Single", "FingerTree", "list");
  s.attr("monoids") = monoids;
  List children = List::create(el);
  s.attr("measures") = measures_from_children(children, monoids);
  return s;
}

SEXP make_digit(const List& children, const List& monoids) {
  List d(children.size());
  for(int i = 0; i < children.size(); ++i) d[i] = children[i];
  d.attr("class") = CharacterVector::create("Digit", "list");
  d.attr("monoids") = monoids;
  d.attr("measures") = measures_from_children(children, monoids);
  return d;
}

SEXP make_node3(SEXP a, SEXP b, SEXP c, const List& monoids) {
  List n = List::create(a, b, c);
  n.attr("class") = CharacterVector::create("Node3", "Node", "list");
  n.attr("monoids") = monoids;
  List children = List::create(a, b, c);
  n.attr("measures") = measures_from_children(children, monoids);
  return n;
}

SEXP make_node2(SEXP a, SEXP b, const List& monoids) {
  List n = List::create(a, b);
  n.attr("class") = CharacterVector::create("Node2", "Node", "list");
  n.attr("monoids") = monoids;
  List children = List::create(a, b);
  n.attr("measures") = measures_from_children(children, monoids);
  return n;
}

SEXP make_deep(SEXP prefix, SEXP middle, SEXP suffix, const List& monoids) {
  List d = List::create(_["prefix"] = prefix, _["middle"] = middle, _["suffix"] = suffix);
  d.attr("class") = CharacterVector::create("Deep", "FingerTree", "list");
  d.attr("monoids") = monoids;
  List children = List::create(prefix, middle, suffix);
  d.attr("measures") = measures_from_children(children, monoids);
  return d;
}

SEXP add_right_cpp(SEXP t, SEXP el, const List& monoids) {
  if(has_class(t, "Empty")) {
    return make_single(el, monoids);
  }
  if(has_class(t, "Single")) {
    List s(t);
    Shield<SEXP> prefix(make_digit(List::create(s[0]), monoids));
    Shield<SEXP> middle(make_empty(monoids));
    Shield<SEXP> suffix(make_digit(List::create(el), monoids));
    return make_deep(prefix, middle, suffix, monoids);
  }
  if(has_class(t, "Digit")) {
    List d(t);
    List children(d.size() + 1);
    for(int i = 0; i < d.size(); ++i) children[i] = d[i];
    children[d.size()] = el;
    return make_digit(children, monoids);
  }
  if(has_class(t, "Deep")) {
    List d(t);
    List suffix = d["suffix"];
    if(suffix.size() == 4) {
      Shield<SEXP> new_suffix(make_digit(List::create(suffix[3], el), monoids));
      Shield<SEXP> middle_node(make_node3(suffix[0], suffix[1], suffix[2], monoids));
      Shield<SEXP> new_middle(add_right_cpp(d["middle"], middle_node, monoids));
      return make_deep(d["prefix"], new_middle, new_suffix, monoids);
    }
    Shield<SEXP> new_suffix(add_right_cpp(d["suffix"], el, monoids));
    return make_deep(d["prefix"], d["middle"], new_suffix, monoids);
  }
  stop("Unsupported node type in ft_cpp_append_right.");
}

SEXP add_left_cpp(SEXP t, SEXP el, const List& monoids) {
  if(has_class(t, "Empty")) {
    return make_single(el, monoids);
  }
  if(has_class(t, "Single")) {
    List s(t);
    Shield<SEXP> prefix(make_digit(List::create(el), monoids));
    Shield<SEXP> middle(make_empty(monoids));
    Shield<SEXP> suffix(make_digit(List::create(s[0]), monoids));
    return make_deep(prefix, middle, suffix, monoids);
  }
  if(has_class(t, "Digit")) {
    List d(t);
    List children(d.size() + 1);
    children[0] = el;
    for(int i = 0; i < d.size(); ++i) children[i + 1] = d[i];
    return make_digit(children, monoids);
  }
  if(has_class(t, "Deep")) {
    List d(t);
    List prefix = d["prefix"];
    if(prefix.size() == 4) {
      Shield<SEXP> new_prefix(make_digit(List::create(el, prefix[0]), monoids));
      Shield<SEXP> middle_node(make_node3(prefix[1], prefix[2], prefix[3], monoids));
      Shield<SEXP> new_middle(add_left_cpp(d["middle"], middle_node, monoids));
      return make_deep(new_prefix, new_middle, d["suffix"], monoids);
    }
    Shield<SEXP> new_prefix(add_left_cpp(d["prefix"], el, monoids));
    return make_deep(new_prefix, d["middle"], d["suffix"], monoids);
  }
  stop("Unsupported node type in ft_cpp_prepend_left.");
}

SEXP clone_with_name(SEXP el, SEXP name_) {
  SEXP out = PROTECT(Rf_duplicate(el));
  CharacterVector nm(name_);
  if(nm.size() != 1 || CharacterVector::is_na(nm[0])) {
    UNPROTECT(1);
    stop("`name` must be a single non-empty string.");
  }
  std::string s = as<std::string>(nm[0]);
  if(s.empty()) {
    UNPROTECT(1);
    stop("`name` must be a single non-empty string.");
  }
  Rf_setAttrib(out, ft_name_sym, Rf_mkString(s.c_str()));
  UNPROTECT(1);
  return out;
}

SEXP clone_with_attrs(SEXP el, SEXP name_) {
  SEXP out = PROTECT(Rf_duplicate(el));

  if(!Rf_isNull(name_)) {
    CharacterVector nm(name_);
    if(nm.size() != 1 || CharacterVector::is_na(nm[0])) {
      UNPROTECT(1);
      stop("Element names must be scalar, non-missing, and non-empty.");
    }
    std::string s = as<std::string>(nm[0]);
    if(s.empty()) {
      UNPROTECT(1);
      stop("Element names must be scalar, non-missing, and non-empty.");
    }
    if(Rf_isNull(out)) {
      UNPROTECT(1);
      stop("Cannot attach a name to NULL element.");
    }
    Rf_setAttrib(out, ft_name_sym, Rf_mkString(s.c_str()));
  }

  UNPROTECT(1);
  return out;
}

SEXP add_all_right_cpp(SEXP t, const List& els, const List& monoids) {
  ReprotectSEXP cur(t);
  for(int i = 0; i < els.size(); ++i) {
    cur.set(add_right_cpp(cur.get(), els[i], monoids));
  }
  return cur.get();
}

SEXP add_all_left_cpp(SEXP t, const List& els, const List& monoids) {
  ReprotectSEXP cur(t);
  for(int i = els.size() - 1; i >= 0; --i) {
    cur.set(add_left_cpp(cur.get(), els[i], monoids));
  }
  return cur.get();
}

List measured_nodes_cpp(const List& xs, const List& monoids) {
  const int n = xs.size();
  if(n < 2) {
    stop("measured_nodes_cpp requires at least two elements.");
  }

  List out;
  int i = 0;
  while(i < n) {
    const int rem = n - i;
    if(rem == 2) {
      out.push_back(make_node2(xs[i], xs[i + 1], monoids));
      break;
    }
    if(rem == 3) {
      out.push_back(make_node3(xs[i], xs[i + 1], xs[i + 2], monoids));
      break;
    }
    if(rem == 4) {
      out.push_back(make_node2(xs[i], xs[i + 1], monoids));
      out.push_back(make_node2(xs[i + 2], xs[i + 3], monoids));
      break;
    }

    out.push_back(make_node3(xs[i], xs[i + 1], xs[i + 2], monoids));
    i += 3;
  }
  return out;
}

SEXP tree_from_sorted_list_cpp(const List& xs, const List& monoids) {
  const int n = xs.size();
  if(n == 0) {
    return make_empty(monoids);
  }
  if(n == 1) {
    return make_single(xs[0], monoids);
  }
  if(n == 2) {
    return make_deep(
      make_digit(List::create(xs[0]), monoids),
      make_empty(monoids),
      make_digit(List::create(xs[1]), monoids),
      monoids
    );
  }
  if(n == 3) {
    return make_deep(
      make_digit(List::create(xs[0], xs[1]), monoids),
      make_empty(monoids),
      make_digit(List::create(xs[2]), monoids),
      monoids
    );
  }
  if(n == 4) {
    return make_deep(
      make_digit(List::create(xs[0], xs[1]), monoids),
      make_empty(monoids),
      make_digit(List::create(xs[2], xs[3]), monoids),
      monoids
    );
  }

  const int prefix_len = (n == 5) ? 1 : 2;
  const int suffix_len = 2;
  const int middle_n = n - prefix_len - suffix_len;
  if(middle_n < 2) {
    stop("Invalid middle segment while building ordered tree.");
  }

  List prefix_children(prefix_len);
  for(int i = 0; i < prefix_len; ++i) {
    prefix_children[i] = xs[i];
  }

  List suffix_children(suffix_len);
  for(int i = 0; i < suffix_len; ++i) {
    suffix_children[i] = xs[n - suffix_len + i];
  }

  List middle_elems(middle_n);
  for(int i = 0; i < middle_n; ++i) {
    middle_elems[i] = xs[prefix_len + i];
  }

  Shield<SEXP> prefix(make_digit(prefix_children, monoids));
  Shield<SEXP> suffix(make_digit(suffix_children, monoids));
  List middle_nodes = measured_nodes_cpp(middle_elems, monoids);
  Shield<SEXP> middle(tree_from_sorted_list_cpp(middle_nodes, monoids));
  return make_deep(prefix, middle, suffix, monoids);
}

SEXP oms_set_merge_cpp_impl(
  SEXP x,
  SEXP y,
  const std::string& mode,
  const List& monoids,
  const std::string& key_type
) {
  std::vector<SEXP> ex;
  std::vector<SEXP> ey;
  collect_leaves_impl(x, ex);
  collect_leaves_impl(y, ey);

  const int nx = static_cast<int>(ex.size());
  const int ny = static_cast<int>(ey.size());
  std::vector<SEXP> out;
  out.reserve(static_cast<size_t>(nx + ny));

  int i = 0;
  int j = 0;

  while(i < nx || j < ny) {
    if(i >= nx) {
      if(mode == "union") {
        oms_append_range(ey, j, ny - 1, out);
      }
      break;
    }

    if(j >= ny) {
      if(mode == "union" || mode == "difference") {
        oms_append_range(ex, i, nx - 1, out);
      }
      break;
    }

    const int cmp = oms_compare_entry_keys(ex[i], ey[j], key_type);
    if(cmp < 0) {
      if(mode == "union" || mode == "difference") {
        const int ie = oms_group_end(ex, i, key_type);
        oms_append_range(ex, i, ie, out);
        i = ie + 1;
      } else {
        i = oms_group_end(ex, i, key_type) + 1;
      }
      continue;
    }

    if(cmp > 0) {
      if(mode == "union") {
        const int je = oms_group_end(ey, j, key_type);
        oms_append_range(ey, j, je, out);
        j = je + 1;
      } else {
        j = oms_group_end(ey, j, key_type) + 1;
      }
      continue;
    }

    const int ie = oms_group_end(ex, i, key_type);
    const int je = oms_group_end(ey, j, key_type);
    const int cx = ie - i + 1;
    const int cy = je - j + 1;

    if(mode == "intersection") {
      const int k = std::min(cx, cy);
      if(k > 0) {
        oms_append_range(ex, i, i + k - 1, out);
      }
    } else if(mode == "difference") {
      const int k = std::min(cx, cy);
      if(cx > k) {
        oms_append_range(ex, i + k, ie, out);
      }
    } else {
      oms_append_range(ex, i, ie, out);
      if(cy > cx) {
        oms_append_range(ey, j, j + (cy - cx) - 1, out);
      }
    }

    i = ie + 1;
    j = je + 1;
  }

  if(out.empty()) {
    return make_empty(monoids);
  }

  List out_list(static_cast<R_xlen_t>(out.size()));
  for(R_xlen_t k = 0; k < static_cast<R_xlen_t>(out.size()); ++k) {
    out_list[k] = out[static_cast<size_t>(k)];
  }
  return tree_from_sorted_list_cpp(out_list, monoids);
}

SEXP app3_cpp(SEXP xs, const List& ts, SEXP ys, const List& monoids) {
  if(has_class(xs, "Empty")) {
    return add_all_left_cpp(ys, ts, monoids);
  }
  if(has_class(ys, "Empty")) {
    return add_all_right_cpp(xs, ts, monoids);
  }
  if(has_class(xs, "Single")) {
    List sx(xs);
    Shield<SEXP> with_ts(add_all_left_cpp(ys, ts, monoids));
    return add_left_cpp(with_ts, sx[0], monoids);
  }
  if(has_class(ys, "Single")) {
    List sy(ys);
    Shield<SEXP> with_ts(add_all_right_cpp(xs, ts, monoids));
    return add_right_cpp(with_ts, sy[0], monoids);
  }
  if(has_class(xs, "Deep") && has_class(ys, "Deep")) {
    List dx(xs);
    List dy(ys);
    List sfx = dx["suffix"];
    List pry = dy["prefix"];

    List bridge(sfx.size() + ts.size() + pry.size());
    int j = 0;
    for(int i = 0; i < sfx.size(); ++i) bridge[j++] = sfx[i];
    for(int i = 0; i < ts.size(); ++i) bridge[j++] = ts[i];
    for(int i = 0; i < pry.size(); ++i) bridge[j++] = pry[i];

    List nts = measured_nodes_cpp(bridge, monoids);
    Shield<SEXP> m(app3_cpp(dx["middle"], nts, dy["middle"], monoids));
    return make_deep(dx["prefix"], m, dy["suffix"], monoids);
  }

  stop("Unsupported node type in ft_cpp_concat.");
}

SEXP monoid_measure_for_child(SEXP ch, const std::string& monoid_name, const List& monoid_spec) {
  if(is_structural_node_cpp(ch)) {
    List cms = Rf_getAttrib(ch, measures_sym);
    if(!Rf_isNull(cms) && !Rf_isNull(cms[monoid_name])) {
      return cms[monoid_name];
    }
  }
  Function measure = as<Function>(monoid_spec["measure"]);
  return measure(ch);
}

int size_for_child(SEXP ch, const List& size_monoid) {
  if(is_structural_node_cpp(ch)) {
    List cms = Rf_getAttrib(ch, measures_sym);
    if(!Rf_isNull(cms) && !Rf_isNull(cms[".size"])) {
      return as<int>(cms[".size"]);
    }
  }
  Function measure = as<Function>(size_monoid["measure"]);
  return as<int>(measure(ch));
}

SEXP measure_sequence(const List& xs, const std::string& monoid_name, const List& monoid_spec) {
  Function f = as<Function>(monoid_spec["f"]);
  ReprotectSEXP acc(static_cast<SEXP>(monoid_spec["i"]));
  for(int i = 0; i < xs.size(); ++i) {
    SEXP m = monoid_measure_for_child(xs[i], monoid_name, monoid_spec);
    acc.set(f(acc.get(), m));
  }
  return acc.get();
}

bool predicate_true(const Function& predicate, SEXP x) {
  return as<bool>(predicate(x));
}

SEXP node_measure_named(SEXP x, const std::string& monoid_name) {
  List ms = Rf_getAttrib(x, measures_sym);
  if(Rf_isNull(ms) || Rf_isNull(ms[monoid_name])) {
    stop("Missing cached measure on structural node.");
  }
  return ms[monoid_name];
}

List locate_tree_impl_cpp(
  const Function& predicate,
  SEXP i,
  SEXP t,
  const List& monoids,
  const std::string& monoid_name,
  const List& monoid_spec,
  const List& size_monoid,
  int i_size
);

List locate_digit_impl_cpp(
  const Function& predicate,
  SEXP i,
  const List& digit,
  const List& monoids,
  const std::string& monoid_name,
  const List& monoid_spec,
  const List& size_monoid,
  int i_size
) {
  if(digit.size() == 0) {
    stop("locate_digit called with empty digit");
  }

  Function f = as<Function>(monoid_spec["f"]);
  ReprotectSEXP acc(i);
  int size_before = i_size;

  for(int idx = 0; idx < digit.size(); ++idx) {
    SEXP el = digit[idx];
    SEXP m_el = monoid_measure_for_child(el, monoid_name, monoid_spec);
    int n_el = size_for_child(el, size_monoid);
    Shield<SEXP> acc_after(f(acc.get(), m_el));

    if(predicate_true(predicate, acc_after)) {
      if(is_structural_node_cpp(el)) {
        return locate_tree_impl_cpp(
          predicate, acc.get(), el, monoids, monoid_name, monoid_spec, size_monoid, size_before
        );
      }

      List right;
      if(idx + 1 < digit.size()) {
        right = List(digit.size() - idx - 1);
        int j = 0;
        for(int k = idx + 1; k < digit.size(); ++k) {
          right[j++] = digit[k];
        }
      } else {
        right = List(0);
      }
      SEXP right_measure = measure_sequence(right, monoid_name, monoid_spec);

      return List::create(
        _["found"] = true,
        _["elem"] = el,
        _["left_measure"] = acc.get(),
        _["hit_measure"] = acc_after,
        _["right_measure"] = right_measure,
        _["index"] = size_before + 1
      );
    }

    acc.set(acc_after);
    size_before += n_el;
  }

  return List::create(
    _["found"] = false,
    _["elem"] = R_NilValue,
    _["left_measure"] = acc.get(),
    _["hit_measure"] = R_NilValue,
    _["right_measure"] = R_NilValue,
    _["index"] = R_NilValue
  );
}

List locate_tree_impl_cpp(
  const Function& predicate,
  SEXP i,
  SEXP t,
  const List& monoids,
  const std::string& monoid_name,
  const List& monoid_spec,
  const List& size_monoid,
  int i_size
) {
  if(!is_structural_node_cpp(t)) {
    List d = List::create(t);
    return locate_digit_impl_cpp(
      predicate, i, d, monoids, monoid_name, monoid_spec, size_monoid, i_size
    );
  }

  if(has_class(t, "Empty")) {
    return List::create(
      _["found"] = false,
      _["elem"] = R_NilValue,
      _["left_measure"] = i,
      _["hit_measure"] = R_NilValue,
      _["right_measure"] = R_NilValue,
      _["index"] = R_NilValue
    );
  }

  if(has_class(t, "Single")) {
    List s(t);
    List d = List::create(s[0]);
    return locate_digit_impl_cpp(
      predicate, i, d, monoids, monoid_name, monoid_spec, size_monoid, i_size
    );
  }

  if(has_class(t, "Deep")) {
    List d(t);
    SEXP prefix = d["prefix"];
    SEXP middle = d["middle"];
    SEXP suffix = d["suffix"];

    List pm = Rf_getAttrib(prefix, measures_sym);
    List mm = Rf_getAttrib(middle, measures_sym);
    List sm = Rf_getAttrib(suffix, measures_sym);

    SEXP mpr = pm[monoid_name];
    SEXP mmid = mm[monoid_name];
    SEXP msf = sm[monoid_name];

    Function f = as<Function>(monoid_spec["f"]);
    Shield<SEXP> vpr(f(i, mpr));
    Shield<SEXP> vm(f(vpr, mmid));

    int npr = as<int>(pm[".size"]);
    int nm = as<int>(mm[".size"]);

    if(predicate_true(predicate, vpr)) {
      List res = locate_tree_impl_cpp(
        predicate, i, prefix, monoids, monoid_name, monoid_spec, size_monoid, i_size
      );
      if(as<bool>(res["found"])) {
        Shield<SEXP> r(static_cast<SEXP>(res["right_measure"]));
        Shield<SEXP> rr(f(r, mmid));
        res["right_measure"] = f(rr, msf);
      }
      return res;
    }

    if(predicate_true(predicate, vm)) {
      List res = locate_tree_impl_cpp(
        predicate, vpr, middle, monoids, monoid_name, monoid_spec, size_monoid, i_size + npr
      );
      if(as<bool>(res["found"])) {
        Shield<SEXP> r(static_cast<SEXP>(res["right_measure"]));
        res["right_measure"] = f(r, msf);
      }
      return res;
    }

    return locate_tree_impl_cpp(
      predicate, vm, suffix, monoids, monoid_name, monoid_spec, size_monoid, i_size + npr + nm
    );
  }

  List as_list(t);
  return locate_digit_impl_cpp(
    predicate, i, as_list, monoids, monoid_name, monoid_spec, size_monoid, i_size
  );
}

SEXP build_digit_cpp(const List& xs, const List& monoids) {
  if(xs.size() == 0) {
    return List(0);
  }
  return make_digit(xs, monoids);
}

SEXP digit_to_tree_cpp(const List& d, const List& monoids) {
  const int n = d.size();
  if(n == 0) {
    return make_empty(monoids);
  }
  if(n == 1) {
    return make_single(d[0], monoids);
  }
  if(n == 2) {
    return make_deep(
      make_digit(List::create(d[0]), monoids),
      make_empty(monoids),
      make_digit(List::create(d[1]), monoids),
      monoids
    );
  }
  if(n == 3) {
    return make_deep(
      make_digit(List::create(d[0], d[1]), monoids),
      make_empty(monoids),
      make_digit(List::create(d[2]), monoids),
      monoids
    );
  }
  if(n == 4) {
    return make_deep(
      make_digit(List::create(d[0], d[1]), monoids),
      make_empty(monoids),
      make_digit(List::create(d[2], d[3]), monoids),
      monoids
    );
  }
  stop("digit_to_tree expects a digit of size 0..4");
}

SEXP node_to_digit_cpp(SEXP node, const List& monoids) {
  List xs(node);
  return make_digit(xs, monoids);
}

List viewL_cpp(SEXP t, const List& monoids);
List viewR_cpp(SEXP t, const List& monoids);

SEXP deepL_cpp(SEXP pr, SEXP m, SEXP sf, const List& monoids) {
  if(Rf_xlength(pr) > 0) {
    return make_deep(pr, m, sf, monoids);
  }
  if(has_class(m, "Empty")) {
    return digit_to_tree_cpp(List(sf), monoids);
  }
  List res = viewL_cpp(m, monoids);
  Shield<SEXP> node(static_cast<SEXP>(res["elem"]));
  Shield<SEXP> m_rest(static_cast<SEXP>(res["rest"]));
  Shield<SEXP> new_pr(node_to_digit_cpp(node, monoids));
  return make_deep(new_pr, m_rest, sf, monoids);
}

SEXP deepR_cpp(SEXP pr, SEXP m, SEXP sf, const List& monoids) {
  if(Rf_xlength(sf) > 0) {
    return make_deep(pr, m, sf, monoids);
  }
  if(has_class(m, "Empty")) {
    return digit_to_tree_cpp(List(pr), monoids);
  }
  List res = viewR_cpp(m, monoids);
  Shield<SEXP> node(static_cast<SEXP>(res["elem"]));
  Shield<SEXP> m_rest(static_cast<SEXP>(res["rest"]));
  Shield<SEXP> new_sf(node_to_digit_cpp(node, monoids));
  return make_deep(pr, m_rest, new_sf, monoids);
}

List viewL_cpp(SEXP t, const List& monoids) {
  if(has_class(t, "Empty")) {
    stop("viewL on Empty");
  }
  if(has_class(t, "Single")) {
    List s(t);
    return List::create(_["elem"] = s[0], _["rest"] = make_empty(monoids));
  }

  List d(t);
  List pr = d["prefix"];
  if(pr.size() > 1) {
    Shield<SEXP> head(static_cast<SEXP>(pr[0]));
    List tail(pr.size() - 1);
    for(int i = 1; i < pr.size(); ++i) tail[i - 1] = pr[i];
    Shield<SEXP> new_pr(build_digit_cpp(tail, monoids));
    return List::create(
      _["elem"] = head,
      _["rest"] = make_deep(new_pr, d["middle"], d["suffix"], monoids)
    );
  }

  Shield<SEXP> head(static_cast<SEXP>(pr[0]));
  Shield<SEXP> m(static_cast<SEXP>(d["middle"]));
  if(has_class(m, "Empty")) {
    return List::create(_["elem"] = head, _["rest"] = digit_to_tree_cpp(List(d["suffix"]), monoids));
  }
  List res = viewL_cpp(m, monoids);
  Shield<SEXP> node(static_cast<SEXP>(res["elem"]));
  Shield<SEXP> m_rest(static_cast<SEXP>(res["rest"]));
  Shield<SEXP> new_pr(node_to_digit_cpp(node, monoids));
  return List::create(_["elem"] = head, _["rest"] = make_deep(new_pr, m_rest, d["suffix"], monoids));
}

List viewR_cpp(SEXP t, const List& monoids) {
  if(has_class(t, "Empty")) {
    stop("viewR on Empty");
  }
  if(has_class(t, "Single")) {
    List s(t);
    return List::create(_["elem"] = s[0], _["rest"] = make_empty(monoids));
  }

  List d(t);
  List sf = d["suffix"];
  if(sf.size() > 1) {
    Shield<SEXP> head(static_cast<SEXP>(sf[sf.size() - 1]));
    List tail(sf.size() - 1);
    for(int i = 0; i < sf.size() - 1; ++i) tail[i] = sf[i];
    Shield<SEXP> new_sf(build_digit_cpp(tail, monoids));
    return List::create(
      _["elem"] = head,
      _["rest"] = make_deep(d["prefix"], d["middle"], new_sf, monoids)
    );
  }

  Shield<SEXP> head(static_cast<SEXP>(sf[0]));
  Shield<SEXP> m(static_cast<SEXP>(d["middle"]));
  if(has_class(m, "Empty")) {
    return List::create(_["elem"] = head, _["rest"] = digit_to_tree_cpp(List(d["prefix"]), monoids));
  }
  List res = viewR_cpp(m, monoids);
  Shield<SEXP> node(static_cast<SEXP>(res["elem"]));
  Shield<SEXP> m_rest(static_cast<SEXP>(res["rest"]));
  Shield<SEXP> new_sf(node_to_digit_cpp(node, monoids));
  return List::create(_["elem"] = head, _["rest"] = make_deep(d["prefix"], m_rest, new_sf, monoids));
}

List split_digit_cpp(
  const Function& predicate,
  SEXP i,
  const List& digit,
  const std::string& monoid_name,
  const List& monoid_spec
) {
  if(digit.size() == 0) {
    stop("split_digit called with empty digit");
  }

  Function f = as<Function>(monoid_spec["f"]);
  ReprotectSEXP acc(i);
  for(int idx = 0; idx < digit.size(); ++idx) {
    SEXP el = digit[idx];
    SEXP m_el = monoid_measure_for_child(el, monoid_name, monoid_spec);
    Shield<SEXP> acc_after(f(acc.get(), m_el));
    if(predicate_true(predicate, acc_after)) {
      List left(idx);
      for(int j = 0; j < idx; ++j) left[j] = digit[j];
      List right(digit.size() - idx - 1);
      for(int j = idx + 1; j < digit.size(); ++j) right[j - idx - 1] = digit[j];
      return List::create(_["left"] = left, _["elem"] = el, _["right"] = right);
    }
    acc.set(acc_after);
  }
  stop("split_digit: predicate never became true; precondition violated");
}

List split_tree_impl_cpp(
  const Function& predicate,
  SEXP i,
  SEXP t,
  const List& monoids,
  const std::string& monoid_name,
  const List& monoid_spec
) {
  if(has_class(t, "Empty")) {
    stop("split_tree_impl requires a non-empty tree");
  }

  Function f = as<Function>(monoid_spec["f"]);
  if(has_class(t, "Single")) {
    List s(t);
    return List::create(
      _["left"] = make_empty(monoids),
      _["elem"] = s[0],
      _["right"] = make_empty(monoids)
    );
  }

  List d(t);
  SEXP prefix = d["prefix"];
  SEXP middle = d["middle"];
  SEXP suffix = d["suffix"];

  Shield<SEXP> vpr(f(i, node_measure_named(prefix, monoid_name)));
  Shield<SEXP> vm(f(vpr, node_measure_named(middle, monoid_name)));

  if(predicate_true(predicate, vpr)) {
    List s = split_digit_cpp(predicate, i, List(prefix), monoid_name, monoid_spec);
    Shield<SEXP> left_tree(digit_to_tree_cpp(as<List>(s["left"]), monoids));
    Shield<SEXP> right_digit(build_digit_cpp(as<List>(s["right"]), monoids));
    Shield<SEXP> right_tree(deepL_cpp(right_digit, middle, suffix, monoids));
    return List::create(_["left"] = left_tree, _["elem"] = s["elem"], _["right"] = right_tree);
  }

  if(predicate_true(predicate, vm)) {
    List sm = split_tree_impl_cpp(predicate, vpr, middle, monoids, monoid_name, monoid_spec);
    Shield<SEXP> inode(f(vpr, node_measure_named(sm["left"], monoid_name)));
    List sx = split_digit_cpp(predicate, inode, List(sm["elem"]), monoid_name, monoid_spec);
    Shield<SEXP> left_digit(build_digit_cpp(as<List>(sx["left"]), monoids));
    Shield<SEXP> right_digit(build_digit_cpp(as<List>(sx["right"]), monoids));
    Shield<SEXP> left_tree(deepR_cpp(prefix, sm["left"], left_digit, monoids));
    Shield<SEXP> right_tree(deepL_cpp(right_digit, sm["right"], suffix, monoids));
    return List::create(_["left"] = left_tree, _["elem"] = sx["elem"], _["right"] = right_tree);
  }

  List s = split_digit_cpp(predicate, vm, List(suffix), monoid_name, monoid_spec);
  Shield<SEXP> left_digit(build_digit_cpp(as<List>(s["left"]), monoids));
  Shield<SEXP> left_tree(deepR_cpp(prefix, middle, left_digit, monoids));
  Shield<SEXP> right_tree(digit_to_tree_cpp(as<List>(s["right"]), monoids));
  return List::create(_["left"] = left_tree, _["elem"] = s["elem"], _["right"] = right_tree);
}

} // namespace

extern "C" SEXP ft_cpp_append_right(SEXP t, SEXP el, SEXP monoids_) {
  BEGIN_RCPP
  List monoids(monoids_);
  return add_right_cpp(t, el, monoids);
  END_RCPP
}

extern "C" SEXP ft_cpp_prepend_left(SEXP t, SEXP el, SEXP monoids_) {
  BEGIN_RCPP
  List monoids(monoids_);
  return add_left_cpp(t, el, monoids);
  END_RCPP
}

extern "C" SEXP ft_cpp_append_right_named(SEXP t, SEXP el, SEXP name_, SEXP monoids_) {
  BEGIN_RCPP
  List monoids(monoids_);
  Shield<SEXP> named_el(clone_with_name(el, name_));
  return add_right_cpp(t, named_el, monoids);
  END_RCPP
}

extern "C" SEXP ft_cpp_prepend_left_named(SEXP t, SEXP el, SEXP name_, SEXP monoids_) {
  BEGIN_RCPP
  List monoids(monoids_);
  Shield<SEXP> named_el(clone_with_name(el, name_));
  return add_left_cpp(t, named_el, monoids);
  END_RCPP
}

extern "C" SEXP ft_cpp_tree_from(SEXP elements_, SEXP monoids_) {
  BEGIN_RCPP
  List elements(elements_);
  List monoids(monoids_);
  ReprotectSEXP t(make_empty(monoids));
  for(int i = 0; i < elements.size(); ++i) {
    t.set(add_right_cpp(t.get(), elements[i], monoids));
  }
  return t.get();
  END_RCPP
}

extern "C" SEXP ft_cpp_tree_from_prepared(SEXP elements_, SEXP names_, SEXP monoids_) {
  BEGIN_RCPP
  List elements(elements_);
  List monoids(monoids_);
  const bool has_names = !Rf_isNull(names_);
  CharacterVector names;
  if(has_names) {
    names = as<CharacterVector>(names_);
    if(names.size() != elements.size()) {
      stop("`names` length must match elements length.");
    }
  }

  ReprotectSEXP t(make_empty(monoids));
  for(int i = 0; i < elements.size(); ++i) {
    Shield<SEXP> name(
      has_names ? static_cast<SEXP>(CharacterVector::create(names[i])) : R_NilValue
    );
    Shield<SEXP> el(clone_with_attrs(elements[i], name));
    t.set(add_right_cpp(t.get(), el, monoids));
  }
  return t.get();
  END_RCPP
}

extern "C" SEXP ft_cpp_tree_from_sorted(SEXP elements_, SEXP monoids_) {
  BEGIN_RCPP
  List elements(elements_);
  List monoids(monoids_);
  return tree_from_sorted_list_cpp(elements, monoids);
  END_RCPP
}

extern "C" SEXP ft_cpp_concat(SEXP x, SEXP y, SEXP monoids_) {
  BEGIN_RCPP
  List monoids(monoids_);
  List ts(0);
  return app3_cpp(x, ts, y, monoids);
  END_RCPP
}

extern "C" SEXP ft_cpp_oms_set_merge(SEXP x, SEXP y, SEXP mode_, SEXP monoids_, SEXP key_type_) {
  BEGIN_RCPP
  std::string mode = as<std::string>(mode_);
  if(mode != "union" && mode != "intersection" && mode != "difference") {
    stop("Unknown ordered_multiset merge mode.");
  }
  std::string key_type = as<std::string>(key_type_);
  if(key_type != "numeric" && key_type != "character" && key_type != "logical") {
    stop("Unsupported ordered_multiset key type.");
  }
  List monoids(monoids_);
  return oms_set_merge_cpp_impl(x, y, mode, monoids, key_type);
  END_RCPP
}

extern "C" SEXP ft_cpp_locate(SEXP t, SEXP predicate_, SEXP monoids_, SEXP monoid_name_, SEXP i_) {
  BEGIN_RCPP
  Function predicate(predicate_);
  List monoids(monoids_);
  std::string monoid_name = as<std::string>(monoid_name_);
  SEXP monoid_spec_sexp = monoids[monoid_name];
  if(Rf_isNull(monoid_spec_sexp)) {
    stop("Unknown monoid name.");
  }
  List monoid_spec(monoid_spec_sexp);
  List size_monoid = as<List>(monoids[".size"]);
  return locate_tree_impl_cpp(predicate, i_, t, monoids, monoid_name, monoid_spec, size_monoid, 0);
  END_RCPP
}

extern "C" SEXP ft_cpp_split_tree(SEXP t, SEXP predicate_, SEXP monoids_, SEXP monoid_name_, SEXP i_) {
  BEGIN_RCPP
  Function predicate(predicate_);
  List monoids(monoids_);
  std::string monoid_name = as<std::string>(monoid_name_);
  SEXP monoid_spec_sexp = monoids[monoid_name];
  if(Rf_isNull(monoid_spec_sexp)) {
    stop("Unknown monoid name.");
  }
  List monoid_spec(monoid_spec_sexp);
  return split_tree_impl_cpp(predicate, i_, t, monoids, monoid_name, monoid_spec);
  END_RCPP
}

extern "C" SEXP ft_cpp_find_name_position(SEXP t, SEXP name_) {
  BEGIN_RCPP
  CharacterVector nm(name_);
  if(nm.size() != 1 || CharacterVector::is_na(nm[0])) {
    stop("`name` must be a single non-missing string.");
  }
  std::string target = as<std::string>(nm[0]);
  if(target.empty()) {
    stop("`name` must be a single non-empty string.");
  }
  int p = find_name_position_impl(t, target, 0);
  if(p < 0) {
    return IntegerVector::create(NA_INTEGER);
  }
  return IntegerVector::create(p);
  END_RCPP
}

extern "C" SEXP ft_cpp_get_by_index(SEXP t, SEXP idx_) {
  BEGIN_RCPP
  IntegerVector idx(idx_);
  if(idx.size() != 1 || IntegerVector::is_na(idx[0])) {
    stop("`idx` must be a single non-missing integer index.");
  }
  return get_by_index_impl(t, idx[0]);
  END_RCPP
}

extern "C" SEXP ft_cpp_get_many_by_index(SEXP t, SEXP idx_) {
  BEGIN_RCPP
  IntegerVector idx(idx_);
  List out(idx.size());
  for(int i = 0; i < idx.size(); ++i) {
    if(IntegerVector::is_na(idx[i])) {
      stop("Only non-missing integer indices are supported.");
    }
    out[i] = get_by_index_impl(t, idx[i]);
  }
  return out;
  END_RCPP
}

extern "C" SEXP ft_cpp_name_positions(SEXP t) {
  BEGIN_RCPP
  SEXP m = Rf_getAttrib(t, measures_sym);
  if(Rf_isNull(m)) {
    stop("Tree has no measures attribute.");
  }
  List ms(m);
  const int n = as<int>(ms[".size"]);
  const int nn = as<int>(ms[".named_count"]);
  if(n == 0 || nn == 0) {
    stop("Tree has no element names.");
  }
  if(nn != n) {
    stop("Invalid name state: mixed named and unnamed elements are not allowed.");
  }

  CharacterVector nms(n);
  int pos = 0;
  collect_names_impl(t, nms, pos);
  if(pos != n) {
    stop("Name collection mismatch with .size measure.");
  }

  IntegerVector out(n);
  for(int i = 0; i < n; ++i) {
    out[i] = i + 1;
  }
  out.attr("names") = nms;
  return out;
  END_RCPP
}
