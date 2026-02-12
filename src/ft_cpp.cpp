#include <Rcpp.h>

using namespace Rcpp;

namespace {

SEXP class_sym = Rf_install("class");
SEXP monoids_sym = Rf_install("monoids");
SEXP measures_sym = Rf_install("measures");
SEXP ft_name_sym = Rf_install("ft_name");

bool has_class(SEXP x, const char* cls) {
  return Rf_inherits(x, cls);
}

bool has_name_attr(SEXP x) {
  SEXP nm = Rf_getAttrib(x, ft_name_sym);
  return !Rf_isNull(nm) && XLENGTH(nm) > 0;
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

List measures_from_children(const List& children, const List& monoids) {
  CharacterVector nms = monoids.names();
  List out(nms.size());
  out.attr("names") = nms;
  double s = 0.0;
  int c = 0;
  for(int i = 0; i < children.size(); ++i) {
    s += child_size(children[i]);
    c += child_named_count(children[i]);
  }
  for(int i = 0; i < nms.size(); ++i) {
    std::string nm = as<std::string>(nms[i]);
    if(nm == ".size") out[i] = s;
    else if(nm == ".named_count") out[i] = c;
    else out[i] = R_NilValue;
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
    else out[i] = R_NilValue;
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
    SEXP prefix = make_digit(List::create(s[0]), monoids);
    SEXP middle = make_empty(monoids);
    SEXP suffix = make_digit(List::create(el), monoids);
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
      SEXP new_suffix = make_digit(List::create(suffix[3], el), monoids);
      SEXP middle_node = make_node3(suffix[0], suffix[1], suffix[2], monoids);
      SEXP new_middle = add_right_cpp(d["middle"], middle_node, monoids);
      return make_deep(d["prefix"], new_middle, new_suffix, monoids);
    }
    SEXP new_suffix = add_right_cpp(d["suffix"], el, monoids);
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
    SEXP prefix = make_digit(List::create(el), monoids);
    SEXP middle = make_empty(monoids);
    SEXP suffix = make_digit(List::create(s[0]), monoids);
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
      SEXP new_prefix = make_digit(List::create(el, prefix[0]), monoids);
      SEXP middle_node = make_node3(prefix[1], prefix[2], prefix[3], monoids);
      SEXP new_middle = add_left_cpp(d["middle"], middle_node, monoids);
      return make_deep(new_prefix, new_middle, d["suffix"], monoids);
    }
    SEXP new_prefix = add_left_cpp(d["prefix"], el, monoids);
    return make_deep(new_prefix, d["middle"], d["suffix"], monoids);
  }
  stop("Unsupported node type in ft_cpp_prepend_left.");
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

extern "C" SEXP ft_cpp_tree_from(SEXP elements_, SEXP monoids_) {
  BEGIN_RCPP
  List elements(elements_);
  List monoids(monoids_);
  SEXP t = make_empty(monoids);
  for(int i = 0; i < elements.size(); ++i) {
    t = add_right_cpp(t, elements[i], monoids);
  }
  return t;
  END_RCPP
}
