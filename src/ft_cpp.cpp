#include <Rcpp.h>

using namespace Rcpp;

namespace {

SEXP class_sym = Rf_install("class");
SEXP monoids_sym = Rf_install("monoids");
SEXP measures_sym = Rf_install("measures");
SEXP ft_name_sym = Rf_install("ft_name");
SEXP value_sym = Rf_install("value");

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
    SEXP acc = r["i"];
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
      acc = f(acc, mv);
    }
    out[i] = acc;
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

SEXP clone_with_attrs(SEXP el, SEXP value_, SEXP name_) {
  SEXP out = PROTECT(Rf_duplicate(el));

  if(!Rf_isNull(value_)) {
    if(Rf_isNull(out)) {
      UNPROTECT(1);
      stop("Cannot attach a value to NULL element.");
    }
    Rf_setAttrib(out, value_sym, value_);
  }

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
  for(int i = 0; i < els.size(); ++i) {
    t = add_right_cpp(t, els[i], monoids);
  }
  return t;
}

SEXP add_all_left_cpp(SEXP t, const List& els, const List& monoids) {
  for(int i = els.size() - 1; i >= 0; --i) {
    t = add_left_cpp(t, els[i], monoids);
  }
  return t;
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

SEXP app3_cpp(SEXP xs, const List& ts, SEXP ys, const List& monoids) {
  if(has_class(xs, "Empty")) {
    return add_all_left_cpp(ys, ts, monoids);
  }
  if(has_class(ys, "Empty")) {
    return add_all_right_cpp(xs, ts, monoids);
  }
  if(has_class(xs, "Single")) {
    List sx(xs);
    return add_left_cpp(add_all_left_cpp(ys, ts, monoids), sx[0], monoids);
  }
  if(has_class(ys, "Single")) {
    List sy(ys);
    return add_right_cpp(add_all_right_cpp(xs, ts, monoids), sy[0], monoids);
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
    SEXP m = app3_cpp(dx["middle"], nts, dy["middle"], monoids);
    return make_deep(dx["prefix"], m, dy["suffix"], monoids);
  }

  stop("Unsupported node type in ft_cpp_concat.");
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
  SEXP named_el = clone_with_name(el, name_);
  return add_right_cpp(t, named_el, monoids);
  END_RCPP
}

extern "C" SEXP ft_cpp_prepend_left_named(SEXP t, SEXP el, SEXP name_, SEXP monoids_) {
  BEGIN_RCPP
  List monoids(monoids_);
  SEXP named_el = clone_with_name(el, name_);
  return add_left_cpp(t, named_el, monoids);
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

extern "C" SEXP ft_cpp_tree_from_prepared(SEXP elements_, SEXP values_, SEXP names_, SEXP monoids_) {
  BEGIN_RCPP
  List elements(elements_);
  List monoids(monoids_);
  const bool has_values = !Rf_isNull(values_);
  const bool has_names = !Rf_isNull(names_);
  List values;
  CharacterVector names;
  if(has_values) {
    values = as<List>(values_);
    if(values.size() != elements.size()) {
      stop("`values` length must match elements length.");
    }
  }
  if(has_names) {
    names = as<CharacterVector>(names_);
    if(names.size() != elements.size()) {
      stop("`names` length must match elements length.");
    }
  }

  SEXP t = make_empty(monoids);
  for(int i = 0; i < elements.size(); ++i) {
    SEXP value = has_values ? static_cast<SEXP>(values[i]) : R_NilValue;
    SEXP name = has_names ? static_cast<SEXP>(CharacterVector::create(names[i])) : R_NilValue;
    SEXP el = clone_with_attrs(elements[i], value, name);
    t = add_right_cpp(t, el, monoids);
  }
  return t;
  END_RCPP
}

extern "C" SEXP ft_cpp_concat(SEXP x, SEXP y, SEXP monoids_) {
  BEGIN_RCPP
  List monoids(monoids_);
  List ts(0);
  return app3_cpp(x, ts, y, monoids);
  END_RCPP
}
