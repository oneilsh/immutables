#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP ft_cpp_append_right(SEXP, SEXP, SEXP);
extern SEXP ft_cpp_prepend_left(SEXP, SEXP, SEXP);
extern SEXP ft_cpp_append_right_named(SEXP, SEXP, SEXP, SEXP);
extern SEXP ft_cpp_prepend_left_named(SEXP, SEXP, SEXP, SEXP);
extern SEXP ft_cpp_tree_from(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"ft_cpp_append_right", (DL_FUNC) &ft_cpp_append_right, 3},
  {"ft_cpp_prepend_left", (DL_FUNC) &ft_cpp_prepend_left, 3},
  {"ft_cpp_append_right_named", (DL_FUNC) &ft_cpp_append_right_named, 4},
  {"ft_cpp_prepend_left_named", (DL_FUNC) &ft_cpp_prepend_left_named, 4},
  {"ft_cpp_tree_from", (DL_FUNC) &ft_cpp_tree_from, 2},
  {NULL, NULL, 0}
};

void R_init_fingertree(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
