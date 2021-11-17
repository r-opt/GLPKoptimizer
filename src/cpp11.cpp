// Generated by cpp11: do not edit by hand
// clang-format off

#include "GLPKoptimizer_types.h"
#include "cpp11/declarations.hpp"
#include <R_ext/Visibility.h>

// r-solver.cpp
external_pointer<glpk_solver> glpk_init();
extern "C" SEXP _GLPKoptimizer_glpk_init() {
  BEGIN_CPP11
    return cpp11::as_sexp(glpk_init());
  END_CPP11
}
// r-solver.cpp
int glpk_add_cols(external_pointer<glpk_solver> ptr, int cols);
extern "C" SEXP _GLPKoptimizer_glpk_add_cols(SEXP ptr, SEXP cols) {
  BEGIN_CPP11
    return cpp11::as_sexp(glpk_add_cols(cpp11::as_cpp<cpp11::decay_t<external_pointer<glpk_solver>>>(ptr), cpp11::as_cpp<cpp11::decay_t<int>>(cols)));
  END_CPP11
}
// r-solver.cpp
int glpk_add_rows(external_pointer<glpk_solver> ptr, int rows);
extern "C" SEXP _GLPKoptimizer_glpk_add_rows(SEXP ptr, SEXP rows) {
  BEGIN_CPP11
    return cpp11::as_sexp(glpk_add_rows(cpp11::as_cpp<cpp11::decay_t<external_pointer<glpk_solver>>>(ptr), cpp11::as_cpp<cpp11::decay_t<int>>(rows)));
  END_CPP11
}
// r-solver.cpp
void glpk_set_col_kind(external_pointer<glpk_solver> ptr, int column_idx, int type);
extern "C" SEXP _GLPKoptimizer_glpk_set_col_kind(SEXP ptr, SEXP column_idx, SEXP type) {
  BEGIN_CPP11
    glpk_set_col_kind(cpp11::as_cpp<cpp11::decay_t<external_pointer<glpk_solver>>>(ptr), cpp11::as_cpp<cpp11::decay_t<int>>(column_idx), cpp11::as_cpp<cpp11::decay_t<int>>(type));
    return R_NilValue;
  END_CPP11
}
// r-solver.cpp
void glpk_set_col_bnd(external_pointer<glpk_solver> ptr, int column_idx, int type, double lb, double ub);
extern "C" SEXP _GLPKoptimizer_glpk_set_col_bnd(SEXP ptr, SEXP column_idx, SEXP type, SEXP lb, SEXP ub) {
  BEGIN_CPP11
    glpk_set_col_bnd(cpp11::as_cpp<cpp11::decay_t<external_pointer<glpk_solver>>>(ptr), cpp11::as_cpp<cpp11::decay_t<int>>(column_idx), cpp11::as_cpp<cpp11::decay_t<int>>(type), cpp11::as_cpp<cpp11::decay_t<double>>(lb), cpp11::as_cpp<cpp11::decay_t<double>>(ub));
    return R_NilValue;
  END_CPP11
}
// r-solver.cpp
void glpk_set_row_bnd(external_pointer<glpk_solver> ptr, int row_idx, int type, double lb, double ub);
extern "C" SEXP _GLPKoptimizer_glpk_set_row_bnd(SEXP ptr, SEXP row_idx, SEXP type, SEXP lb, SEXP ub) {
  BEGIN_CPP11
    glpk_set_row_bnd(cpp11::as_cpp<cpp11::decay_t<external_pointer<glpk_solver>>>(ptr), cpp11::as_cpp<cpp11::decay_t<int>>(row_idx), cpp11::as_cpp<cpp11::decay_t<int>>(type), cpp11::as_cpp<cpp11::decay_t<double>>(lb), cpp11::as_cpp<cpp11::decay_t<double>>(ub));
    return R_NilValue;
  END_CPP11
}
// r-solver.cpp
void glpk_set_mat_row(external_pointer<glpk_solver> ptr, int row_idx, integers indexes, doubles values);
extern "C" SEXP _GLPKoptimizer_glpk_set_mat_row(SEXP ptr, SEXP row_idx, SEXP indexes, SEXP values) {
  BEGIN_CPP11
    glpk_set_mat_row(cpp11::as_cpp<cpp11::decay_t<external_pointer<glpk_solver>>>(ptr), cpp11::as_cpp<cpp11::decay_t<int>>(row_idx), cpp11::as_cpp<cpp11::decay_t<integers>>(indexes), cpp11::as_cpp<cpp11::decay_t<doubles>>(values));
    return R_NilValue;
  END_CPP11
}
// r-solver.cpp
void glpk_set_obj_coefs(external_pointer<glpk_solver> ptr, int col_idx, double value);
extern "C" SEXP _GLPKoptimizer_glpk_set_obj_coefs(SEXP ptr, SEXP col_idx, SEXP value) {
  BEGIN_CPP11
    glpk_set_obj_coefs(cpp11::as_cpp<cpp11::decay_t<external_pointer<glpk_solver>>>(ptr), cpp11::as_cpp<cpp11::decay_t<int>>(col_idx), cpp11::as_cpp<cpp11::decay_t<double>>(value));
    return R_NilValue;
  END_CPP11
}
// r-solver.cpp
void glpk_set_obj_dir(external_pointer<glpk_solver> ptr, int dir);
extern "C" SEXP _GLPKoptimizer_glpk_set_obj_dir(SEXP ptr, SEXP dir) {
  BEGIN_CPP11
    glpk_set_obj_dir(cpp11::as_cpp<cpp11::decay_t<external_pointer<glpk_solver>>>(ptr), cpp11::as_cpp<cpp11::decay_t<int>>(dir));
    return R_NilValue;
  END_CPP11
}
// r-solver.cpp
void glpk_solve_simplex(external_pointer<glpk_solver> ptr);
extern "C" SEXP _GLPKoptimizer_glpk_solve_simplex(SEXP ptr) {
  BEGIN_CPP11
    glpk_solve_simplex(cpp11::as_cpp<cpp11::decay_t<external_pointer<glpk_solver>>>(ptr));
    return R_NilValue;
  END_CPP11
}
// r-solver.cpp
void glpk_solve_MIP(external_pointer<glpk_solver> ptr);
extern "C" SEXP _GLPKoptimizer_glpk_solve_MIP(SEXP ptr) {
  BEGIN_CPP11
    glpk_solve_MIP(cpp11::as_cpp<cpp11::decay_t<external_pointer<glpk_solver>>>(ptr));
    return R_NilValue;
  END_CPP11
}
// r-solver.cpp
double glpk_get_mip_col_val(external_pointer<glpk_solver> ptr, int column_idx);
extern "C" SEXP _GLPKoptimizer_glpk_get_mip_col_val(SEXP ptr, SEXP column_idx) {
  BEGIN_CPP11
    return cpp11::as_sexp(glpk_get_mip_col_val(cpp11::as_cpp<cpp11::decay_t<external_pointer<glpk_solver>>>(ptr), cpp11::as_cpp<cpp11::decay_t<int>>(column_idx)));
  END_CPP11
}
// r-solver.cpp
double glpk_solver_get_col_prim(external_pointer<glpk_solver> ptr, int column_idx);
extern "C" SEXP _GLPKoptimizer_glpk_solver_get_col_prim(SEXP ptr, SEXP column_idx) {
  BEGIN_CPP11
    return cpp11::as_sexp(glpk_solver_get_col_prim(cpp11::as_cpp<cpp11::decay_t<external_pointer<glpk_solver>>>(ptr), cpp11::as_cpp<cpp11::decay_t<int>>(column_idx)));
  END_CPP11
}
// r-solver.cpp
void glpk_set_irowgen_callback(external_pointer<glpk_solver> ptr, function fun);
extern "C" SEXP _GLPKoptimizer_glpk_set_irowgen_callback(SEXP ptr, SEXP fun) {
  BEGIN_CPP11
    glpk_set_irowgen_callback(cpp11::as_cpp<cpp11::decay_t<external_pointer<glpk_solver>>>(ptr), cpp11::as_cpp<cpp11::decay_t<function>>(fun));
    return R_NilValue;
  END_CPP11
}
// r-solver.cpp
double glpk_mip_obj_val(external_pointer<glpk_solver> ptr);
extern "C" SEXP _GLPKoptimizer_glpk_mip_obj_val(SEXP ptr) {
  BEGIN_CPP11
    return cpp11::as_sexp(glpk_mip_obj_val(cpp11::as_cpp<cpp11::decay_t<external_pointer<glpk_solver>>>(ptr)));
  END_CPP11
}
// r-solver.cpp
void glpk_set_mip_obj_constant(external_pointer<glpk_solver> ptr, double constant);
extern "C" SEXP _GLPKoptimizer_glpk_set_mip_obj_constant(SEXP ptr, SEXP constant) {
  BEGIN_CPP11
    glpk_set_mip_obj_constant(cpp11::as_cpp<cpp11::decay_t<external_pointer<glpk_solver>>>(ptr), cpp11::as_cpp<cpp11::decay_t<double>>(constant));
    return R_NilValue;
  END_CPP11
}
// r-solver.cpp
int glpk_get_num_cols(external_pointer<glpk_solver> ptr);
extern "C" SEXP _GLPKoptimizer_glpk_get_num_cols(SEXP ptr) {
  BEGIN_CPP11
    return cpp11::as_sexp(glpk_get_num_cols(cpp11::as_cpp<cpp11::decay_t<external_pointer<glpk_solver>>>(ptr)));
  END_CPP11
}
// r-solver.cpp
int glpk_get_num_rows(external_pointer<glpk_solver> ptr);
extern "C" SEXP _GLPKoptimizer_glpk_get_num_rows(SEXP ptr) {
  BEGIN_CPP11
    return cpp11::as_sexp(glpk_get_num_rows(cpp11::as_cpp<cpp11::decay_t<external_pointer<glpk_solver>>>(ptr)));
  END_CPP11
}
// r-solver.cpp
int glpk_mip_status(external_pointer<glpk_solver> ptr);
extern "C" SEXP _GLPKoptimizer_glpk_mip_status(SEXP ptr) {
  BEGIN_CPP11
    return cpp11::as_sexp(glpk_mip_status(cpp11::as_cpp<cpp11::decay_t<external_pointer<glpk_solver>>>(ptr)));
  END_CPP11
}

extern "C" {
static const R_CallMethodDef CallEntries[] = {
    {"_GLPKoptimizer_glpk_add_cols",             (DL_FUNC) &_GLPKoptimizer_glpk_add_cols,             2},
    {"_GLPKoptimizer_glpk_add_rows",             (DL_FUNC) &_GLPKoptimizer_glpk_add_rows,             2},
    {"_GLPKoptimizer_glpk_get_mip_col_val",      (DL_FUNC) &_GLPKoptimizer_glpk_get_mip_col_val,      2},
    {"_GLPKoptimizer_glpk_get_num_cols",         (DL_FUNC) &_GLPKoptimizer_glpk_get_num_cols,         1},
    {"_GLPKoptimizer_glpk_get_num_rows",         (DL_FUNC) &_GLPKoptimizer_glpk_get_num_rows,         1},
    {"_GLPKoptimizer_glpk_init",                 (DL_FUNC) &_GLPKoptimizer_glpk_init,                 0},
    {"_GLPKoptimizer_glpk_mip_obj_val",          (DL_FUNC) &_GLPKoptimizer_glpk_mip_obj_val,          1},
    {"_GLPKoptimizer_glpk_mip_status",           (DL_FUNC) &_GLPKoptimizer_glpk_mip_status,           1},
    {"_GLPKoptimizer_glpk_set_col_bnd",          (DL_FUNC) &_GLPKoptimizer_glpk_set_col_bnd,          5},
    {"_GLPKoptimizer_glpk_set_col_kind",         (DL_FUNC) &_GLPKoptimizer_glpk_set_col_kind,         3},
    {"_GLPKoptimizer_glpk_set_irowgen_callback", (DL_FUNC) &_GLPKoptimizer_glpk_set_irowgen_callback, 2},
    {"_GLPKoptimizer_glpk_set_mat_row",          (DL_FUNC) &_GLPKoptimizer_glpk_set_mat_row,          4},
    {"_GLPKoptimizer_glpk_set_mip_obj_constant", (DL_FUNC) &_GLPKoptimizer_glpk_set_mip_obj_constant, 2},
    {"_GLPKoptimizer_glpk_set_obj_coefs",        (DL_FUNC) &_GLPKoptimizer_glpk_set_obj_coefs,        3},
    {"_GLPKoptimizer_glpk_set_obj_dir",          (DL_FUNC) &_GLPKoptimizer_glpk_set_obj_dir,          2},
    {"_GLPKoptimizer_glpk_set_row_bnd",          (DL_FUNC) &_GLPKoptimizer_glpk_set_row_bnd,          5},
    {"_GLPKoptimizer_glpk_solve_MIP",            (DL_FUNC) &_GLPKoptimizer_glpk_solve_MIP,            1},
    {"_GLPKoptimizer_glpk_solve_simplex",        (DL_FUNC) &_GLPKoptimizer_glpk_solve_simplex,        1},
    {"_GLPKoptimizer_glpk_solver_get_col_prim",  (DL_FUNC) &_GLPKoptimizer_glpk_solver_get_col_prim,  2},
    {NULL, NULL, 0}
};
}

extern "C" attribute_visible void R_init_GLPKoptimizer(DllInfo* dll){
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
