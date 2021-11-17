#include <cpp11.hpp>
#include "solver.h"
using namespace cpp11;

[[cpp11::register]]
external_pointer<glpk_solver> glpk_init() {
  external_pointer<glpk_solver> ptr(new glpk_solver, true);
  return ptr;
}

[[cpp11::register]]
int glpk_add_cols(external_pointer<glpk_solver> ptr, int cols) {
  return ptr->add_cols(cols);
}

[[cpp11::register]]
int glpk_add_rows(external_pointer<glpk_solver> ptr, int rows) {
  return ptr->add_rows(rows);
}

[[cpp11::register]]
void glpk_set_col_kind(external_pointer<glpk_solver> ptr, int column_idx, int type) {
  ptr->set_col_kind(column_idx, type);
}

[[cpp11::register]]
void glpk_set_col_bnd(external_pointer<glpk_solver> ptr, int column_idx, int type, double lb, double ub) {
  ptr->set_col_bnd(column_idx, type, lb, ub);
}

[[cpp11::register]]
void glpk_set_row_bnd(external_pointer<glpk_solver> ptr, int row_idx, int type, double lb, double ub) {
  ptr->set_row_bnd(row_idx, type, lb, ub);
}

[[cpp11::register]]
void glpk_set_mat_row(external_pointer<glpk_solver> ptr, int row_idx, integers indexes, doubles values) {
  ptr->set_mat_row(row_idx, indexes, values);
}

[[cpp11::register]]
void glpk_set_obj_coefs(external_pointer<glpk_solver> ptr, int col_idx, double value) {
  ptr->set_obj_coefs(col_idx, value);
};

[[cpp11::register]]
void glpk_set_obj_dir(external_pointer<glpk_solver> ptr, int dir) {
  ptr->set_obj_dir(dir);
};


[[cpp11::register]]
void glpk_solve_simplex(external_pointer<glpk_solver> ptr) {
  ptr->solve_simplex();
}

[[cpp11::register]]
void glpk_solve_MIP(external_pointer<glpk_solver> ptr) {
  ptr->solve_MIP();
}

[[cpp11::register]]
double glpk_get_mip_col_val(external_pointer<glpk_solver> ptr, int column_idx) {
  return ptr->get_mip_col_val(column_idx);
}

[[cpp11::register]]
double glpk_solver_get_col_prim(external_pointer<glpk_solver> ptr, int column_idx) {
  return ptr->get_col_prim(column_idx);
}

[[cpp11::register]]
void glpk_set_irowgen_callback(external_pointer<glpk_solver> ptr, function fun) {
  ptr->set_irowgen_callback(fun);
}

[[cpp11::register]]
double glpk_mip_obj_val(external_pointer<glpk_solver> ptr) {
  return ptr->mip_obj_val();
};

[[cpp11::register]]
void glpk_set_mip_obj_constant(external_pointer<glpk_solver> ptr, double constant) {
  ptr->set_obj_val_constant(constant);
};

[[cpp11::register]]
int glpk_get_num_cols(external_pointer<glpk_solver> ptr) {
  return ptr->get_num_cols();
};

[[cpp11::register]]
int glpk_get_num_rows(external_pointer<glpk_solver> ptr) {
  return ptr->get_num_rows();
};

[[cpp11::register]]
int glpk_mip_status(external_pointer<glpk_solver> ptr) {
  return ptr->get_mip_status();
};


