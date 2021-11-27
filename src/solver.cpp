#include <glpk.h>
#include "solver.h"
using namespace cpp11;

glpk_solver::glpk_solver(const bool presolve) {
  glp_ptr = glp_create_prob();
  glp_init_iocp(&glp_mip_parameters);
  glp_init_smcp(&glp_simplex_parameters);
  glp_mip_parameters.cb_func = glpk_solver::callback;
  glp_mip_parameters.cb_info = this;
  glp_simplex_parameters.presolve = presolve ? GLP_ON : GLP_OFF;
}

glpk_solver::~glpk_solver() {
  glp_delete_prob(glp_ptr);
}

int glpk_solver::add_cols(int ncols) {
  return glp_add_cols(glp_ptr, ncols);
}

int glpk_solver::add_rows(int nrows) {
  return glp_add_rows(glp_ptr, nrows);
}

void glpk_solver::set_col_kind(int col_idx, int type) {
  glp_set_col_kind(glp_ptr, col_idx, type);
}

void glpk_solver::set_col_bnd(int column_idx, int type, double lb, double ub) {
  glp_set_col_bnds(glp_ptr, column_idx, type, lb, ub);
}

void glpk_solver::set_row_bnd(int row_idx, int type, double lb, double ub) {
  glp_set_row_bnds(glp_ptr, row_idx, type, lb, ub);
}

void glpk_solver::set_mat_row(int row_idx, integers indexes, doubles values) {
  glp_set_mat_row(
    glp_ptr,
    row_idx,
    indexes.size() - 1,
    INTEGER(indexes.data()),
    REAL(values.data())
  );
}

void glpk_solver::set_obj_coefs(int column_idx, double value) {
  glp_set_obj_coef(glp_ptr, column_idx, value);
}

void glpk_solver::set_obj_dir(int dir) {
  glp_set_obj_dir(glp_ptr, dir);
}

void glpk_solver::set_irowgen_callback(cpp11::function fun) {
  r_irowgen_callback = boost::optional<cpp11::function>(fun);
  glp_mip_parameters.sr_heur = GLP_OFF;
  // TODO: due to a bug in GLPK we have to turn of the rounding heuristic
}

void glpk_solver::callback(glp_tree *tree, void *info) {
  if (glp_ios_reason(tree) != GLP_IROWGEN) {
    return;
  }
  cpp11::check_user_interrupt(); // TODO: might make everything slower
  glpk_solver* solver = static_cast<glpk_solver*>(info);
  if (solver->r_irowgen_callback) {
    const auto& fun = solver->r_irowgen_callback.value();
    fun();
  }
}

void glpk_solver::solve_simplex() {
  glp_simplex(glp_ptr, &glp_simplex_parameters);
}

void glpk_solver::solve_MIP() {
  glp_intopt(glp_ptr, &glp_mip_parameters);
}

void glpk_solver::set_obj_val_constant(double constant) {
  glpk_solver::obj_value_constant = constant;
}

double glpk_solver::get_mip_col_val(int column_idx) {
  return glp_mip_col_val(glp_ptr, column_idx);
}

double glpk_solver::get_col_prim(int column_idx) {
  return glp_get_col_prim(glp_ptr, column_idx);
}

double glpk_solver::mip_obj_val() {
  return glp_mip_obj_val(glp_ptr) + glpk_solver::obj_value_constant;
};

int glpk_solver::get_num_cols() {
  return glp_get_num_cols(glp_ptr);
};

int glpk_solver::get_num_rows() {
  return glp_get_num_rows(glp_ptr);
};

int glpk_solver::get_mip_status() {
  return glp_mip_status(glp_ptr);
}
