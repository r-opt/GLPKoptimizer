#pragma once
#include <glpk.h>
#include <cpp11.hpp>
#include <boost/optional.hpp>

class glpk_solver {
public:
  glpk_solver(const bool presolve);
  ~glpk_solver();
  int add_cols(int ncols);
  int add_rows(int nrows);
  void set_col_kind(int column_idx, int type);
  void set_col_bnd(int column_idx, int type, double lb, double ub);
  void set_row_bnd(int row_idx, int type, double lb, double ub);
  void set_mat_row(int row_idx, cpp11::integers indexes, cpp11::doubles values);
  void set_obj_coefs(int column_idx, double value);
  void set_obj_dir(int dir);
  void set_obj_val_constant(double constant);
  void solve_simplex();
  void solve_MIP();
  void set_irowgen_callback(cpp11::function fun);
  double get_col_prim(int column_idx);
  double get_mip_col_val(int column_idx);
  double mip_obj_val();
  int get_num_cols();
  int get_num_rows();
  int get_mip_status();
private:
  static void callback(glp_tree *tree, void *info);
  glp_prob* glp_ptr;
  glp_iocp glp_mip_parameters;
  glp_smcp glp_simplex_parameters;
  boost::optional<cpp11::function> r_irowgen_callback;
  double obj_value_constant = 0;
};
