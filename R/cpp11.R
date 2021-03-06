# Generated by cpp11: do not edit by hand

glpk_init <- function(presolve) {
  .Call(`_GLPKoptimizer_glpk_init`, presolve)
}

glpk_add_cols <- function(ptr, cols) {
  .Call(`_GLPKoptimizer_glpk_add_cols`, ptr, cols)
}

glpk_add_rows <- function(ptr, rows) {
  .Call(`_GLPKoptimizer_glpk_add_rows`, ptr, rows)
}

glpk_set_col_kind <- function(ptr, column_idx, type) {
  invisible(.Call(`_GLPKoptimizer_glpk_set_col_kind`, ptr, column_idx, type))
}

glpk_set_col_bnd <- function(ptr, column_idx, type, lb, ub) {
  invisible(.Call(`_GLPKoptimizer_glpk_set_col_bnd`, ptr, column_idx, type, lb, ub))
}

glpk_set_row_bnd <- function(ptr, row_idx, type, lb, ub) {
  invisible(.Call(`_GLPKoptimizer_glpk_set_row_bnd`, ptr, row_idx, type, lb, ub))
}

glpk_set_mat_row <- function(ptr, row_idx, indexes, values) {
  invisible(.Call(`_GLPKoptimizer_glpk_set_mat_row`, ptr, row_idx, indexes, values))
}

glpk_set_obj_coefs <- function(ptr, col_idx, value) {
  invisible(.Call(`_GLPKoptimizer_glpk_set_obj_coefs`, ptr, col_idx, value))
}

glpk_set_obj_dir <- function(ptr, dir) {
  invisible(.Call(`_GLPKoptimizer_glpk_set_obj_dir`, ptr, dir))
}

glpk_solve_simplex <- function(ptr) {
  invisible(.Call(`_GLPKoptimizer_glpk_solve_simplex`, ptr))
}

glpk_solve_MIP <- function(ptr) {
  invisible(.Call(`_GLPKoptimizer_glpk_solve_MIP`, ptr))
}

glpk_get_mip_col_val <- function(ptr, column_idx) {
  .Call(`_GLPKoptimizer_glpk_get_mip_col_val`, ptr, column_idx)
}

glpk_solver_get_col_prim <- function(ptr, column_idx) {
  .Call(`_GLPKoptimizer_glpk_solver_get_col_prim`, ptr, column_idx)
}

glpk_set_irowgen_callback <- function(ptr, fun) {
  invisible(.Call(`_GLPKoptimizer_glpk_set_irowgen_callback`, ptr, fun))
}

glpk_mip_obj_val <- function(ptr) {
  .Call(`_GLPKoptimizer_glpk_mip_obj_val`, ptr)
}

glpk_set_mip_obj_constant <- function(ptr, constant) {
  invisible(.Call(`_GLPKoptimizer_glpk_set_mip_obj_constant`, ptr, constant))
}

glpk_get_num_cols <- function(ptr) {
  .Call(`_GLPKoptimizer_glpk_get_num_cols`, ptr)
}

glpk_get_num_rows <- function(ptr) {
  .Call(`_GLPKoptimizer_glpk_get_num_rows`, ptr)
}

glpk_mip_status <- function(ptr) {
  .Call(`_GLPKoptimizer_glpk_mip_status`, ptr)
}
