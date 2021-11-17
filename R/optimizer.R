#' A MOI compatible GLPK optimizer
#'
#' @keywords internal
#' @import MOI
#' @importFrom methods new
#' @importFrom methods .valueClassTest
#' @export
setClass("GLPK_optimizer",
  contains = "MOI_abstract_optimizer",
  slot = c(ptr = "ANY")
)

#' A GLPK Optimizer
#'
#' @param control a list of controls
#'
#' @include optimizer.R
#' @export
GLPK_optimizer <- function(control = list(presolve = FALSE)) {
  stopifnot(is.list(control))
  glpk_ptr <- glpk_init()
  new("GLPK_optimizer", ptr = glpk_ptr)
}

#' Set an irowgen callback
#' @param optimizer a GLPK optimizer
#' @param fun the function that should be called when an integer solution has
#' been found.
#' @export
set_irowgen_callback <- function(optimizer, fun) {
  stopifnot(inherits(optimizer, "GLPK_optimizer"))
  glpk_set_irowgen_callback(optimizer@ptr, fun)
  invisible(TRUE)
}

#' Get the primal value of a variable
#' @param optimizer the GLPK optimizer
#' @param variable a scalar affine term
#' @export
glpk_get_col_prim <- function(optimizer, variable) {
  stopifnot(
    inherits(optimizer, "GLPK_optimizer"),
    inherits(variable, "MOI_scalar_affine_term")
  )
  glpk_solver_get_col_prim(optimizer@ptr, variable@variable@value)
}

add_variable <- function(optimizer, type, lower_bound = -Inf, upper_bound = Inf) {
  stopifnot(inherits(optimizer, "GLPK_optimizer"))
  column_idx <- glpk_add_cols(optimizer@ptr, 1L)
  type <- rmpk_col_type_to_glpk(type)
  glpk_set_col_kind(optimizer@ptr, column_idx, type)
  if (is.finite(lower_bound) && is.finite(upper_bound)) {
    glpk_set_col_bnd(optimizer@ptr, column_idx, glpkAPI::GLP_DB, lower_bound, upper_bound)
  } else if (is.finite(lower_bound)) {
    glpk_set_col_bnd(optimizer@ptr, column_idx, glpkAPI::GLP_LO, lower_bound, 0)
  } else if (is.finite(upper_bound)) {
    glpk_set_col_bnd(optimizer@ptr, column_idx, glpkAPI::GLP_UP, 0, upper_bound)
  }
  column_idx
}

rmpk_col_type_to_glpk <- function(type) {
  glpk_type <- switch(type,
    continuous = glpkAPI::GLP_CV,
    integer = glpkAPI::GLP_IV,
    binary = glpkAPI::GLP_BV
  )
  if (is.null(glpk_type)) {
    stop("Unkown variable type: ", type)
  }
  glpk_type
}

add_linear_constraint <- function(optimizer, linear_expr, type, rhs) {
  stopifnot(inherits(optimizer, "GLPK_optimizer"))
  row_idx <- glpk_add_rows(optimizer@ptr, 1L)
  if (type == "<=") {
    glpk_set_row_bnd(optimizer@ptr, row_idx, glpkAPI::GLP_UP, 0, rhs)
  } else if (type == ">=") {
    glpk_set_row_bnd(optimizer@ptr, row_idx, glpkAPI::GLP_LO, rhs, 0)
  } else if (type == "==") {
    glpk_set_row_bnd(optimizer@ptr, row_idx, glpkAPI::GLP_FX, rhs, rhs)
  }
  variables <- linear_expr@terms
  # GLPK starts at 1 internally it seems
  indexes <- c(NA_integer_, vapply(variables, function(x) x@variable@value, integer(1L)))
  coefficients <- c(NA_integer_, vapply(variables, function(x) x@coefficient, numeric(1L)))
  glpk_set_mat_row(
    optimizer@ptr,
    row_idx,
    indexes,
    coefficients
  )
  row_idx
}

set_variable_type <- function(optimizer, variable_index, type) {
  stopifnot(inherits(optimizer, "GLPK_optimizer"))
  glpk_set_col_kind(optimizer@ptr, variable_index, rmpk_col_type_to_glpk(type))
}

set_variable_lb <- function(optimizer, variable_index, value) {
  stopifnot(inherits(optimizer, "GLPK_optimizer"))
  # TODO: check if that overwrites the ub
  glpk_set_col_bnd(optimizer@ptr, variable_index, glpkAPI::GLP_LO, value, 0)
}

set_variable_ub <- function(optimizer, variable_index, value) {
  stopifnot(inherits(optimizer, "GLPK_optimizer"))
  # TODO: check if that overwrites the lb
  glpk_set_col_bnd(optimizer@ptr, variable_index, glpkAPI::GLP_UP, 0, value)
}

set_linear_objective <- function(optimizer, linear_expr) {
  stopifnot(inherits(optimizer, "GLPK_optimizer"))
  for (var in linear_expr@terms) {
    glpk_set_obj_coefs(optimizer@ptr, var@variable@value, var@coefficient)
  }
  glpk_set_mip_obj_constant(optimizer@ptr, linear_expr@constant)
  optimizer
}

set_objective_sense <- function(optimizer, sense = "min") {
  stopifnot(inherits(optimizer, "GLPK_optimizer"))
  sense <- if (sense == "max") glpkAPI::GLP_MAX else glpkAPI::GLP_MIN
  glpk_set_obj_dir(optimizer@ptr, sense)
}

optimize <- function(optimizer) {
  stopifnot(inherits(optimizer, "GLPK_optimizer"))
  glpk_solve_simplex(optimizer@ptr)
  # if (presolve) {
  # TODO: do it
  # }
  glpk_solve_MIP(optimizer@ptr)
}

get_variable_value <- function(optimizer, var_index) {
  stopifnot(inherits(optimizer, "GLPK_optimizer"))
  glpk_get_mip_col_val(optimizer@ptr, var_index)
}

get_objective_value <- function(optimizer) {
  stopifnot(inherits(optimizer, "GLPK_optimizer"))
  glpk_mip_obj_val(optimizer@ptr)
}

setGeneric("nvars", function(optimizer) {
  standardGeneric("nvars")
}, valueClass = "numeric")

setMethod(
  "nvars", signature(optimizer = "GLPK_optimizer"),
  function(optimizer) {
    glpk_get_num_cols(optimizer@ptr)
  }
)

setGeneric("nconstraints", function(optimizer) {
  standardGeneric("nconstraints")
}, valueClass = "numeric")

setMethod(
  "nconstraints", signature(optimizer = "GLPK_optimizer"),
  function(optimizer) {
    glpk_get_num_rows(optimizer@ptr)
  }
)

#' @importClassesFrom MOI MOI_termination_status_code
setGeneric("get_termination_status", function(optimizer) {
  standardGeneric("get_termination_status")
}, valueClass = "MOI_termination_status_code")

#' @importFrom MOI MOI_OTHER_ERROR
setMethod(
  "get_termination_status", signature(optimizer = "GLPK_optimizer"),
  function(optimizer) {
    code <- glpk_mip_status(optimizer@ptr)
    stopifnot(is.integer(code), code >= 1, code <= 6)
    list(
      MOI_OTHER_ERROR,
      MOI_FEASIBLE_POINT,
      MOI_INFEASIBLE,
      MOI_INFEASIBLE,
      MOI_OPTIMAL,
      MOI_INFEASIBLE_OR_UNBOUNDED
    )[[code]]
  }
)

### MOI interface

#' @export
#' @rdname GLPK_optimizer-class
setMethod("moi_add_variable", signature("GLPK_optimizer"), function(model) {
  var_idx <- add_variable(model, "continuous")
  new("MOI_variable_index", value = var_idx)
})

#' @export
#' @rdname GLPK_optimizer-class
setMethod("moi_add_constraint", signature("GLPK_optimizer", "MOI_scalar_affine_function", "MOI_less_than_set"), function(model, func, set) {
  func <- canonicalize(func)
  constr_id <- add_linear_constraint(model, func, "<=", set@upper - func@constant)
  new("MOI_constraint_index", value = constr_id)
})

#' @export
#' @rdname GLPK_optimizer-class
setMethod("moi_add_constraint", signature("GLPK_optimizer", "MOI_scalar_affine_function", "MOI_equal_to_set"), function(model, func, set) {
  func <- canonicalize(func)
  constr_id <- add_linear_constraint(model, func, "==", set@value - func@constant)
  new("MOI_constraint_index", value = constr_id)
})

#' @export
#' @rdname GLPK_optimizer-class
setMethod("moi_add_constraint", signature("GLPK_optimizer", "MOI_scalar_affine_function", "MOI_greater_than_set"), function(model, func, set) {
  func <- canonicalize(func)
  constr_id <- add_linear_constraint(model, func, ">=", set@lower - func@constant)
  new("MOI_constraint_index", value = constr_id)
})

#' @export
#' @rdname GLPK_optimizer-class
setMethod("moi_add_constraint", signature("GLPK_optimizer", "MOI_scalar_affine_term", "MOI_greater_than_set"), function(model, func, set) {
  func <- moi_scalar_affine_function(list(func), 0)
  constr_id <- add_linear_constraint(model, func, ">=", set@lower)
  new("MOI_constraint_index", value = constr_id)
})

#' @export
#' @rdname GLPK_optimizer-class
setMethod("moi_add_constraint", signature("GLPK_optimizer", "MOI_scalar_affine_term", "MOI_less_than_set"), function(model, func, set) {
  func <- moi_scalar_affine_function(list(func), 0)
  constr_id <- add_linear_constraint(model, func, "<=", set@upper)
  new("MOI_constraint_index", value = constr_id)
})

#' @export
#' @rdname GLPK_optimizer-class
setMethod("moi_add_constraint", signature("GLPK_optimizer", "MOI_scalar_affine_term", "MOI_equal_to_set"), function(model, func, set) {
  func <- moi_scalar_affine_function(list(func), 0)
  constr_id <- add_linear_constraint(model, func, "==", set@value)
  new("MOI_constraint_index", value = constr_id)
})

#' @export
#' @rdname GLPK_optimizer-class
setMethod("moi_add_constraint", signature("GLPK_optimizer", "MOI_single_variable", "MOI_greater_than_set"), function(model, func, set) {
  set_variable_lb(model, func@variable@value, set@lower)
  new("MOI_constraint_index", value = -1) # TODO
})

#' @export
#' @rdname GLPK_optimizer-class
setMethod("moi_add_constraint", signature("GLPK_optimizer", "MOI_single_variable", "MOI_integer_set"), function(model, func, set) {
  set_variable_type(model, func@variable@value, "integer")
  new("MOI_constraint_index", value = -1) # TODO
})

#' @export
#' @rdname GLPK_optimizer-class
setMethod("moi_add_constraint", signature("GLPK_optimizer", "MOI_single_variable", "MOI_zero_one_set"), function(model, func, set) {
  set_variable_type(model, func@variable@value, "binary")
  new("MOI_constraint_index", value = -1) # TODO
})

#' @export
#' @rdname GLPK_optimizer-class
setMethod("moi_add_constraint", signature("GLPK_optimizer", "MOI_single_variable", "MOI_interval_set"), function(model, func, set) {
  set_variable_lb(model, func@variable@value, set@lower)
  set_variable_ub(model, func@variable@value, set@upper)
  new("MOI_constraint_index", value = -1) # TODO
})

#' @export
#' @rdname GLPK_optimizer-class
setMethod("moi_add_constraint", signature("GLPK_optimizer", "MOI_single_variable", "MOI_less_than_set"), function(model, func, set) {
  set_variable_ub(model, func@variable@value, set@upper)
  new("MOI_constraint_index", value = -1) # TODO
})

#' @export
#' @rdname GLPK_optimizer-class
setMethod("moi_set", signature("GLPK_optimizer", "MOI_objective_function", "MOI_scalar_affine_function", "missing"), function(model, type, index, value) {
  index <- canonicalize(index)
  model <- set_linear_objective(model, index)
  model
})

#' @export
#' @rdname GLPK_optimizer-class
setMethod("moi_set", signature("GLPK_optimizer", "MOI_objective_function", "MOI_scalar_affine_term", "missing"), function(model, type, index, value) {
  index <- moi_scalar_affine_function(list(index), 0)
  model <- set_linear_objective(model, index)
  model
})

#' @export
#' @rdname GLPK_optimizer-class
setMethod("moi_set", signature("GLPK_optimizer", "MOI_objective_function", "numeric", "missing"), function(model, type, index, value) {
  model <- set_linear_objective(
    model,
    moi_scalar_affine_function(list(), index)
  )
  model
})

#' @export
#' @rdname GLPK_optimizer-class
setMethod("moi_set", signature("GLPK_optimizer", "MOI_objective_sense", "MOI_optimization_sense", "missing"), function(model, type, index, value) {
  sense <- if (inherits(index, "MOI_optimization_sense_max")) "max" else "min"
  set_objective_sense(model, sense)
  model
})

#' @export
#' @rdname GLPK_optimizer-class
setMethod("moi_optimize", signature("GLPK_optimizer"), function(model) {
  optimize(model)
  model
})

#' @export
#' @rdname GLPK_optimizer-class
setMethod("moi_get", signature("GLPK_optimizer", "MOI_variable_primal_attribute", "MOI_variable_index"), function(model, type, index) {
  get_variable_value(model, index@value)
})

## @export
## @rdname GLPK_optimizer-class
#setMethod("moi_get", signature("GLPK_optimizer", "MOI_variable_dual_attribute", "MOI_variable_index"), function(model, type, index) {
#  #get_variable_dual(model, index@value)
#})
#
## @export
## @rdname GLPK_optimizer-class
#setMethod("moi_get", signature("GLPK_optimizer", "MOI_constraint_dual", "MOI_constraint_index"), function(model, type, index) {
#  get_row_dual(model, index@value)
#})

#' @export
#' @rdname GLPK_optimizer-class
setMethod("moi_get", signature("GLPK_optimizer", "MOI_termination_status", "missing"), function(model, type) {
  get_termination_status(model)
})

#' @export
#' @rdname GLPK_optimizer-class
setMethod("moi_get", signature("GLPK_optimizer", "MOI_number_of_variables", "missing"), function(model, type) {
  nvars(model)
})

#' @export
#' @rdname GLPK_optimizer-class
setMethod("moi_get", signature("GLPK_optimizer", "MOI_number_of_constraints", "missing"), function(model, type) {
  nconstraints(model)
})

# @export
# @rdname GLPK_optimizer-class
# setMethod("moi_get", signature("GLPK_optimizer", "MOI_termination_solver_message_attribute", "missing"), function(model, type) {
#   get_termination_message(model)
# })

#' @export
#' @rdname GLPK_optimizer-class
setMethod("moi_get", signature("GLPK_optimizer", "MOI_objective_value", "missing"), function(model, type) {
  get_objective_value(model)
})

#' @export
#' @rdname GLPK_optimizer-class
setMethod("moi_get", signature("GLPK_optimizer", "MOI_primal_status", "missing"), function(model, type) {
  get_termination_status(model)
})

#' @export
#' @rdname GLPK_optimizer-class
setMethod("moi_get", signature("GLPK_optimizer", "MOI_result_count", "missing"), function(model, type) {
  if (all.equal(get_termination_status(model), MOI_SUCCESS)) {
    1L
  } else {
    0L
  }
})
