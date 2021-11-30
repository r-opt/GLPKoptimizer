test_that("a first example works", {
  set.seed(42)
  v <- pmax(rnorm(10), 0.1)
  w <- pmax(rnorm(10), 0.1)
  model <- rmpk::optimization_model(GLPK_optimizer())
  x <- model$add_variable("x", type = "integer", lb = 0, ub = 1, i = 1:10)
  model$set_objective(rmpk::sum_expr(v[i] * x[i], i = 1:10) + 42, sense = "max")
  model$add_constraint(rmpk::sum_expr(w[i] * x[i], i = 1:10) <= 5)
  model$optimize()
  res <- model$get_variable_value(x[i])
  expect_true(all(res$value %in% c(0, 1)))
  expect_true(any(res$value == 1))
  expect_equal(
    model$objective_value(),
    as.numeric(res[order(res$i), "value"] %*% v) + 42
  )
  expect_output(print(model))
  expect_s4_class(model$termination_status(), "MOI_termination_status_code")
})

test_that("we can get the value of the relaxation", {
  solver <- GLPK_optimizer()
  model <- rmpk::optimization_model(solver)
  x <- model$add_variable("x", type = "binary", i = 1:10)
  model$set_objective(rmpk::sum_expr(x[i], i = 1:10), sense = "max")
  model$add_constraint(rmpk::sum_expr(x[i], i = 1:10) <= 7.5)
  model$optimize()
  values <- vapply(1:10, function(i) {
    # in GLPK you can only access the values of the relaxation
    glpk_get_col_prim(solver, x[i])
  }, numeric(1L))
  expect_equal(sum(values), 7.5)
})

test_that("register a callback", {
  solver <- GLPK_optimizer()
  model <- rmpk::optimization_model(solver)
  x <- model$add_variable("x", type = "binary", i = 1:10)
  model$set_objective(rmpk::sum_expr(x[i], i = 1:10), sense = "max")
  model$add_constraint(rmpk::sum_expr(x[i], i = 1:10) <= 7.5)

  # When an integer solution is found, we dynamically add a constraint
  # further restricting the search space
  set_irowgen_callback(solver, function() {
    values <- vapply(1:10, function(i) {
      # in GLPK you can only access the values of the relaxation
      glpk_get_col_prim(solver, x[i])
    }, numeric(1L))
    all_integral <- all(values %% 1 == 0)
    if (all_integral && sum(values) > 4) {
      model$add_constraint(rmpk::sum_expr(x[i], i = 1:10) <= 4)
    }
  })

  model$optimize()
  res <- model$get_variable_value(x[i])
  expect_equal(sum(res$value), 4)
})

test_that("bounds and equality constraints", {
  solver <- GLPK_optimizer()
  model <- rmpk::optimization_model(solver)
  x <- model$add_variable("x", type = "integer", lb = 0, ub = 1, i = 1:10)
  model$set_objective(rmpk::sum_expr(x[i], i = 1:10), sense = "max")
  model$add_constraint(x[i] == 0, i = 1:5)
  model$set_bounds(x[i], ub = 0, i = 6:9)
  model$optimize()
  res <- model$get_variable_value(x[i])
  res <- res[order(res$i), ]
  expect_equal(res$value, c(rep.int(0, 9), 1))
})

test_that("presolve can be used", {
  solver <- GLPK_optimizer(presolve = TRUE)
  model <- rmpk::optimization_model(solver)
  x <- model$add_variable("x", type = "integer")
  model$set_objective(x)
  model$add_constraint(x >= 0)
  model$optimize()
  res <- model$get_variable_value(x)
  expect_equal(res, 0)
})

test_that("bounds are used correctly", {
  solver <- GLPK_optimizer(presolve = FALSE)
  model <- rmpk::optimization_model(solver)
  x <- model$add_variable("x", type = "integer", lb = 0)
  model$set_objective(x, "min")
  model$optimize()
  expect_equal(model$termination_status(), MOI_OPTIMAL)
})


test_that("fixec col bounds work", {
  solver <- GLPK_optimizer(presolve = FALSE)
  model <- rmpk::optimization_model(solver)
  x <- model$add_variable("x", type = "integer", lb = 5, ub = 5)
  model$set_objective(x, "min")
  model$optimize()
  expect_equal(model$termination_status(), MOI_OPTIMAL)
  expect_equal(model$objective_value(), 5)
})
