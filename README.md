
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GLPKoptimizer

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/r-opt/GLPKoptimizer/workflows/R-CMD-check/badge.svg)](https://github.com/r-opt/GLPKoptimizer/actions)
<!-- badges: end -->

The goal of `GLPKoptimizer` is to provide
[MOI](https://github.com/r-opt/MOI) GLPK bindings. Still very
experimental and incomplete.

`GLPKoptimizer` interacts directly with the solver by creating a pointer
to a GLPK instance and modifying that pointer directly. Most modelling
operations are directly passed through to the solver without making
copies of the data in R.

## Installation

You can install the development version of GLPKoptimizer like so:

``` r
remotes::install_github("r-opt/GLPKoptimizer")
```

## Example

You can use `set_irowgen_callback` to register a callback with the
solver, while still being able to use `rmpk`’s modelling features.

``` r
library(rmpk)
library(GLPKoptimizer)
solver <- GLPK_optimizer()
model <- optimization_model(solver)
x <- model$add_variable("x", type = "binary", i = 1:10)
model$set_objective(sum_expr(x[i], i = 1:10), sense = "max")
model$add_constraint(sum_expr(x[i], i = 1:10) <= 7.5)

# When an integer solution is found, we dynamically add a constraint
# further restricting the search space.
# In this case, we add another constraint of the sum of x <= 4
set_irowgen_callback(solver, function() {
  values <- vapply(1:10, function(i) {
    # in GLPK you can only access the values of the relaxation
    glpk_get_col_prim(solver, x[i])
  }, numeric(1L))
  all_integral <- all(values %% 1 == 0)
  if (all_integral && sum(values) > 4) {
    model$add_constraint(sum_expr(x[i], i = 1:10) <= 4)
  }
})

model$optimize()
model$get_variable_value(x[i])
#>    name  i value
#> 1     x  1     0
#> 2     x  7     1
#> 3     x  5     1
#> 4     x  8     0
#> 5     x  2     0
#> 6     x 10     0
#> 7     x  9     0
#> 8     x  3     0
#> 9     x  6     1
#> 10    x  4     1
```
