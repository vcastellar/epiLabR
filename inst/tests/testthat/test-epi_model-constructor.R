test_that("epi_model builds a valid model and reorders named vectors", {
  mod <- epi_model(
    name = "toy",
    rhs = function(time, state, parms) list(c(dS = -1, dI = 1)),
    par_names = c("beta", "gamma"),
    states = c("S", "I"),
    lower = c(gamma = 0, beta = 0),
    upper = c(gamma = 2, beta = 3),
    defaults = c(gamma = 0.1, beta = 0.3),
    init = c(I = 1, S = 99)
  )

  expect_s3_class(mod, "epi_model")
  expect_equal(names(mod$defaults), c("beta", "gamma"))
  expect_equal(names(mod$init), c("S", "I"))
  expect_equal(mod$lower[["beta"]], 0)
  expect_equal(mod$upper[["gamma"]], 2)
})

test_that("epi_model validates overlapping derived/state names", {
  expect_error(
    epi_model(
      name = "bad-derived",
      rhs = function(time, state, parms) list(c(dS = 0)),
      par_names = "beta",
      states = c("S"),
      derived = c("S")
    )
  )
})

test_that("epi_model validates invalid bounds", {
  expect_error(
    epi_model(
      name = "bad-bounds",
      rhs = function(time, state, parms) list(c(dS = 0)),
      par_names = "beta",
      states = c("S"),
      lower = c(beta = 1),
      upper = c(beta = 1)
    ),
    "Invalid bounds: lower >= upper\\."
  )
})
