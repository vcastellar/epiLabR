test_that("simulate_epi uses defaults and supports parameter overrides", {
  sim_default <- simulate_epi(
    model = SI_MODEL,
    times = 0:5
  )

  expect_s3_class(sim_default, "sim_epi")
  expect_equal(sim_default$params$beta, SI_MODEL$defaults[["beta"]])
  expect_equal(names(sim_default$states), c("time", SI_MODEL$states))

  sim_override <- simulate_epi(
    model = SI_MODEL,
    times = 0:5,
    parms = c(beta = 0.5)
  )

  expect_equal(sim_override$params$beta, 0.5)
})

test_that("simulate_epi validates common error cases", {
  expect_error(
    simulate_epi(SI_MODEL, times = c(0, 1, 1)),
    "`times` must be strictly increasing\\."
  )

  expect_error(
    simulate_epi(SI_MODEL, times = 0:2, parms = c(alpha = 0.2)),
    "Unknown parameters: alpha"
  )

  custom_no_defaults <- epi_model(
    name = "custom",
    rhs = function(time, state, parms) list(c(dS = 0, dI = 0)),
    par_names = c("beta", "gamma"),
    states = c("S", "I"),
    init = c(S = 99, I = 1)
  )

  expect_error(
    simulate_epi(custom_no_defaults, times = 0:2, parms = c(beta = 0.2)),
    "Missing parameters: gamma"
  )

  expect_error(
    simulate_epi(SI_MODEL, times = 0:2, init = c(S = 10)),
    "Missing initial states: I"
  )
})
