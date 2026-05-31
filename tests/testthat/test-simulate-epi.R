test_that("simulate_epi uses defaults and supports parameter overrides", {
  sim_default <- simulate_epi(
    model = SIS_MODEL,
    times = 0:5
  )

  expect_s3_class(sim_default, "sim_epi")
  expect_equal(sim_default$params$beta, SIS_MODEL$defaults[["beta"]])
  expect_equal(names(sim_default$states), c("time", SIS_MODEL$states))

  sim_override <- simulate_epi(
    model = SIS_MODEL,
    times = 0:5,
    parms = c(beta = 0.5)
  )

  expect_equal(sim_override$params$beta, 0.5)
})

test_that("simulate_epi validates common error cases", {
  expect_error(
    simulate_epi(SIS_MODEL, times = c(0, 1, 1)),
    "`times` must be strictly increasing\\."
  )

  expect_error(
    simulate_epi(SIS_MODEL, times = 0:2, parms = c(alpha = 0.2)),
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
    simulate_epi(SIS_MODEL, times = 0:2, init = c(S = 10)),
    "Missing initial states: I"
  )
})

test_that("simulate_epi accepts SIS parameters beta and gamma", {
  sim_sis <- simulate_epi(
    model = SIS_MODEL,
    times = 0:10,
    parms = c(beta = 0.25, gamma = 0.10),
    init = c(S = 999, I = 1)
  )

  expect_s3_class(sim_sis, "sim_epi")
  expect_equal(sim_sis$params[["beta"]], 0.25)
  expect_equal(sim_sis$params[["gamma"]], 0.10)
})
