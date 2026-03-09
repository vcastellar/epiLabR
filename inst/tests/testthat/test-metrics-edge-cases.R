test_that("peak and time metrics return first max on ties", {
  inc <- c(1, 4, 4, 2)
  time <- c(0, 2, 3, 4)

  peak <- peak_incidence(inc, time)
  expect_equal(peak$peak, 4)
  expect_equal(peak$time, 2)
  expect_equal(time_to_peak(inc, time), 2)

  prev <- peak_prevalence(c(5, 5, 1), c(10, 20, 30))
  expect_equal(prev$peak, 5)
  expect_equal(prev$time, 10)
})

test_that("attack_rate handles irregular time grid", {
  inc <- c(0, 2, 2)
  time <- c(0, 1, 3)

  # trapezoidal area: ((0+2)/2)*1 + ((2+2)/2)*2 = 1 + 4 = 5
  expect_equal(attack_rate(inc, time), 5)
})

test_that("growth and doubling metrics handle edge values", {
  flat_inc <- rep(2, 8)
  expect_equal(initial_doubling_time(flat_inc, n = 7), Inf)

  expect_error(
    initial_growth_rate(c(1, 0, 2, 3, 4, 5, 6), n = 7),
    "Incidence must be strictly positive over the estimation window\\."
  )

  dt <- doubling_time_ts(c(5, 4, 3, 2))
  expect_true(all(is.infinite(dt$doubling_time)))
})
