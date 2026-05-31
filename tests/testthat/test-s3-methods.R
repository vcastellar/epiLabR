test_that("S3 methods print, summary, and plot work as smoke tests", {
  sim <- simulate_epi(
    model = SIR_MODEL,
    times = 0:10
  )

  expect_invisible(print(sim))
  sm <- summary(sim)
  expect_type(sm, "list")
  expect_true(all(c("peak_I", "time_peak_I") %in% names(sm)))

  tf <- tempfile(fileext = ".pdf")
  grDevices::pdf(tf)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_invisible(plot(sim, what = "states"))
  expect_invisible(plot(sim, what = "incidence"))
  expect_invisible(plot(sim, what = "derived"))
})

test_that("print.epi_model is a smoke test", {
  expect_invisible(print(SIR_MODEL))
})
