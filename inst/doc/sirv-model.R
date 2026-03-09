## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(epiLabR)

## -----------------------------------------------------------------------------
sirv_rhs <- function(time, state, parms) {
  with(as.list(c(state, parms)), {
    N <- S + I + R + V

    incidence <- beta * S * I / N

    dS <- -incidence - nu * S
    dI <-  incidence - gamma * I
    dR <-  gamma * I
    dV <-  nu * S

    list(
      c(dS, dI, dR, dV),
      incidence = incidence
    )
  })
}

SIRV_MODEL <- epi_model(
  name      = "SIRV",
  rhs       = sirv_rhs,
  par_names = c("beta", "gamma", "nu"),
  states    = c("S", "I", "R", "V"),
  derived   = "incidence",
  defaults  = c(beta = 0.3, gamma = 0.1, nu = 0.01)
)

SIRV_MODEL

## -----------------------------------------------------------------------------
SIRV_MODEL$defaults

init <- c(S = 1e5, I = 10, R = 0, V = 0)
init

## -----------------------------------------------------------------------------
sim_base <- simulate_epi(
  model = SIRV_MODEL,
  times = 0:200,
  parms = SIRV_MODEL$defaults,
  init  = init
)

summary(sim_base)

## ----fig.width=7, fig.height=4.5----------------------------------------------
plot(sim_base)

## ----fig.width=7, fig.height=4.5----------------------------------------------
plot(sim_base, what = "incidence")

## -----------------------------------------------------------------------------
nu_values <- c(0, 0.01, 0.03)

sims <- lapply(nu_values, function(nu_i) {
  p <- SIRV_MODEL$defaults
  p["nu"] <- nu_i
  simulate_epi(SIRV_MODEL, times = 0:200, parms = p, init = init)
})

names(sims) <- paste0("nu=", nu_values)

# Peak prevalence of infectious individuals (I) by scenario
peak_prev <- sapply(sims, function(sim) {
  peak_prevalence(sim$states$I)
})
peak_prev

# Time to peak incidence by scenario
peak_time <- sapply(sims, function(sim) {
  time_to_peak(sim$derived$incidence, sim$derived$time)
})
peak_time

## -----------------------------------------------------------------------------
register_epi_model(SIRV_MODEL)
list_models()

