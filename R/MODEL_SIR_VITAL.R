
#' Right-hand side for SIR model with vital dynamics
#'
#' @description
#' Internal function defining the system of differential equations
#' for \code{SIR_V_MODEL}.
#'
#' @param time Time variable (required by ODE solver).
#' @param state Named numeric vector of state variables.
#' @param parms Named numeric vector of model parameters.
#'
#' @return A list in the format required by \code{deSolve::ode()}. The first
#' element is a numeric vector of derivatives for the state variables, and the
#' remaining named elements are the derived outputs \code{births},
#' \code{infection}, \code{recovery}, \code{death_S}, \code{death_I}, and
#' \code{death_R}, which describe the demographic and epidemiological flows at
#' the current time point.
#'
#' @keywords internal
sir_vital_rhs <- function(time, state, parms) {
  with(as.list(c(state, parms)), {

    N <- S + I + R
    infection <- beta * S * I / N

    births  <- mu * N
    recovery <- gamma * I

    death_S <- mu * S
    death_I <- mu * I
    death_R <- mu * R

    dS <- births - infection - death_S
    dI <- infection - recovery - death_I
    dR <- recovery - death_R

    list(
      c(dS, dI, dR),
      births    = births,
      infection = infection,
      recovery  = recovery,
      death_S   = death_S,
      death_I   = death_I,
      death_R   = death_R
    )
  })
}

#-------------------------------------------------------------------------------
# SIR model with vital dynamics and constant population
#-------------------------------------------------------------------------------
#' SIR epidemic model with vital dynamics and constant population
#'
#' @name SIR_V_MODEL
#' @description
#' An \code{epi_model} object representing a deterministic **SIR**
#' (Susceptible–Infectious–Recovered) epidemic model with demographic turnover
#' and **strictly constant population size**.
#'
#' Natural deaths occur in all compartments at a constant per-capita rate, and
#' births exactly balance deaths at each instant, ensuring that the total
#' population size remains constant over time.
#'
#' @details
#' ## State variables
#' The model is defined in terms of the following state variables:
#' \describe{
#'   \item{S(t)}{Number of susceptible individuals at time \eqn{t}.}
#'   \item{I(t)}{Number of infectious individuals at time \eqn{t}.}
#'   \item{R(t)}{Number of recovered individuals at time \eqn{t}.}
#' }
#'
#' The total population size is conserved:
#' \deqn{N = S(t) + I(t) + R(t).}
#'
#' ## Model variables
#' The SIR model with vital dynamics declares the following variables:
#' \describe{
#'   \item{\code{"S"}}{Susceptible population size.}
#'   \item{\code{"I"}}{Infectious population size.}
#'   \item{\code{"R"}}{Recovered population size.}
#'   \item{\code{"births"}}{Recruitment of new susceptible individuals exactly balancing
#'     natural deaths.}
#'   \item{\code{"infection"}}{Incidence of new infections.}
#'   \item{\code{"recovery"}}{Recovery of infectious individuals.}
#'   \item{\code{"death_S"}}{Natural deaths among susceptibles.}
#'   \item{\code{"death_I"}}{Natural deaths among infectious.}
#'   \item{\code{"death_R"}}{Natural deaths among recovered.}
#' }
#'
#' All declared variables may be used as observables in generic utilities
#' and summary methods built around \code{epi_model} objects.
#'
#' ## Parameters
#' The model depends on the following parameters:
#' \describe{
#'   \item{beta}{Transmission rate (per day).}
#'   \item{gamma}{Recovery/removal rate (per day).}
#'   \item{mu}{Natural mortality rate (per capita).}
#' }
#'
#' ## Model equations
#' New infections occur at rate
#' \deqn{\lambda(t) = \beta \frac{S(t)\, I(t)}{N}.}
#'
#' The system of ordinary differential equations is:
#' \deqn{
#' \begin{aligned}
#' \frac{dS}{dt} &= \mu N - \lambda(t) - \mu S, \\
#' \frac{dI}{dt} &= \lambda(t) - \gamma I(t) - \mu I(t), \\
#' \frac{dR}{dt} &= \gamma I(t) - \mu R(t).
#' \end{aligned}
#' }
#'
#' ## Usage
#' This predefined model object is intended to be used with generic utilities
#' such as \code{\link{simulate_epi}}, \code{\link{plot.sim_epi}}, and
#' \code{\link{summary.sim_epi}} that operate on \code{epi_model} objects.
#'
#' @format
#' An object of class \code{"epi_model"}.
#'
#' @return An object of class \code{"epi_model"} representing the predefined
#' deterministic SIR model with vital dynamics and constant population size.
#' The returned object contains the model right-hand side, declared state and
#' derived variables, and parameter names needed by utilities such as
#' \code{\link{simulate_epi}}.
#'
#' @examples
#' ## Simulate a SIR epidemic with constant population size
#' sim <- simulate_epi(
#'   model = SIR_V_MODEL,
#'   times = 0:300,
#'   parms = c(
#'     beta  = 0.4,
#'     gamma = 0.1,
#'     mu    = 0.01
#'   ),
#'   init = c(S = 0.99, I = 0.01, R = 0)
#' )
#'
#' plot(sim)
#'
#' ## Plot incidence
#' plot(sim, what = "infection")
#'
#' @seealso
#' \code{\link{simulate_epi}},
#' \code{\link{epi_model}},
#' \code{\link{SIR_MODEL}}
#'
#' @export


SIR_V_MODEL <- epi_model(
  name = "SIR-V",
  rhs  = sir_vital_rhs,
  par_names   = c("beta", "gamma", "mu"),

  states = c("S", "I", "R"),
  derived  = c(
    "births",
    "infection",
    "recovery",
    "death_S",
    "death_I",
    "death_R"
  )
)
