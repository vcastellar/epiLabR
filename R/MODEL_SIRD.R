#-------------------------------------------------------------------------------
# SIRD model
#-------------------------------------------------------------------------------
#' @keywords internal
#' @noRd
sird_rhs <- function(time, state, parms) {
  with(as.list(c(state, parms)), {
    N <- S + I + R
    lambda <- beta * S * I / N

    dS <- -lambda
    dI <- lambda - gamma * I - mu * I
    dR <- gamma * I
    dD <- mu * I

    list(
      c(dS, dI, dR, dD),
      incidence = lambda
    )
  })
}

#' SIRD epidemic model
#'
#' @name SIRD_MODEL
#' @description
#' An \code{epi_model} object representing a deterministic **SIRD**
#' (Susceptible–Infectious–Recovered–Deceased) compartmental epidemic model.
#'
#' The model describes the spread of an infection in a closed population where
#' susceptible individuals become infectious at rate \eqn{\lambda(t)}.
#' Infectious individuals are removed either by recovery (at rate \code{gamma})
#' or disease-induced mortality (at rate \code{mu}).
#'
#' @details
#' ## State variables
#' The model is defined in terms of the following state variables:
#' \describe{
#'   \item{S(t)}{Number of susceptible individuals at time \eqn{t}.}
#'   \item{I(t)}{Number of infectious (actively infected) individuals at time \eqn{t}.}
#'   \item{R(t)}{Number of recovered individuals at time \eqn{t}.}
#'   \item{D(t)}{Cumulative number of disease-induced deaths at time \eqn{t}.}
#' }
#'
#' The living population participating in transmission is
#' \deqn{N = S(t) + I(t) + R(t).}
#'
#' ## Model variables
#' The SIRD model declares the following variables:
#' \describe{
#'   \item{\code{"S"}}{Susceptible population size.}
#'   \item{\code{"I"}}{Infectious population size.}
#'   \item{\code{"R"}}{Recovered population size.}
#'   \item{\code{"D"}}{Cumulative disease-induced deaths.}
#'   \item{\code{"incidence"}}{Instantaneous rate of new infections
#'     \eqn{\lambda(t)} returned by the model's right-hand side.}
#' }
#'
#' All declared variables may be used as observables in generic utilities
#' and summary methods built around \code{epi_model} objects.
#'
#' ## Parameters
#' The SIRD model depends on the following parameters:
#' \describe{
#'   \item{beta}{Transmission rate (per day).}
#'   \item{gamma}{Recovery/removal rate (per day).}
#'   \item{mu}{Disease-induced mortality rate among infectious individuals (per day).}
#' }
#'
#' ## Model equations
#' New infections occur at rate
#' \deqn{\lambda(t) = \beta \frac{S(t)\, I(t)}{N}.}
#'
#' The system of ordinary differential equations is:
#' \deqn{
#' \begin{aligned}
#' \frac{dS}{dt} &= -\lambda(t), \\
#' \frac{dI}{dt} &= \lambda(t) - \gamma I(t) - \mu I(t), \\
#' \frac{dR}{dt} &= \gamma I(t), \\
#' \frac{dD}{dt} &= \mu I(t),
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
#' deterministic SIRD (Susceptible-Infectious-Recovered-Deceased) compartmental
#' epidemic model.
#' The returned object contains the model right-hand side, declared state and
#' derived variables, parameter names, and default initial conditions needed by
#' utilities such as \code{\link{simulate_epi}}.
#'
#' @examples
#' ## Simulate a SIRD epidemic
#' sim <- simulate_epi(
#'   model = SIRD_MODEL,
#'   times = 0:200,
#'   parms = c(beta = 0.25, gamma = 0.1, mu = 0.01),
#'   init = c(S = 1e6, I = 10, R = 0, D = 0)
#' )
#'
#' plot(sim)
#'
#' ## Plot incidence
#' plot(sim, what = "incidence")
#'
#' @seealso
#' \code{\link{simulate_epi}},
#' \code{\link{epi_model}}
#'
#' @export
SIRD_MODEL <- epi_model(
  name = "SIRD",
  rhs = sird_rhs,
  states = c("S", "I", "R", "D"),
  derived = c("incidence"),
  par_names = c("beta", "gamma", "mu"),
  defaults = c(beta = 0.25, gamma = 0.1, mu = 0.01),
  init = c(S = 1e6, I = 10, R = 0, D = 0)
)
