
#-------------------------------------------------------------------------------
# SIS model
#-------------------------------------------------------------------------------
#' @keywords internal
#' @noRd
sis_rhs <- function(time, state, parms) {
  with(as.list(c(state, parms)), {
    N <- S + I
    lambda <- beta * S * I / N
    dS <- -lambda + gamma * I
    dI <-  lambda - gamma * I
    list(c(dS, dI), incidence = lambda)
  })
}


#' SIS epidemic model
#'
#' @name SIS_MODEL
#' @description
#' An \code{epi_model} object representing a deterministic **SIS**
#' (Susceptible–Infectious-Susceptible) compartmental epidemic model.
#'
#' The model describes the spread of an infection in a closed population where
#' individuals move from the susceptible compartment \code{S} to the infectious
#' compartment \code{I}, and recover back to \code{S}.
#'
#' @details
#' ## State variables
#' The model is defined in terms of the following state variables:
#' \describe{
#'   \item{S(t)}{Number of susceptible individuals at time \eqn{t}.}
#'   \item{I(t)}{Number of infectious individuals at time \eqn{t}.}
#' }
#'
#' The total population size is conserved:
#' \deqn{N = S(t) + I(t).}
#'
#' ## Model variables
#' The SIS model declares the following variables:
#' \describe{
#'   \item{\code{"S"}}{Susceptible population size.}
#'   \item{\code{"I"}}{Infectious population size.}
#'   \item{\code{"incidence"}}{Instantaneous rate of new infections
#'     \eqn{\lambda(t)} returned by the model's right-hand side.}
#' }
#'
#' All declared variables may be used as observables in generic utilities
#' and summary methods built around \code{epi_model} objects.
#'
#' ## Parameters
#' The SIS model depends on two parameters:
#' \describe{
#'   \item{beta}{Transmission rate (per day).}
#'   \item{gamma}{Recovery rate from \code{I} to \code{S} (per day).}
#' }
#'
#' ## Model equations
#' New infections occur at rate
#' \deqn{\lambda(t) = \beta \frac{S(t)\, I(t)}{N}.}
#'
#' The system of ordinary differential equations is:
#' \deqn{
#' \begin{aligned}
#' \frac{dS}{dt} &= -\lambda(t) + \gamma I(t), \\
#' \frac{dI}{dt} &= \lambda(t) - \gamma I(t).
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
#' deterministic SIS (Susceptible-Infectious-Susceptible) compartmental
#' epidemic model.
#' The returned object contains the model right-hand side, declared state and
#' derived variables, parameter names, and default initial conditions needed by
#' utilities such as \code{\link{simulate_epi}}.
#'
#' @examples
#' ## Simulate an SIS epidemic
#' sim <- simulate_epi(
#'   model = SIS_MODEL,
#'   times = 0:100,
#'   parms = c(beta = 0.4, gamma = 0.2),
#'   init = SIS_MODEL$init
#' )
#'
#' plot(sim)
#'
#' ## Plot observed incidence
#' plot(sim, what = "incidence")
#'
#'
#' @seealso
#' \code{\link{simulate_epi}},
#' \code{\link{epi_model}}
#'
#' @export

SIS_MODEL <- epi_model(
  name = "SIS",
  rhs = sis_rhs,
  par_names = c("beta", "gamma"),
  states = c("S", "I"),
  derived = c("incidence"),
  defaults = c(beta = 0.3, gamma = 0.2),
  init = c(S = 999999, I = 1)
)
