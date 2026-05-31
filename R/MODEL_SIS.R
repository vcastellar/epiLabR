
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
#'   \item{beta}{Transmission rate (contacts per individual per day times
#'     probability of transmission per contact).}
#'   \item{gamma}{Recovery rate from \code{I} back to \code{S} (per day);
#'     \eqn{1/\gamma} is the mean infectious period.}
#' }
#'
#' ## Basic reproduction number
#' The basic reproduction number is
#' \deqn{R_0 = \frac{\beta}{\gamma}.}
#' Unlike the SIR model, the SIS model admits a non-trivial **endemic
#' equilibrium** when \eqn{R_0 > 1}:
#' \deqn{I^* = N\!\left(1 - \frac{1}{R_0}\right), \quad S^* = \frac{N}{R_0}.}
#' When \eqn{R_0 \leq 1} the disease-free equilibrium \eqn{I = 0} is globally
#' stable.
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
#' @references
#' Hethcote, H. W. & Yorke, J. A. (1984).
#' *Gonorrhea Transmission Dynamics and Control*.
#' Lecture Notes in Biomathematics, Vol. 56. Springer.
#' \doi{10.1007/978-3-662-07544-9}
#'
#' Hethcote, H. W. (1976).
#' Qualitative analyses of communicable disease models.
#' *Mathematical Biosciences*, **28**(3–4), 335–356.
#' \doi{10.1016/0025-5564(76)90132-2}
#'
#' Hethcote, H. W. (2000).
#' The mathematics of infectious diseases.
#' *SIAM Review*, **42**(4), 599–653.
#' \doi{10.1137/S0036144500371907}
#'
#' van den Driessche, P. & Watmough, J. (2002).
#' Reproduction numbers and sub-threshold endemic equilibria for
#' compartmental models of disease transmission.
#' *Mathematical Biosciences*, **180**(1–2), 29–48.
#' \doi{10.1016/S0025-5564(02)00108-6}
#'
#' Keeling, M. J. & Rohani, P. (2008).
#' *Modeling Infectious Diseases in Humans and Animals*.
#' Princeton University Press.
#' \doi{10.1515/9781400841035}
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
  lower = c(beta = 1e-8, gamma = 1e-8),
  upper = c(beta = 2,    gamma = 1),
  defaults = c(beta = 0.3, gamma = 0.2),
  init = c(S = 999999, I = 1)
)
