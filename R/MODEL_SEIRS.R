
#-------------------------------------------------------------------------------
# SEIRS model
#-------------------------------------------------------------------------------
#' @keywords internal
#' @noRd
seirs_rhs <- function(time, state, parms) {
  with(as.list(c(state, parms)), {

    N <- S + E + I + R

    lambda <- beta * S * I / N

    dS <- -lambda + omega * R
    dE <-  lambda - sigma * E
    dI <-  sigma * E - gamma * I
    dR <-  gamma * I - omega * R

    list(
      c(dS, dE, dI, dR), incidence = sigma * E
    )
  })
}


#' SEIRS epidemic model with latent period and waning immunity
#'
#' @name SEIRS_MODEL
#' @description
#' An \code{epi_model} object representing a deterministic **SEIRS**
#' (Susceptible–Exposed–Infectious–Recovered–Susceptible) compartmental epidemic
#' model with a latent (exposed) period and waning immunity.
#'
#' The model describes the spread of an infection in a closed population where
#' susceptible individuals become infected at rate \eqn{\lambda(t)} and enter
#' the exposed compartment \code{E}. Exposed individuals progress to the
#' infectious compartment at rate \code{sigma}. Infectious individuals recover
#' at rate \code{gamma}, and recovered individuals lose immunity at rate
#' \code{omega}, returning to the susceptible compartment.
#'
#' @details
#' ## State variables
#' The model is defined in terms of the following state variables:
#' \describe{
#'   \item{S(t)}{Number of susceptible individuals at time \eqn{t}.}
#'   \item{E(t)}{Number of exposed (infected but not yet infectious) individuals at time \eqn{t}.}
#'   \item{I(t)}{Number of infectious (actively infected) individuals at time \eqn{t}.}
#'   \item{R(t)}{Number of recovered individuals with temporary immunity at time \eqn{t}.}
#' }
#'
#' The total population size is conserved:
#' \deqn{N = S(t) + E(t) + I(t) + R(t).}
#'
#' ## Model variables
#' The SEIRS model declares the following variables:
#' \describe{
#'   \item{\code{"S"}}{Susceptible population size.}
#'   \item{\code{"E"}}{Exposed (latent) population size.}
#'   \item{\code{"I"}}{Infectious population size.}
#'   \item{\code{"R"}}{Recovered (temporarily immune) population size.}
#'   \item{\code{"incidence"}}{Rate of progression from \code{E} to \code{I},
#'     \eqn{\sigma E(t)}, representing the instantaneous rate at which
#'     individuals become infectious. Note: this is the E→I flow, not the
#'     force of infection \eqn{\lambda(t) = \beta S(t)I(t)/N} (S→E flow).
#'     Both are epidemiologically meaningful but measure different transitions.}
#' }
#'
#' All declared variables may be used as observables in generic utilities
#' and summary methods built around \code{epi_model} objects.
#'
#' ## Parameters
#' The SEIRS model depends on the following parameters:
#' \describe{
#'   \item{beta}{Transmission rate (contacts per individual per day times
#'     probability of transmission per contact).}
#'   \item{sigma}{Rate of progression from exposed to infectious (per day);
#'     \eqn{1/\sigma} is the mean latent (incubation) period.}
#'   \item{gamma}{Recovery/removal rate (per day);
#'     \eqn{1/\gamma} is the mean infectious period.}
#'   \item{omega}{Rate of waning immunity from \code{R} back to \code{S} (per day);
#'     \eqn{1/\omega} is the mean duration of immunity.}
#' }
#'
#' ## Basic reproduction number
#' The basic reproduction number is
#' \deqn{R_0 = \frac{\beta}{\gamma}.}
#' As in the SIRS model, waning immunity (\eqn{\omega > 0}) allows the disease
#' to persist endemically when \eqn{R_0 > 1}. The latent period (\eqn{1/\sigma})
#' delays but does not shift the epidemic threshold.
#'
#' ## Model equations
#' New exposures occur at force of infection
#' \deqn{\lambda(t) = \beta \frac{S(t)\, I(t)}{N}.}
#'
#' Progression from exposed to infectious (reported as \code{incidence}) occurs
#' at rate
#' \deqn{\text{incidence}(t) = \sigma E(t).}
#'
#' The system of ordinary differential equations is:
#' \deqn{
#' \begin{aligned}
#' \frac{dS}{dt} &= -\lambda(t) + \omega R(t), \\
#' \frac{dE}{dt} &= \lambda(t) - \sigma E(t), \\
#' \frac{dI}{dt} &= \sigma E(t) - \gamma I(t), \\
#' \frac{dR}{dt} &= \gamma I(t) - \omega R(t).
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
#' deterministic SEIRS
#' (Susceptible-Exposed-Infectious-Recovered-Susceptible) compartmental
#' epidemic model. The returned object contains the model right-hand side,
#' declared state and derived variables, parameter names, and default initial
#' conditions needed by utilities such as \code{\link{simulate_epi}}.
#'
#' @examples
#' ## Simulate a SEIRS epidemic
#' sim <- simulate_epi(
#'   model = SEIRS_MODEL,
#'   times = 0:300,
#'   parms = c(beta = 0.3, sigma = 0.2, gamma = 0.14, omega = 0.01),
#'   init  = c(S = 1e6, E = 0, I = 20, R = 0)
#' )
#'
#' plot(sim)
#'
#' ## Plot observed incidence
#' plot(sim, what = "incidence")
#'
#'
#' @references
#' Hethcote, H. W. (2000).
#' The mathematics of infectious diseases.
#' *SIAM Review*, **42**(4), 599–653.
#' \doi{10.1137/S0036144500371907}
#'
#' Anderson, R. M. & May, R. M. (1991).
#' *Infectious Diseases of Humans: Dynamics and Control*.
#' Oxford University Press.
#' \doi{10.1093/oso/9780198540403.001.0001}
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
SEIRS_MODEL <- epi_model(
  name = "SEIRS",
  rhs = seirs_rhs,
  par_names = c("beta", "sigma", "gamma", "omega"),
  lower = c(beta = 1e-8, sigma = 1e-8, gamma = 1e-8, omega = 1e-8),
  upper = c(beta = 5,    sigma = 2,    gamma = 2,    omega = 1),
  defaults = c(beta = 0.3, sigma = 0.2, gamma = 0.14, omega = 0.01),
  init = c("S" = 1e6, "E" = 0, "I" = 10, "R" = 0),
  states = c("S", "E", "I", "R"),
  derived = c("incidence")
)
