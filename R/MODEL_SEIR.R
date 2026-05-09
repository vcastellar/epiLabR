#-------------------------------------------------------------------------------
# SEIR model
#-------------------------------------------------------------------------------
#' @keywords internal
#' @noRd
seir_rhs <- function(time, state, parms) {
  with(as.list(c(state, parms)), {

    N <- S + E + I + R

    lambda <- beta * S * I / N

    dS <- -lambda
    dE <-  lambda - sigma * E
    dI <-  sigma * E - gamma * I
    dR <-  gamma * I

    list(c(dS, dE, dI, dR), incidence = sigma * E)
  })
}

#' SEIR epidemic model with latent (exposed) period
#'
#' @name SEIR_MODEL
#' @description
#' An \code{epi_model} object representing a deterministic **SEIR**
#' (Susceptible–Exposed–Infectious–Recovered) compartmental epidemic model
#' with a latent (exposed) period.
#'
#' The model describes the spread of an infection in a closed population where
#' susceptible individuals become infected at rate \eqn{\lambda(t)} and enter
#' the exposed compartment \code{E}. Exposed individuals progress to the
#' infectious compartment at rate \code{sigma} and subsequently recover with
#' permanent immunity.
#'
#' @details
#' ## State variables
#' The model is defined in terms of the following state variables:
#' \describe{
#'   \item{S(t)}{Number of susceptible individuals at time \eqn{t}.}
#'   \item{E(t)}{Number of exposed (infected but not yet infectious) individuals at time \eqn{t}.}
#'   \item{I(t)}{Number of infectious (actively infected) individuals at time \eqn{t}.}
#'   \item{R(t)}{Number of recovered (immune) individuals at time \eqn{t}.}
#' }
#'
#' The total population size is conserved:
#' \deqn{N = S(t) + E(t) + I(t) + R(t).}
#'
#' ## Model variables
#' The SEIR model declares the following variables:
#' \describe{
#'   \item{\code{"S"}}{Susceptible population size.}
#'   \item{\code{"E"}}{Exposed (latent) population size.}
#'   \item{\code{"I"}}{Infectious population size.}
#'   \item{\code{"R"}}{Recovered (immune) population size.}
#'   \item{\code{"incidence"}}{Rate of progression from \code{E} to \code{I},
#'     \eqn{\sigma E(t)}, representing the instantaneous rate at which
#'     individuals become infectious. Note: this differs from the force of
#'     infection \eqn{\lambda(t) = \beta S(t) I(t)/N} (rate of new exposures).
#'     Both are epidemiologically meaningful but measure different transitions.}
#' }
#'
#' All declared variables may be used as observables in generic utilities
#' and summary methods built around \code{epi_model} objects.
#'
#' ## Parameters
#' The SEIR model depends on the following parameters:
#' \describe{
#'   \item{beta}{Transmission rate (contacts per individual per day times
#'     probability of transmission per contact).}
#'   \item{sigma}{Rate of progression from exposed to infectious (per day);
#'     \eqn{1/\sigma} is the mean latent (incubation) period.}
#'   \item{gamma}{Recovery/removal rate (per day);
#'     \eqn{1/\gamma} is the mean infectious period.}
#' }
#'
#' ## Basic reproduction number
#' The basic reproduction number is
#' \deqn{R_0 = \frac{\beta}{\gamma}.}
#' Note that \eqn{R_0} is independent of \eqn{\sigma}: the latent period does
#' not affect the long-run invasion threshold, but it does delay the epidemic
#' peak compared with the SIR model with the same \eqn{\beta} and \eqn{\gamma}.
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
#' \frac{dS}{dt} &= -\lambda(t), \\
#' \frac{dE}{dt} &= \lambda(t) - \sigma E(t), \\
#' \frac{dI}{dt} &= \sigma E(t) - \gamma I(t), \\
#' \frac{dR}{dt} &= \gamma I(t).
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
#' deterministic SEIR
#' (Susceptible-Exposed-Infectious-Recovered) compartmental epidemic model.
#' The returned object contains the model right-hand side, declared state and
#' derived variables, parameter names, and default initial conditions needed by
#' utilities such as \code{\link{simulate_epi}}.
#'
#' @examples
#' ## Simulate a SEIR epidemic
#' sim <- simulate_epi(
#'   model = SEIR_MODEL,
#'   times = 0:200,
#'   parms = c(beta = 0.3, sigma = 0.2, gamma = 0.14),
#'   init  = c(S = 1e6, E = 5, I = 10, R = 0)
#' )
#'
#' plot(sim)
#'
#' ## Plot incidence
#' plot(sim, what = "incidence")
#'
#' @references
#' Anderson, R. M. & May, R. M. (1991).
#' *Infectious Diseases of Humans: Dynamics and Control*.
#' Oxford University Press. ISBN: 9780198540403.
#'
#' Li, M. Y. & Muldowney, J. S. (1995).
#' Global stability for the SEIR model in epidemiology.
#' *Mathematical Biosciences*, **125**(2), 155–164.
#' \doi{10.1016/0025-5564(95)92756-5}
#'
#' Hethcote, H. W. (2000).
#' The mathematics of infectious diseases.
#' *SIAM Review*, **42**(4), 599–653.
#' \doi{10.1137/S0036144500371907}
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
SEIR_MODEL <- epi_model(
  name = "SEIR",
  rhs = seir_rhs,
  par_names = c("beta", "sigma", "gamma"),
  lower = c(beta = 1e-8, sigma = 1e-8, gamma = 1e-8),
  upper = c(beta = 5,    sigma = 2,    gamma = 2),
  defaults = c(beta = 0.3, sigma = 0.2, gamma = 0.14),
  init = c("S" = 1e6, "E" = 5, "I" = 10, "R" = 0),
  states = c("S", "E", "I", "R"),
  derived  = c("incidence")
)
