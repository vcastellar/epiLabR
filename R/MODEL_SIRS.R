#-------------------------------------------------------------------------------
# SIRS model
#-------------------------------------------------------------------------------
#' @keywords internal
#' @noRd
sirs_rhs <- function(time, state, parms) {
  with(as.list(c(state, parms)), {
    N <- S + I + R
    lambda <- beta * S * I / N
    dS <- -lambda + omega * R
    dI <-  lambda - gamma * I
    dR <-  gamma * I - omega * R
    list(c(dS, dI, dR), incidence = lambda)
  })
}


#' SIRS epidemic model with waning immunity
#'
#' @name SIRS_MODEL
#' @description
#' An \code{epi_model} object representing a deterministic **SIRS**
#' (Susceptible–Infectious–Recovered–Susceptible) compartmental epidemic model
#' with waning immunity.
#'
#' The model describes the spread of an infection in a closed population where
#' susceptible individuals become infectious, subsequently recover, and may
#' lose immunity over time, returning to the susceptible compartment at rate
#' \code{omega}.
#'
#' @details
#' ## State variables
#' The model is defined in terms of the following state variables:
#' \describe{
#'   \item{S(t)}{Number of susceptible individuals at time \eqn{t}.}
#'   \item{I(t)}{Number of infectious (actively infected) individuals at time \eqn{t}.}
#'   \item{R(t)}{Number of recovered individuals with temporary immunity at time \eqn{t}.}
#' }
#'
#' The total population size is conserved:
#' \deqn{N = S(t) + I(t) + R(t).}
#'
#' ## Model variables
#' The SIRS model declares the following variables:
#' \describe{
#'   \item{\code{"S"}}{Susceptible population size.}
#'   \item{\code{"I"}}{Infectious population size.}
#'   \item{\code{"R"}}{Recovered (temporarily immune) population size.}
#'   \item{\code{"incidence"}}{Instantaneous rate of new infections
#'     \eqn{\lambda(t)} returned by the model's right-hand side.}
#' }
#'
#' All declared variables may be used as observables in generic utilities
#' and summary methods built around \code{epi_model} objects.
#'
#' ## Parameters
#' The SIRS model depends on the following parameters:
#' \describe{
#'   \item{beta}{Transmission rate (contacts per individual per day times
#'     probability of transmission per contact).}
#'   \item{gamma}{Recovery/removal rate (per day); \eqn{1/\gamma} is the mean
#'     infectious period.}
#'   \item{omega}{Rate of waning immunity from \code{R} back to \code{S}
#'     (per day); \eqn{1/\omega} is the mean duration of immunity.}
#' }
#'
#' ## Basic reproduction number
#' The basic reproduction number is
#' \deqn{R_0 = \frac{\beta}{\gamma}.}
#' When \eqn{R_0 > 1} the SIRS model admits an **endemic equilibrium**
#' \deqn{
#' I^* = \frac{N(\gamma + \omega)(R_0 - 1)}{\beta(\gamma + \omega)/\gamma},
#' \quad S^* = \frac{N}{R_0}.
#' }
#' Because recovered individuals eventually return to \code{S}, the disease
#' can persist endemically even in a closed population, in contrast to the
#' standard SIR model.
#'
#' ## Model equations
#' New infections occur at rate
#' \deqn{\lambda(t) = \beta \frac{S(t)\, I(t)}{N}.}
#'
#' The system of ordinary differential equations is:
#' \deqn{
#' \begin{aligned}
#' \frac{dS}{dt} &= -\lambda(t) + \omega R(t), \\
#' \frac{dI}{dt} &= \lambda(t) - \gamma I(t), \\
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
#' deterministic SIRS
#' (Susceptible-Infectious-Recovered-Susceptible) compartmental epidemic model.
#' The returned object contains the model right-hand side, declared state and
#' derived variables, parameter names, and default initial conditions needed by
#' utilities such as \code{\link{simulate_epi}}.
#'
#' @examples
#' ## Simulate a SIRS epidemic
#' sim <- simulate_epi(
#'   model = SIRS_MODEL,
#'   times = 0:200,
#'   parms = c(beta = 0.3, gamma = 0.1, omega = 0.02),
#'   init  = c(S = 1e6, I = 20, R = 0)
#' )
#'
#' plot(sim)
#'
#' ## Plot incidence
#' plot(sim, what = "incidence")
#'
#'
#' @references
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
#' Liu, W. M., Hethcote, H. W. & Levin, S. A. (1987).
#' Dynamical behavior of epidemiological models with nonlinear incidence rates.
#' *Journal of Mathematical Biology*, **25**(4), 359–380.
#' \doi{10.1007/BF00277162}
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

SIRS_MODEL <- epi_model(
  name = "SIRS",
  rhs = sirs_rhs,
  par_names = c("beta", "gamma", "omega"),
  lower = c(beta = 1e-8, gamma = 1e-8, omega = 1e-8),
  upper = c(beta = 2,    gamma = 1,    omega = 1),
  defaults = c(beta = 0.3, gamma = 0.1, omega = 0.02),
  init = c("S" = 1e6, "I" = 20, "R" = 0),
  states = c("S", "I", "R"),
  derived  = c("incidence")
)
