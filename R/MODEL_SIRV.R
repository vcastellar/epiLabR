
#-------------------------------------------------------------------------------
# SIRV model
#-------------------------------------------------------------------------------
#' @keywords internal
#' @noRd
sirv_rhs <- function(time, state, parms) {
  with(as.list(c(state, parms)), {

    N <- S + I + R + V

    lambda <- beta * S * I / N

    dS <- -lambda - nu * S
    dI <-  lambda - gamma * I
    dR <-  gamma * I
    dV <-  nu * S

    list(
      c(dS, dI, dR, dV),

      ## instantaneous flows
      incidence   = lambda,
      vaccination = nu * S
    )
  })
}



#' SIRV epidemic model with vaccination
#'
#' @name SIRV_MODEL
#' @description
#' An \code{epi_model} object representing a deterministic **SIRV**
#' (Susceptible–Infectious–Recovered–Vaccinated) compartmental epidemic model
#' with ongoing vaccination.
#'
#' The model describes the spread of an infection in a closed population where
#' susceptible individuals can either become infectious or be vaccinated and
#' move directly to permanent immunity. It is widely used to teach herd
#' immunity concepts and vaccination coverage thresholds.
#'
#' @details
#' ## State variables
#' The model is defined in terms of the following state variables:
#' \describe{
#'   \item{S(t)}{Number of susceptible individuals at time \eqn{t}.}
#'   \item{I(t)}{Number of infectious (actively infected) individuals at time \eqn{t}.}
#'   \item{R(t)}{Number of recovered (immune) individuals at time \eqn{t}.}
#'   \item{V(t)}{Number of vaccinated (immune) individuals at time \eqn{t}.}
#' }
#'
#' The total population size is conserved: vaccination moves individuals from
#' \code{S} to \code{V} without changing \eqn{N}:
#' \deqn{N = S(t) + I(t) + R(t) + V(t).}
#'
#' ## Model variables
#' The SIRV model declares the following variables:
#' \describe{
#'   \item{\code{"S"}}{Susceptible population size.}
#'   \item{\code{"I"}}{Infectious population size.}
#'   \item{\code{"R"}}{Recovered (immune) population size.}
#'   \item{\code{"V"}}{Vaccinated (immune) population size.}
#'   \item{\code{"incidence"}}{Instantaneous rate of new infections
#'     \eqn{\lambda(t) = \beta S(t) I(t) / N} returned by the model's
#'     right-hand side.}
#'   \item{\code{"vaccination"}}{Instantaneous vaccination flow \eqn{\nu S(t)},
#'     i.e. the rate at which susceptible individuals move to the vaccinated
#'     compartment \code{V}.}
#' }
#'
#' All declared variables may be used as observables in generic utilities
#' and summary methods built around \code{epi_model} objects.
#'
#' ## Parameters
#' The SIRV model depends on the following parameters:
#' \describe{
#'   \item{beta}{Transmission rate (contacts per individual per day times
#'     probability of transmission per contact).}
#'   \item{gamma}{Recovery/removal rate (per day); \eqn{1/\gamma} is the mean
#'     infectious period.}
#'   \item{nu}{Vaccination rate (per susceptible individual per day); the
#'     per-capita rate at which susceptible individuals are vaccinated and
#'     acquire permanent immunity.}
#' }
#'
#' ## Basic reproduction number
#' In the absence of vaccination (\eqn{\nu = 0}) the basic reproduction number
#' is
#' \deqn{R_0 = \frac{\beta}{\gamma}.}
#' The effective reproduction number at time \eqn{t} reflects the current
#' susceptible fraction:
#' \deqn{R_{\text{eff}}(t) = R_0 \frac{S(t)}{N}.}
#'
#' ## Herd immunity threshold
#' Herd immunity is achieved when the vaccinated (or otherwise immune) fraction
#' of the population is large enough to prevent epidemic growth. The critical
#' vaccination coverage threshold is
#' \deqn{p^* = 1 - \frac{1}{R_0}.}
#' When the combined fraction of recovered and vaccinated individuals exceeds
#' \eqn{p^*}, new introductions of the pathogen cannot sustain exponential
#' growth.
#'
#' ## Model equations
#' New infections occur at force of infection
#' \deqn{\lambda(t) = \beta \frac{S(t)\, I(t)}{N}.}
#'
#' The system of ordinary differential equations is:
#' \deqn{
#' \begin{aligned}
#' \frac{dS}{dt} &= -\lambda(t) - \nu S(t), \\
#' \frac{dI}{dt} &= \lambda(t) - \gamma I(t), \\
#' \frac{dR}{dt} &= \gamma I(t), \\
#' \frac{dV}{dt} &= \nu S(t).
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
#' deterministic SIRV (Susceptible-Infectious-Recovered-Vaccinated) compartmental
#' epidemic model. The returned object contains the model right-hand side,
#' declared state and derived variables, parameter names, and default initial
#' conditions needed by utilities such as \code{\link{simulate_epi}}.
#'
#' @examples
#' ## Simulate a SIRV epidemic with ongoing vaccination
#' sim <- simulate_epi(
#'   model = SIRV_MODEL,
#'   times = 0:365,
#'   parms = c(beta = 0.3, gamma = 0.1, nu = 0.005),
#'   init  = c(S = 999990, I = 10, R = 0, V = 0)
#' )
#'
#' plot(sim)
#'
#' ## Plot incidence to see the effect of vaccination on epidemic dynamics
#' plot(sim, what = "incidence")
#'
#' ## Explore the herd immunity threshold: with R0 = beta/gamma = 3,
#' ## herd immunity requires vaccinating p* = 1 - 1/R0 = 2/3 of the population.
#' ## Increase nu to see the epidemic suppressed more rapidly.
#' sim_fast <- simulate_epi(
#'   model = SIRV_MODEL,
#'   times = 0:365,
#'   parms = c(beta = 0.3, gamma = 0.1, nu = 0.02),
#'   init  = c(S = 999990, I = 10, R = 0, V = 0)
#' )
#'
#' plot(sim_fast)
#'
#' @references
#' Anderson, R. M. & May, R. M. (1991).
#' *Infectious Diseases of Humans: Dynamics and Control*.
#' Oxford University Press.
#' \doi{10.1093/oso/9780198540403.001.0001}
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
SIRV_MODEL <- epi_model(
  name = "SIRV",
  rhs = sirv_rhs,
  par_names = c("beta", "gamma", "nu"),

  lower = c(beta = 1e-8, gamma = 1e-8, nu = 0),
  upper = c(beta = 2,    gamma = 1,    nu = 0.1),

  defaults = c(beta = 0.3, gamma = 0.1, nu = 0.005),

  init = c(S = 999990, I = 10, R = 0, V = 0),

  states  = c("S", "I", "R", "V"),
  derived = c("incidence", "vaccination")
)
