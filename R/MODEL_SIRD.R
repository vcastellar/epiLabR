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
#' (Susceptible-Infectious-Recovered-Deceased) compartmental epidemic model.
#'
#' @details
#' The model is defined by the system:
#' \deqn{
#' \begin{aligned}
#' \frac{dS}{dt} &= -\lambda(t), \\
#' \frac{dI}{dt} &= \lambda(t) - \gamma I(t) - \mu I(t), \\
#' \frac{dR}{dt} &= \gamma I(t), \\
#' \frac{dD}{dt} &= \mu I(t),
#' \end{aligned}
#' }
#' where \eqn{\lambda(t) = \beta\, S(t)\, I(t)/N} and
#' \eqn{N = S(t) + I(t) + R(t)}.
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
#' sim <- simulate_epi(
#'   model = SIRD_MODEL,
#'   times = 0:200,
#'   parms = c(beta = 0.25, gamma = 0.1, mu = 0.01),
#'   init = c(S = 1e6, I = 10, R = 0, D = 0)
#' )
#'
#' plot(sim)
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
