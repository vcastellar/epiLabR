#' Built-in epidemiological models
#'
#' @description
#' Returns a named list containing all built-in \code{epi_model} objects
#' provided by the package. Useful as a starting point when building a custom
#' model list for \code{\link{run_epi_app}}.
#'
#' @return A named list of \code{epi_model} objects.
#'
#' @seealso \code{\link{run_epi_app}}
#'
#' @export
builtin_models <- function() {
  list(
    SIS   = SIS_MODEL,
    SIR   = SIR_MODEL,
    SIRS  = SIRS_MODEL,
    SEIR  = SEIR_MODEL,
    SEIRS = SEIRS_MODEL,
    SIRV  = SIRV_MODEL,
    `SIR-V` = SIR_V_MODEL,
    SIRD  = SIRD_MODEL
  )
}

#' Launch the epidemic model Shiny simulator
#'
#' @description
#' Launches the interactive Shiny application for simulating deterministic
#' epidemiological models defined as systems of ordinary differential equations
#' (ODEs).
#'
#' By default the app loads all built-in models. Custom models can be added
#' via the \code{models} argument.
#'
#' @param models A list of \code{epi_model} objects to show in the
#'   application, in addition to the built-in models. Each element must be
#'   an object of class \code{"epi_model"} created with \code{\link{epi_model}}.
#'   List names are used as display labels; if names are missing they are
#'   taken from each model's \code{name} field. Pass an empty list
#'   (\code{list()}) to suppress built-in models and show only custom ones —
#'   in that case supply at least one model via a separate named argument or
#'   use \code{only = TRUE}.
#' @param only Logical. If \code{TRUE}, only the models supplied in
#'   \code{models} are shown, without any built-in models. Default
#'   \code{FALSE}.
#'
#' @details
#' The full list of models passed to the app can be constructed manually:
#'
#' ```r
#' run_epi_app(models = list(my_model))
#' run_epi_app(models = list(my_model), only = TRUE)
#' run_epi_app(models = c(builtin_models(), list(MyModel = my_model)))
#' ```
#'
#' @return A \code{shiny.appobj} returned invisibly. In interactive use the
#'   function is called for its side effect of launching the browser-based app.
#'
#' @examples
#' ## ------------------------------------------------------------------
#' ## Launch with all built-in models (default)
#' ## ------------------------------------------------------------------
#' if (interactive()) {
#'   run_epi_app()
#' }
#'
#' ## ------------------------------------------------------------------
#' ## Add a custom SEIRD model alongside the built-ins
#' ## ------------------------------------------------------------------
#' seird_rhs <- function(time, state, parms) {
#'   with(as.list(c(state, parms)), {
#'     N      <- S + E + I + R
#'     lambda <- beta * S * I / N
#'     dS <- -lambda
#'     dE <-  lambda - sigma * E
#'     dI <-  sigma * E - (gamma + mu) * I
#'     dR <-  gamma * I
#'     dD <-  mu * I
#'     list(c(dS, dE, dI, dR, dD), incidence = lambda)
#'   })
#' }
#'
#' seird_model <- epi_model(
#'   name      = "SEIRD",
#'   rhs       = seird_rhs,
#'   states    = c("S", "E", "I", "R", "D"),
#'   derived   = c("incidence"),
#'   par_names = c("beta", "sigma", "gamma", "mu"),
#'   lower     = c(beta = 1e-8, sigma = 1e-8, gamma = 1e-8, mu = 1e-8),
#'   upper     = c(beta = 5,    sigma = 2,    gamma = 2,    mu = 0.5),
#'   defaults  = c(beta = 0.4, sigma = 0.2, gamma = 0.1, mu = 0.02),
#'   init      = c(S = 1e6, E = 5, I = 10, R = 0, D = 0)
#' )
#'
#' if (interactive()) {
#'   run_epi_app(models = list(seird_model))
#' }
#'
#' ## ------------------------------------------------------------------
#' ## Use only custom models (no built-ins)
#' ## ------------------------------------------------------------------
#' if (interactive()) {
#'   run_epi_app(models = list(seird_model), only = TRUE)
#' }
#'
#' @seealso \code{\link{epi_model}}, \code{\link{simulate_epi}},
#'   \code{\link{builtin_models}}
#'
#' @export
run_epi_app <- function(models = NULL, only = FALSE) {

  ## --- validate extra models -----------------------------------------------
  if (!is.null(models)) {
    stopifnot(is.list(models))

    if (length(models) > 0) {
      ok <- vapply(models, inherits, logical(1), "epi_model")
      if (!all(ok)) {
        stop("All elements of 'models' must be objects of class 'epi_model'.")
      }

      ## fill missing names from model$name
      nms <- names(models)
      if (is.null(nms)) nms <- character(length(models))
      blank <- nms == ""
      nms[blank] <- vapply(models[blank], `[[`, character(1), "name")
      names(models) <- nms
    }
  }

  ## --- assemble final model list --------------------------------------------
  if (isTRUE(only)) {
    if (is.null(models) || length(models) == 0) {
      stop("'only = TRUE' requires at least one model in 'models'.")
    }
    all_models <- models
  } else {
    all_models <- builtin_models()
    if (!is.null(models) && length(models) > 0) {
      dup <- intersect(names(models), names(all_models))
      if (length(dup) > 0) {
        warning(
          "Custom model name(s) clash with built-ins and will override them: ",
          paste(dup, collapse = ", ")
        )
      }
      all_models[names(models)] <- models
    }
  }

  ## --- launch ---------------------------------------------------------------
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required to run the app. Please install it.")
  }

  shiny::shinyApp(
    ui     = app_ui(all_models),
    server = function(input, output, session) {
      app_server(input, output, session, all_models)
    }
  )
}
