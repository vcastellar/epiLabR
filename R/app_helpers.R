#===============================================================================
# Shiny helpers for epi app
#===============================================================================

#-------------------------------------------------------------------------------
# Null-coalescing operator
#-------------------------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x


#-------------------------------------------------------------------------------
# Parameter sliders UI
#-------------------------------------------------------------------------------
param_sliders_ui <- function(model) {

  lapply(model$par_names, function(p) {

    val <- model$defaults[[p]] %||% 1

    lower <- model$lower[[p]] %||% (val / 10)
    upper <- upper <- (model$upper[[p]] %||% val) * 3

    shiny::sliderInput(
      inputId = paste0("par_", p),
      label   = p,
      min     = lower,
      max     = upper,
      value   = val
    )
  })
}


init_sliders_ui <- function(model) {

  states <- model$states
  init   <- model$init

  has_init <- !is.null(init) && length(init) > 0 && sum(init) > 0
  N <- if (has_init) sum(init) else 1e6

  lapply(states, function(s) {

    val <- init[[s]] %||% 0

    max_val <- if (s == "S") {
      N
    } else {
      max(100, 5 * val)
    }

    shiny::sliderInput(
      inputId = paste0("init_", s),
      label   = s,
      min     = 0,
      max     = max_val,
      value   = val
    )
  })
}


#-------------------------------------------------------------------------------
# Build parameters vector from input
#-------------------------------------------------------------------------------
get_parms_from_input <- function(input, model) {

  setNames(
    sapply(model$par_names, function(p) {
      input[[paste0("par_", p)]]
    }),
    model$par_names
  )
}


#-------------------------------------------------------------------------------
# Build initial state vector from input
#-------------------------------------------------------------------------------
get_init_from_input <- function(input, model) {

  setNames(
    sapply(model$states, function(s) {
      input[[paste0("init_", s)]]
    }),
    model$states
  )
}


#-------------------------------------------------------------------------------
# RHS code as text (for display)
#-------------------------------------------------------------------------------
rhs_text <- function(model) {
  paste(deparse(body(model$rhs)), collapse = "\n")
}
