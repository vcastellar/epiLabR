get_derived <- function(sim, variable) {
  derived_data <- sim$derived
  if (is.null(derived_data) || !"time" %in% names(derived_data)) {
    stop("Simulation does not define derived variables.")
  }

  if (!variable %in% names(derived_data)) {
    stop("Simulation does not define derived variable: ", variable)
  }

  derived_data[[variable]]
}




#' Peak incidence
#'
#' @description
#' Computes the maximum value of an incidence curve and
#' the time at which it occurs.
#'
#' @details
#' Let \eqn{\lambda(t)} denote the incidence function.
#' The peak incidence is defined as:
#' \deqn{
#' \lambda_{\max} = \max_t \lambda(t),
#' }
#' and the corresponding time is:
#' \deqn{
#' t_{\max} = \arg\max_t \lambda(t).
#' }
#'
#' If multiple time points attain the maximum value,
#' the first occurrence is returned.
#'
#' If \code{time} is not provided, it is assumed to be a regular
#' sequence \code{1:length(incidence)}.
#'
#' @param incidence Numeric vector giving the incidence curve.
#' @param time Optional numeric vector of the same length giving time
#' points. If \code{NULL}, a regular time sequence is assumed.
#'
#' @return A named list with components \code{peak} and \code{time}.
#' \code{peak} is a numeric scalar giving the maximum value attained by the
#' incidence curve, and \code{time} is a numeric scalar giving the first time
#' point at which that maximum occurs.
#'
#' @references
#' Centers for Disease Control and Prevention (CDC). <i>Describing
#' Epidemiologic Data</i>. In: <i>Field Epidemiology Manual</i>.
#' Peak incidence and its timing are standard descriptive summaries of
#' epidemic curves.
#'
#' Centers for Disease Control and Prevention (CDC). <i>Principles of
#' Epidemiology in Public Health Practice</i>, Lesson 3. General reference
#' for interpreting incidence curves in outbreak analysis.
#'
#' @examples
#' inc  <- c(1, 3, 7, 5, 2)
#' time <- 0:4
#' peak_incidence(inc, time)
#'
#' @export
peak_incidence <- function(incidence, time = NULL) {

  stopifnot(is.numeric(incidence),
            length(incidence) > 0)

  if (is.null(time)) {
    time <- seq_len(length(incidence))
  }

  stopifnot(is.numeric(time),
            length(time) == length(incidence))

  idx <- which.max(incidence)

  list(
    peak = incidence[idx],
    time = time[idx]
  )
}


#' Time to peak incidence
#'
#' @description
#' Computes the time at which the incidence curve reaches its maximum.
#'
#' @details
#' Let \eqn{\lambda(t)} denote the incidence over time.
#' The time to peak is defined as:
#' \deqn{
#' t_{\max} = \arg\max_t \lambda(t).
#' }
#'
#' If multiple time points attain the maximum value,
#' the first occurrence is returned.
#'
#' If \code{time} is not provided, it is assumed to be a regular
#' sequence \code{1:length(incidence)}.
#'
#' @param incidence Numeric vector giving the incidence curve.
#' @param time Optional numeric vector of time points. Must have the
#' same length as \code{incidence}. If \code{NULL}, a regular time
#' sequence is assumed.
#'
#' @return A numeric scalar giving the first time point at which the incidence
#' curve reaches its maximum. This is the timing of peak incidence, not the
#' peak incidence magnitude itself.
#'
#' @references
#' Centers for Disease Control and Prevention (CDC). <i>Describing
#' Epidemiologic Data</i>. In: <i>Field Epidemiology Manual</i>.
#' The timing of the epidemic peak is a standard feature of epidemic curves.
#'
#' Centers for Disease Control and Prevention (CDC). <i>Principles of
#' Epidemiology in Public Health Practice</i>, Lesson 3. General reference
#' for interpreting temporal patterns in incidence.
#'
#' @examples
#' inc  <- c(1, 4, 6, 3)
#' time <- 0:3
#' time_to_peak(inc, time)
#'
#' @export
time_to_peak <- function(incidence, time = NULL) {

  stopifnot(is.numeric(incidence),
            length(incidence) > 0)

  if (is.null(time)) {
    time <- seq_len(length(incidence))
  }

  stopifnot(is.numeric(time),
            length(time) == length(incidence))

  idx <- which.max(incidence)

  time[idx]
}


#' Peak prevalence
#'
#' @description
#' Computes the maximum value of a prevalence curve and
#' the time at which it occurs.
#'
#' @details
#' Let \eqn{I(t)} denote prevalence over time.
#' The peak prevalence is defined as:
#' \deqn{
#' I_{\max} = \max_t I(t).
#' }
#'
#' The function returns both the maximum value and the
#' corresponding time point.
#'
#' If \code{time} is not provided, it is assumed to be a regular
#' sequence \code{1:length(prevalence)}.
#'
#' @param prevalence Numeric vector representing prevalence over time.
#' @param time Optional numeric vector of time points. Must have the
#' same length as \code{prevalence}. If \code{NULL}, a regular time
#' sequence is assumed.
#'
#' @return A named list with components \code{peak} and \code{time}.
#' \code{peak} is a numeric scalar giving the maximum prevalence observed, and
#' \code{time} is a numeric scalar giving the first time point at which that
#' maximum occurs.
#'
#' @references
#' Centers for Disease Control and Prevention (CDC). <i>Principles of
#' Epidemiology in Public Health Practice</i>, Lesson 3. General reference
#' for prevalence measures in descriptive epidemiology.
#'
#' Caswell-Jin JL, Lim JKH. <i>Prevalence</i>. StatPearls Publishing; 2023.
#' NCBI Bookshelf. General definition of prevalence and related measures.
#'
#' @examples
#' I <- c(1, 5, 8, 4, 2)
#' peak_prevalence(I)
#'
#' @export
peak_prevalence <- function(prevalence, time = NULL) {

  stopifnot(is.numeric(prevalence),
            length(prevalence) > 0)

  if (is.null(time)) {
    time <- seq_len(length(prevalence))
  }

  stopifnot(is.numeric(time),
            length(time) == length(prevalence))

  idx <- which.max(prevalence)

  list(
    peak = prevalence[idx],
    time = time[idx]
  )
}

#' Attack rate
#'
#' @description
#' Computes the cumulative number of events from an incidence curve.
#'
#' @details
#' Let \eqn{\lambda(t)} denote the incidence function.
#' The attack rate is defined as:
#' \deqn{
#' AR = \int_0^T \lambda(t)\, dt.
#' }
#'
#' In discrete time, this is approximated by:
#' \deqn{
#' AR \approx \sum_i \lambda(t_i)\,\Delta t_i,
#' }
#' where \eqn{\Delta t_i = t_{i+1} - t_i}.
#'
#' If \code{time} is not provided, a regular time step of 1 is assumed,
#' and the attack rate reduces to the simple sum of the incidence values.
#'
#' @param incidence Numeric vector giving the incidence curve.
#' @param time Optional numeric vector of time points. Must have the
#' same length as \code{incidence}, contain no missing values, and be
#' strictly increasing. If \code{NULL}, unit time steps are assumed.
#'
#' @return A numeric scalar giving the cumulative attack rate over the input
#' time horizon. It represents the total number of incident events, or the
#' time-integrated incidence when irregular observation times are supplied.
#'
#' @references
#' Centers for Disease Control and Prevention (CDC). <i>Principles of
#' Epidemiology in Public Health Practice</i>, Lesson 3. Classic reference
#' for attack rate as an outbreak frequency measure.
#'
#' Bouter LM, Zeegers MP, Li T. <i>Textbook of Epidemiology</i>. 2nd ed.
#' Wiley Blackwell; 2021. General reference for cumulative incidence and
#' related frequency measures.
#'
#' @examples
#' inc <- c(1, 2, 3, 4)
#' attack_rate(inc)
#'
#' @export
attack_rate <- function(incidence, time = NULL) {

  stopifnot(is.numeric(incidence),
            length(incidence) > 0)

  if (is.null(time)) {
    return(sum(incidence, na.rm = TRUE))
  }

  stopifnot(is.numeric(time),
            length(time) == length(incidence),
            all(!is.na(time)),
            all(diff(time) > 0))

  dt <- diff(time)

  # Trapezoidal approximation for better consistency
  sum((incidence[-length(incidence)] +
         incidence[-1]) / 2 * dt,
      na.rm = TRUE)
}


#' Initial exponential growth rate
#'
#' @description
#' Estimates the early exponential growth rate by fitting a log-linear model
#' to the initial segment of an incidence curve.
#'
#' @details
#' During the early phase of an epidemic, incidence is often approximated by:
#' \deqn{
#' \lambda(t) \approx C e^{r t},
#' }
#' where \eqn{r} is the exponential growth rate.
#'
#' Taking logarithms:
#' \deqn{
#' \log \lambda(t) = \log C + r t,
#' }
#' so that \eqn{r} can be estimated as the slope of a linear regression
#' of \eqn{\log(\lambda(t))} on \eqn{t}.
#'
#' The growth rate is estimated using the first \code{n} time points,
#' which are assumed to belong to the exponential growth phase.
#'
#' If \code{time} is not provided, it is assumed to be a regular
#' sequence \code{1:length(incidence)}.
#'
#' @param incidence Numeric vector of incidence values.
#' Values must be strictly positive over the estimation window.
#' @param time Optional numeric vector of time points. Must have the
#' same length as \code{incidence}. If \code{NULL}, a regular time
#' sequence is assumed.
#' @param n Integer. Number of initial time points used for estimation.
#' Must be at least 2. Default is 7.
#'
#' @return A numeric scalar giving the estimated initial exponential growth
#' rate \eqn{r}. Positive values indicate early epidemic growth, values near
#' zero indicate little change, and negative values indicate decline.
#'
#' @references
#' Wallinga J, Lipsitch M. How generation intervals shape the relationship
#' between growth rates and reproductive numbers. <i>Proceedings of the Royal
#' Society B</i>. 2007;274(1609):599-604.
#'
#' Hethcote HW. The mathematics of infectious diseases. <i>SIAM Review</i>.
#' 2000;42(4):599-653.
#'
#' Ma J. Estimating epidemic exponential growth rate and basic reproduction
#' number. <i>Infectious Disease Modelling</i>. 2020;5:129-141.
#'
#' @export
initial_growth_rate <- function(incidence,
                                time = NULL,
                                n = 7) {

  stopifnot(is.numeric(incidence),
            length(incidence) >= n,
            n >= 2)

  if (is.null(time)) {
    time <- seq_len(length(incidence))
  }

  stopifnot(is.numeric(time),
            length(time) == length(incidence))

  inc <- incidence[seq_len(n)]
  t   <- time[seq_len(n)]

  if (any(inc <= 0)) {
    stop("Incidence must be strictly positive over the estimation window.")
  }

  fit <- stats::lm(log(inc) ~ t)

  unname(coef(fit)[2])
}

#' Initial doubling time
#'
#' @description
#' Estimates the epidemic doubling time during the initial
#' exponential growth phase.
#'
#' @details
#' During the early phase of an epidemic, incidence is often
#' approximated by exponential growth:
#' \deqn{
#' \lambda(t) \approx \lambda_0 e^{r t}.
#' }
#'
#' Under this assumption, the doubling time is:
#' \deqn{
#' T_d = \frac{\log 2}{r},
#' }
#' where \eqn{r} is the initial exponential growth rate,
#' estimated using \code{\link{initial_growth_rate}} over the
#' first \code{n} time points.
#'
#' If \eqn{r \le 0}, the doubling time is set to \code{Inf}.
#'
#' If \code{time} is not provided, it is assumed to be a regular
#' sequence \code{1:length(incidence)}.
#'
#' @param incidence Numeric vector of incidence values.
#' @param time Optional numeric vector of time points. Must have the
#' same length as \code{incidence}. If \code{NULL}, a regular time
#' sequence is assumed.
#' @param n Integer. Number of initial time points used to estimate
#' the exponential growth rate. Default is 7.
#'
#' @return A numeric scalar giving the estimated initial doubling time. The
#' value is on the same time scale as \code{time}; \code{Inf} is returned when
#' the estimated initial growth rate is non-positive, meaning the epidemic is
#' not doubling.
#'
#' @references
#' Wallinga J, Lipsitch M. How generation intervals shape the relationship
#' between growth rates and reproductive numbers. <i>Proceedings of the Royal
#' Society B</i>. 2007;274(1609):599-604.
#'
#' Muniz-Rodriguez K, Fung IC-H, Ferdosi SR, et al. Doubling time of the
#' COVID-19 epidemic by province, China. <i>Emerging Infectious Diseases</i>.
#' 2020;26(8):1912-1914.
#'
#' Saito S, Watashi K, Tsuchiya Y, et al. Doubling time of infectious
#' diseases. <i>Journal of Theoretical Biology</i>. 2022;553:111238.
#'
#' @export
initial_doubling_time <- function(incidence,
                                  time = NULL,
                                  n = 7) {

  stopifnot(is.numeric(incidence),
            length(incidence) >= n,
            n >= 2)

  if (is.null(time)) {
    time <- seq_len(length(incidence))
  }

  stopifnot(is.numeric(time),
            length(time) == length(incidence))

  r <- initial_growth_rate(
    incidence = incidence,
    time      = time,
    n         = n
  )

  if (isTRUE(all.equal(r, 0)) || r < 0) {
    return(Inf)
  }

  log(2) / r
}



#' Instantaneous growth rate
#'
#' @description
#' Computes the time-varying exponential growth rate from an incidence curve.
#'
#' @details
#' The instantaneous growth rate is approximated in discrete time by:
#' \deqn{
#' r(t_i) \approx \frac{\log(\lambda(t_{i+1}) + c) -
#' \log(\lambda(t_i) + c)}
#' {t_{i+1} - t_i},
#' }
#' where \eqn{\lambda(t_i)} is the incidence at time \eqn{t_i}
#' and \eqn{c} is a small offset added to avoid numerical issues
#' when incidence is zero.
#'
#' If \code{window > 1}, a centered moving average of size
#' \code{window} is applied to the incidence curve before computing
#' growth rates. This smoothing reduces short-term fluctuations
#' but shortens the effective time series due to edge effects.
#'
#' If \code{time} is not provided, it is assumed to be a regular
#' sequence \code{1:length(incidence)}.
#'
#' Positive values of \code{r} indicate epidemic growth,
#' negative values indicate decline, and \code{r = 0}
#' corresponds to constant incidence.
#'
#' @param incidence Numeric vector of incidence values.
#' @param time Optional numeric vector of time points. Must have the
#' same length as \code{incidence}. If \code{NULL}, a regular time
#' sequence is assumed.
#' @param window Integer. Size of the centered moving average
#' window. Default is 1 (no smoothing).
#' @param offset Numeric. Small positive constant added to avoid
#' \code{log(0)}. Default is 0.5.
#'
#' @return A \code{data.frame} with one row per estimated interval and two
#' columns: \code{time}, containing the time points associated with each growth
#' rate estimate, and \code{r}, containing the corresponding instantaneous
#' exponential growth rates. Positive \code{r} values indicate growth and
#' negative values indicate decline.
#'
#' @references
#' Parag KV, Donnelly CA, Jha R, Thompson RN. Are epidemic growth rates more
#' informative than reproduction numbers? <i>Journal of the Royal Statistical
#' Society Series A</i>. 2022;185(S1):S264-S282.
#'
#' Wallinga J, Lipsitch M. How generation intervals shape the relationship
#' between growth rates and reproductive numbers. <i>Proceedings of the Royal
#' Society B</i>. 2007;274(1609):599-604.
#'
#' @export
instantaneous_growth_rate <- function(incidence,
                                      time = NULL,
                                      window = 1,
                                      offset = 0.5) {

  stopifnot(is.numeric(incidence),
            length(incidence) > 1)

  if (is.null(time)) {
    time <- seq_len(length(incidence))
  }

  stopifnot(is.numeric(time),
            length(time) == length(incidence),
            window >= 1,
            offset > 0)

  inc <- incidence
  t   <- time

  if (window > 1) {
    inc <- as.numeric(stats::filter(inc,
                                    rep(1 / window, window),
                                    sides = 2))
    valid <- !is.na(inc)
    inc <- inc[valid]
    t   <- t[valid]
  }

  log_inc <- log(inc + offset)

  r <- diff(log_inc) / diff(t)

  data.frame(
    time = t[-1],
    r = r
  )
}



#' Time-varying doubling time
#'
#' @description
#' Computes the time-varying doubling time from an incidence curve.
#'
#' @details
#' The doubling time is derived from the instantaneous growth rate:
#' \deqn{
#' T_d(t) = \frac{\log 2}{r(t)},
#' }
#' where \eqn{r(t)} is computed using
#' \code{\link{instantaneous_growth_rate}}.
#'
#' If \eqn{r(t) > 0}, the doubling time represents the time required
#' for incidence to double under exponential growth.
#'
#' If \eqn{r(t) \le 0}, doubling time is set to \code{Inf},
#' indicating that incidence is not increasing.
#'
#' If \code{window > 1}, smoothing is applied before computing
#' growth rates (see \code{\link{instantaneous_growth_rate}}).
#'
#' If \code{time} is not provided, it is assumed to be a regular
#' sequence \code{1:length(incidence)}.
#'
#' @param incidence Numeric vector of incidence values.
#' @param time Optional numeric vector of time points. Must have the
#' same length as \code{incidence}. If \code{NULL}, a regular time
#' sequence is assumed.
#' @param window Integer. Size of the centered moving average
#' window. Default is 1 (no smoothing).
#' @param offset Numeric. Small positive constant added to avoid
#' \code{log(0)}. Default is 0.5.
#'
#' @return A \code{data.frame} with one row per estimated interval and two
#' columns: \code{time}, containing the time points associated with each
#' estimate, and \code{doubling_time}, containing the implied doubling time on
#' the same scale as \code{time}. Values of \code{Inf} indicate intervals in
#' which incidence is not increasing.
#'
#' @references
#' Parag KV, Donnelly CA, Jha R, Thompson RN. Are epidemic growth rates more
#' informative than reproduction numbers? <i>Journal of the Royal Statistical
#' Society Series A</i>. 2022;185(S1):S264-S282.
#'
#' Saito S, Watashi K, Tsuchiya Y, et al. Doubling time of infectious
#' diseases. <i>Journal of Theoretical Biology</i>. 2022;553:111238.
#'
#' Muniz-Rodriguez K, Fung IC-H, Ferdosi SR, et al. Doubling time of the
#' COVID-19 epidemic by province, China. <i>Emerging Infectious Diseases</i>.
#' 2020;26(8):1912-1914.
#'
#' @export
doubling_time_ts <- function(incidence,
                             time = NULL,
                             window = 1,
                             offset = 0.5) {

  stopifnot(is.numeric(incidence),
            length(incidence) > 1)

  if (is.null(time)) {
    time <- seq_len(length(incidence))
  }

  stopifnot(is.numeric(time),
            length(time) == length(incidence),
            window >= 1,
            offset > 0)

  gr <- instantaneous_growth_rate(
    incidence = incidence,
    time      = time,
    window    = window,
    offset    = offset
  )

  Td <- log(2) / gr$r
  Td[gr$r <= 0] <- Inf

  data.frame(
    time = gr$time,
    doubling_time = Td
  )
}
