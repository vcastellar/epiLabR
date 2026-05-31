# epiLabR

**epiLabR** is an R package for simulating and exploring deterministic
compartmental epidemiological models formulated as systems of ordinary
differential equations (ODEs).

The package focuses on **model definition and simulation**, providing
a small but flexible framework to study epidemic dynamics under different
assumptions and parameter values. A **Shiny application** is
included for interactive exploration.

---

## Features

- Deterministic simulation of epidemic models using ODEs
- Flexible definition of custom compartmental models
- Several classical models included:
  - SIS (Susceptible-Infectious-Susceptible)
  - SIR (Susceptible-Infectious-Recovered)
  - SIRS (Susceptible-Infectious-Recovered-Susceptible)
  - SEIR (Susceptible-Exposed-Infectious-Recovered)
  - SEIRS (Susceptible-Exposed-Infectious-Recovered-Susceptible)
  - SIRV (Susceptible-Infectious-Recovered-Vaccinated)
  - SIR-V (Susceptible-Infectious-Recovered with vital dynamics)
  - SIRD (Susceptible-Infectious-Recovered-Deceased)
- Numerical integration based on `deSolve`
- Built-in post-simulation metrics (e.g., peak incidence, attack rate)
- Interactive Shiny app for visual exploration of model dynamics

> ⚠️ This package does **not** perform parameter estimation or statistical
> inference. Its scope is simulation and exploration only.

---

## Installation

Install the package from source:

```r
devtools::install_local("path/to/epiLabR")
library(epiLabR)

sim <- simulate_epi(
  model = SIR_MODEL,
  times = 0:200,
  parms = c(beta = 0.3, gamma = 0.1),
  init  = c(S = 1e6, I = 10, R = 0)
)

plot(sim)
plot(sim, what = "incidence")
```

> ℹ️ `library(epiLabR, lib.loc = "path/to/epiLabR")` does **not** load a
> source checkout. R expects installed package metadata such as
> `Meta/package.rds`, so loading the repository directory directly can produce
> warnings like `package 'epiLabR' has no 'package.rds' in Meta/` and missing
> S3 method registrations. Use `devtools::install_local()`,
> `remotes::install_local()`, or `pkgload::load_all()` from the repository
> root instead.

You can also install from GitHub:

```r
remotes::install_github("vcastellar/epiLabR")
```

## Defining a custom model

Users can define their own epidemic models by specifying the right-hand
side of the ODE system and creating an epi_model object.

```r
sir_rhs <- function(time, state, parms) {
  with(as.list(c(state, parms)), {
    N <- S + I + R
    lambda <- beta * S * I / N

    dS <- -lambda
    dI <-  lambda - gamma * I
    dR <-  gamma * I

    list(c(dS, dI, dR), incidence = lambda)
  })
}

my_sir <- epi_model(
  name        = "MySIR",
  rhs         = sir_rhs,
  states      = c("S", "I", "R"),
  par_names   = c("beta", "gamma"),
  init        = c(S = 1e6, I = 10, R = 0)
)
```

This model can then be simulated using simulate_epi() like any built-in
model.

## Using a custom model in the Shiny app

Pass any custom model directly to `run_epi_app()`. Built-in models are
always included by default.

```r
run_epi_app(models = list(my_sir))          # built-ins + my_sir
run_epi_app(models = list(my_sir), only = TRUE)  # only my_sir
```

To inspect or compose the built-in model list programmatically:

```r
builtin_models()           # named list of all 8 built-in epi_model objects
names(builtin_models())    # "SIS" "SIR" "SIRS" "SEIR" "SEIRS" "SIRV" "SIR-V" "SIRD"
```

## Working with simulation outputs

After simulation, the resulting object includes state trajectories and any
derived variables configured by the model. These outputs can be post-processed
with user-defined analysis workflows outside the package core.

## Shiny application

The package includes a Shiny app for interactive exploration of epidemic
models.

```r
library(epiLabR)
if (interactive()) {
  run_epi_app()
}
```

The app allows users to:

- select a built-in model
- adjust parameters and initial conditions
- visualize state trajectories and incidence
- inspect the model equations

The Shiny app is included in the package and uses the installed `shiny`
dependency.

## Scope and philosophy

epiLabR is designed as a simulation-oriented package:

- deterministic models
- explicit compartmental structure
- transparent dynamics
- minimal hidden state

It is intended for teaching, exploration, and rapid prototyping of
epidemiological models, rather than for statistical inference or
data-driven estimation.
