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
  - SI (Susceptible-Infectious)
  - SIR (Susceptible-Infectious-Recovered)
  - SIR-V (Susceptible-Infectious-Recovered with vital dynamics)
  - SIRS (Susceptible-Infectious-Recovered-Susceptible)
  - SEIR (Susceptible-Exposed-Infectious-Recovered)
  - SEIRS (Susceptible-Exposed-Infectious-Recovered-Susceptible)
- Numerical integration based on `deSolve`
- Built-in post-simulation metrics (e.g., peak incidence, attack rate, growth rates)
- In-session model registry (`register_epi_model()`, `list_models()`, `get_model()`)
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

## Working with the model registry

Custom models can be registered in the package's in-memory registry so they
can be discovered and reused during the current R session.

```r
register_epi_model(my_sir)
list_models()
model <- get_model("MySIR")
```

To remove a model from the active session:

```r
unregister_epi_model("MySIR")
```

## Built-in summary metrics

After simulation, helper functions can be used to summarize epidemic
trajectories, for example:

```r
inc <- sim$derived$incidence
t   <- sim$states$time

peak_incidence(inc, t)
time_to_peak(inc, t)
attack_rate(inc)
```

## Shiny application

The package includes a Shiny app for interactive exploration of epidemic
models.

```r
library(epiLabR)
run_epi_app()
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

## Numerical limitations and stability considerations

All simulations in **epiLabR** are numerical approximations of ODE systems.
As with any ODE workflow, results depend on solver configuration and model
scaling.

- **No guaranteed positivity or conservation from the solver alone**: if a
  model is poorly scaled or tolerances are too loose, state variables can
  become slightly negative due to numerical error.
- **Stiff systems require care**: fast/slow dynamics in the same model can
  make explicit methods unstable unless very small time steps are used.
- **Large populations and very small rates can be ill-conditioned**: mixing
  very large and very small magnitudes may amplify rounding error.
- **Output grid vs. internal steps**: `times` defines reporting points, not
  necessarily the internal step size used by the integrator.

For better numerical robustness:

- start with default adaptive settings, then tighten `rtol`/`atol` when needed;
- inspect trajectories for artifacts (negative compartments, oscillations,
  non-physical spikes);
- compare outcomes under at least two solver settings before drawing
  conclusions.

## Integration methods: practical note (`lsoda`, `rk4`, ...)

`lsoda` refers to the Livermore Solver for Ordinary Differential Equations with
automatic method switching, and `rk4` refers to the classical fourth-order
Runge-Kutta method.


`simulate_epi()` forwards extra arguments to `deSolve::ode()`, so you can
choose integration methods such as `method = "lsoda"` (default in most setups)
or `method = "rk4"`.

Quick guidance:

- **`lsoda`**: good default for most users; automatically switches between
  non-stiff and stiff strategies.
- **`rk4`**: fixed-step explicit Runge-Kutta; often useful for teaching,
  reproducibility at fixed grids, or simple smooth systems.
- **Other `deSolve` methods** (e.g., `ode45`, `bdf`, the backward differentiation formula method): can be tested when
  stiffness, speed, or accuracy requirements differ.

> Benchmark note: runtime and accuracy are model-dependent. In practice,
> compare at least two candidate methods on your own model and check both
> elapsed time and numerical behavior (e.g., positivity and smoothness), not
> runtime alone.
