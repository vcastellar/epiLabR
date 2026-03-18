## Acronym notes for CRAN review

To make the documentation clearer for CRAN review, the package now expands
non-obvious acronyms in user-facing description text where they first appear and
keeps the following glossary for reference.

### Epidemiological model acronyms

- **SI**: Susceptible-Infectious.
- **SIR**: Susceptible-Infectious-Recovered.
- **SIR-V**: Susceptible-Infectious-Recovered with vital dynamics.
- **SIRS**: Susceptible-Infectious-Recovered-Susceptible.
- **SEIR**: Susceptible-Exposed-Infectious-Recovered.
- **SEIRS**: Susceptible-Exposed-Infectious-Recovered-Susceptible.
- **SIRD**: Susceptible-Infectious-Recovered-Deceased.
- **SEIRD**: Susceptible-Exposed-Infectious-Recovered-Deceased.
- **SIRV**: Susceptible-Infectious-Recovered-Vaccinated.
- **SIRVS**: Susceptible-Infectious-Recovered-Vaccinated-Susceptible.

### Numerical and interface acronyms

- **ODE**: Ordinary differential equation.
- **RHS**: Right-hand side, referring to the right-hand side function that defines
  the model equations.
- **UI**: User interface.
- **AR**: Attack rate.
- **LSODA**: Livermore Solver for Ordinary Differential Equations with automatic
  method switching.
- **RK4**: Classical fourth-order Runge-Kutta method.
- **BDF**: Backward differentiation formula.

### Notes

- Widely familiar acronyms such as **DNA** or **OLS** are not used in the
  package documentation.
- The package name **shiny** is kept in its canonical package form.
