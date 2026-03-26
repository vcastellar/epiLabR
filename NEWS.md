# epiLabR News

## epiLabR (development)

### Breaking changes
- Removed built-in summary metric helpers from the package API to keep the package focused on deterministic ODE simulation workflows.

## epiLabR 0.3.0

### Documentation
- Refined `DESCRIPTION` text to align with CRAN style and remove non-essential wording.
- Added this `NEWS.md` file to track package changes.
- Expanded `README.md` with a dedicated section on numerical limitations and stability considerations.
- Added practical guidance on integration methods (`lsoda`, `rk4`, and other `deSolve` methods) in `README.md`.

### Notes
- `R CMD check --as-cran` completed successfully in the release preparation workflow.
