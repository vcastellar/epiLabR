## ============================================================================
## Epidemic model simulator (ODE-based)
## Wrapper script
## ============================================================================

load_epilabr_for_app <- function() {
  if (requireNamespace("epiLabR", quietly = TRUE)) {
    suppressPackageStartupMessages(library(epiLabR))
    return(invisible(TRUE))
  }

  this_file <- NULL
  for (i in rev(seq_len(sys.nframe()))) {
    candidate <- sys.frame(i)$ofile
    if (!is.null(candidate)) {
      this_file <- normalizePath(candidate, winslash = "/", mustWork = FALSE)
      break
    }
  }

  repo_root <- NULL
  if (!is.null(this_file)) {
    repo_root <- normalizePath(
      file.path(dirname(this_file), "..", "..", ".."),
      winslash = "/",
      mustWork = FALSE
    )
  }

  desc_path <- if (!is.null(repo_root)) {
    file.path(repo_root, "DESCRIPTION")
  } else {
    NULL
  }

  if (!is.null(desc_path) && file.exists(desc_path) &&
      requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(repo_root, quiet = TRUE, export_all = FALSE)
    return(invisible(TRUE))
  }

  stop(
    paste(
      "epiLabR is not installed.",
      "Install the package from the repository root with",
      "remotes::install_local('.') or devtools::install_local('.').",
      "For a development checkout, run pkgload::load_all('.') before",
      "launching the Shiny app."
    ),
    call. = FALSE
  )
}

load_epilabr_for_app()

## Launch the Shiny app with built-in models
run_epi_app()
