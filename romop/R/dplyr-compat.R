## adadpted from
## https://raw.githubusercontent.com/tidyverse/googledrive/master/R/dplyr-compat.R
## found in 
## https://community.rstudio.com/t/functions-that-input-output-tibbles-and-have-a-class-system/2369/4 

## adapted from
## https://github.com/hadley/dtplyr/blob/2308ff25e88bb81fe84f9051e37ddd9d572189ee/R/compat-dplyr-0.6.0.R

## function is called in .onLoad()
register_s3_method <- function(pkg, generic, class, fun = NULL) { # nocov start
  is_string <- rlang::is_string
  stopifnot(is_string(pkg))
  envir <- asNamespace(pkg)

  stopifnot(is_string(generic))
  stopifnot(is_string(class))
  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  }
  stopifnot(is.function(fun))

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = envir)
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = envir)
    }
  )
} # nocov end

## googlesheets does not import any generics from dplyr,
## but if dplyr is loaded and main verbs are used on a cdm
## we want to retain the cdm class if it is proper to do so
##
## therefore these S3 methods are registered manually in .onLoad()
arrange.cdm <- function(.data, ...) {
  NextMethod()
}

filter.cdm <- function(.data, ...) {
  NextMethod()
}

mutate.cdm <- function(.data, ...) {
  NextMethod()
}

rename.cdm <- function(.data, ...) {
  NextMethod()
}

select.cdm <- function(.data, ...) {
  NextMethod()
}

slice.cdm <- function(.data, ...) {
  NextMethod()
}
