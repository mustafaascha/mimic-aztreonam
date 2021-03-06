# See: https://github.com/tidyverse/googledrive/blob/master/R/aaa.R

#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom purrr %>%
#' @usage lhs \%>\% rhs
NULL

if (getRversion() >= "2.15.1") utils::globalVariables(c(":="))

.onLoad <- function(libname, pkgname) {
  #set_auth_active(TRUE)
  #set_api_key(.state[["tidyverse_api_key"]])
  #set_oauth_app(.state[["tidyverse_app"]])
  
  if (requireNamespace("dplyr", quietly = TRUE)) {
    register_s3_method("dplyr", "arrange", "cdm")
    register_s3_method("dplyr", "filter", "cdm")
    register_s3_method("dplyr", "mutate", "cdm")
    register_s3_method("dplyr", "rename", "cdm")
    register_s3_method("dplyr", "select", "cdm")
    register_s3_method("dplyr", "slice", "cdm")
  }
  
  invisible()
}

# ## This function is never called
# ## Exists to suppress this NOTE:
# ## "Namespaces in Imports field not imported from:"
# ## https://github.com/opencpu/opencpu/blob/10469ee3ddde0d0dca85bd96d2873869d1a64cd6/R/utils.R#L156-L165
# stub <- function() {
#   ## I have to use curl directly somewhere, if I import it.
#   ## I have to import it if I want to state a minimum version.
#   curl::curl_version()
# }
