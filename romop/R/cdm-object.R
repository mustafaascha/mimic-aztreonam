#' cdm object
#' 
#' @description A `cdm` object contains either a tibble or a 
#'    database connection that refers to a table. This allows 
#'    for opinionated constructors and selectors whose behavior
#'    depends on the attributes of the `cdm` object. 
#'
#' @description 
#' @seealso [cdm_visit_occurrences, cdm_persons, who_took, who_had]
#' 
#' 
NULL

new_cdm <- function(cdm_data, ..., subclass = NULL) {
  stopifnot(
    is.data.frame(cdm_data) | 
      tibble::is_tibble(cdm_data) | 
      is.tbl(cdm_data)
    )
  
  structure(
    cdm_data,
    ...,
    class = c(subclass, "cdm", class(cdm_data))
  )
}

as_cdm <- function(cdm_thing, ...) {
  UseMethod("as_cdm", cdm_thing)
}

as_cdm.cdm <- function(cdm_thing) {
  cdm_thing
}

as_cdm.default <- function(cdm_thing) {
  cdm_thing
}


  
  