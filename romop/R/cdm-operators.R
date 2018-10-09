
cdm_add <- function(lhs, rhs) {
  stopifnot(sum("cdm" %in% c(class(lhs), class(rhs))) == 2)
  
  if (sum(("cdm_persons" %in% c(class(lhs), class(rhs)))) == 2) {
    bind_rows(lhs, rhs)
  } else if ("cdm_persons" %in% class(lhs)) {
    left_join(lhs, rhs)
  }
}

`+.cdm` <- function(lhs, rhs) {
  cdm_add(lhs = lhs, rhs = rhs)
}
  
  