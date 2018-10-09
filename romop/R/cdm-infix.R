#' CDM infix combinator
#' 

cdm_infix <- function(lhs, rhs, operator = NULL, subclass = NULL) {
  last_class <- tryCatch(class(lhs), error = function(z) NULL)
  structure(list(lhs = lhs, 
                 rhs = rhs, 
                 operator = operator), 
            class = c(subclass, last_class)
  )
}

eval_cdm_infix <- function(infix_obj, ...) {
  UseMethod("eval_cdm_infix")
}

eval_cdm_infix_base <- function(lhs, rhs, op, ...) {
  filter_expr <- which_ppl(quo_name(lhs))
  op(global_con(), 
     rhs, 
     !!filter_expr, 
     ...)
}

eval_cdm_infix.default <- function(infix_obj, ...) {
  eval_cdm_infix_base(
    infix_obj[["lhs"]], 
    infix_obj[["rhs"]], 
    infix_obj[["operator"]], 
    ...)
}

eval_cdm_infix.cdm_took <- function(infix_obj, ...) {
  eval_cdm_infix_base(
    infix_obj[["lhs"]], 
    infix_obj[["rhs"]], 
    who_took, 
    ...)
}

eval_cdm_infix.cdm_had <- function(infix_obj, ...) {
  eval_cdm_infix_base(
    infix_obj[["lhs"]], 
    infix_obj[["rhs"]], 
    who_had, 
    ...)
}


#' All drug_exposure records where `the_ppl` took `the_drugs`
#' 
#' Use `everyone` or `anyone` in place of `the_ppl` to search all persons
#' 
`%who_took%` <- function(the_ppl, the_drugs) {
  cdm_infix(substitute(the_ppl), 
            the_drugs, 
            subclass = "cdm_took") %>% 
  eval_cdm_infix(collect = FALSE)
}

#' The first drug_exposure record in which `the_ppl` took `the_drugs`
#' 
#' Use `everyone` or `anyone` in place of `the_ppl` to search all persons
#' 
`%first_took%` <- function(the_ppl, the_drugs) {
  cdm_infix(substitute(the_ppl), 
            the_drugs, 
            subclass = "cdm_took") %>% 
  eval_cdm_infix(only_first = TRUE, 
                 collect = TRUE)
}

#' All `condition_occurrence` records in which `the_ppl` had `the_conditions`, 
#' where `the_ppl` are joined with `the_conditions`
#' 
`%who_had%` <- function(the_ppl, the_conditions) {
  cdm_infix(substitute(the_ppl), 
            the_conditions, 
            subclass = "cdm_had") %>% 
    eval_cdm_infix(collect = FALSE)
}

#' The first `condition_occurrence` record in which `the_ppl` had `the_conditions`, 
#' without joins
`%first_had%` <- function(the_ppl, the_conditions) {
  cdm_infix(substitute(the_ppl), 
            the_conditions, 
            subclass = "cdm_had") %>% 
    eval_cdm_infix(only_first = TRUE)
}


