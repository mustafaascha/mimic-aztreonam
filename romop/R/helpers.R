
fix_drug_names <- function(drgs_vctr) {
  drgs_vctr <- tolower(drgs_vctr)
  check_drug <- function(vct, drg) {
    ifelse(grepl(drg, vct), drg, vct)
  }
  reduce(
    c("ceftazidime", "aztreonam", "ceftriaxone", "cefepime", "cefazolin"),
    check_drug,
    .init = drgs_vctr
  )
}

global_con <- function() {
  connection <- 
    ls(envir = .GlobalEnv)[
      map(ls(envir = .GlobalEnv), compose(class, eval, sym)) %>% 
        map_lgl(function(z) any(grepl("onnection", z) & grepl("[sS][qQ][lL]", z)))
      ]
  if (length(connection) > 1 & any(grepl("omop", connection))) {
    connection <- connection[grepl("omop", connection)]
  } else if (length(connection) == 1 & grepl("omop", connection)) {
  } else {
    stop("Ambiguous connection: There must either be a single SQL connection 
         in the .GlobalEnv, or one of the SQL connections must have the word 
         'omop' as part of its name.")
  }
  message(paste("Using connection:", connection))
  get(connection, envir = .GlobalEnv)
}
