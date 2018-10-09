

ensure_integer_id <- function(the_df, id_var) {
  if (!is.integer(the_df[[id_var]]))
    the_df[[id_var]] <- as.integer(the_df[[id_var]])
  the_df
}

sanitize_concepts <- function(the_concepts, the_domain, named = TRUE) {
  #browser()  
  if (is(the_concepts, "sanitized")) return(the_concepts)
  
  stopifnot(length(the_domain) == 1)
  tbl_concept_id_nm <- paste(the_domain, "concept_id", sep = "_")
  tc <- the_concepts
  
  # fix names
  if (named & !(any(grepl("[nN]ame", names(tc))))) {
    stop("To add concept names, the concepts dataframe must 
         contain a column with the word 'name' as part of its name")
  } else if (named & sum(grepl("[nN]ame", names(tc))) > 1) {
    stop("There must be only one column with the word 'name' as 
         part of its name")
  } else {
    names(tc)[grep("[nN]ame", names(tc))] <- "Name"
  }
  
  # fix concept_ids
  if (is.character(tc)) {
    stop("Concept IDs must be either an integer vector or a 
         dataframe containing a concept_id column")
    
  } else if (is.integer(tc)) {
    
    tc <- data.frame(concept_id = tc)
    names(tc) <- tbl_concept_id_nm
    
  } else if (is.data.frame(tc)) {
    
    if (tbl_concept_id_nm %in% names(tc)) {
      
    } else if ("concept_id" %in% names(tc)) {
      
      names(tc)[names(tc) %in% "concept_id"] <- tbl_concept_id_nm
      
    } else if (!(any(c("concept_id", "code", tbl_concept_id_nm) %in% names(tc)))) {
      stop(paste("The dataframe containing concept IDs should contain 
                  a column called code, concept_id or", tbl_concept_id_nm))
    } else {
      #stop("Something went wrong!")
    }
  }
  
  tryCatch(tc <- ensure_integer_id(tc, tbl_concept_id_nm), 
           error = function(e) invisible(e))
  
  structure(tc, class = c("sanitized", class(tc)))
}



