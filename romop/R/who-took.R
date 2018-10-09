

#' Interrogative returning who took a drug, i.e. had a matching drug concept ids in a drug_exposure table
#' 
#' @param these_drugs Must be a dataframe or list containing a column called "drug_concept_id"
#' @param ... Any other conditions to use for filtering the "drug_exposure" table. This is most useful to filter by person_ids.   
#' @param only_first Whether to return only the first observation of a 'drug_concept_id' contained in `these_drugs`. Defaults to FALSE. 
#' @param add_duration Whether to calculate exposure duration for drugs of interest. Defaults to FALSE. 
#' @param named If there is a 'name' column in `these_drugs`, whether to join `these_drugs` with the exposed patients. Defaults to FALSE. 
#' @param collect Whether to delay table collection from the server. Defaults to TRUE. 
#' 
#' @return A `drug_exposure` table, with an added 'name' column if `named`
#' 
who_took <- function(
  cn, 
  these_drugs, 
  ..., 
  only_first = FALSE, 
  add_duration = FALSE, 
  named = FALSE, 
  collect = TRUE) {
  
  message(
    paste(
          "Parameters are: ", 
          "only_first @", only_first, 
          "add_duration @", add_duration, 
          "named @", named, 
          "collect @", collect 
          )
  )
  
  these_drugs <- sanitize_concepts(these_drugs, named = named, "drug")
  assert_concordant_arguments(only_first, add_duration, named, collect)
  
  took_drug <- 
    tbl(cn, "drug_exposure") %>%
      filter(!!!enquos(...)) %>% 
      filter(drug_concept_id %in% these_drugs[["drug_concept_id"]]) %>% 
      collector(collect)  %>%
      new_cdm(subclass = "drug_exposed")
  
  if (!collect) took_drug
  else 
    took_drug %>% 
      first_time(drug_exposure_start_datetime, for_real = only_first
                 #, drug_concept_id
                 ) %>% 
      duration_of("drug_exposure",             for_real = add_duration) %>% 
      add_names(these_drugs, "drug",           for_real = named)
}

