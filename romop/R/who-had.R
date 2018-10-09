
#' Interrogative returning who took a drug, i.e. had a matching drug concept ids in a drug_exposure table
#' 
#' @param these_drugs Must be a dataframe or list containing a column called "drug_concept_id"
#' @param ... Any other conditions to use for filtering the "drug_exposure" table. This is most useful to filter by person_ids.   
#' @param only_first Whether to return only the first observation of a 'drug_concept_id' contained in `these_drugs`. Defaults to FALSE. 
#' @param add_duration Whether to calculate exposure duration for drugs of interest. Defaults to FALSE. 
#' @param named If there is a 'name' column in `these_drugs`, whether to join `these_drugs` with the exposed patients. Defaults to FALSE. 
#' @param collect Whether to delay table collection from the server. Defaults to TRUE. 
who_had <- function(
  cn, 
  these_conditions, 
  ..., 
  only_first = FALSE, 
  add_duration = FALSE, 
  named = FALSE, 
  collect = TRUE) {
  
  these_conditions <- sanitize_concepts(these_conditions, named = named, "condition")
  assert_concordant_arguments(only_first, add_duration, named, collect)
  
  had_condition <- 
    tbl(cn, "condition_occurrence") %>%
      filter(!!!enquos(...)) %>% 
      filter(condition_concept_id %in% these_conditions[["condition_concept_id"]]) %>% 
      collector(for_real = collect) %>% 
      new_cdm(subclass = "condition_occurred")
  
  if (!collect) return(new_cdm(had_condition, subclass = "condition_occurred"))
  else 
    had_condition %>% 
      first_time(condition_start_datetime,        for_real = only_first, 
                 condition_concept_id) %>% 
      duration_of("condition",                    for_real = add_duration) %>% 
      add_names(these_conditions, "condition_nm", for_real = named)
}





