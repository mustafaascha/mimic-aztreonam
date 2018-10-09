

cdm_visits <- function(cn, persons = NULL, ..., only_first = FALSE, collect = FALSE) {
  if (missing(persons))
    the_ppl <- TRUE
  else 
    the_ppl <- quo(person_id %in% persons)
  
  v_df <- 
    tbl(cn, "visit_occurrence") %>% 
      filter(UQS(enquos(...))) %>% 
      filter(!!the_ppl) %>% 
      collector(collect) %>% 
      first_time(visit_start_datetime, for_real = only_first)
    
  new_cdm(v_df, subclass = "visit_occurrence")
}

