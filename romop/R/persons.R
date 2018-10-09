

cdm_persons <- function(cn, persons = NULL, ..., collect = FALSE) {
  if (missing(persons)) 
    the_ppl <- TRUE
  else 
    the_ppl <- quo(person_id %in% persons)
  
  p_df <- 
    tbl(cn, "person") %>% 
      #keep only ppl who were exposed
      filter(!!the_ppl) %>%
      collector(collect)
  
  new_cdm(p_df, subclass = "cdm_person")

}
