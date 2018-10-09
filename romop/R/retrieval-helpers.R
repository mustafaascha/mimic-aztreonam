

add_class <- function(x, cls) {
  if (!(cls %in% class(x)))
    class(x) <- c(cls, class(x))
  x
}

collector <- function(sql_tbl, for_real) {
  if (for_real)
    dplyr::collect(sql_tbl)
  else
    sql_tbl
}

which_ppl <- function(some_ppl) {
  if (some_ppl %in% c("everyone", "anyone"))
    return(TRUE)
  else
    return(quo(person_id %in% the_ppl[["person_id"]]))
}


add_names <- function(the_tbl, nms_df, new_nm, for_real = TRUE) {
  if (is(nms_df, "sanitized")) {
  } else {
    nms_df <- sanitize_concepts(nms_df, new_nm)
  }
  if (for_real)  {
    stopifnot(any(grepl("[nN]ame", names(nms_df))))
    old_nm <- grep("[nN]ame", names(nms_df), value = TRUE)
    left_join(the_tbl, nms_df) %>% 
    rename(!!new_nm := !!old_nm)
  } else
    the_tbl
}

first_time <- function(the_tbl, var_to_first, for_real = TRUE, ...) {
  var_to_first <- enquo(var_to_first)
  if (for_real) {
    the_tbl %>% 
      group_by(person_id, ...) %>% 
      filter(!!var_to_first == min(!!var_to_first, na.rm = TRUE)) %>% 
      ungroup()
  } else {
    the_tbl
  }
}

duration_of <- function(the_tbl, which_datetime, for_real = TRUE) {
  stopifnot(is.character(which_datetime))
  paster <- function(z) sym(paste(which_datetime, z, sep = "_"))
  beginning <- paster("start_datetime")
  the_end   <- paster("end_datetime")
  new_nm    <- paster("duration")
  #browser()
  if (for_real) {
    the_tbl %>% 
      group_by(person_id) %>% 
      mutate(!!new_nm := 
               difftime(!!the_end, !!beginning, units = "days")) %>% 
      ungroup()
  } else {
    the_tbl
  }
}

get_icd_concept_ids <- function(icds) {
  
  icds <- sanitize_concepts(icds, "", named = FALSE)
  
  relevant_vocabs <- 
    tbl(omop_cn, "vocabulary") %>% 
    filter(grepl("ICD", vocabulary_id)) %>% 
    collect() %>% 
    select(vocabulary_id) %>% 
    unlist()
  
  relevant_concepts <- 
    tbl(omop_cn, "concept") %>% 
    filter(vocabulary_id %in% relevant_vocabs) %>% 
    filter(concept_code %in% icds$code) %>% 
    collect()
  
  relevant_crs <-
    tbl(omop_cn, "concept_relationship") %>% 
    filter(concept_id_1 != concept_id_2) %>% 
    filter(relationship_id == "Maps to" & 
             concept_id_1 %in% relevant_concepts$concept_id) %>% 
    select(concept_id = concept_id_1, concept_id_2) %>% 
    collect()
  
  relevant_concepts <- 
    left_join(relevant_concepts, relevant_crs, by = "concept_id") %>% 
    select(concept_id, concept_name, concept_code, concept_id_2)
  
  top <- 
    relevant_concepts %>% 
    select(-concept_id_2)
  bottom <- 
    relevant_concepts %>% 
    select(-concept_id) %>% 
    rename(concept_id = concept_id_2)
  
  bind_rows(top, bottom)
}




