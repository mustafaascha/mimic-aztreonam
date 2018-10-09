rm_missing_cols <- function(df) {
  select(df, which(map_lgl(missingness(df), ~ .x != 1)))
}

read_all_sheets <- function(xl_location) {
  map(readxl::excel_sheets(xl_location), function(the_sheet) {
    mutate(readxl::read_xlsx(xl_location, sheet = the_sheet, col_types = "text"), 
           condition = gsub("[^a-z]", "_", tolower(the_sheet)))
  })
}

merge_dpv <- function(drug_ppl, persons, visits) {
  q_separate <- function(...) quietly(separate)(...)[["result"]]
  left_join(persons, visits) %>% 
    collect() %>% 
    right_join(drug_ppl) %>% 
    mutate(visit_los = difftime(visit_end_datetime, visit_start_datetime, units = "days")) %>% 
    select(person_id, 
           gender_cid = gender_concept_id, 
           dob = birth_datetime, 
           race_cid = race_concept_id, 
           visit_type = visit_source_value,
           visit_los,
           admit_type = admitting_source_value,
           discharge_to = discharge_to_source_value, 
           quantity,
           drug, 
           drug_start = drug_exposure_start_datetime,
           time_on_drug) %>% 
    q_separate(col = "drug", 
               into = c("drug", "drug_strength"), 
               sep = "\\ ")
}





