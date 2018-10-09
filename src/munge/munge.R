{
  library(RPostgreSQL)
  library(DBI)
  library(dbplyr)
  library(tidyverse)
  library(lubridate)
  library(lmerTest)
  library(zeallot)
  devtools::load_all("cdmdb")
  devtools::load_all("romop")
  source("src/verbose-objects.R")
}

c(omop_cn, mimic_cn, paper_products, pull_these, 
  the_measures, criteria, conditions, indications) %<-% 
  config()

pull_these$d$Name <- fix_drug_names(pull_these$d$Name)

drug_exposed  <- 
  who_took(omop_cn, 
           these_drugs = pull_these$d, 
           only_first = TRUE, 
           add_duration = TRUE, 
           named = TRUE, 
           collect = TRUE) %>% 
  select(
    person_id, 
    drug,
    drug_exposure_start_datetime, 
    drug_exposure_end_datetime, 
    time_on_drug = drug_exposure_duration, 
    quantity, 
    quantity_source_value
  ) 

persons_table <- 
  cdm_persons(omop_cn, drug_exposed$person_id) %>% 
  select(person_id, gender_concept_id, 
         birth_datetime, race_concept_id) 

visits_table <- 
  cdm_visits(omop_cn, drug_exposed$person_id, only_first = TRUE)

the_ppl <- 
  left_join(persons_table, visits_table) %>% 
  collect() %>% 
  right_join(drug_exposed) %>% 
  mutate(visit_los = difftime(visit_end_datetime, 
                              visit_start_datetime, units = "days")
         ) %>% 
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
         time_on_drug)

measurements <- 
  tbl(omop_cn, "measurement") %>% 
    filter(measurement_concept_id %in% pull_these$m$measurement_concept_id) %>% 
    filter(person_id %in% drug_exposed$person_id) %>% 
    collect()

measurements <- munge_measurements(measurements, pull_these$m, drug_exposed) 

#join w drug start, get time since exposure, filter < 3 > 7
measurements <- 
  left_join(measurements, 
            select(the_ppl, 
                   person_id, drug_start, time_on_drug
            )
  )

criteria[["measured_outside_window"]] <- 
  measurements %>% 
  mutate(time_since_exposure = 
           difftime(measurement_date, drug_start, units = "days")) %>% 
  mutate(time_since_discont = time_since_exposure - time_on_drug) %>% 
  filter(time_since_exposure < 3 | time_since_discont <= 7)

measurements <- 
  measurements %>% 
  filter_measurements(the_measures)

the_ppl <- left_join(the_ppl, measurements)

#treatment cohort construction===========================
criteria[["not_prescribed_long_enough"]] <- 
  length(which(the_ppl$time_on_drug < 3))

selected_pop <- filter(the_ppl, time_on_drug >= 3)

selected_pop[["new_data"]] <- map(selected_pop[["data"]], summarise_outcomes)

#========================================================

had_conditions <- 
  who_had(cn = omop_cn, 
          these_conditions = conditions, 
          person_id %in% selected_pop$person_id, 
          only_first = FALSE, 
          named = TRUE, 
          collect = TRUE
  )  %>% 
  munge_conditions()
  
selected_pop <- left_join(selected_pop, had_conditions)

indications <- 
  get_icd_concept_ids(indications) %>% 
  left_join(select(indications, 
                   indication, 
                   concept_code = code), 
            by = "concept_code"
            ) %>% 
  select(concept_id, name = indication)

had_indication <- 
  who_had(cn = omop_cn, 
          these_conditions = indications, 
          person_id %in% selected_pop$person_id, 
          only_first = FALSE, 
          named = TRUE, 
          collect = TRUE
  ) %>% 
  distinct(person_id, condition_nm, .keep_all = TRUE) %>% 
  munge_conditions() %>% 
  (function(a_df) {
    names(a_df) <- gsub("[^a-zA-Z]", "_", names(a_df))
    names(a_df) <- gsub("___", "_", names(a_df))
    a_df
  })

selected_pop <- left_join(selected_pop, had_indication)

analytic_pop <- 
  selected_pop %>% 
  select(-data) %>% 
  unnest()

lookup_demos <- 
    tbl(omop_cn, "concept") %>% 
    filter(vocabulary_id %in% c("Race", "Gender")) %>% 
    select(concept_id, concept_name, vocabulary_id) %>% 
    collect() %>% 
    group_by(vocabulary_id) %>% 
    tidyr::nest()

lookup_demos[["data"]] <- 
  map2(lookup_demos[["data"]], c("gender_cid", "race_cid"), 
       function(dta, nm) {
         names(dta) <- 
           c(nm, paste(gsub("_cid", "", nm), "", sep = ""))
         dta
       })

analytic_pop <- 
  reduce(lookup_demos$data, 
         left_join, 
         .init = analytic_pop
  ) %>% 
  rename(sex = gender) %>% 
  modify_at("race", ~ fct_lump(.x, prop = 0.05)) %>% 
  modify_at(c("sex", "admit_type"), str_to_title) %>% 
  modify_at(c("visit_los", "time_on_drug"), as.numeric) %>% 
  modify_at("admit_type", ~ fct_lump(str_to_title(.x), n = 4)) %>%
  modify_at(c(indication_vars, condition_vars), 
            ~ relevel(factor(ifelse(.x == "none", "none", "yes")), 
                      ref = "none")) %>% 
  modify_at(grep("elevated", names(.), value = TRUE), 
            ~ relevel(factor(.x), ref = "not elevated"))


analytic_pop[["dili01"]] <- as.numeric(analytic_pop[["dili"]] == "DILI")
