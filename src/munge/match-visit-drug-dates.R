{
  library(RPostgreSQL)
  library(DBI)
  library(dbplyr)
  library(tidyverse)
  library(lubridate)
  library(lmerTest)
  library(zeallot)
}

devtools::load_all("cdmdb")
devtools::load_all("romop")

c(omop_cn, mimic_cn, paper_products, pull_these, 
  the_measures, criteria, conditions) %<-% 
  config()


pull_these$d$Name <- fix_drug_names(pull_these$d$Name)

# rename vs select
# only_first = FALSE
drug_exposed  <- 
  who_took(omop_cn, 
           these_drugs = pull_these$d, 
           only_first = FALSE, 
           add_duration = TRUE, 
           named = TRUE, 
           collect = TRUE)

persons_table <- 
  cdm_persons(omop_cn, drug_exposed$person_id) %>% 
  select(person_id, gender_concept_id, 
         birth_datetime, race_concept_id) 

visits_table <- 
  cdm_visits(omop_cn, 
             drug_exposed$person_id, 
             only_first = TRUE)

the_ppl <- 
  left_join(persons_table, visits_table, by = "person_id") %>% 
  collect() %>% 
  right_join(drug_exposed, by = "person_id") %>% 
  mutate(visit_los = difftime(visit_end_datetime, visit_start_datetime, units = "days")) %>% 
  rename( 
         gender_cid = gender_concept_id, 
         dob = birth_datetime, 
         race_cid = race_concept_id, 
         visit_type = visit_source_value,
         admit_type = admitting_source_value,
         discharge_to = discharge_to_source_value, 
         drug_start = drug_exposure_start_datetime
         )

if (exists("for_real")) {
  measurements <- 
    tbl(omop_cn, "measurement") %>% 
    filter(measurement_concept_id %in% pull_these$m$measurement_concept_id) %>% 
    filter(person_id %in% drug_exposed$person_id) %>% 
    collect()
  write_csv(measurements, "cache/measurements.csv.gz")
} else {
  measurements <- read_csv("cache/measurements.csv.gz")
}

measurements <- munge_measurements(measurements, pull_these$m, drug_exposed) 

#join w drug start, get time since exposure, filter < 3 > 7
measurements <- 
  left_join(measurements, 
            select(the_ppl, 
                   person_id, drug_start, time_on_drug = drug_exposure_duration
            )
  ) %>% 
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
          only_first = TRUE, 
          named = TRUE, 
          collect = TRUE
  )  %>% 
  spread(condition_nm, condition_nm, fill = NA) %>% 
  select(-starts_with("condition")) %>% 
  select(-starts_with("visit_")) %>% 
  select(-stop_reason, -provider_id)

had_conditions %>% 
  group_by(person_id) %>% 
  summarise_all(function(x) {
    if (all(is.na(x)))
      NA
    else
      unique(x)
  })


left_join(selected_pop, had_conditions)








analytic_pop <- 
  selected_pop %>% 
  select(-data) %>% 
  unnest()
