library(RPostgreSQL)
library(DBI)
library(dbplyr)
library(tidyverse)
library(lubridate)
library(lmerTest)
library(zeallot)

devtools::load_all("cdmdb")

c(cn, 
   mimic_cn, 
   paper_products, 
   pull_these, 
   the_measures, 
   criteria, 
   conditions, 
   indications) %<-% 
  config()

drug_exposed <-
  tbl(cn, "drug_exposure") %>%
  filter(drug_concept_id %in% pull_these$d$drug_concept_id) %>% 
  group_by(person_id) %>% 
  collect() 

#we're only considering the first exposure
drug_exposed <- 
  filter(drug_exposed, 
         drug_exposure_start_datetime == min(drug_exposure_start_datetime, 
                                             na.rm = TRUE))
drug_exposed <- 
  compose(make_exposure_times, rm_missing_cols)(
    left_join(drug_exposed, pull_these$d)
  )

the_ppl <- 
  tbl(cn, "person") %>% 
  filter(person_id %in% drug_exposed$person_id) %>% 
  select(person_id, gender_concept_id, 
         birth_datetime, race_concept_id) 

first_visit <- 
  tbl(cn, "visit_occurrence") %>% 
  filter(person_id %in% drug_exposed$person_id) %>% 
  group_by(person_id) %>% 
  filter(visit_start_datetime == min(visit_start_datetime, na.rm = TRUE)) 

the_ppl <- 
  left_join(the_ppl, first_visit) %>% 
  collect() %>% 
  right_join(drug_exposed) %>% 
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
         time_on_drug)
rm(first_visit, drug_exposed)

if (exists("for_real")) {
  measurements <- 
    tbl(cn, "measurement") %>% 
    filter(measurement_concept_id %in% pull_these$m$measurement_concept_id) %>% 
    filter_exposed() %>% 
    collect()
  write_csv(measurements, "cache/measurements.csv.gz")
} else {
  measurements <- read_csv("cache/measurements.csv.gz")
}

# for_ranges <- 
#   measurements_ %>%
#     select(measurement_concept_id, range_low, range_high) %>% 
#     left_join(pull_these$m)

measurements <-  
  measurements %>% 
  munge_measurements(pull_these$m) %>% 
  remove_double_measurements() %>% 
  spread(key, value) %>% 
  rename(measurement_date = md) #%>%
  #make_outcomes(the_measures = the_measures) %>% 
  #group_by(person_id) %>% 
  #tidyr::nest()

#join w drug start, get time since exposure, filter < 3 > 7

measurements <- 
  left_join(measurements, select(the_ppl, person_id, drug_start, time_on_drug))

measurements[["time_since_exposure"]] <- 
  difftime(measurements$measurement_date, measurements$drug_start, units = "days")
measurements[["time_since_discont"]] <- 
  measurements[["time_since_exposure"]] - measurements[["time_on_drug"]]

measurements <- 
  filter(measurements, 
         time_since_exposure >= 3 & time_since_discont <= 7) %>% 
    select(-starts_with("time"), -measurement_date, -drug_start) %>% 
    reduce(the_measures, 
           function(msr_df, msr) {
             separate(msr_df, 
                      col = msr, 
                      into = c(msr, "rm"), 
                      sep = "_")
           },
           .init = .) %>% 
    select(-rm) %>% 
    group_by(person_id) %>% 
    tidyr::nest()

ppl_nms <- names(the_ppl)
the_ppl <- 
  left_join(the_ppl, measurements) %>% 
  modify_at("drug", ~ gsub("\\ MG.*", "", .x)) %>% 
  separate("drug", into = c("drug", "drug_strength"), sep = "\\ ")

#===========================

criteria[["not_prescribed_long_enough"]] <- length(which(the_ppl$time_on_drug < 3))
the_ppl <- filter(the_ppl, time_on_drug >= 3)

the_ppl[["data"]] <- 
  map(the_ppl[["data"]], compose(summarise_outcomes, make_outcomes))

the_ppl <- unnest(the_ppl)

paper_products[["chisq"]] <- Epi::twoby2(the_ppl$drug, the_ppl$dili, print = FALSE)














write_rds(paper_products, "paper_products.rds")


#check
#the_ppl[["data"]][193]

#     difftime(the_ppl$measurement_date, the_ppl$drug_start, units = "days")
# 
# criteria[["measure_fewer_than_three_days_after_ex"]] <- length(which(the_ppl$days_expsr_msr < 3))
# the_ppl <- filter(the_ppl, days_expsr_msr >= 3)
# 
# criteria[["measure_more_than_seven_days_after_ex"]] <- 
#   length(which((the_ppl$days_expsr_msr - the_ppl$time_on_drug) > 7))
# the_ppl <- filter(the_ppl, (days_expsr_msr - time_on_drug) > 7)
# 
# xthe_ppl <- 
#   the_ppl %>% 
#     group_by_(ppl_nms)




# paper_products[["not_3_after_start"]] <- 
#   filter(measurements, time_since_start < 3) %>% 
#   group_by(drug) %>% summarise(count = n())
# paper_products[["not_7_after_end"]] <- 
#   filter(measurements, time_since_end > 7) %>% 
#   group_by(drug) %>% summarise(count = n())
# 
# paper_products[["lme_uni_days_time"]] <- 
#   map(set_names(the_measures, the_measures), lmer_this(reg_df)) %>% 
#   flatten() %>% 
#   imap_dfr(function(z, nm) mutate(tidy_lmer(z), mdl = nm))
# 
# plot_df <- 
#   reg_df %>%
#   gather(test, value, 
#          -drug, -person_id, -time_since_exposure, 
#          -drug_exposure_end_datetime, -time_since_end
#          ) %>%
#   group_by(test) %>% 
#   tidyr::nest() 
# 
# paper_products[["plts"]] <- map2(plot_df$test, plot_df$data, plot_msr)
# paper_products[["plts_faceted"]] <- faceted_plot(reg_df)
# paper_products[["wilcox_tests"]] <- 
#   wilcox_measures(reg_df, the_measures) %>% 
#   left_join(
#     rownames_to_column(data.frame(missingness(reg_df)), var = "Outcome") %>% 
#       rename(Missing = missingness.reg_df.) %>% 
#       filter(!grepl("levated|_u$", Outcome) & !(Missing == 0)) %>% 
#       modify_at("Outcome", ~ str_to_title(gsub("_", " ", .x))) %>% 
#       modify_at("Missing", ~ paste(sprintf("%.1f", 100 * .x), "%")) 
#   )
#   
# write_csv(table_elevated(reg_df), "counts-dili-by-lft.csv")
# write_csv(table_dili(reg_df), "counts-dili-overall.csv")
# paper_products[["dili_table_by_lft"]] <- table_elevated(reg_df)
# paper_products[["dili_table_overall"]] <- table_dili(reg_df)
# paper_products[["chisq"]] <- Epi::twoby2(reg_df$drug, reg_df$dili, print = FALSE)
#
# paper_products[["missingness"]] <- 
#   rownames_to_column(data.frame(missingness(reg_df)), var = "Outcome") %>% 
#   rename(Missing = missingness.reg_df.) %>% 
#   filter(!grepl("levated|_u$", Outcome) & !(Missing == 0)) %>% 
#   modify_at("Outcome", ~ str_to_title(gsub("_", " ", .x))) %>% 
#   modify_at("Missing", ~ paste(sprintf("%.1f", 100 * .x), "%")) 

# message(paste("The p-value for a test of difference in DILI occurrence between each drug is", 
#               sprintf("%.5f", paper_products$chisq[["p.value"]][[1]]))
#         )
# map(set_names(names(measurements)[-c(1:2)], names(measurements)[-c(1:2)]),
#     ~ lm(reformulate("time_since_exposure", .x), 
#          data = measurements %>% 
#            modify_at(.x, function(z) gsub("_.*", "", z)))) %>% 
#   imap_dfr(~ bind_cols(broom::tidy(.x), broom::confint_tidy(.x)) %>% 
#              mutate(outcome = .y)) %>% 
#   filter(term != "(Intercept)") %>% 
#   select(outcome, estimate, conf.low, conf.high, p.value)

# ppl_m <- left_join(the_ppl, measurements)
# names(pvdm) <- gsub("drug_", "", names(pvdm))

#when I search for "visit" or "race", I get really weird results...
#keys <- map(c(g = "gender", r = "race", v = "visit"), ~ search_vocab(cn, .x))
# lfts <- 
#   map_dfr(c(quote(first), quote(last)), 
#       function(z) {
#           filter(measurements, md == !!z) %>% 
#           mutate(time_since = difftime(last, first, units = "days"),
#                  timing = as.character(z))
#         }) %>% 
#   mutate(key = paste(timing, gsub("\\ .*", "", key), sep = "_"), 
#         value = paste(van, usv, round(time_since), sep = "_")) %>% 
#   select(person_id, key, value) %>% 
#   spread(key, value)
# 
# lfts <- 
#   reduce(names(lfts)[-1], 
#          function(df, s) 
#            separate(df, s, c(s, paste(s, "u", sep = "_"), "timediff"), "_"), 
#          .init = lfts)
# 
# filter for people whose first and last measurements were a few days apart? 


# tbl_nms <- dbListTables(cn)
# introspection <- filter(introspect_db(cn), fields != "")


# observations <-
#   tbl(cn, "observation") %>%
#   filter(person_id %in% drug_exposed$person_id) %>%
#   collect() 
# 
# observations <- left_join(observations, pull_these$o)
