

filter_measurements <- function(msr_df, the_measures) {
  msr_df[["time_since_exposure"]] <- 
    difftime(msr_df$measurement_date, msr_df$drug_start, units = "days")
  msr_df[["time_since_discont"]] <- 
    msr_df[["time_since_exposure"]] - msr_df[["time_on_drug"]]
  
  filter(msr_df, 
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
}

remove_double_measurements <- function(msr_df) {
  nstd <- 
    msr_df %>% 
      group_by(person_id, md, key) %>% 
      tidyr::nest()
  nstd[["data"]] <- map(nstd[["data"]], function(maybe_dupe) maybe_dupe[1,])
  unnest(nstd)
}

filter_e_fn <- function(exposed) {
  function(z) filter(z, person_id %in% exposed$person_id)
}

one_measurement_per_day <- function(a_df) {
  if (nrow(a_df) == 2) {
    stopifnot(length(unique(a_df[["usv"]])) == 1)
    data.frame(van = mean(a_df[["van"]], na.rm = TRUE), 
               usv = a_df[["usv"]][[1]], 
               stringsAsFactors = FALSE)
  } else {
    a_df
  }
}

munge_measurements <- function(msr_df, pull_these, ppl) {
  left_join(msr_df, pull_these) %>% 
    rm_missing_cols() %>% 
    select(person_id,
           md = measurement_datetime, 
           van = value_as_number, 
           usv = unit_source_value, 
           key = Name) %>% 
    filter(grepl("serum|plasma", key)) %>% 
    filter(person_id %in% ppl$person_id) %>% 
    modify_at("key", ~ gsub("\\ ", "_", gsub("\\ serum.*", "", tolower(.x)))) %>% 
    unite(value, van, usv) %>% 
    remove_double_measurements() %>% 
    spread(key, value) %>% 
    rename(measurement_date = md) 
}

not_munge_measurements <- function(msr_df, the_measures, pull_these, ppl) {
  left_join(msr_df, pull_these) %>% 
    rm_missing_cols() %>% 
    select(person_id,
           md = measurement_datetime, 
           van = value_as_number, 
           usv = unit_source_value, 
           key = Name) %>% 
    filter(grepl("serum|plasma", key)) %>% 
    filter(!(person_id %in% ppl$person_id)) %>% 
    modify_at("key", ~ gsub("\\ ", "_", gsub("\\ serum.*", "", tolower(.x)))) %>% 
    unite(value, van, usv) %>% 
    remove_double_measurements() %>% 
    spread(key, value) %>% 
    rename(measurement_date = md) %>% 
    reduce(the_measures, 
           function(msr_df, msr) {
             separate(msr_df, 
                      col = msr, 
                      into = c(msr, "rm"), 
                      sep = "_")
           },
           .init = .) %>% 
    select(-rm)
}

join_measurements <- function(msr_df1, msr_df2) {
  vars_in_both_dfs <- intersect(names(msr_df1), names(msr_df2))
  map_dfr(list(msr_df1, msr_df2), 
          function(a_df) select(a_df, one_of(vars_in_both_dfs))
          )
}
