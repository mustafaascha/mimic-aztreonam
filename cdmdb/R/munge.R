

munge_conditions <- function(conditions) {
  conditions %>% 
    spread(condition_nm, condition_nm, fill = NA) %>% 
    select(-starts_with("condition")) %>% 
    select(-starts_with("visit_")) %>% 
    select(-stop_reason, -provider_id) %>% 
    group_by(person_id) %>% 
    summarise_all(function(x) {
      if (all(is.na(x)))
        "none"
      else
        unique(x)[!is.na(unique(x))]
    })
}

