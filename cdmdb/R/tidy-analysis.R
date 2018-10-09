tidy_lm <- function(the_lm){
  tlm <- 
    bind_cols(broom::tidy(the_lm), 
              broom::confint_tidy(the_lm)) %>%
    filter(term != "(Intercept)") %>% 
    select(term, estimate, conf.low, conf.high, p.value)  %>% 
    mutate(r_sq = summary(the_lm)[["r.squared"]]) %>% 
    modify_at(c("estimate", "conf.low", "conf.high"), 
              ~ sprintf("%.1f", .x)) %>% 
    modify_at(c("p.value", "r_sq"), ~ sprintf("%.4f", .x)) %>% 
    modify_at("term", ~ gsub("time_since_exposure", "days_since_tx", .x)) %>% 
    modify_at("term", ~ gsub("drug", "", .x))
  if (any(grepl(":", tlm[["term"]]))) 
    tlm <- filter(tlm, grepl(":", tlm[["term"]]))
  tlm
}

tidy_lmer <- function(mdl) {
  rn_df <- compose(rownames_to_column, data.frame)
  fe <- rn_df(fixef(mdl))
  tlm <- 
    rn_df(confint(mdl)) %>% 
    filter(!grepl("^\\.", rowname)) %>% 
    rename(conf.low = X2.5.., conf.high = X97.5..) %>% 
    left_join(fe) %>% 
    filter(!grepl("Intercept", rowname)) %>% 
    rename(term = rowname)
  tlm[["p.value"]] <- anova(mdl)[["Pr(>F)"]]
  if (any(grepl(":", tlm[["term"]]))) 
    tlm <- filter(tlm, grepl(":", tlm[["term"]]))
  tlm
}


tidy_glm <- function(mdl) {
  bind_cols(
    broom::tidy(mdl), 
    broom::confint_tidy(mdl)
  ) %>% 
    modify_at(c("estimate", "conf.low", "conf.high"),
              ~ sprintf("%.3f", exp(.x))) %>% 
    modify_at("p.value", ~ sprintf("%.5f", .x)) %>% 
    select(term, estimate, conf.low, conf.high, p.value) %>% 
    modify_at("term", ~ gsub("drug", "", .x)) %>% 
    filter(term != "(Intercept)") %>% 
    rename(odds_ratio = estimate)
}

