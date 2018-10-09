missingness <- function(a_df){
  map_dbl(a_df, ~ compose(length, which, is.na)(.x) / length(.x))
}

lmer_this <- function(a_df) {
  stopifnot(all(c("time_since_exposure", "drug") %in% names(a_df)))
  function(an_outcome) {
    map(set_names(c("time_since_exposure", "drug", "drug*time_since_exposure"),
                  paste(c("tse", "d", "dtse"), an_outcome, sep = "_")),
        function(prd) {
          the_frm <- reformulate(paste(prd, "+ (1 | person_id)"), an_outcome)
          do.call(lmer, list(formula = the_frm, data = a_df))
        })
  }
}

wilcox_measures <- function(a_df, the_msrs) {
  a_df <- 
    a_df %>% 
      select(person_id, drug, one_of(the_msrs)) %>% 
      group_by(person_id, drug) %>%
      summarise_all(~ median(.x, na.rm = TRUE)) %>% 
      ungroup()
  tidy_wilcox_w_this <- function(a_df) {
    function(otcm) {
      broom::tidy(wilcox.test(as.formula(paste(otcm, "~ drug")), 
                              data = a_df)) %>% 
        mutate(outcome = otcm) %>% 
        select(outcome, p.value) 
    }
  }
  
  a_df <-
    a_df %>% 
    select(person_id, drug, one_of(the_msrs)) %>% 
    group_by(person_id, drug) %>%
    summarise_all(~ median(.x, na.rm = TRUE)) %>% 
    ungroup() %>% 
    modify_at("drug", ~ paste("median", tolower(.x), sep = "_")) %>% 
    group_by(drug) %>% 
    summarise_all(~ median(.x, na.rm = TRUE)) %>% 
    gather(outcome, v, -drug) %>% 
    spread(drug, v) %>% 
    right_join(
      map_dfr(set_names(the_measures, the_measures),
              tidy_wilcox_w_this(a_df))
    ) %>% 
    modify_at("outcome", ~ str_to_title(gsub("_", " ", .x)))
  names(a_df) <- str_to_title(gsub("_|\\.", " ", names(a_df)))
  a_df
}

table_elevated <- function(a_df) {
  to_return <- 
    a_df %>% 
      select(starts_with("elevated")) %>% 
      imap_dfr(function(msr, msr_nm) {
        mutate(data.frame(table(reg_df$drug, msr)), 
               criteria = msr_nm)
      }) %>% 
      spread(msr, Freq) %>% 
      mutate(total = elevated + not_elevated, 
             prop = elevated / total) %>% 
      select(drug = Var1, criteria, elevated, prop) %>% 
      mutate(prop = paste(elevated, " (", sprintf("%.1f", 100*prop), 
                          "%)", sep = "")) %>% 
      select(-elevated) %>% 
      spread(criteria, prop)
  names(to_return) <- str_to_title(gsub("_", " ", names(to_return)))
  names(to_return) <- 
    reduce2(c("_", "Altb", 
              "Alp", "Alt"), 
            c(" ", "ALT-Bili", 
              "ALP", "ALT"), 
            function(x, y, z) gsub(y, z, x), 
            .init = names(to_return))
  to_return
}

table_dili <- function(a_df) {
  # to_return <- 
  #   data.frame(table(a_df$drug, a_df$dili)) %>%
  #     spread(Var2, Freq) %>% 
  #     mutate(total = DILI + No_DILI, 
  #            prop = DILI / total) %>% 
  #     rename(drug = Var1)
  # names(to_return) <- str_to_title(gsub("_", " ", names(to_return)))
  # names(to_return) <- gsub("Dili", "DILI", names(to_return))
  # to_return
  group_by(a_df, drug, dili) %>% 
    summarise(freq = n()) %>% 
    spread(dili, freq) %>% 
    mutate(total = DILI + No_DILI) %>% 
    mutate(value = paste(DILI, " (", round(100 * DILI / total), "%)", sep = "")) %>% 
    select(drug, DILI = value, No_DILI, total) %>% 
    arrange(desc(drug))
}

elevated_lft_table <- function(msrs_df) {
  in_df <- 
    c("person_id", 
      "drug", 
      "alanine_aminotransferase", 
      "alkaline_phosphatase", 
      "aspartate_aminotransferase", 
      "total_bilirubin", 
      "elevated_alt", 
      "elevated_alp", 
      "elevated_altb", 
      "dili"
      )
  stopifnot(all(in_df %in% names(msrs_df)))
  outcomes <- c(alt = "elevated_alt", alp = "elevated_alp", altb = "elevated_altb")
  
  imap_dfr(outcomes, 
           function(otcm, nm) {
             table(analytic_pop[[otcm]], 
                   analytic_pop[["drug"]]) %>%
               as.data.frame() %>% 
               mutate(outcome = nm)
           }) %>% 
    spread(Var1, Freq) %>% 
    mutate(total = elevated + `not elevated`) %>% 
    mutate(value = paste(elevated, " (", round(100 * elevated / total), "%)", sep = "")) %>% 
    select(Exposure = Var2, outcome, value) %>% 
    spread(outcome, value) %>% 
    rename(Elevated_ALT = alt, Elevated_ALP = alp, Elevated_ALT_or_Bilirubin = altb)
}


dili_drug_table <- function(a_df) {
  stopifnot(all(c("drug", "dili") %in% names(a_df)))
  a_df %>% 
  group_by(drug, dili) %>% 
    summarise(freq = n()) %>% 
    spread(dili, freq) %>% 
    mutate(total = DILI + No_DILI) %>% 
    mutate(value = paste(DILI, " (", round(100 * DILI / total), "%)", sep = "")) %>% 
    select(drug, DILI = value, No_DILI, total) %>% 
    arrange(desc(drug))
}


fix_rownames <- function(a_table) {
  rownames(a_table) <- stringr::str_to_title(rownames(a_table))
  rownames(a_table) <-
    reduce2(c("Genital_and_Urinary_Tract", 
              "Infection_without_further_specification", 
              "aspartate_aminotransferase", 
              "alanine_aminotransferase",
              "alkaline_phosphatase",
              "Abdominal_and_Gastrointestinal", 
              "\\ \\=\\ elevated",
              "\\ \\=\\ yes",
              "\\ \\=\\ ",
              "_"
              ), 
            c("Genital/Urinary", 
              "Infection_NOS", 
              "AST", 
              "ALT",
              "ALP",
              "Abdom./GI", 
              "",
              "",
              ": ",
              " "
              ), 
            function(x, y, z) gsub(y, z, x), 
            .init = rownames(a_table))
  a_table
}


do_to <- function(a_df, vrs, strt) {
  
  nonnorm <- 
    c("alanine_aminotransferase", "alkaline_phosphatase", 
      "aspartate_aminotransferase", "total_bilirubin", 
      "visit_los")
  
  print(
    CreateTableOne(vars = vrs, 
                   data = a_df, 
                   strata = strt),
    nonnormal = nonnorm,
    printToggle = FALSE
  ) %>% 
    fix_rownames()
}

