
make_outcomes <- function(msrs_df, the_measures = NULL) {
  
  if (is.null(msrs_df)) 
    return(
      data.frame(
        alanine_aminotransferase = NA, 
        alkaline_phosphatase = NA, 
        aspartate_aminotransferase = NA, 
        total_bilirubin = NA, 
        elevated_alt = NA, 
        elevated_alp = NA, 
        elevated_altb = NA, 
        dili = NA
      )
    )
  
  if (missing(the_measures)) {
    the_measures <- 
      c("alanine_aminotransferase", 
        "alkaline_phosphatase", 
        "aspartate_aminotransferase", 
        "total_bilirubin")
  }
  ifelse_elevated <- function(...) ifelse(..., "elevated", "not_elevated")
  
  elevated <- 
    msrs_df %>% 
    # reduce(the_measures, function(a_df, msr) {
    #   separate(a_df, 
    #            msr, # expecting a value and a unit
    #            into = c(msr, paste(msr, "u", sep = "_")), 
    #            sep = "_")
    # }, .init = msrs_df) %>%
    modify_at(the_measures, ~ quietly(as.numeric)(.x)[["result"]]) %>% 
    mutate(elevated_alt = ifelse_elevated(alanine_aminotransferase > 150), 
           elevated_alp = ifelse_elevated(alkaline_phosphatase > 320), 
           elevated_altb = ifelse_elevated(alanine_aminotransferase > 90 & 
                                             total_bilirubin > 2.0))
  # for a DILI variable
  missing_all <- 
    with(elevated, is.na(elevated_alt) & 
           is.na(elevated_alp) & 
           is.na(elevated_altb))
  not_missing <- function(x) ifelse(is.na(x) & !missing_all, "", x)
  elevated <- 
    mutate(elevated, 
           dili = ifelse(not_missing(elevated_alt) == "elevated" | 
                           not_missing(elevated_alp) == "elevated" | 
                           not_missing(elevated_altb) == "elevated", 
                         "DILI", "No_DILI"))
  elevated
}

#' Accepts a dataframe of one patient's LFTs, outputs whether elevated
#' 
#' @param msrs_df The dataframe of measurements 
#' 
#' 
summarise_outcomes <- function(msrs_df) {
  stopifnot((all(
    c("alanine_aminotransferase", 
      "alkaline_phosphatase", 
      "aspartate_aminotransferase", 
      "total_bilirubin"
      ) %in% 
      names(msrs_df)
  ) | is.null(msrs_df)))

  outcomes_df <- make_outcomes(msrs_df)
  
  any_el <- function(z) 
    ifelse(any(z %in% "elevated"), "elevated", "not elevated")
  any_dil <- function(z) 
    ifelse(any(z %in% "DILI"), "DILI", "No_DILI") #ifelse(is.na(z), NA, "No_DILI")) - should this be here?
  
#  browser()
  odf <- outcomes_df
  data.frame(
    alanine_aminotransferase = median(odf$alanine_aminotransferase, na.rm = TRUE), 
    alkaline_phosphatase = median(odf$alkaline_phosphatase, na.rm = TRUE), 
    aspartate_aminotransferase = median(odf$aspartate_aminotransferase, na.rm = TRUE), 
    total_bilirubin = median(odf$total_bilirubin, na.rm = TRUE), 
    elevated_alt = any_el(odf$elevated_alt), 
    elevated_alp = any_el(odf$elevated_alp), 
    elevated_altb = any_el(odf$elevated_altb), 
    dili = any_dil(odf$dili), 
    stringsAsFactors = FALSE
  ) %>% 
    mutate(dili01 = as.numeric(dili == "DILI"))
}



