
read_dir_csvs <- function(the_dir, rm_pattern = NULL) {
  csvs <- list.files(the_dir)
  if (missing(rm_pattern)) keep_these <- seq(length(csvs))
  else keep_these <- 
      setdiff(seq(length(csvs)), grep(rm_pattern, csvs))
  csvs_l <-
    map(paste(the_dir, csvs, sep = "/"),
        compose((function(df) select(df, -1)), read_csv)) 
  names(csvs_l) <- gsub("\\..*", "", csvs)
  csvs_l[keep_these]
}

separate_domains <-  function(cs_dfs) {
  cs_dfs <- map(cs_dfs, ~ modify_at(.x, "Code", as.character))
  map(c(d = "Drug", m = "Measurement", o = "Observation", c = "Condition"), 
      function(the_tbl) {
        new_nm <- paste(tolower(the_tbl), "_concept_id", sep = "")
        bind_rows(cs_dfs) %>% 
          filter(Domain == the_tbl & !grepl("[Uu]rine", Name)) %>% 
          select(!!new_nm := Id, Name)
      })
}

make_conditions <- function(pth) {
  read_all_sheets(pth) %>% 
    bind_rows() %>%
    mutate(Name = condition) %>% 
    select(condition_concept_id = Id, Name, condition)
}

expand_x <- function(a_df) {
  cd <- unlist(a_df[["code"]])
  dt <- a_df[,-grep("code", names(a_df))]
  stopifnot(nrow(dt) == 1 & length(cd) == 1)
  if (!grepl("[xX]", cd)) {
    return(
      data.frame(code = cd,#gsub("\\.", "", cd), 
                 name = dt[["name"]], 
                 indication = dt[["indication"]], 
                 stringsAsFactors = FALSE)
    )
  }
  #code_split <- unlist(strsplit(gsub("\\.", "", cd), ""))
  code_split <- unlist(strsplit(cd, ""))
  which_x <- grep("[xX]", code_split)
  if (length(which_x) > 1) which_x <- which_x[1]
  
  code_split_list <- as.list(code_split)
  #message(paste("which_x is", which_x))
  
  paste2 <- function(x, y) paste(x, y, sep = '')
  the_head <- 
    reduce(code_split_list[1:(which_x - 1)], paste2)
  the_tail <- 
    reduce(code_split_list[(which_x + 1):length(code_split_list)], paste2)
  if (which_x == length(code_split_list)) the_tail <- ""
  data.frame(
    code = paste(the_head, 0:9, the_tail, sep = ""), 
    name = dt[["name"]], 
    indication = dt[["indication"]],
    stringsAsFactors = FALSE
  )
}

expand_xs <- function(a_df) {
  a_df <- 
    a_df %>% 
    mutate(which_row = 1:n()) %>% 
    group_by(which_row) %>% 
    nest()
  map_dfr(a_df[["data"]], expand_x)
}

config <- function(){
  omop_cn <- make_omop_con()
  mimic_cn <- make_mimic_con()
  paper_products <- list()
  concept_sets <- read_dir_csvs("concepts/drugs-outcomes")#[-c(4:11)]
  
  conditions <- 
    make_conditions("concepts/conditions/concept ids (for table 1).xlsx")
  
  pull_these <- separate_domains(concept_sets)
  pull_these[["d"]] <- modify_at(pull_these[["d"]], "Name", tolower)
  the_measures <- 
    c("alanine_aminotransferase", 
      "alkaline_phosphatase", 
      "aspartate_aminotransferase", 
      "total_bilirubin")
  criteria <- list()
  
  indications <- 
    readxl::read_xlsx("concepts/indications/idgrouping-mimic.xlsx")
  names(indications) <- c("code", "name", "indication")
  indications_br <- indications
  indications <- expand_xs(indications) %>% expand_xs()
  
  indications <- 
    bind_rows(indications, indications_br) %>% 
    distinct()
  
  list(
       omop_cn, 
       mimic_cn, 
       paper_products, 
       pull_these, 
       the_measures, 
       criteria, 
       conditions, 
       indications 
       )
}