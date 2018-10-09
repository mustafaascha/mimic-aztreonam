


search_domain <- function(domain) {
  function(con, search_term, collect = TRUE) {
    
    search_results <- 
      search_vocab(con, search_term) %>% 
      filter(domain_id == domain) %>% 
      collector(collect)
    
    prefix <- paste(tolower(domain), "_", sep = "")
    
    conceptual <- function(z) z[grepl("^concept", z)]
    old_nms <- names(search_results)
    names(search_results)[grep("^concept", old_nms)] <- 
      paste(prefix, conceptual(old_nms), sep = "")
    
    add_class(search_results, "vocab")
  }
}

search_drugs <- search_domain("Drug")

search_conditions <- search_domain("Condition")

name_query <- function(nm) {
  function(z)
    paste(paste("SELECT * FROM omop.", 
                nm, 
                " WHERE ", 
                nm, 
                "_name ~'", 
                sep = ""), 
          z, "'", sep = "")
}

search_concepts <- function(con, z) {
  dbGetQuery(con, name_query("concept")(z))
}

search_vocab <- function(con, z) {
  dbGetQuery(con, name_query("vocabulary")(z))
}

# translate_code <- function(con, codes_to_translate) {
#   omop_cid_mappings <- 
#     tbl(con, "concept_relationship") %>% 
#       filter(relationship_id == "Maps to")
# }

