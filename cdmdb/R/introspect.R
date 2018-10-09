

introspect_db <- function(con) {
  
  the_tables <- dbListTables(con)
  #the_tables <- the_tables[-grep("^source$|source_daimon", the_tables)]
  
  # List fields in a table
  table_fields <- 
    map(the_tables, 
        function(z) {
          tryCatch(dbListFields(con, z), 
                   error = function(e) c("", "", ""))
        })
  
  con_stuff <- rep_len(list(), length.out = length(the_tables))
  names(con_stuff) <- the_tables
  
  nrow_tbl <- function(the_table) {
    z <- tbl_nrow_factory(con)(the_table)
    z <- ifelse(is.null(z), 0, z)
    as.integer(unlist(z))
  }
  
  con_stuff <- 
    add_to_elems(con_stuff, table_fields, "fields") %>% 
    add_to_elems(map(the_tables, nrow_tbl), "nrows")
  
  
  the_df <- 
    tbl_df(data.frame(the_tbl = names(con_stuff), 
                      nrows = map_int(con_stuff, "nrows")
    ))
  the_df[["fields"]] <- map(con_stuff, "fields")
  
  unnest(the_df)
}




add_to_elems <- function(list_of_lists, things_to_add, things_nm) {
  map2(list_of_lists, things_to_add, 
       function(a, b) {
         a[[things_nm]] <- b
         a
       })
}

query_this <- function(con) {
  function(a_query) {
    dbGetQuery(con, a_query)
  }
}


tbl_nrow_factory <- function(con) {
  function(the_tbl) {
    query_this(con)(paste0("SELECT reltuples AS approximate_row_count FROM pg_class WHERE relname = '", 
                           the_tbl, "'", sep = ""))
  }
}

nmd_l_to_df <- function(named_list) {
  l <- named_list
  if (is.null(names(l))) stop("named_list must be a list with names")
  transpose(l) %>% 
    as_data_frame() %>% 
    mutate(table = names(l)) %>% 
    select(table, everything())
}
