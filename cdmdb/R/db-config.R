

make_con <- function(sp) {
  dbConnect(RPostgreSQL::PostgreSQL(),
            host = 'your-mimic-db.institution.edu', # enter DB address here
            dbname = "mimic", # enter DB name here
            user = 'mimicuser', # enter DB user here
            password = .rs.askForPassword('Enter password:'), # maybe enter password here
            options = paste("-c search_path=", sp, sep = ""))
}

make_omop_con <- function() make_con("omop") # maybe remove prefix
make_mimic_con <- function() make_con("mimiciii")
