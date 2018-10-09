
{
  library(tableone)
  library(glmnet)
  library(Matrix)
  library(plotmo)
  #library(glmnetUtils)
}

do_analysis <- TRUE
for_real <- TRUE
source("src/munge/munge.R")
paper_products[["dilis"]] <- dili_drug_table(analytic_pop)
paper_products[["LFTs"]] <- elevated_lft_table(analytic_pop)
paper_products[["uv_mdl"]] <- 
   tidy_glm(glm(dili01 ~ drug, data = analytic_pop))

paper_products[["table_one"]] <- 
  do_to(analytic_pop, table_one_vars, "dili")

paper_products[["table_one_drugs"]] <- 
  do_to(analytic_pop, 
        c(table_one_vars[-grep("drug", table_one_vars)], 
          "dili"), 
        "drug")[,1:3]

indication_vars <- indication_vars[indication_vars %in% names(analytic_pop)]
condition_vars  <- condition_vars[condition_vars %in% names(analytic_pop)]
# lasso to select predictors
model_df <- analytic_pop[,c("dili01", indication_vars, condition_vars)]
model_matrix <- 
  sparse.model.matrix(
    reformulate(c(indication_vars, condition_vars, "dili01")), 
    data = model_df
  )
xs <- model_matrix[,-grep("dili01", colnames(model_matrix))]
y <- model_matrix[,"dili01"]
glmnet_model <- glmnet(xs, y, "binomial")
paper_products[["mv_mdl"]] <- 
  glm(dili01 ~ drug + Liver__Biliary + sepsis + hypertension, data = analytic_pop) %>% 
  tidy_glm()


#write_rds(paper_products, "paper_products.rds")


