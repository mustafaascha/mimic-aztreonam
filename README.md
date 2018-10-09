# Antibiotics in the ICU: Aztreonam and Ceftazidime

*Note, these instructions are under development*

This is a reproducibility repository documenting procedure for a study of antibiotic hepatotoxicity in the intensive care unit setting. 

To reproduce this work (which is possible because you can use the *exact same data* that we did), you will first have to [obtain the MIMIC III dataset](https://mimic.physionet.org/). For a direct link to the application site, [click here](https://mimic.physionet.org/gettingstarted/access/). 

Study preparation requires [setting up a PostgreSQL database with MIMIC III data](https://github.com/MIT-LCP/mimic-code), and then running an [ETL to the OMOP CDM V5](https://github.com/MIT-LCP/mimic-omop).

After setting up your database, enter its credentials in the cdmdb/R/db-config.R file. If you prefer not to write your password in plaintext, uncomment the line including `.rs.askForPassword('Enter password:')`. 

From there, assuming you have a working installation of R and relevant dependencies, you may knit the report with `make report`. 


