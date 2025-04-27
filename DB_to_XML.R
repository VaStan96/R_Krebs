library(DBI)
library(RPostgreSQL)
library(xml2)

source('utils.R')

con <- dbConnect(PostgreSQL(),
                 dbname = 'Krebs_DS',
                 host = 'localhost',
                 port = 5432,
                 user = 'postgres',
                 password = 'postgres')

query <- "SELECT * FROM patients"
result_df <- dbGetQuery(con, query)
result_xml <- df_to_xml(result_df, 'patients')
write_xml(result_xml, 'data/patients.xml')

query <- "SELECT * FROM disease_types"
result_df <- dbGetQuery(con, query)
result_xml <- df_to_xml(result_df, 'disease_types')
write_xml(result_xml, 'data/disease_types.xml')

query <- "SELECT * FROM diagnoses"
result_df <- dbGetQuery(con, query)
result_xml <- df_to_xml(result_df, 'diagnoses')
write_xml(result_xml, 'data/diagnoses.xml')

query <- "SELECT * FROM treatments"
result_df <- dbGetQuery(con, query)
result_xml <- df_to_xml(result_df, 'treatments')
write_xml(result_xml, 'data/treatments.xml')

dbDisconnect(conn = con)