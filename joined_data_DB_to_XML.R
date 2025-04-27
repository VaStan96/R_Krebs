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

query <- "SELECT p.patient_id, p.first_name, p.last_name, p.gender, p.date_of_birth,
                 d.icd10_code, d.diagnosis_date, dt.disease_name,
	               t.treatment_date, t.treatment_type
          FROM patients AS p
          LEFT JOIN diagnoses AS d
          ON p.patient_id = d.patient_id
          JOIN disease_types AS dt
          ON d.icd10_code = dt.icd10_code
          JOIN treatments AS t
          ON p.patient_id = t.patient_id;"

result_df <- dbGetQuery(con, query)
result_xml <- df_to_xml(result_df, 'cases')
write_xml(result_xml, 'data/cases.xml')