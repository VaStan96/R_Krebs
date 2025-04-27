library(xml2)
library(lubridate)
library(dplyr)

source('utils.R')

load_data <- function(){
  # get data from xml
  cases_xml <- read_xml('data/cases.xml')
  # convert to dataframe
  cases_df <- xml_to_df(cases_xml)
  # convert id in INT and dates in DATE
  cases_df = mutate(cases_df, 
                    date_of_birth = ymd(date_of_birth), 
                    diagnosis_date = ymd(diagnosis_date), 
                    treatment_date = ymd(treatment_date),
                    patient_id = as.integer(patient_id),
                    gender = factor(gender, levels = c('M', 'F')))
  
  # add column with alt
  cases_df <- mutate(cases_df, age = floor(interval(date_of_birth, today())/years(1)))
  #make factor for grouping age
  cases_df <- mutate(cases_df, age_groups = cut(age,
                                                breaks = c(-Inf, 19, 29, 39, 49, 59, Inf),
                                                labels = c("20-", "20+", "30+", "40+", "50+", "60+"),
                                                right = TRUE))
  
  return(cases_df)
}