library(DBI)
library(RPostgres)

con <- dbConnect(Postgres(),
                 dbname = 'Krebs_DS',
                 host = 'localhost',
                 port = 5432,
                 user = 'postgres',
                 password = 'postgres')


# create new table
new_table <- data.frame(
  id = c(1, 2),
  description = c("Lungenkrebs", "Brustkrebs"),
  int_code = c(1, 23),
  date_code = c(date("1985-06-23"), date("1977-06-23"))
)
dbWriteTable(con, "test_table", new_table)

# ------------------------------------------------------------------------------
# add one row in table
new_data <- data.frame(
  id = 3,
  description = "tstst",
  int_code = 28,
  date_code = as.Date("1885-06-23")
)
dbAppendTable(con, "test_table", new_data)

# or this Variant
# dbExecute(con, "
#   INSERT INTO patients (patient_id, name, date_of_birth, gender)
#   VALUES (11, 'Müller Thomas', '1985-06-23', 'M')
# ")


# ------------------------------------------------------------------------------
# update one row 
dbExecute(con, "
  UPDATE test_table
  SET description = 'iibibi', int_code = 999
  WHERE id = 3
")

# ------------------------------------------------------------------------------
# clean table
dbExecute(con, "TRUNCATE TABLE test_table RESTART IDENTITY")


# ------------------------------------------------------------------------------
# clean and fill table
dbWriteTable(con, "test_table", new_table, overwrite = TRUE)

# ------------------------------------------------------------------------------
#delete table
dbExecute(con, "DROP TABLE IF EXISTS test_table")


dbDisconnect(conn = con)
