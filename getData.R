library(duckdb)
library(duckplyr)
library(dplyr)
library(here)

con <- dbConnect(duckdb::duckdb(), here("NTPF_WL.duckdb"))

#dbGetQuery(con, "PRAGMA show_tables;")
#dbGetQuery(con, "Describe OPD;")

df <- tbl(con, "OPD") %>%
  filter(`hospital name` %in% c("St. James's Hospital", "Tallaght University Hospital")) %>% 
  select(`report_date`,`hospital name`, `Specialty`, `Monthly Time Bands`,`Current`) %>% 
  collect()

dbDisconnect(con)

df <- df %>%
  mutate(
    `Monthly Time Bands` = gsub("\\+", "", `Monthly Time Bands`),
    `Monthly Time Bands`= sub("^(\\S+).*", "\\1", `Monthly Time Bands`),
    `Monthly Time Bands`= as.integer(`Monthly Time Bands`) + 0.5
  ) 


