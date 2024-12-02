library(duckdb)
library(duckplyr)
library(dplyr)
library(here)

con <- dbConnect(duckdb::duckdb(), here("NTPF_WL.duckdb"))

#dbGetQuery(con, "PRAGMA show_tables;")
#dbGetQuery(con, "Describe OPD;")

dubmid <- tbl(con, "OPD") %>%
  filter(`hospital name` %in% c("St. James's Hospital", 
                               "Tallaght University Hospital",
                               "Naas General Hosptial",
                               "Midland Regional Hospital Mullingar",
                               "Midland Regional Hospital Portlaoise",
                               "Midland Regional Hospital Tullamore")) %>% 
  select(`report_date`,`hospital name`, `Specialty`, `Monthly Time Bands`,`Current`) %>% 
  collect()

dbDisconnect(con)

df <- dubmid %>%
  mutate(
    `Monthly Time Bands` = gsub("\\+", "", `Monthly Time Bands`),
    `Monthly Time Bands`= sub("^(\\S+).*", "\\1", `Monthly Time Bands`),
    `Monthly Time Bands`= as.integer(`Monthly Time Bands`) + 0.5
  ) 


Hospital_WaW <- df %>%
  select(-Specialty) %>% 
  group_by(report_date, `hospital name` ) %>% 
  mutate(total = sum(Current),
         weight = Current/total,
         waw = weight * `Monthly Time Bands`) %>% 
  summarise(WaW = sum(waw) %>% round(2))

current_WaW <- Hospital_WaW %>% 
  group_by(`hospital name`) %>% 
  filter(report_date == max(report_date)) %>% 
  mutate(WaW = round(WaW, 2))

