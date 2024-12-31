library(duckdb)
library(duckplyr)
library(dplyr)
library(tidyr)
library(here)


#---------- DuckDB Operations ------------#
con <- dbConnect(duckdb::duckdb(), here("NTPF_WL.duckdb"))

#dbGetQuery(con, "PRAGMA show_tables;")
#dbGetQuery(con, "Describe IPDC;")

dubmid_gi <- tbl(con, "IPDC") %>%
  filter(`WL Type` %in% c('GI Scope')) %>% 
  filter(`Hospital` %in% c("St. James's Hospital", 
                           "Tallaght University Hospital",
                           "Naas General Hosptial",
                           "Midland Regional Hospital Mullingar",
                           "Midland Regional Hospital Portlaoise",
                           "Midland Regional Hospital Tullamore")) %>% 
  select(`report_date`,`Hospital`, `Specialty`, `Monthly Time Band`,`This Week`) %>% 
  collect()

dbDisconnect(con)

#-------- GI--DubMid Data Gathering ---------#
df_gi <- dubmid_gi %>%
  mutate(
    `Monthly Time Band` = gsub("\\+", "", `Monthly Time Band`),
    `Monthly Time Band` = gsub("\\-", " ", `Monthly Time Band`),
    `Monthly Time Band`= sub("^(\\S+).*", "\\1", `Monthly Time Band`),
    `Monthly Time Band`= as.integer(`Monthly Time Band`) + 0.5
  ) 


#---------- GI--Get WaW by Hospital ---------#
Hospital_WaW_GI <- df_gi %>%
  select(-Specialty) %>% 
  group_by(report_date, `Hospital` ) %>% 
  mutate(total = sum(`This Week`),
         weight = `This Week`/total,
         waw = weight * `Monthly Time Band`) %>% 
  summarise(WaW = sum(waw) %>% round(2)) %>% 
  ungroup()


#--------- GI---Get This Week WaW by Hospital, Specialty -----------#
Spec_Hos_WaW_GI <- df_gi %>%
  filter(report_date == max(report_date)) %>% 
  select(-report_date) %>% 
  group_by(`Hospital`, Specialty ) %>% 
  mutate(total = sum(`This Week`),
         weight = `This Week`/total,
         waw = weight * `Monthly Time Band`) %>% 
  summarise(WaW = sum(waw) %>% round(2)) %>% 
  ungroup()

#--------- GI --Get This Week WaW by Hospital ------------#
current_hospital_WaW_GI <- Hospital_WaW_GI %>% 
  group_by(`Hospital`) %>% 
  filter(report_date == max(report_date)) %>% 
  mutate(WaW = round(WaW, 2)) %>% 
  ungroup()




