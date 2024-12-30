library(duckdb)
library(duckplyr)
library(dplyr)
library(tidyr)
library(here)


#---------- DuckDB Operations ------------#
con <- dbConnect(duckdb::duckdb(), here("NTPF_WL.duckdb"))

#dbGetQuery(con, "PRAGMA show_tables;")
#dbGetQuery(con, "Describe IPDC;")

dubmid_ipdc <- tbl(con, "IPDC") %>%
  filter(`WL Type` %in% c('Adult', 'Child')) %>% 
  filter(`Hospital` %in% c("St. James's Hospital", 
                                "Tallaght University Hospital",
                                "Naas General Hosptial",
                                "Midland Regional Hospital Mullingar",
                                "Midland Regional Hospital Portlaoise",
                                "Midland Regional Hospital Tullamore")) %>% 
  select(`report_date`,`Hospital`, `Specialty`, `Monthly Time Band`,`This Week`) %>% 
  collect()

dbDisconnect(con)

#-------- IPDC---DubMid Data Gathering ---------#
df_ipdc <- dubmid_ipdc %>%
  mutate(
    `Monthly Time Band` = gsub("\\+", "", `Monthly Time Band`),
    `Monthly Time Band` = gsub("\\-", " ", `Monthly Time Band`),
    `Monthly Time Band`= sub("^(\\S+).*", "\\1", `Monthly Time Band`),
    `Monthly Time Band`= as.integer(`Monthly Time Band`) + 0.5
  ) 


big_7_ipdc <- c("Otolaryngology (ENT)",
               "Cardiology",
               "Dermatology",
               "Gynaecology",
               "Ophthalmology",
               "Rheumatology",
               "Orthopaedics")

#---------- IPDC---Get WaW by Hospital ---------#
Hospital_WaW_IPDC <- df_ipdc %>%
  select(-Specialty) %>% 
  group_by(report_date, `Hospital` ) %>% 
  mutate(total = sum(`This Week`),
         weight = `This Week`/total,
         waw = weight * `Monthly Time Band`) %>% 
  summarise(WaW = sum(waw) %>% round(2)) %>% 
  ungroup()

#-------- IPDC---Get WaW by Specialty -------#
Specialty_WaW_IPDC <- df_ipdc %>%
  select(-`Hospital`) %>% 
  group_by(report_date, Specialty ) %>% 
  mutate(total = sum(`This Week`),
         weight = `This Week`/total,
         waw = weight * `Monthly Time Band`) %>% 
  summarise(WaW = sum(waw) %>% round(2)) %>% 
  ungroup()

#--------- IPDC---Get This Week WaW of BIG 7 by Hospital, Specialty -----------#
Spec_Hos_WaW_IPDC <- df_ipdc %>%
  filter(report_date == max(report_date),
         Specialty %in% big_7_ipdc) %>% 
  select(-report_date) %>% 
  group_by(`Hospital`, Specialty ) %>% 
  mutate(total = sum(`This Week`),
         weight = `This Week`/total,
         waw = weight * `Monthly Time Band`) %>% 
  summarise(WaW = sum(waw) %>% round(2)) %>% 
  ungroup()

#--------- IPDC---Get This Week WaW by Hospital ------------#
current_hospital_WaW_IPDC <- Hospital_WaW_IPDC %>% 
  group_by(`Hospital`) %>% 
  filter(report_date == max(report_date)) %>% 
  mutate(WaW = round(WaW, 2)) %>% 
  ungroup()

#-------- IPDC---Get This Week WaW by Specialty ------------#
current_speciality_WaW_IPDC <- Specialty_WaW_IPDC %>% 
  group_by(Specialty) %>% 
  filter(report_date == max(report_date)) %>% 
  mutate(WaW = round(WaW, 2)) %>% 
  filter(!is.nan(WaW)) %>% 
  ungroup()



