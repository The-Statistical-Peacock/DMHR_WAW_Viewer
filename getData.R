library(duckdb)
library(duckplyr)
library(dplyr)
library(tidyr)
library(here)


#---------- DuckDB Operations ------------#
con <- dbConnect(duckdb::duckdb(), here("NTPF_WL.duckdb"))

#dbGetQuery(con, "PRAGMA show_tables;")
#dbGetQuery(con, "Describe IPDC;")

dubmid_opd <- tbl(con, "OPD") %>%
  filter(`hospital name` %in% c("St. James's Hospital", 
                               "Tallaght University Hospital",
                               "Naas General Hosptial",
                               "Midland Regional Hospital Mullingar",
                               "Midland Regional Hospital Portlaoise",
                               "Midland Regional Hospital Tullamore")) %>% 
  select(`report_date`,`hospital name`, `Specialty`, `Monthly Time Bands`,`Current`) %>% 
  collect()

dubmid_ipdc <- tbl(con, "IPDC") %>%
  filter(`Hospital` %in% c("St. James's Hospital", 
                                "Tallaght University Hospital",
                                "Naas General Hosptial",
                                "Midland Regional Hospital Mullingar",
                                "Midland Regional Hospital Portlaoise",
                                "Midland Regional Hospital Tullamore")) %>% 
  select(`report_date`,`Hospital`, `Specialty`, `Monthly Time Band`,`This Week`) %>% 
  collect()

dbDisconnect(con)

#-------- OPD---DubMid Data Gathering ---------#
df_opd <- dubmid_opd %>%
  mutate(
    `Monthly Time Bands` = gsub("\\+", "", `Monthly Time Bands`),
    `Monthly Time Bands`= sub("^(\\S+).*", "\\1", `Monthly Time Bands`),
    `Monthly Time Bands`= as.integer(`Monthly Time Bands`) + 0.5
  ) 


big_7_opd <- c("Otolaryngology (ENT)",
               "Cardiology",
               "Dermatology",
               "Gynaecology",
               "Ophthalmology",
               "Rheumatology",
               "Orthopaedics")

#---------- OPD---Get WaW by Hospital ---------#
Hospital_WaW_OPD <- df_opd %>%
  select(-Specialty) %>% 
  group_by(report_date, `hospital name` ) %>% 
  mutate(total = sum(Current),
         weight = Current/total,
         waw = weight * `Monthly Time Bands`) %>% 
  summarise(WaW = sum(waw) %>% round(2)) %>% 
  ungroup()

#-------- OPD---Get WaW by Specialty -------#
Specialty_WaW_OPD <- df_opd %>%
  select(-`hospital name`) %>% 
  group_by(report_date, Specialty ) %>% 
  mutate(total = sum(Current),
         weight = Current/total,
         waw = weight * `Monthly Time Bands`) %>% 
  summarise(WaW = sum(waw) %>% round(2)) %>% 
  ungroup()

#--------- OPD---Get CURRENT WaW of BIG 7 by Hospital, Specialty -----------#
Spec_Hos_WaW_OPD <- df_opd %>%
  filter(report_date == max(report_date),
         Specialty %in% big_7_opd) %>% 
  select(-report_date) %>% 
  group_by(`hospital name`, Specialty ) %>% 
  mutate(total = sum(Current),
         weight = Current/total,
         waw = weight * `Monthly Time Bands`) %>% 
  summarise(WaW = sum(waw) %>% round(2)) %>% 
  ungroup()

#--------- OPD---Get CURRENT WaW by Hospital ------------#
current_hospital_WaW_OPD <- Hospital_WaW_OPD %>% 
  group_by(`hospital name`) %>% 
  filter(report_date == max(report_date)) %>% 
  mutate(WaW = round(WaW, 2)) %>% 
  ungroup()

#-------- OPD---Get CURRENT WaW by Specialty ------------#
current_speciality_WaW_OPD <- Specialty_WaW_OPD %>% 
  group_by(Specialty) %>% 
  filter(report_date == max(report_date)) %>% 
  mutate(WaW = round(WaW, 2)) %>% 
  filter(!is.nan(WaW)) %>% 
  ungroup()



