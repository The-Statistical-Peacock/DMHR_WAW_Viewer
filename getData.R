library(duckdb)
library(duckplyr)
library(dplyr)
library(tidyr)
library(here)


#---------- DuckDB Operations ------------#
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

#-------- DubMid Data Gathering ---------#
df <- dubmid %>%
  mutate(
    `Monthly Time Bands` = gsub("\\+", "", `Monthly Time Bands`),
    `Monthly Time Bands`= sub("^(\\S+).*", "\\1", `Monthly Time Bands`),
    `Monthly Time Bands`= as.integer(`Monthly Time Bands`) + 0.5
  ) 


big_7 <- c("Otolaryngology (ENT)",
           "Cardiology",
           "Dermatology",
           "Gynaecology",
           "Ophthalmology",
           "Rheumatology",
           "Orthopaedics")

#---------- Get WaW by Hospital ---------#
Hospital_WaW <- df %>%
  select(-Specialty) %>% 
  group_by(report_date, `hospital name` ) %>% 
  mutate(total = sum(Current),
         weight = Current/total,
         waw = weight * `Monthly Time Bands`) %>% 
  summarise(WaW = sum(waw) %>% round(2)) %>% 
  ungroup()

#-------- Get WaW by Specialty -------#
Specialty_WaW <- df %>%
  select(-`hospital name`) %>% 
  group_by(report_date, Specialty ) %>% 
  mutate(total = sum(Current),
         weight = Current/total,
         waw = weight * `Monthly Time Bands`) %>% 
  summarise(WaW = sum(waw) %>% round(2)) %>% 
  ungroup()

#--------- Get CURRENT WaW of BIG 7 by Hospital, Specialty -----------#
Spec_Hos_WaW <- df %>%
  filter(report_date == max(report_date),
         Specialty %in% big_7) %>% 
  select(-report_date) %>% 
  group_by(`hospital name`, Specialty ) %>% 
  mutate(total = sum(Current),
         weight = Current/total,
         waw = weight * `Monthly Time Bands`) %>% 
  summarise(WaW = sum(waw) %>% round(2)) %>% 
  ungroup()

#--------- Get CURRENT WaW by Hospital ------------#
current_hospital_WaW <- Hospital_WaW %>% 
  group_by(`hospital name`) %>% 
  filter(report_date == max(report_date)) %>% 
  mutate(WaW = round(WaW, 2)) %>% 
  ungroup()

#-------- Get CURRENT WaW by Specialty ------------#
current_speciality_WaW <- Specialty_WaW %>% 
  group_by(Specialty) %>% 
  filter(report_date == max(report_date)) %>% 
  mutate(WaW = round(WaW, 2)) %>% 
  filter(!is.nan(WaW)) %>% 
  ungroup()



