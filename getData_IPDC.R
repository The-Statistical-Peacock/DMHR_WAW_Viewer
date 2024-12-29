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
    `Monthly Time Bands` = gsub("\\+", "", `Monthly Time Bands`),
    `Monthly Time Bands`= sub("^(\\S+).*", "\\1", `Monthly Time Bands`),
    `Monthly Time Bands`= as.integer(`Monthly Time Bands`) + 0.5
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
  group_by(report_date, `hospital name` ) %>% 
  mutate(total = sum(Current),
         weight = Current/total,
         waw = weight * `Monthly Time Bands`) %>% 
  summarise(WaW = sum(waw) %>% round(2)) %>% 
  ungroup()

#-------- IPDC---Get WaW by Specialty -------#
Specialty_WaW_IPDC <- df_ipdc %>%
  select(-`hospital name`) %>% 
  group_by(report_date, Specialty ) %>% 
  mutate(total = sum(Current),
         weight = Current/total,
         waw = weight * `Monthly Time Bands`) %>% 
  summarise(WaW = sum(waw) %>% round(2)) %>% 
  ungroup()

#--------- IPDC---Get CURRENT WaW of BIG 7 by Hospital, Specialty -----------#
Spec_Hos_WaW_IPDC <- df_ipdc %>%
  filter(report_date == max(report_date),
         Specialty %in% big_7_ipdc) %>% 
  select(-report_date) %>% 
  group_by(`hospital name`, Specialty ) %>% 
  mutate(total = sum(Current),
         weight = Current/total,
         waw = weight * `Monthly Time Bands`) %>% 
  summarise(WaW = sum(waw) %>% round(2)) %>% 
  ungroup()

#--------- IPDC---Get CURRENT WaW by Hospital ------------#
current_hospital_WaW_IPDC <- Hospital_WaW_IPDC %>% 
  group_by(`hospital name`) %>% 
  filter(report_date == max(report_date)) %>% 
  mutate(WaW = round(WaW, 2)) %>% 
  ungroup()

#-------- IPDC---Get CURRENT WaW by Specialty ------------#
current_speciality_WaW_IPDC <- Specialty_WaW_IPDC %>% 
  group_by(Specialty) %>% 
  filter(report_date == max(report_date)) %>% 
  mutate(WaW = round(WaW, 2)) %>% 
  filter(!is.nan(WaW)) %>% 
  ungroup()



