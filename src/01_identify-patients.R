library(tidyverse)
library(lubridate)
library(edwr)

dir_raw <- "data/raw"

# run MBO query
#   * Patients - by Visit Type
#       - Location: HH Clinics;HC Childrens
#       - Visit Type: Bedded Outpatient;Day Surgery;Inpatient;Observation

patients <- read_data(dir_raw, "patients", FALSE) %>%
    as.patients() %>%
    filter(age < 18)

id_mbo <- concat_encounters(patients$millennium.id)

# run EDW query
#   * Identifiers - by Millennium Encounter ID

identifiers <- read_data(dir_raw, "identifiers") %>%
    as.id()

id_pie <- concat_encounters(identifiers$pie.id)

# run EDW query
#   * Surgeries

surgeries <- read_data(dir_raw, "surgeries") %>%
    as.surgeries()

surgery_pts <- distinct(surgeries, pie.id) %>%
    left_join(identifiers, by = "pie.id") %>%
    left_join(patients, by = "millennium.id")

id_mbo <- concat_encounters(surgery_pts$millennium.id)

# run MBO queries
#   * Medications - Inpatient - All

meds <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt()

intraop <- meds %>%
    left_join(identifiers[c("millennium.id", "pie.id")], by = "millennium.id") %>%
    left_join(surgeries, by = "pie.id") %>%
    filter(med.datetime >= surg.start.datetime,
           med.datetime <= surg.stop.datetime) %>%
    count(med, sort = TRUE) %>%
    mutate(num_surg = nrow(surgeries),
           pct_surg = n / num_surg)

write_csv(intraop, "data/external/intraop_meds.csv")

time_day <- meds %>%
    left_join(identifiers[c("millennium.id", "pie.id")], by = "millennium.id") %>%
    left_join(surgeries, by = "pie.id") %>%
    filter(med.datetime >= surg.start.datetime,
           med.datetime <= surg.stop.datetime) %>%
    mutate(hour_day = hour(med.datetime)) %>%
    count(hour_day) %>%
    arrange(hour_day)

write_csv(time_day, "data/external/time_day.csv")
