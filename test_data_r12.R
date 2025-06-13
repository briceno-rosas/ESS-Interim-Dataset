# European Social Survey - European Research Infrastructure (ESS-ERIC)
# europeansocialsurvey.org
# Notes
# Version: 12.0
# Data format: SPSS

# == SCRIPT TO PREPROCESS TEST DATA FOR ESS ROUND 12 ===========================

# Import data

library(tidyverse)
library(foreign)
library(haven)

Main <- read.spss("DATA_R12/ESS12_Data_File_1_Main_Questionnaire_checked.sav",
                  use.value.labels = F, use.missings = F, to.data.frame = T)

# Remove country-specific variables

Main <- Main %>% select(!starts_with("A", ignore.case = F) & !starts_with("B", ignore.case = F))

# Fix missing start and end dates

Main <- Main %>% mutate(
  inwds = ifelse(is.na(inwds), sample(Main$inwds[!is.na(Main$inwds)], 1), inwds),
  ainws = ifelse(is.na(ainws), inwds, ainws),
  einwe = ifelse(is.na(einwe), inwds + sample(1200:11000, nrow(Main)), einwe)
)

# Generate synthetic data

n_synth = 2000

Main1 <- Main %>%
  mutate(
    nwspol = ifelse(nwspol > 1440 & !nwspol %in% c(6666, 7777, 8888), 1440, nwspol),
    netustm = ifelse(netustm > 1440 & !netustm %in% c(6666, 7777, 8888), 1440, netustm)) %>%
  select(-c(cntry, idno, inwds, ainws, einwe, scresolu))
Synth <- names(Main1) %>% lapply(function(cname) {
  col <- Main1[[cname]]
  if (is.na(sum(as.numeric(col)))) {
    sample(col, n_synth, replace = TRUE)
  } else if ("Very much like me" %in% names(attr(col, "value.labels"))) { # HVS
    sample(c(9, 8, 7, 1:6, 1:6, 1:6, 1:6), n_synth, replace = TRUE)
  } else if (length(attr(col, "value.labels")) > 3) {
    sample(attr(col, "value.labels"), n_synth, replace = TRUE)
  } else {
    sample(min(col):max(col), n_synth, replace = TRUE)
  }
})
names(Synth) <- names(Main1)
Synth <- Synth %>% as.data.frame() %>% mutate_if(is.character, as.numeric)

Synth$idno <- (max(Main$idno) + 1):(max(Main$idno) + n_synth)
Synth$cntry <- sample(Main$cntry, n_synth, replace = TRUE)
Synth$inwds <- sample(min(Main$inwds):max(Main$inwds), n_synth, replace = TRUE)
Synth$ainws <- Synth$inwds
Synth$einwe <- Synth$inwds + sample(1200:11000, n_synth, replace = TRUE)

# Write synthetic data

write_sav(Synth, "DATA_R12/data_simulated.sav")

Synth %>% mutate(
  inwds = as_datetime(inwds, origin = "1582-10-14") %>% format("%d-%m-%Y %H:%M:%S"),
  ainws = as_datetime(ainws, origin = "1582-10-14") %>% format("%d-%m-%Y %H:%M:%S"),
  einwe = as_datetime(einwe, origin = "1582-10-14") %>% format("%d-%m-%Y %H:%M:%S")
) %>% write.csv("DATA_R12/data_simulated.csv", row.names = F)

Synth1 = Synth %>% mutate(
  inwds = as_datetime(inwds, origin = "1582-10-14") %>% format("%d-%m-%Y %H:%M:%S"),
  ainws = as_datetime(ainws, origin = "1582-10-14") %>% format("%d-%m-%Y %H:%M:%S"),
  einwe = as_datetime(einwe, origin = "1582-10-14") %>% format("%d-%m-%Y %H:%M:%S")
) %>% write.dta("DATA_R12/data_simulated.dta", convert.dates = F)

# Simulate interviewers

## Shuffle idnos

ids <- (Synth %>% filter(mode %in% c(1, 2)))$idno %>% sample(replace = F)

## Generate interviewer ids and assign interviews to random interviewers

n_int <- 40

intnums <- tibble(
  idno = ids, 
  intnum = sample(1:n_int, length(ids), replace = T))

## Export assignments

write_sav(intnums, "DATA_R12/intnums_simulated.sav")
write.csv(intnums, "DATA_R12/intnums_simulated.csv", row.names = F)
write.dta(intnums, "DATA_R12/intnums_simulated.dta")
