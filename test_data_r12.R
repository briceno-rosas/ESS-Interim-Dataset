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
n_synth = 1000

# Synth <- matrix(nrow = n_synth, ncol = ncol(Main) - 1)

Synth <- Main %>%
  mutate(
    nwspol = ifelse(nwspol > 1440 & !nwspol %in% c(6666, 7777, 8888), 1440, nwspol),
    netustm = ifelse(netustm > 1440 & !netustm %in% c(6666, 7777, 8888), 1440, netustm)) %>%
  select(-c(cntry, idno, inwds, ainws, einwe, scresolu)) %>%
  apply(2, function(col) {
    if (is.na(sum(as.numeric(col)))) {
      sample(col, n_synth, replace = TRUE)
    } else {
      if (length(attr(col, "value.labels")) > 3) {
        sample(attr(col, "value.labels"), n_synth, replace = TRUE)
      } else {
        sample(min(col):max(col), n_synth, replace = TRUE)
      }
    }
  }) %>% as.data.frame() %>% mutate_if(is.character, as.numeric)

Synth$idno <- (max(Main$idno) + 1):(max(Main$idno) + n_synth)
Synth$cntry <- sample(Main$cntry, n_synth, replace = TRUE)
Synth$inwds <- sample(min(Main$inwds):max(Main$inwds), n_synth, replace = TRUE)
Synth$ainws <- Synth$inwds
Synth$einwe <- Synth$inwds + sample(1200:11000, n_synth, replace = TRUE)

# Merge datasets

# Combined <- bind_rows(Main, Synth)
Combined <- Synth

# Write combined data

write_sav(Combined, "DATA_R12/data_simulated.sav")

# Simulate interviewers

## Shuffle idnos

ids <- (Combined %>% filter(mode %in% c(1, 2)))$idno %>% sample(replace = F)

## Generate interviewer ids and assign interviews to random interviewers

n_int <- 60

intnums <- sample(1:n_int, length(ids), replace = T)

## Export assignments

tibble(idno = ids, intnum = intnums) %>% 
  write_sav("DATA_R12/intnums_simulated.sav")
