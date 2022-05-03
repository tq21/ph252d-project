library(readxl)
library(dplyr)
library(tidyverse)

df_demo <- read_excel("data/GIPP-Demographics.xlsx", sheet = 3)
df_med_hist <- read_excel("data/GIPP-Demographics.xlsx", sheet = 4)
df_intra <- read_excel("data/GIPP-Demographics.xlsx", sheet = 5)
df_labs <- read_excel("data/GIPP-Demographics.xlsx", sheet = 6)
df_rand <- read_excel("data/Randomization gabapentin RCT.xlsx", sheet = 1)
df_out <- read_excel("data/GIPP-Cognitive.xlsx", sheet = 6)

# potential research questions:
# dosage of anesthetic agents on postoperative delirium status

# intra-operative data
df_intra <- df_intra %>% 
  select(STUDYNO, Prodose, SURGSITE, Anesdur) %>% 
  drop_na() %>% 
  filter(Prodose >= 0)

# lab data, significant missingness, not use
rm(df_labs)

# original randomization
df_rand <- df_rand %>% 
  select("GIPP NO") %>% 
  rename(STUDYNO = "GIPP NO")

# demographic data
# read some papaer find risk factors
df_demo <- df_demo %>% 
  select(STUDYNO, Gender, Age, HOCNS, Delirium, Stroke, TIA, 
         cursmoke, ETOH, Dementia, Depress) %>% # nrow = 752
  drop_na() %>%  # nrow = 745
  filter(if_all(where(is.numeric), ~ . >= 0)) # nrow = 737

# medical history
df_med_hist <- df_med_hist %>% 
  select(STUDYNO, Charlest)

# outcome: post-op day 1 delirium
df_out <- df_out %>% 
  select(STUDYNO, D01DDIAG, D02DDIAG, D03DDIAG)
df_out$D01DDIAG <- as.numeric(df_out$D01DDIAG)
df_out$D02DDIAG <- as.numeric(df_out$D02DDIAG)
df_out$D03DDIAG <- as.numeric(df_out$D03DDIAG)

df_out <- df_out %>% 
  mutate(y = case_when(
    D01DDIAG == 1 | D02DDIAG == 1 | D03DDIAG == 1 ~ 1,
    D01DDIAG == 0 & D02DDIAG == 0 & D03DDIAG == 0 ~ 0,
    is.na(D01DDIAG) & is.na(D02DDIAG) & is.na(D03DDIAG) ~ -100,
    TRUE ~ 0
  )) %>% 
  filter(y != -100)

# join
X <- c("y", "Gender", "Age", "HOCNS", "Delirium", "Stroke",
       "TIA", "cursmoke", "ETOH", "SURGSITE", "Anesdur", "Charlest",
       "Dementia", "Depress")
df_all <- df_demo %>% 
  left_join(df_out, by = "STUDYNO") %>% 
  left_join(df_intra, by = "STUDYNO") %>% 
  left_join(df_rand, by = "STUDYNO") %>% 
  left_join(df_med_hist, by = "STUDYNO") %>% 
  select(STUDYNO, Prodose, X) %>% 
  drop_na()

df_all <- df_all %>% 
  mutate(StrokeOrTIA = case_when(
    Stroke == 1 | TIA == 1 ~ 1,
    TRUE ~ 0
  )) %>% 
  select(-c(Stroke, TIA))

# drop outlier
# drop negative Anesdur
df_all <- df_all %>% 
  filter(Prodose <= 46634) %>% 
  filter(Anesdur >= 0)

df_all <- df_all %>% 
  mutate(SURGSITE = case_when(
    SURGSITE == "Spine" ~ 1,
    SURGSITE == "Knee" ~ 2,
    SURGSITE == "Hip" ~ 3
  ))

write.csv(df_all, "delirium.csv")

