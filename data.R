# ________________________________________
# TRATAMENTO DE DADOS
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

# 2012 -----

library(dplyr)
NHANES_12_t = NHANES_12 %>%
  mutate(PovertyGroup = case_when(
    Poverty < 1 ~ "0-0.9",
    Poverty < 2 ~ "1-1.9",
    Poverty < 3 ~ "2-2.9",
    Poverty < 4 ~ "3-3.9", 
    Poverty >= 4 ~ "4-",
    TRUE ~ NA
  ))

NHANES_12_t = subset(NHANES_12_t, !is.na(Poverty))
length(NHANES_12_t$Poverty) # n = 9274

NHANES_12_t = subset(NHANES_12_t, !is.na(HealthGen))
length(NHANES_12_t$HealthGen) # n = 7032

NHANES_12_t = subset(NHANES_12_t, !is.na(Depressed))
length(NHANES_12_t$Depressed) # n = 6230

NHANES_12_t = subset(NHANES_12_t, !is.na(LittleInterest))
length(NHANES_12_t$LittleInterest) # n = 6222

# 2023 ----- 

NHANES_23_t = NHANES_23 %>%
  mutate(INDFMMPI_txt = case_when(
    INDFMMPI < 1 ~ "0-0.9",
    INDFMMPI < 2 ~ "1-1.9",
    INDFMMPI < 3 ~ "2-2.9",
    INDFMMPI < 4 ~ "3-3.9", 
    INDFMMPI >= 4 ~ "4-",
    TRUE ~ NA
  ))

# Health
NHANES_23_t = NHANES_23_t %>%
  mutate(HUQ010_txt = case_when(
    HUQ010 == "1" ~ "Excellent",
    HUQ010 == "2" ~ "Vgood",
    HUQ010 == "3" ~ "Good",
    HUQ010 == "4" ~ "Fair", 
    HUQ010 == "5" ~ "Poor",
    TRUE ~ NA
  ))

# Depressed
NHANES_23_t = NHANES_23_t %>%
  mutate(DPQ020_txt = case_when(
    DPQ020 == "0" ~ "None",
    DPQ020 == "1" ~ "Several",
    DPQ020 == "2" ~ "Most",
    DPQ020 == "3" ~ "Most",
    TRUE ~ NA
  ))

# Little interest
NHANES_23_t = NHANES_23_t %>%
  mutate(DPQ010_txt = case_when(
    DPQ010 == "0" ~ "None",
    DPQ010 == "1" ~ "Several",
    DPQ010 == "2" ~ "Most",
    DPQ010 == "3" ~ "Most",
    TRUE ~ NA
  ))

NHANES_23_t = subset(NHANES_23_t, INDFMMPI <= 5)
length(NHANES_23_t$IND310) # n = 8989

NHANES_23_t = subset(NHANES_23_t, HUQ010 <= 5)
length(NHANES_23_t$HUQ010) # n = 8983

NHANES_23_t = subset(NHANES_23_t, DPQ020 <= 5)
length(NHANES_23_t$DPQ020) # n = 4428

NHANES_23_t = subset(NHANES_23_t, DPQ010 <= 5)
length(NHANES_23_t$DPQ010) # n = 4412