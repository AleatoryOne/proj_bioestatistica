# ________________________________________
# COMPARAÇÃO: Fatores socioeconômicos da saúde
# 2012 x 2020
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

# Preparação dos dados -----

library(dplyr)

## NHANES 2012

NHANES_c1 = NHANES %>%
  mutate(AgeGroups = case_when(
    Age < 20 ~ "<20",
    Age < 40 ~ "20-39",
    Age < 60 ~ "40-59",
    Age >= 60 ~ ">=60", 
    TRUE ~ NA
  ))

NHANES_c1 = NHANES_c1 %>%
  mutate(MarriageStatus = case_when(
    MaritalStatus == "Married" ~ "Married/Living with partner",
    MaritalStatus == "LivePartner" ~ "Married/Living with partner",
    MaritalStatus == "Widowed" ~ "Widowed/Divorced/Separated",
    MaritalStatus == "Divorced" ~ "Widowed/Divorced/Separated",
    MaritalStatus == "Separated" ~ "Widowed/Divorced/Separated",
    MaritalStatus == "NeverMarried" ~ "Never Married",
    TRUE ~ NA
  ))

## NHANES 2012

NHANES_20_t = subset(NHANES_20, HUQ010<=5)

NHANES_20_c1 = NHANES_20_t %>%
  mutate(HUQ010_txt = case_when(
    HUQ010 == "1" ~ "Excellent",
    HUQ010 == "2" ~ "Vgood",
    HUQ010 == "3" ~ "Good",
    HUQ010 == "4" ~ "Fair", 
    HUQ010 == "5" ~ "Poor",
    TRUE ~ NA
  ))

NHANES_20_c1 = NHANES_20_c1 %>%
  mutate(DMDHRAGZ_txt = case_when(
    DMDHRAGZ == "1" ~ "<20",
    DMDHRAGZ == "2" ~ "20-39",
    DMDHRAGZ == "3" ~ "40-59",
    DMDHRAGZ == "4" ~ ">=60", 
    TRUE ~ NA
  ))

NHANES_20_c1 = NHANES_20_c1 %>%
  mutate(RIAGENDR_txt = case_when(
    RIAGENDR == "1" ~ "male",
    RIAGENDR == "2" ~ "female",
    TRUE ~ NA
  ))

NHANES_20_c1 = NHANES_20_c1 %>%
  mutate(RIDRETH1_txt = case_when(
    RIDRETH1 == "1" ~ "Mexican",
    RIDRETH1 == "2" ~ "Hispanic",
    RIDRETH1 == "3" ~ "White",
    RIDRETH1 == "4" ~ "Black",
    RIDRETH1 == "5" ~ "Other",
    TRUE ~ NA
  ))

NHANES_20_c1 = NHANES_20_c1 %>%
  mutate(DMDMARTZ_txt = case_when(
    DMDMARTZ == "1" ~ "Married/Living with partner",
    DMDMARTZ == "2" ~ "Widowed/Divorced/Separated",
    DMDMARTZ == "3" ~ "Never Married",
    TRUE ~ NA
  ))

NHANES_comp = data.frame(
  Saude = NHANES_c1$HealthGen,
  Pobreza = NHANES_c1$Poverty,
  Idade = NHANES_c1$AgeGroups,
  Genero = NHANES_c1$Gender,
  Etnia = NHANES_c1$Race1,
  StatusCasam = NHANES_c1$MarriageStatus,
  Ano = "2012"
)

NHANES20_comp = data.frame(
  Saude = NHANES_20_c1$HUQ010_txt,
  Pobreza = NHANES_20_c1$INDFMMPI,
  Idade = NHANES_20_c1$DMDHRAGZ_txt,
  Genero = NHANES_20_c1$RIAGENDR_txt,
  Etnia = NHANES_20_c1$RIDRETH1_txt,
  StatusCasam = NHANES_20_c1$DMDMARTZ_txt,
  Ano = "2020"
)

comparison = rbind(NHANES_comp, NHANES20_comp)
comparison = subset(comparison, !is.na(Saude) | is.na(Idade))

library(ggplot2)library(ggplot2)Saude
ggplot(comparison, aes(x = Idade, y = Pobreza, fill = Ano)) +
  geom_boxplot(aes(shape = factor(Saude)), alpha=0.6) +
  scale_fill_brewer(palette="Pastel1") +
  stat_summary(
    aes(color = Ano, group = Ano),
    fun = median,
    geom = "line",
    linetype = "dashed",
    linewidth = 0.8) 
  theme_minimal()
