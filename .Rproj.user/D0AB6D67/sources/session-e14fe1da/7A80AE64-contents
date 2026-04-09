# ________________________________________
# COMPARAÇÃO: Saúde Mental x Pobreza
# 2012 x 2020
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

NHANES_c2a = data.frame(
  Depressao = NHANES$Depressed,
  Pobreza = NHANES$Poverty,
  Ano = "2012"
)

NHANES_c2b = data.frame(
  PInteresse = NHANES$LittleInterest,
  Pobreza = NHANES$Poverty,
  Ano = "2012"
)

##Consideramos os dois casos extremos de 2020 como um caso único de depressão severa, em consonância com a database de 2012.

library(dplyr)
NHANES_20_t4 = NHANES_20_t4 %>%
  mutate(DPQ020_txt = case_when(
    DPQ020 == "0" ~ "None",
    DPQ020 == "1" ~ "Several",
    DPQ020 == "2" ~ "Most",
    DPQ020 == "3" ~ "Most",
    TRUE ~ NA
  ))

NHANES_20_c2a = data.frame(
  Depressao = NHANES_20_t4$DPQ020_txt,
  Pobreza = NHANES_20_t4$INDFMMPI,
  Ano = "2020"
)

NHANES_20_t4 = NHANES_20_t4 %>%
  mutate(DPQ010_txt = case_when(
    DPQ010 == "0" ~ "None",
    DPQ010 == "1" ~ "Several",
    DPQ010 == "2" ~ "Most",
    DPQ010 == "3" ~ "Most",
    TRUE ~ NA
  ))

NHANES_20_c2b = data.frame(
  PInteresse = NHANES_20_t4$DPQ010_txt,
  Pobreza = NHANES_20_t4$INDFMMPI,
  Ano = "2020"
)

comparison2a = rbind(NHANES_c2a, NHANES_20_c2a)
comparison2a = na.omit(comparison2a)

comparison2b = rbind(NHANES_c2b, NHANES_20_c2b)
comparison2b = na.omit(comparison2b)

library(ggplot2)
ggplot(comparison2a, aes(x = Depressao, y = Pobreza, fill = Ano)) +
  geom_boxplot(alpha=0.6) +
  scale_fill_brewer(palette="Pastel2") +
  stat_summary(
    aes(color = Ano, group = Ano),
    fun = median,
    geom = "line",
    linetype = "dashed",
    linewidth = 0.8) +
  labs(
    x = "Índice de depressão",
    y = "Índice de pobreza (0 - mais pobre / 5 - mais rico)",
    fill = "Ano"
  ) +
  theme_minimal()

ggplot(comparison2b, aes(x = PInteresse, y = Pobreza, fill = Ano)) +
  geom_boxplot(alpha=0.6) +
  scale_fill_brewer(palette="Pastel3") +
  stat_summary(
    aes(color = Ano, group = Ano),
    fun = median,
    geom = "line",
    linetype = "dashed",
    linewidth = 0.8) +
  labs(
    x = "Nível de perda de interesse",
    y = "Índice de pobreza (0 - mais pobre / 5 - mais rico)",
    fill = "Ano"
  ) +
  theme_minimal()