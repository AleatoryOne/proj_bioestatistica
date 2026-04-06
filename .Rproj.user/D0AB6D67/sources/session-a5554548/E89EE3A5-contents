# ________________________________________
# COMPARAÇÃO: Saúde x Pobreza
# 2012 x 2020
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

# Regressões lineares -----

regr1 = lm(Poverty ~ HealthGen, data=NHANES) 
regr2 = lm(INDFMMPI ~ as.factor(HUQ010), data=NHANES_20_t1)

fit <- lm(Poverty ~ HealthGen, NHANES)
fit_anova <- anova(fit)
fit_anova

# Preparação dos dados -----

NHANES_c1 = data.frame(
  Saude = NHANES$HealthGen,
  Pobreza = NHANES$Poverty,
  base = "NHANES"
  )

library(dplyr)
NHANES_20_t1 = NHANES_20_t1 %>%
  mutate(HUQ010_txt = case_when(
    HUQ010 == "1" ~ "Excellent",
    HUQ010 == "2" ~ "Vgood",
    HUQ010 == "3" ~ "Good",
    HUQ010 == "4" ~ "Fair", 
    HUQ010 == "5" ~ "Poor",
    TRUE ~ NA
  ))

NHANES_20_c1 = data.frame(
  Saude = NHANES_20_t1$HUQ010_txt,
  Pobreza = NHANES_20_t1$INDFMMPI,
  base = "NHANES_20_t1"
)

comparison = rbind(NHANES_c1, NHANES_20_c1)
comparison = na.omit(comparison)

comparison = comparison %>%
  mutate(Ano = case_when(
    base == "NHANES" ~ "2012",
    base == "NHANES_20_t1" ~ "2020",
  ))

# Comparação dos dados -----

library(ggplot2)
ggplot(comparison, aes(x = Saude, y = Pobreza, fill = Ano)) +
  geom_boxplot(alpha=0.6) +
  scale_fill_brewer(palette="Pastel1") +
  stat_summary(
    aes(color = Ano, group = Ano),
    fun = median,
    geom = "line",
    linetype = "dashed",
    linewidth = 0.8) +
  labs(
    x = "Nível de saúde",
    y = "Índice de pobreza",
    fill = "Ano"
  ) +
  theme_minimal()
  
  