# ________________________________________
# COMPARAÇÃO: Saúde x Pobreza
# 2012 x 2020
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

# Regressões lineares -----

# regr1 = lm(Poverty ~ HealthGen, data=NHANES_12) 
# regr2 = lm(INDFMMPI ~ as.factor(HUQ010), data=NHANES_23_t)
# 
# fit <- lm(Poverty ~ HealthGen, NHANES_12)
# fit_anova <- anova(fit)
# fit_anova

# Preparação dos dados -----

NHANES_c1 = data.frame(
  Saude = NHANES_12$HealthGen,
  Pobreza = NHANES_12$Poverty,
  Ano = "2012"
  )

library(dplyr)
NHANES_23_t = NHANES_23_t %>%
  mutate(HUQ010_txt = case_when(
    HUQ010 == "1" ~ "Excellent",
    HUQ010 == "2" ~ "Vgood",
    HUQ010 == "3" ~ "Good",
    HUQ010 == "4" ~ "Fair", 
    HUQ010 == "5" ~ "Poor",
    TRUE ~ NA
  ))

NHANES_20_c1 = data.frame(
  Saude = NHANES_23_t$HUQ010_txt,
  Pobreza = NHANES_23_t$INDFMMPI,
  Ano = "2023"
)

comparison = rbind(NHANES_c1, NHANES_20_c1)
comparison = na.omit(comparison)

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
  stat_summary(
    aes(color = Ano, group = Ano),
    fun = mean,
    geom = "line",
    linetype = "dotted",
    linewidth = 0.8) +
  labs(
    x = "Nível de saúde",
    y = "Índice de pobreza",
    fill = "Ano"
  ) +
  theme_minimal()
  
  