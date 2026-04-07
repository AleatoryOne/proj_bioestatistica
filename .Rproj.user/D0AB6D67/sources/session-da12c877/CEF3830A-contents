# ________________________________________
# ANÁLISE TESTE 2
# IMC x Índice de Pobreza (2020)
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

# Poverty: "INDFMMPI"
# BMI: "BMXBMI" 

# Plotagem e análise preliminar -----
library(ggplot2)
ggplot(NHANES_20, aes(BMXBMI, INDFMMPI, color = BMXBMI_WHO)) + 
  geom_point() +
  theme_gray() +
  scale_color_brewer(type = "div", palette = "PuOr")

library(dplyr)
NHANES_20 = NHANES_20 %>%
  mutate(BMXBMI_WHO = case_when(
    BMXBMI < 18.5 ~ "12.0_18.5",
    BMXBMI < 25.0 ~ "18.5_to_24.9",
    BMXBMI < 30.0 ~ "25.0_to_29.9",
    BMXBMI >= 30.0 ~ "30.0_plus", 
    TRUE ~ NA
  ))

ggplot(NHANES_20, aes(BMXBMI_WHO, INDFMMPI, fill=BMXBMI_WHO)) + 
  geom_boxplot() +
  theme_gray() +
  scale_fill_brewer(palette = "PuOr") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "darkgreen")

cor(NHANES_20$BMXBMI, NHANES_20$INDFMMPI)

'| Não foi encontrada correlação entre o IMC e a saúde financeira em nenhum dos anos.'