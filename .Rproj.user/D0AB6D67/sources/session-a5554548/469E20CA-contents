# ________________________________________
# ANÁLISE TESTE 2
# IMC x Índice de Pobreza (2012)
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

# Plotagem e análise preliminar -----
library(ggplot2)
ggplot(NHANES, aes(BMI, Poverty, color = BMI_WHO)) + 
  geom_point() +
  theme_gray() +
  scale_color_brewer(type = "div", palette = "PuOr")

ggplot(NHANES, aes(BMI_WHO, Poverty, fill=BMI_WHO)) + 
  geom_boxplot() +
  theme_gray() +
  scale_fill_brewer(palette = "PuOr") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "darkgreen")

cor(NHANES$BMI, NHANES$Poverty)

'| Não foi encontrada correlação entre o IMC e a saúde financeira, ao menos não em 2012.'