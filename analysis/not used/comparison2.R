# ________________________________________
# COMPARAÇÃO: Índices de Saúde
# 2012 x 2020
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

# Preparação dos dados -----

NHANES_12_f = data.frame(
  Saude = NHANES_12_t$HealthGen,
  Depressao = NHANES_12_t$Depressed,
  PerdaInteresse = NHANES_12_t$LittleInterest,
  Ano = "2012"
  )

NHANES_23_f = data.frame(
  Saude = NHANES_23_t$HUQ010_txt,
  Depressao = NHANES_23_t$DPQ020_txt,
  PerdaInteresse = NHANES_23_t$DPQ010_txt,
  Ano = "2023"
)

comparison1 = rbind(NHANES_12_f, NHANES_23_f)

# Comparação da saúde -----

library(ggplot2)
ggplot(comparison1, aes(x = Saude, fill = Ano)) +
  geom_bar(color='black', position="dodge", 
           aes(y = after_stat(prop), group = Ano)) +
  theme_bw() +
  scale_fill_brewer(palette="Set2") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Gráfico de níveis de saúde ao longo dos anos",
    x = "Nível de saúde",
    y = "Porcentagem de indivíduos",
    fill = "Ano\nde pesquisa"
  )

# Comparação da saúde mental -----

library(ggplot2)
ggplot(comparison1, aes(x = Depressao, fill = Ano)) +
  geom_bar(color='black', position="dodge", 
           aes(y = after_stat(prop), group = Ano)) +
  theme_bw() +
  scale_fill_brewer(palette="Set3") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Gráfico de níveis de indícios de depressão ao longo dos anos",
    x = "Proporção de dias com sintomas de depressão",
    y = "Porcentagem de indivíduos",
    fill = "Ano\nde pesquisa"
  )

library(ggplot2)
ggplot(comparison1, aes(x = PerdaInteresse, fill = Ano)) +
  geom_bar(color='black', position="dodge", 
           aes(y = after_stat(prop), group = Ano)) +
  theme_bw() +
  scale_fill_brewer(palette="Set3") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Gráfico de níveis de perda de interesse ao longo dos anos",
    x = "Proporção de dias com perda de interesse",
    y = "Porcentagem de indivíduos",
    fill = "Ano\nde pesquisa"
  )
