# ________________________________________
# ANÁLISE TESTE 5
# Índice de Pobreza real (2012-23)
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

# Média e mediana populacional de pobreza (2012) -----

mediana_am_12 = median(na.omit(NHANES_12$Poverty))
media_am_12 = mean(na.omit(NHANES_12$Poverty))
desv_pad_12 = sd(na.omit(NHANES_12$Poverty))
n_12 = length(na.omit(NHANES_12$Poverty))
erro_pad_12 = desv_pad/sqrt(n)

c("Mediana:" = mediana_am_12,
  "Média:" = media_am_12,
  "Desvio padrão:" = desv_pad_12,
  "Erro padrão:" = erro_pad_12,
  "Amostras:" = n_12)

alpha <- 0.05
z <- qnorm(1 - alpha/2)

IC_inferior_12 = media_am_12 - z * erro_pad_12
IC_superior_12 = media_am_12 + z * erro_pad_12

cat("Intervalo de confiança: [", IC_inferior_12, ",", IC_superior_12, "]")

# Média e mediana populacional de pobreza (2023) -----

mediana_am_23 = median(na.omit(NHANES_23$INDFMMPI))
media_am_23 = mean(na.omit(NHANES_23$INDFMMPI))
desv_pad_23 = sd(na.omit(NHANES_23$INDFMMPI))
n_23 = length(na.omit(NHANES_23$INDFMMPI))
erro_pad_23 = desv_pad/sqrt(n)

c("Mediana:" = mediana_am_23,
  "Média:" = media_am_23,
  "Desvio padrão:" = desv_pad_23,
  "Erro padrão:" = erro_pad_23,
  "Amostras:" = n_23)

alpha <- 0.05
z <- qnorm(1 - alpha/2)

IC_inferior_23 = media_am_23 - z * erro_pad_23
IC_superior_23 = media_am_23 + z * erro_pad_23

cat("Intervalo de confiança: [", IC_inferior_23, ",", IC_superior_23, "]")
