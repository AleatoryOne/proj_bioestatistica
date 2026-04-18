# ________________________________________
# ANÁLISE INICIAL
# Análises iniciais (2021-23)
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

# Gráfico de saúde ----- 

library(ggplot2)
ggplot(NHANES_23_t, aes(reorder(HUQ010_txt, HUQ010), fill=reorder(HUQ010_txt, HUQ010))) + 
  geom_bar(color='black') +
  theme_bw() +
  scale_fill_brewer(palette="PuOr") +
  labs(title = "Gráfico de indivíduos com determinado nível de saúde",
       subtitle = "(2021-2023)",
       x = "Saúde geral",
       y = 'Número de indivíduos') +
  theme(legend.position = "none")

library(summarytools)
ctable(NHANES_23_t$HUQ010_txt, NHANES_23_t$HUQ010_txt,
       prop = "r", headings=FALSE)

ctable(NHANES_23_t$HUQ010_txt, NHANES_23_t$INDFMMPI_txt,
       prop = 'c', useNA = 'no', headings=FALSE)

# Gráficos de saúde mental -----

ggplot(NHANES_23_t, aes(reorder(DPQ020_txt, DPQ020), fill=reorder(DPQ020_txt, DPQ020))) + 
  geom_bar(color='black') +
  theme_bw() +
  scale_fill_brewer(palette="YlGnBu") +
  labs(title = "Gráfico de indivíduos com indícios de depressão",
       subtitle = "(2021-2023)",
       x = 'Proporção de dias com sintomas de depressão',
       y = 'Número de indivíduos') +
  theme(legend.position = "none")

ctable(NHANES_23_t$DPQ020_txt, NHANES_23_t$DPQ020_txt,
       prop = "r", headings=FALSE)

ctable(NHANES_23_t$DPQ020_txt, NHANES_23_t$INDFMMPI_txt,
       prop = 'c', useNA = 'no', headings=FALSE)

ggplot(NHANES_23_t, aes(reorder(DPQ010_txt, DPQ010), fill=reorder(DPQ010_txt, DPQ010))) + 
  geom_bar(color='black') +
  theme_bw() +
  scale_fill_brewer(palette="YlGnBu") +
  labs(title = "Gráfico de indivíduos com perda de interesse",
       subtitle = "(2021-2023)",
       x = 'Proporção de dias com perda de interesse',
       y = 'Número de indivíduos') +
  theme(legend.position = "none")

ctable(NHANES_23_t$DPQ010_txt, NHANES_23_t$DPQ010_txt,
       prop = "r", headings=FALSE)

ctable(NHANES_23_t$DPQ010_txt, NHANES_23_t$INDFMMPI_txt,
       prop = 'c', useNA = 'no', headings=FALSE)

# Gráfico de pobreza -----

ggplot(NHANES_23_t, aes(y=INDFMMPI)) + 
  geom_boxplot(fill='lightgreen') +
  theme_bw() +
  scale_fill_brewer(palette="PuOr") +
  geom_hline(yintercept = mean(NHANES_23_t$INDFMMPI), color = "purple", linetype = "dashed") +
  labs(title = "Boxplot de índice de pobreza",
       subtitle = "(2021-2023)",
       y = "Índice de pobreza") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

summary(NHANES_23_t$INDFMMPI)

# Média e mediana populacional de pobreza (2023) -----

mediana_am_23 = median(NHANES_23_t$INDFMMPI)
media_am_23 = mean(NHANES_23_t$INDFMMPI)
desv_pad_23 = sd(NHANES_23_t$INDFMMPI)
n_23 = length(NHANES_23_t$INDFMMPI)
erro_pad_23 = desv_pad_23/sqrt(n_23)

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
