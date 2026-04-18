# ________________________________________
# ANÁLISE TESTE 4
# 
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

# Poverty: "INDFMMPI"
# HH Income: "IND310"
# Health Insurance: "HIQ011" 
# Routine Place Exists: "HUQ030"
# What Place is it: "HUQ042"
# Went to Physician: "HUQ055"
# Went to Psychiatrist: "HUQ090

# Análise A: Plano de saúde -----
NHANES_23_t = subset(NHANES_23, IND310<=5 & HIQ011 <=5)

library(ggplot2)
ggplot(NHANES_23_t, aes(HIQ011, INDFMMPI, group=HIQ011)) + 
  geom_boxplot() +
  theme_gray()

'| A ampla maioria dos que não têm plano de saúde possuem índice de pobreza entre 1 e 2.'

ggplot(NHANES_23_t, aes(HIQ011, IND310, group=HIQ011)) + 
  geom_boxplot() +
  theme_gray()

'| Praticamente TODO o percentil principal (excluindo outliers) que não possui plano de saúde ganha menos de $3000 de patrimônio no momento da pesquisa.'

# Análise B: Visita regularmente instituição médica -----
NHANES_23_t = subset(NHANES_20, IND310<=5 & HUQ030<=5)

ggplot(NHANES_23_t, aes(IND310, fill=as.factor(HUQ030))) + 
  geom_bar(aes(y = after_stat(count / tapply(count, x, sum)[x])),
           position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Percentage of people") +
  theme_gray()


# Análise C: Qual instituição médica visita mais -----
NHANES_23_t = subset(NHANES_20, IND310<=5 & HUQ042<=6)

library(dplyr)
NHANES_23_t = NHANES_23_t %>%
  mutate(IND310_txt = case_when(
    IND310 == "1" ~ "< 3K",
    IND310 == "2" ~ "3K-5K",
    IND310 == "3" ~ "5K-10K",
    IND310 == "4" ~ "10K-15K", 
    IND310 == "5" ~ "< 15K",
    TRUE ~ "Nao sabe/nao respondeu"
  ))

NHANES_23_t = NHANES_23_t %>%
  mutate(HUQ042_txt = case_when(
    HUQ042 == "1" ~ "Hospital/consultorio",
    HUQ042 == "2" ~ "Farmacia",
    HUQ042 == "3" ~ "Emergencia",
    HUQ042 == "4" ~ "Sist. saude ao veteranos de guerra", 
    HUQ042 == "5" ~ "Outro",
    HUQ042 == "6" ~ "Nenhum",
    TRUE ~ "Nao sabe/nao respondeu"
  ))

ggplot(NHANES_23_t, aes(reorder(IND310_txt, IND310), fill=HUQ042_txt)) + 
  geom_bar(aes(y = after_stat(count / tapply(count, x, sum)[x])),
           position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Percentage of people") +
  theme_gray()

'| A VASTA maioria das pessoas vai a hospitais e consultórios com frequência.'
'| O único conjunto em que existe uma porcentagem que afirma NÃO IR a um consultório ou semelhantes é a com menor poder aquisitivo.'

# Análise D: Teve teleconsulta no último ano -----
NHANES_23_t = subset(NHANES_20, IND310<=5 & HUQ055<=5)

NHANES_23_t = NHANES_23_t %>%
  mutate(IND310_txt = case_when(
    IND310 == "1" ~ "< 3K",
    IND310 == "2" ~ "3K-5K",
    IND310 == "3" ~ "5K-10K",
    IND310 == "4" ~ "10K-15K", 
    IND310 == "5" ~ "< 15K",
    TRUE ~ "Nao sabe/nao respondeu"
  ))

NHANES_23_t = NHANES_23_t %>%
  mutate(HUQ055_txt = case_when(
    HUQ055 == "1" ~ "Sim",
    HUQ055 == "2" ~ "Não",
    TRUE ~ "Nao sabe/nao respondeu"
  ))

ggplot(NHANES_23_t, aes(reorder(IND310_txt, IND310), fill=HUQ055_txt)) + 
  geom_bar(aes(y = after_stat(count / tapply(count, x, sum)[x])),
           position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Percentage of people") +
  theme_gray()

'| Não há um padrão de quem possui mais dinheiro vai mais ao médico.'

# Análise E: Foi ao psiquiatra no último ano -----
NHANES_23_t = subset(NHANES_20, IND310<=5 & HUQ090<=5)

NHANES_23_t = NHANES_23_t %>%
  mutate(IND310_txt = case_when(
    IND310 == "1" ~ "< 3K",
    IND310 == "2" ~ "3K-5K",
    IND310 == "3" ~ "5K-10K",
    IND310 == "4" ~ "10K-15K", 
    IND310 == "5" ~ "< 15K",
    TRUE ~ "Nao sabe/nao respondeu"
  ))

NHANES_23_t = NHANES_23_t %>%
  mutate(HUQ090_txt = case_when(
    HUQ090 == "1" ~ "Sim",
    HUQ090 == "2" ~ "Não",
    TRUE ~ "Nao sabe/nao respondeu"
  ))

ggplot(NHANES_23_t, aes(reorder(IND310_txt, IND310), fill=HUQ090_txt)) + 
  geom_bar(aes(y = after_stat(count / tapply(count, x, sum)[x])),
           position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Percentage of people") +
  theme_gray()

'| Novamente, não há um padrão de quem possui mais dinheiro vai mais ao psiquiatra com mais frequência.'