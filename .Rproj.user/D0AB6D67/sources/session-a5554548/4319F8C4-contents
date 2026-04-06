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

# Análise: Plano de saúde -----
NHANES_20_t3 = subset(NHANES_20, IND310<=5 & HIQ011 <=5)

ggplot(NHANES_20_t3, aes(HIQ011, INDFMMPI, group=HIQ011)) + 
  geom_boxplot() +
  theme_gray()

'| A ampla maioria dos que não têm plano de saúde possuem índice de pobreza entre 1 e 2.'

ggplot(NHANES_20_t3, aes(HIQ011, IND310, group=HIQ011)) + 
  geom_boxplot() +
  theme_gray()

'| Praticamente TODO o percentil principal (excluindo outliers) que não possui plano de saúde ganha menos de $3000 de patrimônio no momento da pesquisa.'

# Análise: Visita regularmente instituição médica -----
NHANES_20_t3 = subset(NHANES_20, IND310<=5 & HUQ030<=5)

ggplot(NHANES_20_t3, aes(IND310, fill=as.factor(HUQ030))) + 
  geom_bar(aes(y = after_stat(count / tapply(count, x, sum)[x])),
           position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Percentage of people") +
  theme_gray()
