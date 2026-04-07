# ________________________________________
# ANÁLISE TESTE 1.1
# Saúde Mental x Índice de Pobreza (2020)
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

# Poverty: "INDFMMPI"
# Depression: "DPQ020"
# LittleInterest: "DPQ010"

# Filtragem -----
NHANES_20_t4 = subset(NHANES_20, INDFMMPI<=5 & DPQ020<=5 & DPQ010<=5)

'| São desprezados os dados que de indivíduos que não sabem/não responderam (scores 7 e 9).'

# Plotagem e análise preliminar -----
library(ggplot2)
ggplot(NHANES_20_t4, aes(as.factor(DPQ020), INDFMMPI, fill=as.factor(DPQ020))) + 
  geom_boxplot() +
  theme_gray() +
  scale_fill_brewer(palette="BrBG") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "purple") +
  labs(x="Perceived mental health level (0 = no depression / 3 = strong depression)")

'| A priori, percebe-se a presença de um *quarto* nível de avaliação na escala.'
'| Não obstante, o padrão se preserva, isto é, pessoas mais pobres aparentam apresentar maior nível de depressão.'
'| Vamos analisar a ausência de interesse também.'

ggplot(NHANES_20_t4, aes(as.factor(DPQ010), INDFMMPI, fill=as.factor(DPQ010))) + 
  geom_boxplot() +
  theme_gray() +
  scale_fill_brewer(palette="BrBG") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "purple") +
  labs(x="Level of lack of interest in doing things")

# Análise de variâncias -----
ggplot(NHANES_20_t4, aes(sample=INDFMMPI))+
  geom_qq() +
  geom_qq_line() + 
  facet_grid(~as.factor(DPQ020))

modelo3a.1 = aov(INDFMMPI ~ as.factor(DPQ020), data=NHANES_20_t4)
res3a.1 = residuals(modelo3a.1)

modelo3b.1 = aov(INDFMMPI ~ as.factor(DPQ010), data=NHANES_20_t4)
res3b.1 = residuals(modelo3b.1)

hist(res3a.1)
hist(res3b.1)

'| O histograma dos resíduos dos casos mostram distribuições bem distintas da normal.'
'| Note que, desta vez, as distribuições, embora ainda sejam parecidas, não são idênticas.'

library(car)
leveneTest(INDFMMPI ~ as.factor(DPQ010), data=NHANES_20_t4) #Ausência de interesse
leveneTest(INDFMMPI ~ as.factor(DPQ020), data=NHANES_20_t4) #Depressão

'| O teste de Levene para homogeneidade das variâncias mostra que nenhuma das distribuições apresentam variâncias homogêneas.'

## Depressão
oneway.test(INDFMMPI ~ as.factor(DPQ020), data=NHANES_20_t4, var.equal = FALSE)
kruskal.test(INDFMMPI ~ as.factor(DPQ020), data=NHANES_20_t4)

## Ausência de interesse
oneway.test(INDFMMPI ~ as.factor(DPQ010), data=NHANES_20_t4, var.equal = FALSE)
kruskal.test(INDFMMPI ~ as.factor(DPQ010), data=NHANES_20_t4)

'| Os testes de Kruskal-Wallis e de Welch indicam diferença significativa entre os grupos.'