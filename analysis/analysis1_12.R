# ________________________________________
# ANÁLISE TESTE 1
# Saúde Geral x Índice de Pobreza (2012)
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

# Plotagem e análise preliminar -----

library(ggplot2)
ggplot(NHANES_12_t, aes(HealthGen, Poverty, fill=HealthGen)) + 
  geom_boxplot() +
  theme_bw() +
  scale_fill_brewer(palette="PuOr") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "green") +
  stat_summary(fun = median, geom = "line", aes(group = 1), color ="blue") +
  labs(title = "Boxplot de saúde x índice de pobreza",
       subtitle = "(2009-2012)",
       x = "Saúde geral",
       y = 'Índice de pobreza') +
  theme(legend.position = "none")

'| Existe um padrão, geralmente pessoas com melhor saúde aparentam ter um menor nível de pobreza.'
'| Vamos verificar se isto é estatisticamente real.'

# Análise de variâncias -----
ggplot(NHANES_12_t, aes(sample=Poverty))+
  geom_qq() +
  geom_qq_line() + 
  facet_grid(~HealthGen)

modelo1 = aov(Poverty ~ HealthGen, data=NHANES_12)
res1 = residuals(modelo1)

#shapiro.test(res1)

'| O teste de Shapiro para normalidade dos resíduos não pode ser feito pois n >> 5000.'

hist(res1)

'| O histograma mostra claramente uma distribuição não normal.'

library(car)
leveneTest(Poverty ~ HealthGen, data = NHANES_12_t)

'| O teste de Levene para homogeneidade das variâncias mostra distribuição não homogênea.'

oneway.test(Poverty ~ HealthGen, data=NHANES_12_t, var.equal = FALSE)
kruskal.test(Poverty ~ HealthGen, data=NHANES_12_t)

# Análise por qui-quadrado (correlação renda-saúde) -----
summary(NHANES_12_t$HealthGen)

NHANES_12_table = table(NHANES_12_t$HealthGen, NHANES_12_t$PovertyGroup)
qui2 = chisq.test(NHANES_12_table, correct = TRUE)
qui2

qui2$stdres

'A partir do teste de qui quadrado, temos que há uma correlação, de forma que pessoas com menor poder aquisitivo, têm FORTE tendência a ter uma saúde mediana ou ruim. De igual forma, podemos explicitar que pessoas com boa condição de vida, em especial com renda superior a 75000 dólares, possuem FORTE tendência a terem saúde muito boa ou excelente.'
