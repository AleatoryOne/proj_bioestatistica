# ________________________________________
# ANÁLISE TESTE 1
# Saúde Geral x Índice de Pobreza (2012)
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

# Plotagem e análise preliminar -----
library(ggplot2)
ggplot(NHANES, aes(HealthGen, Poverty, fill=HealthGen)) + 
  geom_boxplot() +
  theme_gray() +
  scale_fill_brewer(palette="PuOr") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "darkgreen")

'| Existe um padrão, geralmente pessoas com melhor saúde aparentam ter um menor nível de pobreza.
 | Vamos verificar se isto é estatisticamente real.'

# Análise de variâncias -----
ggplot(NHANES, aes(sample=Poverty))+
  geom_qq() +
  geom_qq_line() + 
  facet_grid(~HealthGen)

modelo1 = aov(Poverty ~ HealthGen, data=NHANES)
res1 = residuals(modelo1)

#shapiro.test(res1)

'| O teste de Shapiro para normalidade dos resíduos não pode ser feito pois n >> 5000.'

hist(res1)

'| O histograma mostra claramente uma distribuição não normal.'

library(car)
leveneTest(Poverty ~ HealthGen, data = NHANES)

'| O teste de Levene para homogeneidade das variâncias mostra distribuição não homogênea.'

oneway.test(Poverty ~ HealthGen, data=NHANES, var.equal = FALSE)
kruskal.test(Poverty ~ HealthGen, data=NHANES)

'| O p-valor do teste de Kruskal-Wallis, somado ao teste de ANOVA de Welch, indica que a visualização que obtemos faz sentido estatisticamente, isto é, que existe algum nível de diferença significante entre os grupos.'