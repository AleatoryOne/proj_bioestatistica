# ________________________________________
# ANÁLISE TESTE 3
# Saúde Mental x Índice de Pobreza (2012)
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

# Plotagem e análise preliminar -----
library(ggplot2)
ggplot(NHANES, aes(Depressed, Poverty, fill=Depressed)) + 
  geom_boxplot() +
  theme_gray() +
  scale_fill_brewer(palette="BrBG") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "darkred")

ggplot(NHANES, aes(LittleInterest, Poverty, fill=LittleInterest)) + 
  geom_boxplot() +
  theme_gray() +
  scale_fill_brewer(palette="BrBG") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "darkred")

'| Nota-se um padrão, geralmente pessoas com menor poder econômico têm maior tendência a ter depressão e perda de interesse.'
'| Podemos analisar para saber se isso é estatísticamente real.'

# Análise de variâncias -----
ggplot(NHANES, aes(sample=Poverty))+
  geom_qq() +
  geom_qq_line() +
  facet_grid(~Depressed)

ggplot(NHANES, aes(sample=Poverty))+
  geom_qq() +
  geom_qq_line() +
  facet_grid(~LittleInterest)

modelo3a = aov(Poverty ~ Depressed, data=NHANES)
res3a = residuals(modelo3a)

modelo3b = aov(Poverty ~ LittleInterest, data=NHANES)
res3b = residuals(modelo3b)

par(mfrow=c(1,2))
hist(res3a) #Depressão
hist(res3b) #Ausência de interesse

'| Nenhuma das distribuições é normal.'
'| Nota-se também que elas são bastante parecidas. Isso pode implicar uma relação intricada entre depressão e perda de interesse.'

library(car)
leveneTest(Poverty ~ Depressed, data = NHANES) #Depressão
leveneTest(Poverty ~ LittleInterest, data = NHANES) #Ausência de interesse

'| O teste de Levene para homogeneidade das variâncias muito inferior que p = 0.05 mostra distribuição não homogênea.'
'| Utilizamos testes alternativos para verificar a variância.'

## Depressão
oneway.test(Poverty ~ Depressed, data=NHANES, var.equal = FALSE)
kruskal.test(Poverty ~ Depressed, data=NHANES)

'| O p-valor do teste de Kruskal-Wallis, somado ao teste de ANOVA de Welch, indica que a visualização que obtemos faz sentido estatisticamente, isto é, que existe algum nível de diferença significante entre os grupos.'

## Ausência de interesse
oneway.test(Poverty ~ LittleInterest, data=NHANES, var.equal = FALSE)
kruskal.test(Poverty ~ LittleInterest, data=NHANES)

'| De igual forma, o p-valor muito inferior ao nível de significância implica na existência de diferença significante ente os grupos.'