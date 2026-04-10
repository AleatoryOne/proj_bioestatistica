# ________________________________________
# ANÁLISE TESTE 1.1
# Saúde Geral x Índice de Pobreza (2023)
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

# Poverty: "INDFMMPI"
# HealthGen: "HUQ010"

# Filtragem -----
NHANES_23_t = subset(NHANES_23, INDFMMPI<=5 & HUQ010<=5)

'| São desprezados os dados que de indivíduos que não sabem/não responderam (scores 7 e 9).'

# Plotagem e análise preliminar -----
library(ggplot2)
ggplot(NHANES_23_t, aes(reorder(HUQ010_txt, HUQ010), INDFMMPI, fill=reorder(HUQ010_txt, HUQ010))) + 
  geom_boxplot() +
  theme_gray() +
  scale_fill_brewer(palette="PuOr") +  
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "green") +
  stat_summary(fun = median, geom = "line", aes(group = 1), color ="blue") +
  labs(x="Saúde geral",
       y='Índice de pobreza',
       fill='Saúde')

'| O padrão não aparenta ser tão claro como era 8 anos atrás. Ainda há uma leve relação entre riqueza e melhor saúde, mas houve uma melhora: os níveis de saúde estão mais equilibrados.
 | Vamos analisar estatisticamente.'

# Análise de variâncias -----
ggplot(NHANES_23_t, aes(sample=INDFMMPI))+
  geom_qq() +
  geom_qq_line() + 
  facet_grid(~as.factor(HUQ010))

modelo1.1 = aov(INDFMMPI ~ as.factor(HUQ010), data=NHANES_23_t)
res1.1 = residuals(modelo1.1)

#shapiro.test(res1.1)

'| O teste de Shapiro para normalidade dos resíduos não pode ser feito pois n >> 5000.'

hist(res1.1)

'| O histograma dos resíduos não é normal, mas aparenta mais normal que em 2012.'

library(car)
leveneTest(INDFMMPI ~ as.factor(HUQ010), data=NHANES_23_t)

'| Novamente, o teste de Levene para homogeneidade das variâncias mostra distribuição não homogênea.'

oneway.test(INDFMMPI ~ as.factor(HUQ010), data=NHANES_23_t, var.equal = FALSE)
kruskal.test(INDFMMPI ~ as.factor(HUQ010), data=NHANES_23_t)

'| O p-valor do teste de Kruskal-Wallis, somado ao teste de ANOVA de Welch, indica que, mesmo passados quase dez anos, ainda existe algum nível de diferença significante entre os grupos.'
