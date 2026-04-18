# ________________________________________
# ANÁLISE TESTE 1.1
# Saúde Geral x Índice de Pobreza (2023)
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

# Plotagem e análise preliminar -----

library(ggplot2)
ggplot(NHANES_23_t, aes(reorder(HUQ010_txt, HUQ010), INDFMMPI, fill=reorder(HUQ010_txt, HUQ010))) + 
  geom_boxplot() +  
  theme_bw() +
  scale_fill_brewer(palette="PuOr") +  
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "green") +
  stat_summary(fun = median, geom = "line", aes(group = 1), color ="blue") +
  labs(title = "Boxplot de saúde x índice de pobreza",
       subtitle = "(2021-2023)",
       x = "Saúde geral",
       y = 'Índice de pobreza') +
  theme(legend.position = "none")

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

# Análise por qui-quadrado (correlação renda-saúde) -----
NHANES_23_t$HUQ010_txt = as.factor(NHANES_23_t$HUQ010_txt)
NHANES_23_t$HUQ010_txt <- factor(NHANES_23_t$HUQ010_txt,
                                 levels = c("Excellent",
                                         "Vgood",
                                         "Good",
                                         "Fair",
                                         "Poor"))

NHANES_23_table = table(NHANES_23_t$INDFMMPI_txt, NHANES_23_t$HUQ010_txt)
qui2_23 = chisq.test(NHANES_23_table, correct = TRUE)

qui2_23
qui2_23$stdres

'A partir do teste de qui quadrado, temos que, a despeito da queda generalizada no nível de saúde \"excelente\", temos uma correlação entre o poder aquisitivo e o nível de saúde, tal que indivíduos com menor poder aquisitivo, têm tendência a ter uma saúde mediana ou ruim. De semelhante forma, pessoas com maior poder aquisitivo têm tendência a ter uma saúde muito boa, e uma tendência levemente menor (mas ainda relevante, z = 2.38), a ter uma saúde excelente.'