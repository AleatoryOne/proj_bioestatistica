# ________________________________________
# ANÁLISE TESTE 1.2
# Saúde Geral x Índice de Pobreza (2012-23)
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

# Quais idades possuem maior índice de pobreza (2012)? -----

library(ggplot2)
ggplot(NHANES_12_t, aes(HealthGen, Poverty, fill=AgeDecade)) + 
  geom_boxplot() +
  theme_bw() +
  scale_fill_brewer(palette="Spectral") +
  #stat_summary(fun = mean, geom = "line", aes(group = 1), color = "green") +
  #stat_summary(fun = median, geom = "line", aes(group = 1), color ="blue") +
  #facet_wrap(~AgeDecade) +
  labs(title='Análise de saúde geral x índice de pobreza',
       subtitle='Agrupado por faixa de idade (2009-2012)',
       x='Saúde geral',
       y='Índice de pobreza',
       fill='Idades')

ggplot(NHANES_12_t, aes(HealthGen, Poverty, fill=HealthGen)) + 
  geom_boxplot() +
  theme_bw() +
  scale_fill_brewer(palette="PuOr") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "green") +
  stat_summary(fun = median, geom = "line", aes(group = 1), color ="blue") +
  facet_wrap(~AgeDecade) +
  labs(title='Análise de saúde geral x índice de pobreza',
       subtitle='Agrupado por faixa de idade (2009-2012)',
       x='Idades',
       y='Índice de pobreza',
       fill='Saúde') +
  theme(legend.position = "none")

'| Percebe-se, em primeira análise, quase que imediatamente, que conforme a idade aumenta, apenas os mais ricos possuem saúde Excelente ou Muito Boa, até passarmos dos 70 anos, em que a saúde de todas as classses declina.'
'| O pior índice de saúde apenas se manifesta entre 10 e 29 anos para as classes >=3.'

# Quais gêneros possuem maior índice de pobreza (2012)? -----

library(ggplot2)
ggplot(NHANES_12_t, aes(HealthGen, Poverty, fill=Gender)) + 
  geom_boxplot() +
  theme_bw() +
  scale_fill_brewer(palette="Set1") +
  stat_summary(fun = median, geom = "line", 
               aes(group = Gender, color=Gender)) +
  labs(title='Análise de saúde geral x índice de pobreza',
       subtitle='Agrupado por gênero (2009-2012)',
       x='Saúde geral',
       y='Índice de pobreza',
       fill='Saúde')

# Quais etnias possuem maior índice de pobreza (2012)? -----

ggplot(NHANES_12_t, aes(HealthGen, Poverty, fill=Race1)) + 
  geom_boxplot() +
  theme_bw() +
  scale_fill_brewer(palette="YlOrBr") +
  #stat_summary(fun = median, geom = "line", aes(group = Race1, color=HealthGen)) +
  labs(title='Análise de saúde geral x índice de pobreza',
       subtitle='Separado por etnias (2009-2012)',
       x='Saúde geral',
       y='Índice de pobreza',
       fill='Etnia') +
  theme(legend.position = "none")

ggplot(NHANES_12_t, aes(HealthGen, Poverty, fill=HealthGen)) + 
  geom_boxplot() +
  theme_bw() +
  scale_fill_brewer(palette="PuOr") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "green") +
  stat_summary(fun = median, geom = "line", aes(group = 1), color ="blue") +
  facet_wrap(~Race1) +
  labs(title='Análise de saúde geral x índice de pobreza',
       subtitle='Agrupado por etnia (2009-2012)',
       x='Idades',
       y='Índice de pobreza',
       fill='Saúde') +
  theme(legend.position = "none")

'| Podemos analisar com maior profundidade a relação etnia-pobreza e etnia-saúde.'

plot(NHANES_12_t$Race1, NHANES_12_t$HealthGen)

ggplot(NHANES_12_t, aes(Gender, Poverty, fill=Gender)) + 
  geom_boxplot() +
  theme_bw() +
  scale_fill_brewer(palette="Set1") +
  stat_summary(fun = median, geom = "line", aes(group=Gender, color=Race1)) +
  labs(title='Análise de gênero x índice de pobreza',
       subtitle='(2009-2012)',
       x='Gêneros biológicos',
       y='Índice de pobreza') +
  theme(legend.position = "none")

ggplot(NHANES_12_t, aes(Gender, Poverty, fill=Race1)) + 
  geom_boxplot() +
  theme_bw() +
  scale_fill_brewer(palette="YlOrBr") +
  stat_summary(fun = median, geom = "line", aes(group=Gender, color=Race1)) +
  labs(title='Análise de gênero x índice de pobreza',
       subtitle='Agrupado por etnia (2009-2012)',
       x='Gêneros biológicos',
       y='Índice de pobreza',
       fill='Etnia')

'| Existe uma leve disparidade de renda entre os gêneros, e uma ENORME disparidade de renda entre as etnias.'
'| Podemos explorar se a disparidade de renda entre gêneros é estatísticamente significante.'

# Quão significante é a disparidade de renda entre os gêneros (2012)? -----

ggplot(NHANES_12_t, aes(Race1, Poverty, fill=Gender)) + 
  geom_boxplot() +
  theme_bw() +
  scale_fill_brewer(palette="Set1") +
  #stat_summary(fun = median, geom = "line", aes(group=Gender, color=Race1))
  labs(title='Análise de gênero x índice de pobreza',
       subtitle='Agrupado por gênero (2009-2012)',
       x='Etnias',
       y='Índice de pobreza',
       fill='Gênero')

'| À primeira análise, essa disparidade praticamente apenas existe entre pessoas brancas.'

ggplot(NHANES_12_t, aes(Gender, Poverty, fill=MaritalStatus)) + 
  geom_boxplot() +
  theme_gray() +
  scale_fill_brewer(palette="YlOrBr") +
  stat_summary(fun = median, geom = "line", aes(group=Gender, color=Race1))

'| A maioria das mulheres que têm mesma renda que os homens estão casadas ou em união estável, implicando que a renda exibida é a renda da casa.'
'| Dentre as pessoas solteiras, as mulheres costumam receber menos que os homens.'
'| Vamos analisar se a diferença é estatisticamente significativa.'

ggplot(NHANES_12_t, aes(sample=Poverty))+
  geom_qq() +
  geom_qq_line() + 
  facet_grid(~Gender)

'| Havendo dois grupos, podemos fazer um teste-t.'

if(!require(RVAideMemoire)) install.packages("RVAideMemoire")
library(RVAideMemoire)

byf.shapiro(Poverty ~ Gender, NHANES_12_t)
leveneTest(Poverty ~ Gender, NHANES_12_t)#, center=mean)

t.test(Poverty ~ Gender, NHANES_12_t, var.equal=TRUE)

'| O p-valor menor que 0.05 indica que, ao nível de 95% de certeza, existe variação significativa entre a renda de homens e mulheres.'
'| Mas esse dado, embora muito relevante, foge um pouco do escopo do trabalho.'
'| ENTRETANTO...'

ggplot(NHANES_12_t, aes(HealthGen, Poverty, fill=Gender)) + 
  geom_boxplot() +
  theme_gray() +
  scale_fill_brewer(palette="YlOrBr") +
  stat_summary(fun = median, geom = "line", aes(group=Gender, color=Gender))

'| Podemos fazer uma ANOVA de duas vias para testar as hipóteses:'
'| I. A saúde muda conforme a renda?'
'| II. A saúde muda conforme o gênero?'
'| III. EXISTE INTERAÇÃO entre a renda e o gênero?'

NHANES_12_t$HHIncomeMid_log <- log(NHANES_12_t$HHIncomeMid + 1)

library(dplyr)
NHANES_12_t %>%
  group_by(Gender, HealthGen) %>%
  summarise(media = mean(HHIncomeMid_log), .groups = "drop")

ggplot(NHANES_12_t, aes(HealthGen, HHIncomeMid_log, fill=Gender)) + 
  geom_boxplot() +
  theme_gray() +
  scale_fill_brewer(palette="YlOrBr") +
  stat_summary(fun = median, geom = "line", aes(group=Gender, color=Gender))

modelo2 <- lm(HHIncomeMid_log ~ HealthGen * Gender, data = NHANES_12_t)
res2 <- residuals(modelo2)

hist(res2) # -> Dados não normais

library(car)
leveneTest(HHIncomeMid_log ~ HealthGen * Gender, data = NHANES_12) # -> Impossibilidade de usar a ANOVA regular

# Não há como fazer uma ANOVA de Welsh ou teste de Kruskal-Wallis em duas vias.

# _________________________________
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

# Poverty: "INDFMMPI"
# HealthGen: "HUQ010"
# Gender: "RIAGENDR"
# Age: "RIDAGEYR"
# Ethnicity: "RIDRETH1"

library(dplyr)
NHANES_23_t = NHANES_23 %>%
  mutate(HUQ010_txt = case_when(
    HUQ010 == "1" ~ "Excellent",
    HUQ010 == "2" ~ "Vgood",
    HUQ010 == "3" ~ "Good",
    HUQ010 == "4" ~ "Fair", 
    HUQ010 == "5" ~ "Poor",
    TRUE ~ NA
  ))

NHANES_23_t = NHANES_23_t %>%
  mutate(RIDAGEYR_txt = case_when(
    RIDAGEYR == "1" ~ "<20",
    RIDAGEYR == "2" ~ "20-39",
    RIDAGEYR == "3" ~ "40-59",
    RIDAGEYR == "4" ~ ">=60", 
    TRUE ~ NA
  ))

NHANES_23_t = NHANES_23_t %>%
  mutate(RIAGENDR_txt = case_when(
    RIAGENDR == "1" ~ "Male",
    RIAGENDR == "2" ~ "Female",
    TRUE ~ NA
  ))

NHANES_23_t = NHANES_23_t %>%
  mutate(RIDRETH1_txt = case_when(
    RIDRETH1 == "1" ~ "Mexican",
    RIDRETH1 == "2" ~ "Other Hispanic",
    RIDRETH1 == "3" ~ "Non-Hispanic White",
    RIDRETH1 == "4" ~ "Non-Hispanic Black",
    RIDRETH1 == "5" ~ "Other",
    TRUE ~ NA
  ))

NHANES_23_t = NHANES_23_t %>%
  mutate(DMDMARTZ_txt = case_when(
    DMDMARTZ == "1" ~ "Married/Living with partner",
    DMDMARTZ == "2" ~ "Widowed/Divorced/Separated",
    DMDMARTZ == "3" ~ "Never Married",
    TRUE ~ NA
  ))

# Quais idades possuem maior índice de pobreza (2023)? -----

library(ggplot2)
ggplot(NHANES_23_t, aes(reorder(RIDAGEYR_txt, RIDAGEYR), INDFMMPI, fill=HUQ010_txt)) + 
  geom_boxplot() +
  theme_gray() +
  scale_fill_brewer(palette="Spectral") +
  stat_summary(fun = median, geom = "line", aes(group=HUQ010_txt, color=HUQ010_txt))

library(ggplot2)
ggplot(subset(NHANES_23_t, !is.na(HUQ010_txt) & !is.na(RIDAGEYR_txt)), aes(reorder(HUQ010_txt, HUQ010), INDFMMPI, fill=reorder(RIDAGEYR_txt, RIDAGEYR))) + 
  geom_boxplot() +
  theme_bw() +
  scale_fill_brewer(palette="Spectral") +
  #stat_summary(fun = mean, geom = "line", aes(group = 1), color = "green") +
  #stat_summary(fun = median, geom = "line", aes(group = 1), color ="blue") +
  #facet_wrap(~AgeDecade) +
  labs(title='Análise de saúde geral x índice de pobreza',
       subtitle='Agrupado por faixa de idade (2021-2023)',
       x='Saúde geral',
       y='Índice de pobreza',
       fill='Idades')

'| Desta vez, não há nenhum padrão notável que correlacione idades e saúde.'
'| Também percebem-se ausências em algumas categorias, o que pode indicar ausências de integrantes nestas categorias.'

par(mfrow=c(2,1))
hist(NHANES_12$Age)
hist(NHANES_23$RIDAGEYR)

'| Há integrantes de todas as idades, porém distribuídos de forma distinta.'

ggplot(NHANES_23_t, aes(reorder(HUQ010_txt, HUQ010), RIDAGEYR, fill=HUQ010_txt)) + 
  geom_boxplot() +
  theme_bw() +
  scale_fill_brewer(palette="Spectral") +
  stat_summary(fun = median, geom = "line", aes(group=HUQ010_txt, color=HUQ010_txt))

'| Diferentemente de 2012, em que existem integrantes de todas as idades, a maioria das pessoas com saúde muito ruim têm maior idade.'

# Quais gêneros possuem maior índice de pobreza (2020)? -----

NHANES_23_t = subset(NHANES_23_t, INDFMMPI<=5 & HUQ010<=5)

ggplot(NHANES_23_t, aes(HUQ010_txt, INDFMMPI, fill=RIAGENDR_txt)) + 
  geom_boxplot() +
  theme_gray() +
  scale_fill_brewer(palette="Set1") +
  stat_summary(fun = median, geom = "line", aes(group=RIAGENDR_txt, color=RIAGENDR_txt))

# Quais etnias possuem maior índice de pobreza (2020)? -----

ggplot(NHANES_23_t, aes(HUQ010_txt, INDFMMPI, fill=RIDRETH1_txt)) + 
  geom_boxplot() +
  theme_gray() +
  scale_fill_brewer(palette="YlOrBr") +
  stat_summary(fun = median, geom = "line", aes(group=RIDRETH1_txt, color=RIDRETH1_txt))

'| Podemos analisar com maior profundidade a relação etnia-pobreza e etnia-saúde.'

ggplot(NHANES_23_t, aes(RIAGENDR_txt, INDFMMPI, fill=RIAGENDR_txt)) + 
  geom_boxplot() +
  theme_gray() +
  scale_fill_brewer(palette="Set1") +
  stat_summary(fun = median, geom = "line", aes(group=RIAGENDR_txt, color=RIDRETH1_txt))

ggplot(NHANES_23_t, aes(RIAGENDR_txt, INDFMMPI, fill=RIDRETH1_txt)) + 
  geom_boxplot() +
  theme_gray() +
  scale_fill_brewer(palette="YlOrBr") +
  stat_summary(fun = median, geom = "line", aes(group=RIAGENDR_txt, color=RIDRETH1_txt))

'| Desta vez, existe disparidade de renda tanto considerando os gêneros quanto considerando as etnias..'
'| Podemos explorar se a disparidade de renda entre gêneros é estatísticamente significante.'

# Quão significante é a disparidade de renda entre os gêneros (2020)? -----

ggplot(NHANES_23_t, aes(RIDRETH1_txt, INDFMMPI, fill=RIAGENDR_txt)) + 
  geom_boxplot() +
  theme_gray() +
  scale_fill_brewer(palette="YlOrBr") +
  stat_summary(fun = median, geom = "line", aes(group=RIAGENDR_txt, color=RIDRETH1_txt))

'| Em contraste à 2012, a disparidade agora é nítida entre todos as etnias.'

ggplot(NHANES_23_t, aes(RIAGENDR_txt, INDFMMPI, fill=DMDMARTZ_txt)) + 
  geom_boxplot() +
  theme_gray() +
  scale_fill_brewer(palette="YlOrBr") +
  stat_summary(fun = median, geom = "line", aes(group=RIAGENDR_txt, color=RIDRETH1_txt))

'| Exceto pelos casados, as mulheres costumam receber menos que os homens.'
'| Vamos analisar se a diferença é estatisticamente significativa.'

ggplot(NHANES_23_t, aes(sample=INDFMMPI))+
  geom_qq() +
  geom_qq_line() + 
  facet_grid(~RIAGENDR_txt)

'| Havendo dois grupos, podemos fazer um teste-t.'

if(!require(RVAideMemoire)) install.packages("RVAideMemoire")
library(RVAideMemoire)

byf.shapiro(INDFMMPI ~ RIAGENDR_txt, NHANES_23_t)
leveneTest(INDFMMPI ~ RIAGENDR_txt, NHANES_23_t)#, center=mean)

t.test(INDFMMPI ~ RIAGENDR_txt, NHANES_23_t, var.equal=TRUE)

'| O p-valor MUITO menor que 0.05 indica que, ao nível de 95% de certeza, existe variação significativa entre a renda de homens e mulheres.'
'| Ainda não sei se isso é relevante ao artigo.'