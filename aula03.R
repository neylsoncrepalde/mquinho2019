#########################
## MQuinho 2019
## Aula 03
## Prof. Neylson
## Meu nome super bonito
#########################

# Hoje, vamos trabalhar com a
# PNAD contínua do 2 trimestre
# de 2019.

# Pacotes necessários
library(PNADcIBGE)
library(dplyr)
library(ggplot2)
library(questionr)

# Faz download, le, trata, coloca labels
# e constrói survey design tudo junto
pnad = get_pnadc(year=2019, quarter = 2,
                 vars = c("Ano", "Trimestre",
                          "UF", "Capital",
                          "UPA", "Estrato",
                          "V1022", "V1028",
                          "V2007", "V2009",
                          "V2010", "VD3005",
                          "VD4016", "VD4019",
                          "VD4031"))
pnad
# Extrai os dados para trabalhar
bd = pnad$variables # pega o banco de dados
bd = as_tibble(bd)  # Transformar em tibble
bd %>% select(V2007:VD4031)

### Transformar anos de escolaridade em num
class(bd$VD3005)
bd$anosesco = as.integer(bd$VD3005) - 1
t = table(bd$anosesco, bd$VD3005)
View(t)
head(t)
tail(t)
##################################
# Sexo e Cor/raça

# Tabela de frequência simples
freq(bd$V2007)

# tabela_ponderada = function(x) {
#   wtd.table(x, weights = bd$V1028)
# }

# Tabela de frequencia absoluta ponderada
t = wtd.table(bd$V2007, weights = bd$V1028)
# Tabela de proporções / percentual
prop.table(t) * 100

## Cor/raça
t = wtd.table(bd$V2010, weights = bd$V1028)
prop.table(t) * 100
round(prop.table(t) * 100, 3)

# Estimando a proporção de cor/raça em
# estados específicos
freq(bd$UF)

# Estimando as proporções de cor
# apenas para o estado da Bahia
bahia = bd %>% 
  filter(UF == "Bahia")

t = wtd.table(bahia$V2010, 
              weights = bahia$V1028)
prop.table(t) * 100

###############################
# anosesco, VD4019 (renda de todos os trabalhos)
summary(bd$anosesco)
weighted.mean(bd$anosesco, bd$V1028, na.rm = T)

# Vamos estimar médias de renda
dados = bd %>% 
  filter(V2009 >= 25 & V2009 <= 60)

summary(dados$V2009)
##############################
# Renda
dados %>%
  group_by(V2007, V2010) %>% 
  summarise(m = weighted.mean(VD4019, 
                              w=V1028,
                              na.rm=T)) %>% 
  arrange(desc(m))

# Testando a diferença de renda entre 
# homens e mulheres
# Teste T de Student
options(scipen = 6)
t.test(dados$VD4019 ~ dados$V2007)

## Investigar a relação condicional
## entre escolaridade, sexo, cor da pele 
## com a renda
## Modelo de regressão linear

hist(dados$VD4019)
hist(log(dados$VD4019))

# Regressão linear
fit = lm(log(VD4019) ~ anosesco +
           V2007 + relevel(V2010, "Preta"), 
         data = dados)
summary(fit)
# Correção de Pearson
cor(dados$VD4019, dados$anosesco,
    use =  "complete.obs")

plot(dados$anosesco, 
     log(dados$VD4019))
abline(fit,
       col = "red", lwd=2)

## Curva de regressão
dados %>% 
  ggplot(aes(x=anosesco,
             y=log(VD4019)))+
  geom_point() +
  stat_smooth(method = "lm")

