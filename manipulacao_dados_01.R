#######################
## MQuinho 2019
## Aula 02
## Prof. Neylson
## Meu nome super bonito
#######################

# install.packages("dplyr")
# install.packages("readr")
# install.packages("PNADcIBGE")
# install.packages("ggplot2")
# install.packages("questionr")

library(dplyr)  # manipulação de dados
library(readr)  # leitura de dados
library(PNADcIBGE) # ler a PNAD contínua
library(ggplot2) # visualização de dados
library(questionr) # estatísticas descritivas

# Vamos começar com um banco de dados
# já embutido no R

data(iris)
iris = as_tibble(iris)
iris
?iris

View(iris)

######### Investigando o banco de dados iris
# tamanho
dim(iris)  # dimensões
nrow(iris) # n linhas
ncol(iris) # n colunas

names(iris)  # nomes das variáveis

glimpse(iris)  # estrutura do objeto

## Investigando a espécie
# Tabela de frequência
t = table(iris$Species)
t
prop.table(t) * 100

freq(iris$Species)

###################
# Investigando o tamanho da Sépala
# Sepal.Length
class(iris$Sepal.Length)

mean(iris$Sepal.Length)   # média
median(iris$Sepal.Length) # mediana
var(iris$Sepal.Length)    # variância
sd(iris$Sepal.Length)     # desvio padrão
min(iris$Sepal.Length)    # mínimo
max(iris$Sepal.Length)    # máximo

# Várias estat. desc. de uma só vez
summary(iris$Sepal.Length)
round(summary(iris$Sepal.Length), 2)

## Percentis
quantile(iris$Sepal.Length)

## quintis
quantile(iris$Sepal.Length, 
         probs = c(0, 0.2, 0.4, 0.6, 0.8, 1))

#####################################
# Manipulação de dados estilo tidy
names(iris)
# Começamos com o banco de dados iris
# seleciona colunas
# filtra casos em que Sepal.Length > 6
# ordena pela var Petal.Length descendente
# pega apenas os 10 primeiros
iris %>% 
  select(Sepal.Length:Petal.Width) %>% 
  filter(Sepal.Length >= 6) %>% 
  arrange(desc(Petal.Length))


iris %>% 
  select(Sepal.Length:Petal.Width) %>% 
  filter(Sepal.Length >= 6) %>% 
  arrange(Petal.Length)
##########################
# Tirando estatísticas descritivas
iris %>% 
  group_by(Species) %>% 
  summarise(m = mean(Sepal.Length),
            med = median(Sepal.Length),
            desv = sd(Sepal.Length)) %>% 
  arrange(desc(m)) %>% 
  ggplot(aes(x=m, y=desv, 
             color=Species)) +
  geom_point(size=5)
  
