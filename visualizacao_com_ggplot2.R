#########################
## MQuinho 2019
## Aula 04
## Prof. Neylson
## Meu nome super bonito
#########################

# Comando que remove tudo do ambiente
## CUIDADO!!!
rm(list = ls())

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

##########################################
# Escrever os dados da PNAD processada
# em um novo arquivo

#install.packages("readr")
library(readr)
write_csv(bd, "pnadc_20192_amostra.csv")

#install.packages("writexl")
library(writexl)
write_xlsx(bd, "pnadc_20192_amostra.xlsx")

# Lendo planilhas de excel
#install.packages("readxl")
library(readxl)
bd = read_xlsx("pnadc_20192_amostra.xlsx")
bd
?read_xlsx

#########################################
# Visualização de dados com ggplot2 S2

# Vamos visualizar a distribuição de cor/raça
# no Brasil
#install.packages("ggthemes")
#install.packages("forcats")
#install.packages("ggrepel")
library(ggplot2)
library(dplyr)
library(scales)
library(ggthemes)
library(forcats)
library(ggrepel)

bd %>% 
  group_by(V2010) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x=fct_reorder(V2010, n), 
             y=n,fill = V2010,
             label = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Cor/Raça no Brasil",
       subtitle = "MQuinho 2019",
       caption = "Fonte: PNAD contínua 2019/2",
       x="Cor/Raça",
       y="Frequência") +
  scale_y_continuous(labels = comma,
                     limits = c(0, 380000)) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom") +
  # scale_fill_brewer(name = "Legenda",
  #                   palette = "Set1") 
  scale_fill_manual(name = "Legenda",
      values = c("red", "orange", "purple",
                 "blue", "black", "pink")) +
  geom_text(size=4, hjust=-.1)

################################
# Histograma
# Plotar distribuição de var contínuas
# Vamos plotar a dist da renda
bd %>% 
  ggplot(aes(x=log(VD4019))) +
  geom_histogram(color = "white")


## Para cruzar duas numericas
## usamos o scatterplot
# point
bd %>% 
  ggplot(aes(x=anosesco,
             y=log(VD4019))) +
  geom_point()


# Boxplot
## Distribuição de renda por
## categorias de cor/raça
bd %>% 
  filter(V2010 != "Ignorado") %>% 
  ggplot(aes(x=V2007, y=VD4019))+
  geom_boxplot() +
  ylim(0, 4000) +
  facet_wrap(~V2010)
