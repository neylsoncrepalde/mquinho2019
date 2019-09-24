###############################
#### MQuinho 2019
### Leitura da PNAD contínua
### Prof. Dr. Neylson Crepalde
###############################

## Vamos ler a PNAD contínua do 2 trimestre de 2019. Vamos separar também as 
## variáveis de interesse.

## Se o pacote PNADcIBGE não estiver instalado, faça a instalação
if (! "PNADcIBGE" %in% installed.packages()) install.packages("PNADcIBGE")

# Carrega o Pacote necessário
library(PNADcIBGE)
library(dplyr)

# Pega a pnad
pnad = get_pnadc(2019, 2, 
                  vars = c("Ano", "Trimestre", "UF", "Capital", "UPA", "Estrato", "V1022", "V1028", "V2007",
                           "V2009", "V2010", "VD3005", "VD4016", "VD4019", "VD4031"))

# Verifica - cria um objeto survey design
pnad

# Isola o banco de dados para 
bd = pnad$variables %>% as_tibble()

# Coloca anos de escolaridade como numérica
bd$anosesco = as.integer(bd$VD3005) - 1
summary(bd$anosesco)

# Confere
bd %>% select(V2007:anosesco)
