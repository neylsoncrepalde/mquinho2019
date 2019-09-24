## Você deve colocar os aquivos
## PNADC_012019_20190729.txt e
## Input_PNADC_trimestral.txt 
## na pasta DOCUMENTOS

library(PNADcIBGE)

bd = read_pnadc("PNADC_012019_20190729.txt",
                "Input_PNADC_trimestral.txt",
                vars = c("Ano", "Trimestre", "UF", "Capital", "UPA", "Estrato", "V1022", "V1028", "V2007",
                         "V2009", "V2010", "VD3005", "VD4016", "VD4019", "VD4031"))
library(dplyr)
bd %>% select(V1028:VD4031)
