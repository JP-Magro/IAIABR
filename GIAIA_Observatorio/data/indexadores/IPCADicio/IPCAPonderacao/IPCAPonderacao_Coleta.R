#-----------------------------------------------------------------------------
##---------------------------IPCA PONDERAÇÕES---------------------------------
#-----------------------------------------------------------------------------

# BIBLIOTECAS
library(tidyverse)
library(readxl)
library(writexl)

#-------------------------------COLETA EXCEL----------------------------------
# Fonte da função de coleta de sheets: https://stackoverflow.com/questions/12945687/read-all-worksheets-in-an-excel-workbook-into-an-r-list-with-data-frames

## POF 95-96
path <- "~/R program/_IAIABr-main/GIAIA_Observatorio/Data/Dicionarios/IPCADicio/IPCAPonderacao/DadosBase/ESTRUTURA_POND_96_99.xlsx"

Pond_96_99_Bruto <- path %>%
   readxl::excel_sheets() %>%
   purrr::set_names() %>%
   purrr::map_dfr(read_excel, path = path, .id = "SheetName")

## POF 02-03
path <- "~/R program/_IAIABr-main/GIAIA_Observatorio/Data/Dicionarios/IPCADicio/IPCAPonderacao/DadosBase/ESTRUTURA_POND_02_03.xls"

Pond_02_03_Bruto <- path %>%
   readxl::excel_sheets() %>%
   purrr::set_names() %>%
   purrr::map_dfr(read_excel, path = path, .id = "SheetName")

## POF 08-09
path <- "~/R program/_IAIABr-main/GIAIA_Observatorio/Data/Dicionarios/IPCADicio/IPCAPonderacao/DadosBase/ESTRUTURA_POND_08_09.xls"

Pond_08_09_Bruto <- path %>%
   readxl::excel_sheets() %>%
   purrr::set_names() %>%
   purrr::map_dfr(read_excel, path = path, .id = "SheetName")

### POF 08-09 - AC, MA e SE
Pond_08_09_AC_MA_SE_Bruto <- read_excel("~/R program/_IAIABr-main/GIAIA_Observatorio/Data/Dicionarios/IPCADicio/IPCAPonderacao/DadosBase/ESTRUTURA_POND_08_09_AC_MA_SE.xlsx", skip = 2)

Pond_08_09_AC_MA_SE_V1 <- Pond_08_09_AC_MA_SE_Bruto %>% 
   drop_na(Subitem) %>% 
   pivot_longer(cols = 3:5, names_to = "NOME", values_to = "VAL_POND") %>% 
   drop_na(VAL_POND) %>% 
   arrange(NOME) %>% 
   rename("SUBITEM" = Subitem,
          "cod" = `Código`) %>% 
   mutate(VAL_POND = as.numeric(VAL_POND) %>% round(4),
          POF = "POF 2008-2009",
          PERIODO = "Jan 2012 - Dez 2019",
          PERIODO_POND = "Jan 2009") %>% 
   select(NOME, SUBITEM, VAL_POND, cod, POF, PERIODO, PERIODO_POND)


## POF 17-18
path <- "~/R program/_IAIABr-main/GIAIA_Observatorio/Data/Dicionarios/IPCADicio/IPCAPonderacao/DadosBase/ESTRUTURA_POND_18.xlsx"

Pond_18_Bruto <- path %>%
   readxl::excel_sheets() %>%
   purrr::set_names() %>%
   purrr::map_dfr(read_excel, path = path, .id = "SheetName")


path <- "~/R program/_IAIABr-main/GIAIA_Observatorio/Data/Dicionarios/IPCADicio/IPCAPonderacao/DadosBase/ESTRUTURA_POND_19.xlsx"

Pond_19_Bruto <- path %>%
   readxl::excel_sheets() %>%
   purrr::set_names() %>%
   purrr::map_dfr(read_excel, path = path, .id = "SheetName")

rm(path)

#-------------------------------PROCESSAMENTO---------------------------------
## POF 95-96
Pond_96_99_V1 <- Pond_96_99_Bruto %>% 
   select(1,2,4,6,8) %>% 
   rename("NOME" = SheetName,
          "cod" = 2,
          "SUBITEM" = 3,
          "Set 1996" = 4,
          "Ago 1999" = 5) %>% 
   na.omit(cod) %>% 
   filter(!cod %in% c("_________", "CÓDIGO", "FONTE: I"))

### Corrigindo números com vírgula e símbolo no final - coluna `Ago 1999`
DETEC <- which(grepl(",",Pond_96_99_V1$`Ago 1999`)) #Detectando linhas a corrigir

CORR <- Pond_96_99_V1 %>% 
   slice(DETEC) %>% 
   mutate(`Ago 1999` = gsub('.{1}$', '', `Ago 1999`),
          SUBSTITUTION = str_replace(`Ago 1999`, ",", ".")) %>% 
   select(-c(4,5))

Pond_96_99 <- full_join(Pond_96_99_V1, CORR,
                        by = c("NOME","cod","SUBITEM")) %>% 
   mutate(`Ago 1999` = if_else(is.na(SUBSTITUTION) == TRUE, 
                               `Ago 1999`, SUBSTITUTION),
          `Set 1996` = as.numeric(`Set 1996`) %>% round(4),
          `Ago 1999` = as.numeric(`Ago 1999`) %>% round(4),
          POF = "POF 1995-1996",
          PERIODO = "Ago 1999 - Jun 2006") %>% 
   select(-6) %>% 
   pivot_longer(cols = c(`Set 1996`, `Ago 1999`),
                names_to = "PERIODO_POND",
                values_to = "VAL_POND") %>% 
   mutate(PERIODO_POND = factor(PERIODO_POND, 
                                levels = c("Set 1996", "Ago 1999"))) %>% 
   group_by(NOME, SUBITEM) %>% 
   arrange(PERIODO_POND)

rm(Pond_96_99_V1, DETEC, CORR)

## POF 02-03
Pond_02_03 <- Pond_02_03_Bruto %>% 
  pivot_longer(cols = c(2, 8:18),names_to = "tabelas", values_to = "cod") %>% 
  rename("NOME" = SheetName,
         "SUBITEM" = `...4`,
         "Jan 2003" = `...5`,
         "Abr 2006" = `...6`) %>% 
  select(!c(2,3,tabelas)) %>% 
  na.omit(SUBITEM) %>% 
  mutate(`Jan 2003` = as.numeric(`Jan 2003`) %>% round(4),
         `Abr 2006` = as.numeric(`Abr 2006`) %>% round(4),
         POF = "POF 2002-2003",
         PERIODO = "Jul 2006 - Dez 2011") %>% 
  pivot_longer(cols = c(`Jan 2003`, `Abr 2006`),
               names_to = "PERIODO_POND",
               values_to = "VAL_POND") %>% 
  mutate(PERIODO_POND = factor(PERIODO_POND, 
                               levels = c("Jan 2003", "Abr 2006"))) %>% 
  group_by(NOME, SUBITEM) %>% 
  arrange(PERIODO_POND)

## POF 08-09
Pond_08_09_V1 <- Pond_08_09_Bruto %>% 
  pivot_longer(cols = c(2, 7:19), names_to = "tabelas", values_to = "cod") %>% 
  rename("NOME" = SheetName,
         "SUBITEM" = `...4`,
         "VAL_POND" = `...5`) %>% 
  select(!c(2,3,tabelas)) %>% 
  na.omit(SUBITEM) %>% 
  mutate(VAL_POND = as.numeric(VAL_POND) %>% round(4),
         POF = "POF 2008-2009",
         PERIODO = "Jan 2012 - Dez 2019",
         PERIODO_POND = "Jan 2009")

### Juntando com estados faltantes (AC, MA e SE)
Pond_08_09 <- rbind(Pond_08_09_V1, Pond_08_09_AC_MA_SE_V1)


## POF 17-18
Pond_18 <- Pond_18_Bruto %>%
  pivot_longer(cols = c(2, 6:21),names_to = "tabelas", values_to = "cod") %>% 
  rename("NOME" = SheetName,
         "SUBITEM" = `...3`,
         "VAL_POND" = `...4`) %>% 
  select(!c(2,tabelas)) %>% 
  mutate(VAL_POND = as.numeric(VAL_POND) %>% round(4),
         POF = "POF 2017-2018",
         PERIODO = "Jan 2020 - 2023",
         PERIODO_POND = "Jan 2018") %>% 
  na.omit(VAL_POND)
  
  
Pond_19 <- Pond_19_Bruto %>% 
  pivot_longer(cols = c(2, 6:21),names_to = "tabelas", values_to = "cod") %>% 
  rename("NOME" = SheetName,
         "SUBITEM" = `...3`,
         "VAL_POND" = `...4`) %>% 
  select(!c(2,tabelas)) %>% 
  mutate(VAL_POND = as.numeric(VAL_POND) %>% round(4),
         POF = "POF 2017-2018",
         PERIODO = "Jan 2020 - 2023",
         PERIODO_POND = "Dez 2019") %>% 
  na.omit(VAL_POND)
  
Pond_17_18 <- rbind(Pond_18, Pond_19)
  

rm(Pond_96_99_Bruto,Pond_02_03_Bruto, Pond_08_09_Bruto,Pond_08_09_AC_MA_SE_Bruto,Pond_08_09_AC_MA_SE_V1,Pond_08_09_V1, Pond_18_Bruto, Pond_19_Bruto, Pond_18, Pond_19)
  

#---------------------------JUNTANDO PONDERAÇÕES-------------------------------

POND_J <- rbind(Pond_96_99, Pond_02_03, Pond_08_09, Pond_17_18)

write_rds(POND_J, "~/R program/_IAIABr-main/GIAIA_Observatorio/Data/Dicionarios/IPCADicio/IPCAPonderacao/RawPond/POND_J.rds")

rm(Pond_96_99, Pond_02_03, Pond_08_09, Pond_17_18)


#------------------------------CORRIGINDO NOME LOCAIS--------------------------

POND_J <- read_rds("~/R program/_IAIABr-main/GIAIA_Observatorio/Data/Dicionarios/IPCADicio/IPCAPonderacao/RawPond/POND_J.rds")

DICIO_LOCAL <- data.frame(
   NOME_GEO = c("Brasil","Rio de Janeiro", "Porto Alegre", "Belo Horizonte",
                "Recife", "São Paulo", "Brasília", "Belém", "Fortaleza",
                "Salvador", "Curitiba", "Goiânia", "Grande Vitória",
                "Campo Grande", "Aracaju", "Rio Branco", "São Luís",
                "Rio Branco", "São Luís", "Aracaju"),
   NOME = unique(POND_J$NOME))

POND_96_19_J <- full_join(POND_J, DICIO_LOCAL, by = c("NOME")) %>% 
   ungroup() %>% 
   select(NOME_GEO, PERIODO, POF, PERIODO_POND, cod, SUBITEM, VAL_POND) %>% 
   mutate(cod = as.numeric(cod))

rm(DICIO_LOCAL, POND_J)


#--------------------------Corrigindo Subitens e Códigos-----------------------

## Carregando base de dados IPCA----
IPCA_1K99_2K23_DATA_FINAL_H_1 <- read_rds("~/R program/_IAIABr-main/GIAIA_Observatorio/Data/Raw/IPCARaw/IPCA_1K99_2K23_DATA_FINAL_H_1.rds")

## Criando dicionário com base de dados IPCA
DICIO_ICPA <- IPCA_1K99_2K23_DATA_FINAL_H_1 %>% 
   group_by(COD_REF) %>% 
   summarise(ITEM_CORRETO = unique(ITEM))%>% 
   mutate(SUB = stringi::stri_trans_general(ITEM_CORRETO, "Latin-ASCII"),
          SUB = stringr::str_to_upper(SUB),
          SUB = stringr::str_remove_all(SUB, "[-() .,/]"),
          cod = case_when(
             COD_REF <= 0 & COD_REF <10 ~ COD_REF*10^7,
             COD_REF <= 10 & COD_REF <100 ~ COD_REF*10^6,
             COD_REF <= 100 & COD_REF <1000 ~ COD_REF*10^5,
             COD_REF <= 1000 & COD_REF <10000 ~ COD_REF*10^4,
             COD_REF <= 10000 & COD_REF <100000 ~ COD_REF*10^3,
             COD_REF <= 100000 & COD_REF <1000000 ~ COD_REF*10^2,
                TRUE ~ as.numeric(COD_REF)
          ))

## Criando indexador para a ponderação (permitir juntar com `DICIO_IPCA`)
POND_INDEX <- POND_96_19_J %>% 
   mutate(SUB = stringi::stri_trans_general(SUBITEM, "Latin-ASCII"),
          SUB = stringr::str_to_upper(SUB),
          SUB = stringr::str_remove_all(SUB, "[-() .,/]"))

## Juntando indexadores
Dicio_Index <- full_join(POND_INDEX, DICIO_ICPA, by = c("SUB", "cod"))

rm(IPCA_1K99_2K23_DATA_FINAL_H_1, POND_96_19_J, POND_INDEX, DICIO_ICPA)

### Verificando valores NA
Dicio_Index_NA <- Dicio_Index[!complete.cases(Dicio_Index), ] %>% 
   group_by(cod, SUBITEM) %>% 
   summarise(SUB = unique(SUB)) %>% 
   arrange(SUBITEM)

### Corrigindo entradas
### Método: Serão adicionados os `COD_REF` e `ITEM_CORRIGIDO` aos produtos sem esses, a partir da consulta à base de dados `Dicio_Index` aos equivalentes corretos. Será realizado via Excel, mediante consulta aos data frames.

# Diretório da planilha: "~/R program/_IAIABr-main/GIAIA_Observatorio/Data/Dicionarios/IPCADicio/IPCAPonderacao/RawPond/Dicio_Index_CORR.xlsx"

## `DICIO_PONDERADA_CORR` substitui `Dicio_Index_NA`
DICIO_PONDERADA_CORR <- read_excel("~/R program/_IAIABr-main/GIAIA_Observatorio/Data/Dicionarios/IPCADicio/IPCAPonderacao/RawPond/Dicio_Index_CORR.xlsx")

Dicio_Index_J <- full_join(Dicio_Index, DICIO_PONDERADA_CORR, 
                           by = c("cod", "SUBITEM", "SUB"))

POND_96_19_FINAL <- Dicio_Index_J %>%
   mutate(COD_REF = if_else(is.na(COD_REF_CORRIGIDO) == TRUE, COD_REF, COD_REF_CORRIGIDO),
          ITEM_CORRETO = if_else(is.na(ITEM_CORRIGIDO) == TRUE, ITEM_CORRETO, ITEM_CORRIGIDO)) %>% 
   drop_na(SUBITEM) %>% 
   select(NOME_GEO:PERIODO_POND, COD_REF, ITEM_CORRETO, VAL_POND) %>% 
   rename("ITEM" = ITEM_CORRETO)


rm(Dicio_Index_J, DICIO_PONDERADA_CORR, Dicio_Index_NA, Dicio_Index)

#---------------------------SALVAR PONDERAÇÃO FINAL----------------------------

write_rds(POND_96_19_FINAL, "~/R program/_IAIABr-main/GIAIA_Observatorio/Data/Dicionarios/IPCADicio/POND_96_19_FINAL.rds")


