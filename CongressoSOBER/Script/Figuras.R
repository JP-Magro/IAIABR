# Artigo internaiconal

library(tidyverse)
library(readxl)

# Figura 1. Variação do Índice de Preços Reais dos Alimentos da FAO, 1961 a 2021
fig1 <- read_excel(
   "_Artigo_Internacional/Script/DATA_BASE/Gráficos_artigo/food_price_index_nominal_real_jan665.xls", 
    skip = 2)

fig1_proc <- fig1 %>% 
   select(1:3) %>% 
   na.omit() %>% 
   rename(Ano = Month)

ggplot(data = fig1_proc,
       aes(x = Ano, y = Real))+
   geom_line(size = 1.1, color = "blue")+
   scale_x_continuous(breaks = seq(1961, 2022, 3))+
   scale_y_continuous(breaks = seq(70, 140, 10))+
   labs(x = NULL, y = NULL)+
   theme_minimal()+
   theme(axis.text = element_text(size = 11),
         axis.title.y = element_text(size = 11))

rm(fig1, fig1_proc)
   
   
# Figura 2. Índice de crescimento do valor da exportação, importação e saldo comercial do agronegócio brasileiro, 1997 a 2019

fig2 <- read_excel("_Artigo_Internacional/Script/DATA_BASE/Gráficos_artigo/Comex_agro.xlsx")

fig2 %>% 
   rename(`Exportação` = `Índice Exportação`,
          `Importação` = `Índice Importação`,
          `Saldo comercial` = `Índice Saldo Comercial`) %>% 
   pivot_longer(cols = 2:4, names_to = "tipo", values_to = "valor") %>% 
   ggplot(aes(x = Data, y = valor, color = tipo, shape = tipo))+
   geom_line(size = 1.5)+
   geom_point(size = 3.5)+
   scale_x_continuous(breaks = seq(1997, 2019, 1))+
   scale_y_continuous(breaks = seq(100, 600, 100))+
   scale_color_brewer(palette = "Dark2", direction = -1)+
   labs(x = NULL, y = NULL, color = NULL, shape = NULL)+
   theme_minimal()+
   theme(axis.text = element_text(size = 12),
         legend.position = "bottom",
         legend.text = element_text(size=11),
         legend.key.width= unit(1, 'cm'))


rm(fig2)
