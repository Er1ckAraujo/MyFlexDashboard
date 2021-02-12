setwd("C:/Users/Erick/Desktop/ProcessoSeletivo")


install.packages("shiny")
install.packages("magrittr")
install.packages("tidyverse")
install.packages("esquisse")
install.packages("plotly")
install.packages("gtsummary")
install.packages("flexdashboard")

library(tidyverse)
library(dplyr)
library(tidyr)
library(shiny)
library(flexdashboard)
library(magrittr)
library(plotly)
library(esquisse)
library(gtsummary)

base_ipea <- as_tibble(read.csv2("base_ipea.csv", stringsAsFactors = T), sep = ".") # Lendo a base de dados

summary(base_ipea) # Resumo geral dos dados

# Da pra entender que um homem que cresceu em uma familia violenta agrida sua mulher

library(ggplot2)
library(plotly)

grafico1 <- ggplot(base_ipea) + # GRAFICO 1
  aes(x = da.pra.entender.que.um.homem.que.cresceu.em.uma.familia.violenta.agrida.sua.mulher, fill = sexo) +
  geom_bar(position = "dodge") +
  #scale_fill_viridis_d(option = "cividis") +
  labs(x = " ", y = "Numero de Entrevistados") +
  coord_flip() +
  theme_bw()

ggplotly(grafico1)

library(gtsummary)

tbl <- base_ipea %>% # TABELA 1
  tbl_cross(row = sexo,
            col = da.pra.entender.que.um.homem.que.cresceu.em.uma.familia.violenta.agrida.sua.mulher,
            percent = "row")

tbl

# Se as mulheres soubessem se comportar haveriam menos estupros

library(ggplot2)
library(plotly)

grafico2 <- ggplot(base_ipea) + #GRAFICO 2
  aes(x = se.as.mulheres.soubessem.como.se.comportar..haveria.menos.estupros) +
  geom_bar(fill = "#FF3900") +
  labs(x = " ", y = "Numero de Entrevistados") +
  coord_flip() +
  theme_bw() +
  facet_grid(vars(), vars(sexo))

ggplotly(grafico2)

library(gtsummary)

tbl2 <- base_ipea %>% # TABELA 2
  tbl_cross(row = sexo,
            col = se.as.mulheres.soubessem.como.se.comportar..haveria.menos.estupros,
            percent = "row")

tbl2

# Outras informaçoes relevantes:

# IDADE

minimo <- min(base_ipea$idade)

q1 <- quantile(base_ipea$idade, 0.25)

q2 <- quantile(base_ipea$idade,0.5)

q3 <- quantile(base_ipea$idade,0.75)

maximo <- max(base_ipea$idade)

base_ipea$classes_idade= 
  cut(base_ipea$idade, breaks = c(minimo, q1, q2, q3, maximo),
      labels = c("16-28", "28-41", "41-56", "56-88"),
      include.lowest = T)

table(base_ipea$classes_idade)


grafico_hist <- ggplot(base_ipea,aes(x=classes_idade,fill= regiao)) + 
  geom_bar(position = "dodge", color = "black")+
  ggtitle("Grafico histograma da distribuicao do numero de entrevistados")+
  xlab("Frequencia das idades por quartis") +
  ylab("Frequencia simples") +
  theme_bw()

ggplotly(grafico_hist)

# RELIGIAO 

base_ipea <- base_ipea %>%  mutate(religiao2 = fct_lump(religiao, 2, other_level = "Outros")) # Reclassificacao do fator religiao para niveis menos gerais

table(base_ipea$religiao2)


df_pizza_porcent <- prop.table(table(base_ipea$religiao2)) # Criando uma tabela com as frequencias relativas das religioes
df_pizza_porcent <- as.data.frame(df_pizza_porcent)
df_pizza_porcent


grafico_pizza_iterativo = plot_ly(df_pizza_porcent, # plotando grafico interativo com plot_ly
                                  labels = ~Var1, 
                                  values = ~Freq, 
                                  type = 'pie') %>%
  layout(title = 'Frequencia relativa da Classe Social (%)')

grafico_pizza_iterativo

# etinias

etinias <- 
  ggplot(base_ipea, aes(x = cor.ou.raca,fill = regiao)) +
  geom_bar(position = "dodge") +
  labs(x = " ", title = "Cor ou Raça") +
  ggtitle("Número de entrevistados")+
  xlab("Cor ou Raça") +
  ylab("Número de entrevistados")+
  theme_bw()

ggplotly(etinias)

