setwd("C:\\Users\\LENOVO\\Desktop\\ERICK")

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

base_ipea <- as_tibble(read.csv2("base_ipea.csv", stringsAsFactors = T)) # Lendo a base de dados

base_ipea

summary(base_ipea) # Resumo geral dos dados

# D� pra entender que um homem que cresceu em uma fam�lia violnta agrida sua mulher

library(ggplot2)
library(plotly)

grafico1 <- ggplot(base_ipea) + # GRAFICO 1
  aes(x = d�.para.entender.que.um.homem.que.cresceu.em.uma.fam�lia.violenta.agrida.sua.mul, fill = sexo) +
  geom_bar(position = "dodge") +
  scale_fill_viridis_d(option = "cividis") +
  labs(x = " ", y = "N� de Entrevistados") +
  coord_flip() +
  theme_bw()

ggplotly(grafico1)

library(gtsummary)

tbl <- base_ipea %>% # TABELA 1
  tbl_cross(row = sexo,
    col = d�.para.entender.que.um.homem.que.cresceu.em.uma.fam�lia.violenta.agrida.sua.mul,
    percent = "row")

tbl

# Se as mulheres soubessem se comportar haveriam menos estupros

library(ggplot2)
library(plotly)

grafico2 <- ggplot(base_ipea) + #GR�FICO 2
  aes(x = se.as.mulheres.soubessem.como.se.comportar..haveria.menos.estupros) +
  geom_bar(fill = "#093c6d") +
  labs(x = " ", y = "N� de Entrevistados") +
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

# Outras informa��es relevantes:

# IDADE

library(ggplot2)
library(plotly)

idade <- ggplot(base_ipea) +
  aes(x = idade..qual.a.sua.idade..em.anos.completos..) +
  geom_histogram(bins = 30L, fill = "#052f58") +
  labs(x = " ", y = "N� de Entrevistados", title = "Idade em anos completos") +
  theme_bw()

ggplotly(idade)

# RELIGI�O 

library(ggplot2)
library(plotly)
library(forcats)

religiao <- base_ipea %>%
  mutate(religi�o = fct_lump(religi�o, 2, other_level = "Outros")) # Reclassifica��o do fator religi�o para n�veis menos gerais

bar <- ggplot(religiao, aes(x = factor(1) , fill = factor(religi�o))) +
  geom_bar(width = 1) +
  labs(x = " ", y = "N� de Entrevistados", title = "Religi�es") +
  theme_bw()

ggplotly(bar)

# Cor ou ra�a

library(ggplot2)

etinias <- 
  ggplot(base_ipea) +
  aes(x = cor.ou.ra�a) +
  geom_bar(fill = "#092e52") +
  labs(x = " ", title = "Cor ou Ra�a") +
  theme_bw()

ggplotly(etinias)
  