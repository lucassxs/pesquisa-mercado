install.packages('ggplots')
library("ggplot2")
install.packages("dplyr")
library("dplyr")
install.packages('readxl')
library('readxl')
library(tidyverse)

setwd('C:\\Users\\lucas-stefano\\Documents\\pesquisa-mercado\graficos-lucas')
bd <- read_excel("pesquisa_de_mercado2.xlsx",sheet = "respostas")

itens <-read.table("pergunta.csv",header=T,sep=";")


library(readxl)

dados <- read_excel("pesquisa_de_mercado2.xlsx")

color_paleta <- colorRampPalette(c("black","gray"))


dados %>%
  na.omit() %>% # Tira os NA
  count(`11`, name = "count") %>%
  mutate(`11` = as.factor(`11`),
         fraction = count/sum(count),
         ymax = cumsum(count), ymin = c(0, head(ymax, n = -1)),
         labelPosition = (ymax + ymin) / 2,
         label = paste0(`11`, "\nfrequência: ", count)) %>%
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = `11`))+
  geom_rect()+
  geom_label(x = 3.5, aes(y = labelPosition, label = label), fill = "white", size = 4)+
  coord_polar(theta = "y")+
  xlim(c(2, 4))+
  theme_void(10)+
  scale_fill_manual(values = color_paleta(5))+
  labs(fill="Os professores, em geral, desenvolvem atividades 
interessantes para os alunos dominarem os novos conceitos ?")
ggsave("donuts-professores-geral.png")