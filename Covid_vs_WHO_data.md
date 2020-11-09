# Importação das bibliotecas

library(tidymodels)
library(tidyverse)
library(ggrepel)
library(skimr)
library(MASS)
library(dplyr)
library(factoextra)
library(plotly)
library(cluster)

# Obtenção dos dados

dados <- read.csv("../Index_Covid.csv")

view(dados)

# Tratamento da base de dados

nome_linhas <- nrow(dados)
nome_colunas <- ncol(dados)

covid <- dados %>%
  select_if(is.numeric) %>%
  as.data.frame() %>%
  scale()

rownames(covid) <- dados$Country.Name

dados_df <- dados[,-1]

# Agrupamento hierárquico - complete

hc <- hclust(dist(dados_df), method = "complete")

fviz_dend(hc, 
          k = 5,
          main = "Dados",
          color_labels_by_k = TRUE, 
          horiz = TRUE,) + 
  theme_void()

tibble(coord1 = dados_df$Handwashing, 
       coord2 = dados_df$Hospital.beds, 
       label = rownames(dados_df), 
       cluster = factor(cutree(hc, 5))) %>% 
  ggplot(aes(coord1, coord2)) + 
  geom_text_repel(aes(label = label), size = 6) +
  geom_point(aes(color = cluster), size = 3, show.legend = FALSE) + 
  theme_bw()

# Agrupamento hierárquico - Single

hc_s <- hclust(dist(dados_df), method = "single")

fviz_dend(hc_s, 
          k = 5,
          main = "Dados",
          color_labels_by_k = TRUE, 
          horiz = TRUE,) + 
  theme_void()

tibble(coord1 = dados_df$Handwashing, 
       coord2 = dados_df$Hospital.beds, 
       label = rownames(dados_df), 
       cluster = factor(cutree(hc_s, 5))) %>% 
  ggplot(aes(coord1, coord2)) + 
  geom_text_repel(aes(label = label), size = 6) +
  geom_point(aes(color = cluster), size = 3, show.legend = FALSE) + 
  theme_bw()

# Agrupamento hierárquico - Centróide

hc_c <- hclust(dist(dados_df), method = "centroid")

fviz_dend(hc_c, 
          k = 5,
          main = "Dados",
          color_labels_by_k = TRUE, 
          horiz = TRUE,) + 
  theme_void()

tibble(coord1 = dados_df$Handwashing, 
       coord2 = dados_df$Hospital.beds, 
       label = rownames(dados_df), 
       cluster = factor(cutree(hc_c, 5))) %>% 
  ggplot(aes(coord1, coord2)) + 
  geom_text_repel(aes(label = label), size = 6) +
  geom_point(aes(color = cluster), size = 3, show.legend = FALSE) + 
  theme_bw()

# Método K-Means

teste <- kmeans(dados_df, centers = 2)

auxiliar <- tibble(cluster = teste$cluster) %>% 
  bind_cols(as_tibble(dados_df))

teste$withinss # para acessar a variável

auxiliar %>% 
  group_by(cluster) %>% 
  summarise(beer = sum((beer - mean(beer))^2), 
            spirit = sum((spirit - mean(spirit))^2), 
            wine = sum((wine - mean(wine))^2),
            total_litres_alcohol = sum((total_litres_alcohol - mean(total_litres_alcohol))^2)) %>% 
  mutate(within = beer + spirit + wine + total_litres_alcohol)

# Bibliography

https://coronavirus.jhu.edu/data/mortality
https://www.heritage.org/index/
https://apps.who.int/gho/data/node.main.WSHHYGIENE?lang=en
https://www.tutorialgateway.org/barplot-in-r-programming/
http://howtoinr.weebly.com/
https://www.r-graph-gallery.com/210-custom-barplot-layout
