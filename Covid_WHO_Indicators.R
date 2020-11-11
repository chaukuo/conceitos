install.packages("digest")

# Importação das bibliotecas

library(tidymodels)
library(tidyverse)
library(ggrepel)
library(skimr)
library(factoextra)
library(plotly)
library(cluster)
library(dplyr)

# Obtenção dos dados

dados <- read.csv("C:/Users/chau_/OneDrive/Insper/2o T/Modelos_Preditivos_Avancado/08. Seminarios/WHO.csv")

view(dados)

# Gráficos

df <-dados[order(dados$Covid),]

barplot(df$Covid, beside=TRUE, col=c("red"),  xlab = "Country",ylab = "Death rate (per 100 ths)", main = "Death Rate, by country", names.arg = df$Country.Name,cex.axis=0.8, cex.names=0.8,las=2)

df1 <-dados[order(dados$Hospital.beds),]

barplot(df1$Hospital.beds, beside=TRUE, col=c("blue"),  xlab = "Country",ylab = "Hospital beds rate", main = "Hospital beds rate (per 10 ths), by country", names.arg = df1$Country.Name,cex.axis=0.8, cex.names=0.8, horiz = FALSE, density = 200,las=2)

df2 <- dados[order(dados$Handwashing),]

barplot(df2$Handwashing, beside=TRUE, col=c("green"),  xlab = "Country",ylab = "Handwashing", main = "Handwashing rate, by country", names.arg = df1$Country.Name,cex.axis=0.8, cex.names=0.8, horiz = FALSE, density = 200,las=2)

df3 <- dados[order(dados$Public.medicine.availability),]

barplot(df2$Public.medicine.availability, beside=TRUE, col=c("green"),  xlab = "Country",ylab = "Medicie availability", main = "Public medicines availability, by country", names.arg = df1$Country.Name,cex.axis=0.8, cex.names=0.8, horiz = FALSE, density = 200,las=2)


# Tratamento da base de dados

nome_linhas <- nrow(dados)
nome_colunas <- ncol(dados)

covid <- dados %>%
  select_if(is.numeric) %>%
  as.data.frame() %>%
  scale()

rownames(dados) <- dados$Country.Name

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
  geom_text_repel(aes(label = label), size = 5) +
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
  geom_text_repel(aes(label = label), size = 3) +
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
  geom_text_repel(aes(label = label), size = 3) +
  geom_point(aes(color = cluster), size = 3, show.legend = FALSE) + 
  theme_bw()


# Agrupamento PCA Complete

who_pca <- dados %>%
  select_if(is.numeric) %>%
  select(-Covid) %>%
  prcomp(scale = TRUE, center = TRUE)

fviz_eig(who_pca, addlabels = TRUE)

# Agrupamento hierárquico - complete

hc_pcaw <- hclust(dist(who_pca$x), method = "complete")

fviz_dend(hc_pcaw, 
          k = 5,
          main = "Dados",
          color_labels_by_k = TRUE, 
          horiz = TRUE,) + 
  theme_void()

tibble(coord1 = who_pca$Covid[,1], 
       coord2 = who_pca$Covid[,2], 
       label = rownames(dados_df), 
       cluster = factor(cutree(hc_pcaw, 5))) %>% 
  ggplot(aes(coord1, coord2)) + 
  geom_text_repel(aes(label = label), size = 3) +
  geom_point(aes(color = cluster), size = 3, show.legend = FALSE) + 
  theme_bw()

# Agrupamento hierárquico - single

hc_pcaw_s <- hclust(dist(who_pca$x), method = "single")

fviz_dend(hc_pcaw_s, 
          k = 5,
          main = "Dados",
          color_labels_by_k = TRUE, 
          horiz = TRUE,) + 
  theme_void()

tibble(coord1 = who_pca$Covid[,1], 
       coord2 = who_pca$Covid[,2], 
       label = rownames(dados_df), 
       cluster = factor(cutree(hc_pcaw_s, 5))) %>% 
  ggplot(aes(coord1, coord2)) + 
  geom_text_repel(aes(label = label), size = 3) +
  geom_point(aes(color = cluster), size = 3, show.legend = FALSE) + 
  theme_bw()

# Agrupamento hierárquico - centroid

hc_pcaw_c <- hclust(dist(who_pca$x), method = "centroid")

fviz_dend(hc_pcaw_c, 
          k = 5,
          main = "Dados",
          color_labels_by_k = TRUE, 
          horiz = TRUE,) + 
  theme_void()

tibble(coord1 = who_pca$Covid[,1], 
       coord2 = who_pca$Covid[,2], 
       label = rownames(dados_df), 
       cluster = factor(cutree(hc_pcaw_c, 5))) %>% 
  ggplot(aes(coord1, coord2)) + 
  geom_text_repel(aes(label = label), size = 3) +
  geom_point(aes(color = cluster), size = 3, show.legend = FALSE) + 
  theme_bw()

# Método K-Means

kmedio <- kmeans(dados_df, centers = 5)

kmedio

tibble(coord1 = kmedio$Handwashing, 
       coord2 = kmedio$Hospital.beds, 
       label = rownames(dados_df), 
       cluster = factor(cutree(kmedio, 5))) %>% 
  ggplot(aes(coord1, coord2)) + 
  geom_text_repel(aes(label = label), size = 3) +
  geom_point(aes(color = cluster), size = 3, show.legend = FALSE) + 
  theme_bw()

teste$withinss # para acessar a variável
# W(Ck)=∑xi∈Ck(xi−μk)2

auxiliar %>% 
  group_by(cluster) %>% 
  summarise(Handwashing = sum((Handwashing - mean(Handwashing))^2), 
            Hospital.beds = sum((Hospital.beds - mean(Hospital.beds))^2), 
            Public.medicine.availability = sum((Public.medicine.availability - mean(Public.medicine.availability))^2),
            Health.Legislation = sum((Health.Legislation - mean(Health.Legislation))^2),
            Health.Coordination = sum((Health.Coordination - mean(Health.Coordination))^2),
            Health.expense.GDP = sum((Health.expense.GDP - mean(Health.expense.GDP))^2),
            total_litres_alcohol = sum((total_litres_alcohol - mean(total_litres_alcohol))^2)) %>% 
  mutate(within = beer + spirit + wine + total_litres_alcohol)
