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

auxiliar <- tibble(cluster = kmedio$cluster) %>% 
  bind_cols(as_tibble(dados_df))

# Total SS

kmedio$totss

auxiliar %>% 
  transmute(Handwashing = (Handwashing - mean(Handwashing))^2, 
            Hospital.beds = (Hospital.beds - mean(Hospital.beds))^2,
            Public.medicine.availability = (Public.medicine.availability - mean(Public.medicine.availability))^2,
            Health.Legislation = (Health.Legislation - mean(Health.Legislation))^2,
            Health.Coordination = (Health.Coordination - mean(Health.Coordination))^2,
            Health.expense.GDP = (Health.expense.GDP - mean(Health.expense.GDP))^2,
            totss = Handwashing + Hospital.beds + Public.medicine.availability + Health.Legislation + Health.Coordination + Health.expense.GDP) %>% 
  summarise(totss = sum(totss))

# WITHIN SS

kmedio$withinss # para acessar a variável

auxiliar %>% 
  group_by(cluster) %>% 
  summarise(Handwashing = sum((Handwashing - mean(Handwashing))^2), 
            Hospital.beds = sum((Hospital.beds - mean(Hospital.beds))^2), 
            Public.medicine.availability = sum((Public.medicine.availability - mean(Public.medicine.availability))^2),
            Health.Legislation = sum((Health.Legislation - mean(Health.Legislation))^2),
            Health.Coordination = sum((Health.Coordination - mean(Health.Coordination))^2),
            Health.expense.GDP = sum((Health.expense.GDP - mean(Health.expense.GDP))^2),
            totss = Handwashing + Hospital.beds + Public.medicine.availability + Health.Legislation + Health.Coordination + Health.expense.GDP) %>%
  mutate(within = Handwashing + Hospital.beds + Public.medicine.availability + Health.Legislation + Health.Coordination + Health.expense.GDP)

# método do cotovelo ------------------------------------------------------

criterio <- function(k) kmeans(dados_df, k)$tot.withinss

estudo <- tibble(k = 1:15) %>% 
  mutate(w = map_dbl(k, criterio))

estudo %>% 
  ggplot(aes(k, w)) + 
  geom_point(size = 3) + 
  geom_line() + 
  labs(y = "total within sum of squares", x = "k") +
  scale_x_continuous(breaks = 1:15)

# especifica k  

kmedias <- kmeans(dados_df, centers = 6)

dados <- dados %>% 
  mutate(cluster = kmedias$cluster)

dados %>% 
  mutate(cluster = as.factor(cluster)) %>% 
  ggplot(aes(Handwashing, Hospital.beds, color = cluster)) + 
  geom_point(size = 3, alpha = .5) + 
  theme(legend.position = "top")

# silhoutte ---------------------------------------------------------------

kmedias <- kmeans(dados_df, centers = 6)

res_silhouette <- silhouette(kmedias$cluster, dist(dados_df)^2)

res_silhouette

fviz_silhouette(res_silhouette) 

fviz_nbclust(dados_df, kmeans, method = "silhouette")

# gap ---------------------------------------------------------------------

res_gap <- clusGap(dados_df, 
                   FUN = kmeans, 
                   d.power = 2,
                   nstart = 30, 
                   iter.max = 30,
                   K.max = 20, 
                   B = 50)

fviz_gap_stat(res_gap)
