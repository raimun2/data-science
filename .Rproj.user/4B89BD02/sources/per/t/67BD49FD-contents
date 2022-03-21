pacman::p_load(dbscan, tidyverse, Rtsne)

# cargo datos
data  <- read.csv("data/video_games_sales.csv") %>% 
  filter(!(is.na(Critic_Score) | is.na(User_Score))) %>% 
  as_tibble()

# les echo un vistazo
data %>% glimpse()

# grafico exploratorio
ggplot(data, aes(Critic_Score, User_Score)) + 
  geom_point()

# limpio los NAs, cambio a numerico
data_clean <- data %>% 
  mutate(User_Score = as.numeric(User_Score)) %>% 
  filter(!(is.na(Critic_Score) | is.na(User_Score))) %>%
  select(Critic_Score, User_Score, User_Count, Global_Sales) %>% 
  unique()

# echo un vistazo
data_clean %>% glimpse()

# extraigo los componentes principales usando el operador %>%
data_PCA <- data_clean %>% 
  prcomp(scale. = TRUE) %>% 
  predict() %>% 
  .[,1:2] %>% 
  as_tibble()

# hago lo mismo utilizando R base
data_PCA2 <- as_tibble(predict(prcomp(data_clean, scale. = TRUE))[,1:2])

# visualizo PCA
ggplot(data_PCA, aes(PC1, PC2)) + 
  geom_point(alpha = 0.5)

set.seed(42)

# extraigo dimensiones obtenidas del tsne
data_tsne <- data_clean %>% 
  unique() %>% 
  Rtsne() %>% 
  .$Y %>% 
  as.data.frame()

# visualizo tsne
ggplot(data_tsne, aes(V1, V2)) + 
  geom_point(alpha = 0.5)


# visualizo grafico de kNN
kNNdistplot(data_tsne, k = 4)
abline(h=1.2, col = "red", lty = 2)
dev.off() 

# hago un DBScan con parametro observado
modelo_dbscan <- dbscan(data_tsne, eps = 1.2, minPts = 5)

#visualizo
ggplot(data_tsne, aes(V1, V2, col = factor(modelo_dbscan$cluster))) + 
  geom_point(alpha = 0.5) +
  theme(legend.position = "none") +
  scale_color_manual(values = colors())

modelo_dbscan$cluster %>% max()

max(modelo_dbscan$cluster)

modelo_dbscan$cluster %>% unique() %>% length()

estad <- data_clean %>% 
  mutate(cluster = modelo_dbscan$cluster) %>% 
  group_by(cluster) %>%
  summarise(mean(User_Score),
            mean(Critic_Score),
            mean(Global_Sales),
            mean(User_Count))

