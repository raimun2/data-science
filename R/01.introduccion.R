pacman::p_load(tidyverse, stringi)

url_data <- "http://dataminingsoccer.com/de/wp-content/downloads/SoccerDataAllWorldCups.csv"

mundiales <- 
  read.table(url_data, sep=";", header=TRUE) %>% 
  mutate(goles_partido = score.A + score.B, 
         fase = case_when(stri_detect_regex(which.stage.of.the.turnament, 
                                            "^final|^semi final|^final round") ~ "finales",
                          stri_detect_regex(which.stage.of.the.turnament, 
                                            "third") ~ "3y4",
                          TRUE ~ "grupos"))



head(mundiales) 

hist(mundiales$goles_partido)

table(mundiales$which.stage.of.the.turnament, mundiales$fase)


fase_agg <-
  mundiales %>% 
  group_by(year, fase) %>% 
  summarise(goles_partido = mean(goles_partido))
           

head(fase_agg)


ggplot(fase_agg, aes(x=year, y =goles_partido, col=fase)) + 
  geom_point() +
  geom_smooth(method = 'loess' , formula = 'y ~ x') + 
  theme_minimal() + 
  xlab("Año del mundial") +
  ylab("# Goles") + 
  ggtitle("Promedio de goles por partido segun año y fase de los Mundiales")         

