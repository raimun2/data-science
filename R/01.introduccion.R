#    aca estoy leyendo una tabla csv que se encuentra publica en internet, y viene separada por ";" y con encabezados
Mundiales = read.table("http://dataminingsoccer.com/de/wp-content/downloads/SoccerDataAllWorldCups.csv", sep=";",header=T)

#  con esta linea de codigo inspeccionamos la data, cada fila en la data representa un partido
head(Mundiales) 

#    creamos una variable con los goles por partido
Mundiales$goles_partido = Mundiales$score.A + Mundiales$score.B

hist(Mundiales$goles_partido)

#    veo si las etapas del torneo son consistentes
table(Mundiales$which.stage.of.the.turnament)

# iniciamos la variable con el valor "grupos" para todos los partidos
Mundiales$fase = "grupos"

#    luego cambiamos aquellas fases que contengan la palabra "final" (final, semi final y final round)
Mundiales$fase[grep("^final|^semi final|^final round",Mundiales$which.stage.of.the.turnament)] = "finales"

#    y finalmente cambiamos aquellas fases que contengan la palabra "third" por 3y4
Mundiales$fase[grep("third",Mundiales$which.stage.of.the.turnament)] = "3y4"

table(Mundiales$fase)

data_fase = aggregate(goles_partido ~ year+fase, Mundiales, mean)

head(data_fase)

data_fase = aggregate(goles_partido ~ year+fase, Mundiales, mean)

head(data_fase)

#    creamos grafico de evolucion de los goles por partido en el tiempo, diferenciado por fase simplificada
library(ggplot2)
ggplot(data_fase, aes(x=year, y =goles_partido, col=fase)) + 
  geom_point() +
  geom_smooth(method = 'loess' , formula = 'y ~ x') + 
  theme_minimal() + 
  xlab("Año del mundial") +
  ylab("# Goles") + 
  ggtitle("Promedio de goles por partido segun año y fase de los Mundiales")

### reduccion de dimensiones
autos = mtcars

# aplicamos componentes principales y generamos un objeto
PCA_cars = prcomp(autos)

cars2 = as.data.frame(predict(PCA_cars))

ggplot(cars2, aes(PC1, PC2)) + geom_point()

PCA_cars_scales = prcomp(autos, scale. = TRUE)

cars3 = as.data.frame(predict(PCA_cars_scales))

ggplot(cars3, aes(PC1, PC2)) + geom_point()

library(Rtsne)
set.seed(42)

tsne_cars = Rtsne(autos, perplexity = 8)

cars4 = as.data.frame(tsne_cars$Y)

ggplot(cars4, aes(V1, V2)) + geom_point()




