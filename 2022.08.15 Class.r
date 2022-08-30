# using the ggplot2 library
library(ggplot2)

mtcars

## Graficos de dispersi'on

#relacionar cilindros con caballos de fuerza
ggplot(mtcars, aes(x=cyl, y=hp)) +
  geom_point() + #1er nivel: tipo de gr'afico, geom_point para hacer graficos de dispersion
  theme_light() + #colocr un theme, color
  facet_wrap("am") + #separar las gr'aficas por variables categoricas
  xlab("Cilindros") + #agregar etiquetas para el eje de las X
  ylab("Caballos de Fuerza") #agregar etiquetas para el eje de las y


## Graficos de histogramas
boxp$Categoria

hist(boxp$Mediciones) #estadistica descriptiva

#usando ggplot
ggplot(boxp, aes(x=Mediciones)) + 
  geom_histogram(bins = 16, binwidth = 5) #agrupaciones de datos
