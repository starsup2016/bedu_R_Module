View(boxp)
bien <- complete.cases(data)
View(bien)
head(bien, 20)
datos <- boxp[bien,]
View(datos)
library(dplyr)
datos2 <- mutate(datos, Categoria = factor(Categoria), Grupo = factor(Grupo))
View(datos2)

library(ggplot2)
ggplot(datos2, aes(x = Categoria, y = Mediciones, fill = Grupo)) + geom_boxplot() +
  ggtitle("Boxplots") +
  xlab("Categorias") +
  ylab("Mediciones")

ggplot(datos2, aes(x = Categoria, y = Mediciones, fill = Grupo)) + geom_boxplot() +
  scale_fill_discrete(name = "Dos Gps", labels = c("G1", "G2")) + 
  ggtitle("Boxplots") +
  xlab("Categorias") +
  ylab("Mediciones")
