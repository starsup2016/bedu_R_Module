GeneralStatistics = function(x){
  m = mean(x)
  md = median(x)
  n = length(x)
  s = sd(x)
  sesgo = sum(((x-m)^3/n)/s^3)
  curt = sum(((x-m)^4/n)/s^4) - 3
  return(c(n=n, mean = m, median = md, des.estandar = s, sesgo = sesgo, curtosis =curt))
}

iris

x = iris$Petal.Length

GeneralStatistics(x)

df1 = data.frame(x=x)

ggplot(df1, aes(x=df1$x)) + 
  geom_histogram(bins = 15)

####################
set.seed(1548)

x = rnorm(3315)
x2 = iris$Sepal.Width
x3= c(2258,2258,5478,569598,58418,78,48,784,84,887,7,589,859,494,94,94,94,41,14,51,5,5,54,5,78,451,58,478,4,587,8,48,74874,85,45,787,1578,541,5878,1485,45748,41,8574,8514,85,48,46,57,984,98,74,87,84,4,98,489,4,8974,8,74,41,621,534,5,21,657,54,98)

GeneralStatistics(x)
hist(x)
ggplot(x, aes(x)) +
  geom_histogram(x)
  
GeneralStatistics(x2)
hist(x2)
GeneralStatistics(x3)
hist(x3)

## pruebas de Hipotesis
# prueba de normalidad
install.packages("ggpubr")
data = read.csv("boxp.csv")

ggplot(data, aes(Mediciones)) +
  geom_histogram()

GeneralStatistics(na.omit(data$Mediciones))

shapiro.test(data$Mediciones)
# 1er. Todas la pruebas de hipotesis en R est'an al 95% de confianza
# 2do. Rechazaremos las hipotesis nula si el P/value es menor a 0.05 o 5%
# 3er. Siempre se considera que la hipotesis nula es igual al valor a contrastar e.g.
#      sies un prueba de normalidad por lo tanto la h0 es igual que la dist. es normal

## Regresi'on lineal univariada
prod = read.table("production.txt", header=T)
names(prod)
# plot equivale a geom_point en ggplot, es decir un gr'afico de dispersi'on
plot(prod$RunTime, prod$RunSize)

plot(prod$RunTime, prod$RunSize,
     xlab = "Time",
     ylab = "Size",
     pch = 14)

### correr un modelo de regresion lineal simple
modelo = lm(data = prod, RunSize ~ RunTime)
summary(modelo)

y = b0 + b1x
y = -367.35 + 2.81*X1

modelo$residuals

# los residuales deben ser de distribuci'on normal para x
shapiro.test(modelo$residuals)

## grafico de dispersion con recta de regresion
plot(prod$RunTime, prod$RunSize,
     xlab = "Time",
     ylab = "Size",
     pch = 8)
abline(lsfit(prod$RunTime, prod$RunSize))

## Reto

lin = read.csv("datoslineal.csv")
names(lin)
View(lin)
hist(lin$x)
plot(lin$x, lin$y)
abline(lsfit(lin$x,lin$y))
modelo_lin = lm(data = data.frame(lin), x ~ y)
summary(modelo_lin)
# y = -70.400 + 2.044*X1

ggplot(lin, aes(x = x, y = y)) +
  geom_point() +
  geom_abline()
