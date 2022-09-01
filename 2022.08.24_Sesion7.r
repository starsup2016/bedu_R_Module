library(DBI)
con <- dbConnect(odbc::odbc(), "mysql", timeout = 10)
dbListTables(con)
# CONSEJO: evitar hacer proyecciones y modelaciones de series de esta clase 
# (no cíclicas ni en el modelo aditivo ni en el multiplicativo), a pesar de que 
# en este caso el valor tendencia si sirva y sea bueno, pero al no haber 
# correlaciones en los errores (no patrones en random) implica que esa serie 
# puede tener eventualmente explosividad.




library("TSA")
library("ggplot2")
ok <- complete.cases(Month_Value_1)
datos <- Month_Value_1[ok,]
View(datos)

GeneralStatistics = function(x){
  m = mean(x)
  md = median(x)
  n = length(x)
  s = sd(x)
  sesgo = sum(((x-m)^3/n)/s^3)
  curt = sum(((x-m)^4/n)/s^4) - 3
  return(c(n=n, mean = m, median = md, des.estandar = s, sesgo = sesgo, curtosis =curt))
}

names(datos)

revenue <- ts(datos[,2], start = c(2015,1), frequency = 12)
plot(revenue)
sales <- ts(datos[,3], start = c(2015,1), frequency = 12)
plot(sales)

ggplot(datos, aes(x = Revenue, y = Sales_quantity)) +
  geom_point() +
  geom_abline()

aditivo.revenue = decompose(revenue, type = "additive")
plot(aditivo.revenue)
multi.revenue = decompose(revenue, type = "multiplicative")
plot(multi.revenue)
aditivo.revenue.df <- as.data.frame(x=aditivo.revenue$trend)


aditivo.sales = decompose(sales, type = "additive")
plot(aditivo.sales)
multi.sales = decompose(sales, type = "multiplicative")
plot(multi.sales)
aditivo.sales.df <- as.data.frame(x=aditivo.sales$trend)


# rezagos??
# tasa unificada de crecimiento

# autocorrelacion
# correlacion del rezago del valor observado
acf(elec)

# si es descendiente y todos sobrepasan la l'inea de significancia en un modelo MA

# autocorrelacion parcial
# correlacion del rezago del valor error (o aleatorio)
pacf(elec)

# Medias moviles, Movile Average (MA): Todas las líneas no son significativas y son descendentes en el ACF
# Average regresive (AR): 

# Proyección
library(forecast)
# approximation to TRUE is better. AKAIKE
modelo <- auto.arima(elec, d=1, D=1, stepwise = FALSE, approximation = FALSE, trace = TRUE)

# principio de informacion de akaike (puntua la complejidad del modelo, donde
# un orden inferior , especialmente el 1 sera preferido)

# Se usa la función forecast para proyectar
# h -: número de meses a proyectar
# level: sería el intervalo de confianza para valores max y min
fcs = forecast(modelo, h=12, level = 95)
summary(fcs)

autoplot(fcs) +
  ggtitle("Next MOnths")
  



