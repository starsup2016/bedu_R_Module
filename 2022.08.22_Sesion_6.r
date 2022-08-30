summary(nyc)

modelo = lm(data = nyc, Price ~ Food + Decor + Service)

summary(modelo)

install.packages("odbc")
db = dbConnect(odbc(),
               Driver = "MySQL ODBC 5.3 ANSI Driver",
               Server = "127.0.0.1",
               database = "database name",
               uid = "username",
               pwd = "password")

modelo = lm(data = nyc, log(Price) ~ log(Food) + log(Decor))
print(log(nyc$Price))


## Time series
View(cbe)
cbe2 = sort(cbe, cbe$lec)

install.packages("TSA") #time series analysis
library("TSA")

names(cbe)

# van de enero de 1958 a diciembre del '90
choc = ts(cbe[,1], start = c(1958,1), frequency = 12)
beer = ts(cbe[,2], start = 1958, frequency = 12)
elec = ts(cbe[,3], start = 1958, frequency = 12)

plot(elec)
class(elec)
# los objetos ts tienen atributos especiales que no encontraremos en otros

# Modelaci'on de componentes de la serie de tiempo

# aditivo
aditivo.elec = decompose(elec, type = "additive")

# Si la varianza es constante, entonces se usa el modelo aditivo
aditivo.elec = decompose(elec)
plot(aditivo.elec)

# Multiplicativo
data.elect = elec

# Si la varianza es no constante, entonces se usa el modelo multiplicativo
mult.elec = decompose(elec, type = "multiplicative")
#mult.elec = decompose(elec + aditivo.elec$seasonal)
plot(mult.elec)

#################################################
## INVESTIGAR Modelo aditivo vs Multiplicativo ##
#################################################

