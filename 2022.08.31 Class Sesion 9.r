install.packages("DBI", "RMySQL")
library("RMySQL")

# Connecting to MySQL localhost database
datos = dbConnect(
  RMySQL::MySQL(), dbname = "basket", username = "root", password = "password"
)
dbListTables(datos)

# Connect to a remote amazon database
datos <- dbConnect(
  drv = RMySQL::MySQL(),
  dbname = "shinydemo",
  host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
  username = "guest",
  password = "guest"
)

dbListTables(datos)
dbListFields(datos, "Country")

query <- "Select * from City"
dataframe <- dbGetQuery(datos, query)
print(dataframe)

install.packages("ISLR")
library("ISLR")
library("tidyverse")

hist(as.numeric(Default$default))
data = Default
data = data %>%
  select(default, balance) %>%
  mutate(default = recode(default,
                          "No" = 0,
                          "Yes" = 1))

ggplot(data = data) +
  aes(x=balance, y=default) +
  geom_point(aes(color=as.factor(default))) +
  geom_smooth(method = "lm", se = F)
# Para datos categ'oricos no es factible usar un modelo de regresi'on lineal simple

modelo = glm(default ~ balance, data = data, family = "binomial")
plot(modelo)

ggplot(data = data) +
  aes(x=balance, y=default) +
  geom_point(aes(color=as.factor(default)), shape = 1) +
  stat_function(fun = function(x){
    predict(modelo,
            newdata = data.frame(balance=x),
            type = "response")
  })


predict(object = modelo, type = "response",
        newdata = data.frame(balance = 50))
