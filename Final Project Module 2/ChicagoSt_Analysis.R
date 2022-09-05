# Libraries
# install.packages("quantmod")
# install.packages("tseries")
# install.packages("fImport")
# install.packages("ggpubr")
# install.packages("rstatix")

# activating libraries
library("DBI")
library("tidyverse")
library("quantmod")
library("tseries")
library("fImport")
library("dplyr")
library("ggpubr")
library("rstatix")

GeneralStatistics = function(x){
  m = mean(x)
  md = median(x)
  n = length(x)
  s = sd(x)
  Asimetry = skewness(x)
  kurtosis = kurtosis(x)
  return(c(n=n, mean = m, median = md, des.estandar = s, Asimetry = Asimetry, kurtosis = kurtosis))
}

# Connecting to database via ODBC
connection <- dbConnect(odbc::odbc(), "mysql", timeout = 10)
dbListTables(connection)

# Reading data for the team to do the analysis
Chicago.Query <- "select * from mregularseasondetailedresults as dere join mteams as team on WTeamID = TeamID where (WTeamID=1152 or LTeamID=1152) and Season<=2020;"
Chicago.df <- dbGetQuery(connection, Chicago.Query)
View(Chicago.df)

# Cleaning dada
Chicago.ok <- complete.cases(Chicago.df)
Chicago.df.clean <- Chicago.df[Chicago.ok,]
View(Chicago.df.clean)


# Since we will use a few columns for the analysis, let extract the columns:
# 

Chicago.df.base <- Chicago.df.clean %>% select("Season", "TeamName", "WScore", "WTeamID", "WFGA", "WFGM", "LScore", "LTeamID", "LFGA", "LFGM")
# Building the Score Columns 
Chicago.df.base$Chicago.Atte <- 0.0000000
Chicago.df.base$Chicago.Made <- 0.0000000
view(Chicago.df.base)

# Unifiying the columns
Chicago.df.base$Chicago.Atte <- with( Chicago.df.base, ifelse( TeamName == "Chicago", WFGA, LFGA ))
Chicago.df.base$Chicago.Made <- with( Chicago.df.base, ifelse( TeamName != "Chicago", WFGM, LFGM ))
view(Chicago.df.base)

# General Analysis results
GeneralStatistics(Chicago.df.base$Chicago.Perc)

# Calculating the Kurtosis. The Kurtosis is negative so the the distribution is platicurtic
kurtosis(Chicago.df.base$Chicago.Perc)

# Calculating skewness/Sesgo. The asymmetry is negative but really close to 0. The most of the data is grouped in a position a little
# less than the media
# 
skewness(Chicago.df.base$Chicago.Perc)

#Calculating Median - Negative asymmetry 
median(Chicago.df.base$Chicago.Perc)

#Calculating Mean
mean(Chicago.df.base$Chicago.Perc)

# Testing = Passed
a<- Chicago.df.base %>% 
  select(Season,TeamName,Chicago.Atte, Chicago.Made) %>%
  group_by(Season, TeamName) %>%
  summarise(total=n(), Chicago.Sum.Atte=sum(Chicago.Atte), Chicago.Sum.Made=sum(Chicago.Made)) %>%
  mutate(Total.Win.Perc=total/sum(total), Total.Atte= sum(Chicago.Sum.Atte), Total.Made= sum(Chicago.Sum.Made), Chicago.Succ=sum(Chicago.Sum.Made)/sum(Chicago.Sum.Atte))
view(a)

# write.csv(Chicago.df.base,"Chicago.df.base.csv")

data.f <- data.frame(subset(a, TeamName=="Chicago St"))
view(data.f)
hist(data.f$Total.Win.Perc)
hist(data.f$Chicago.Succ)
plot(data.f)

plot(x = data.f$Chicago.Succ, y = data.f$Total.Win.Perc, col=data.f$Season)


# Ecuación lineal del modelo
# y = 0.46009 + -0.04797*X1

# Aplicando Prueba de Shapiro
# El test de Shapiro-Wilks plantea la hipótesis nula que una muestra proviene de una distribución normal. 
# Eligimos un nivel de significanza, por ejemplo 0,05, y tenemos una hipótesis alternativa que sostiene 
# que la distribución no es normal.

# Tenemos:
#   H0: La distribución es normal
#   H1: La distribución no es normal
#
# 	Shapiro-Wilk normality test
#     data:  data.f$Total.Win.Perc
#     W = 0.91876, p-value = 0.1229
shapiro.test(data.f$Total.Win.Perc)

shapiro.test(Chicago.modelo2$residuals)
# como el p-value es significativamente mayor que 0.05
plot(Chicago.modelo$residuals)

ggplot(data = data.f) +
  aes(x=Chicago.Succ, y=Total.Win.Perc) +
  labs(x="Porcentaje de Aciertos", y= "Porcentaje de Victorias") +
  geom_point() +
  stat_function(fun = function(x){
    predict(Chicago.modelo,
            newdata = data.frame(Chicago.Succ=x),
            type = "response")
  })

data.f

# 0. Null Hypothesis - There is a positive relation between the 'Porcentaje de Aciertos' y
#                      El porcentaje de victorias
#    Alternative Hypothesys - There is a null or negative relation  the 'Porcentaje de Aciertos' y
#                      El porcentaje de victorias
# 
# 1. Determinar si La relación es lineal entre Porcentaje de aciertos y Porcentaje de Victorias
#    R= -0.3, p= 0.22
ggscatter(data = data.frame(data.f), x = "Chicago.Succ", y = "Total.Win.Perc",
          add = "reg.line",conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman",
          xlab = "Porcentaje de Acierto", ylab = "Porcentaje de Victorias")

ggplot(data.f)+
  aes(x = Chicago.Succ, y = Total.Win.Perc) +
  xlab("Porcentaje de Aciertos") + 
  ylab("Porcentaje de Victorias") +
  geom_point() +
  geom_smooth(method = "lm", se = F)

# 2. Prueba de Shapiro_Wilk para determinar si la distribuci'on es normal'
#   data:  data.f$Total.Win.Perc
#   W = 0.91876, p-value = 0.1229 / La distribución no es normal
shapiro.test(data.f$Total.Win.Perc)

#   data:  data.f$Chicago.Succ
#   W = 0.95304, p-value = 0.4747 / La distribución es normal
shapiro.test(data.f$Chicago.Succ)

# 3. Como solo una de las variables presenta distribución normal no se puede 
#    usar el metodo de Pearson, por ende se usa el método de Spearman
# Pearson rho value
#   -1 – a perfectly negative association between the two variables
#    0 – no association between the two variables
#    1 – a perfectly positive association between the two variables
# Dado que  rho = -0.3047522, tenemos una correlación ligeramente negativa
# Adicionalmente, como el P/value=0.8906 es mayor que 0.05 se rechaza la Hipotesis Nula
cor.test(data.f$Chicago.Succ, data.f$Total.Win.Perc, method = "spearman", alternative = "greater")

# 4. Construir un modelo de regresión lineal
Chicago.modelo = lm(data = data.frame(data.f),  Total.Win.Perc ~ Chicago.Succ)

# 5. Predecir usando el Modelo
Chicago.Valores <- data.frame(Chicago.Succ=0.80) # Que pasar'ia si llega al 50% los aciertos
predict(Chicago.modelo, Chicago.Valores)
summary(Chicago.modelo)  


