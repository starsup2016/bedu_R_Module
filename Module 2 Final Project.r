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
  Duke.Query <- "select * from mregularseasondetailedresults as dere join mteams as team on WTeamID = TeamID where (WTeamID=1181 or LTeamID=1181) and Season<=2020;"
  Duke.df <- dbGetQuery(connection, Duke.Query)
  View(Duke.df)

# Cleaning dada
  Duke.ok <- complete.cases(Duke.df)
  Duke.df.clean <- Duke.df[Duke.ok,]
  View(Duke.df.clean)


# Since we will use a few columns for the analysis, let extract the columns:
# 

Duke.df.base <- Duke.df.clean %>% select("Season", "TeamName", "WScore", "WTeamID", "WFGA", "WFGM", "LScore", "LTeamID", "LFGA", "LFGM")
# Building the Score Columns 
Duke.df.base$Duke.Atte <- 0.0000000
Duke.df.base$Duke.Made <- 0.0000000
view(Duke.df.base)

# Unifiying the columns
Duke.df.base$Duke.Atte <- with( Duke.df.base, ifelse( TeamName == "Duke", WFGA, LFGA ))
Duke.df.base$Duke.Made <- with( Duke.df.base, ifelse( TeamName != "Duke", WFGM, LFGM ))
view(Duke.df.base)

# General Analysis results
GeneralStatistics(Duke.df.base$Duke.Perc)

# Calculating the Kurtosis. The Kurtosis is negative so the the distribution is platicurtic
kurtosis(Duke.df.base$Duke.Perc)

# Calculating skewness/Sesgo. The asymmetry is negative but really close to 0. The most of the data is grouped in a position a little
# less than the media
# 
skewness(Duke.df.base$Duke.Perc)

#Calculating Median - Negative asymmetry 
median(Duke.df.base$Duke.Perc)

#Calculating Mean
mean(Duke.df.base$Duke.Perc)

# Testing = Passed
a<- Duke.df.base %>% 
  select(Season,TeamName,Duke.Atte, Duke.Made) %>%
  group_by(Season, TeamName) %>%
  summarise(total=n(), Duke.Sum.Atte=sum(Duke.Atte), Duke.Sum.Made=sum(Duke.Made)) %>%
  mutate(Total.Win.Perc=total/sum(total), Total.Atte= sum(Duke.Sum.Atte), Total.Made= sum(Duke.Sum.Made), Duke.Succ=sum(Duke.Sum.Made)/sum(Duke.Sum.Atte))
view(a)

# write.csv(Duke.df.base,"duke.df.base.csv")

data.f <- data.frame(subset(a, TeamName=="Duke"))
view(data.f)
hist(data.f$Total.Win.Perc)
hist(data.f$Duke.Succ)
plot(data.f)

plot(x = data.f$Duke.Succ, y = data.f$Total.Win.Perc, col=data.f$Season)


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
#     W = 0.83391, p-value = 0.0029
shapiro.test(data.f$Total.Win.Perc)
  
shapiro.test(Duke.modelo2$residuals)
# como el p-value es significativamente mayor que 0.05
plot(Duke.modelo$residuals)

ggplot(data = data.f) +
  aes(x=Duke.Succ, y=Total.Win.Perc) +
  labs(x="Porcentaje de Aciertos", y= "Porcentaje de Victorias") +
  geom_point() +
  stat_function(fun = function(x){
    predict(Duke.modelo,
            newdata = data.frame(Duke.Succ=x),
            type = "response")
  })

data.f

# 0. Null Hypothesis - There is a positive relation between the 'Porcentaje de Aciertos' y
#                      El porcentaje de victorias
#    Alternative Hypothesys - There is a null or negative relation  the 'Porcentaje de Aciertos' y
#                      El porcentaje de victorias
# 
# 1. Determinar si La relación es lineal entre Porcentaje de aciertos y Porcentaje de Victorias
#    R= -0.16, p= 0.5
  ggscatter(data = data.frame(data.f), x = "Duke.Succ", y = "Total.Win.Perc",
            add = "reg.line",conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman",
            xlab = "Porcentaje de Acierto", ylab = "Porcentaje de Victorias")
  
  ggplot(data.f)+
    aes(x = Duke.Succ, y = Total.Win.Perc) +
    xlab("Porcentaje de Aciertos") + 
    ylab("Porcentaje de Victorias") +
    geom_point() +
    geom_smooth(method = "lm", se = F)

# 2. Prueba de Shapiro_Wilk para determinar si la distribuci'on es normal'
  #   data:  data.f$Total.Win.Perc
  #   W = 0.83391, p-value = 0.0029 / La distribución no es normal
  shapiro.test(data.f$Total.Win.Perc)
  
  #   data:  data.f$Duke.Succ
  #   W = 0.96085, p-value = 0.5609 / La distribución es normal
  shapiro.test(data.f$Duke.Succ)
  
# 3. Como solo una de las variables presenta distribución normal no se puede 
#    usar el metodo de Pearson, por ende se usa el método de Spearman
# Pearson rho value
#   -1 – a perfectly negative association between the two variables
#    0 – no association between the two variables
#    1 – a perfectly positive association between the two variables
# Dado que  rho = -0.3352172, tenemos una correlación ligeramente negativa
# Adicionalmente, como el P/value=0.926 es mayor que 0.05 se rechaza la Hipotesis Nula
  cor.test(data.f$Duke.Succ, data.f$Total.Win.Perc, method = "spearman", alternative = "greater")
  
# 4. Construir un modelo de regresión lineal
  Duke.modelo = lm(data = data.frame(data.f),  Total.Win.Perc ~ Duke.Succ)
  
# 5. Predecir usando el Modelo
  Duke.Valores <- data.frame(Duke.Succ=0.50) # Que pasar'ia si llega al 50% los aciertos
  predict(Duke.modelo, Duke.Valores)
  summary(Duke.modelo)  
  

