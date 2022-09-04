# Connecting to database via ODBC
library(DBI)
connection <- dbConnect(odbc::odbc(), "mysql", timeout = 10)
dbListTables(connection)

# Reading data for the team to do the analysis
Duke.Query <- "select * from mregularseasondetailedresults as dere join mteams as team on WTeamID = TeamID where WTeamID=1181 or LTeamID=1181;"
Duke.df <- dbGetQuery(connection, Duke.Query)
View(Duke.df)

# Cleaning dada
Duke.ok <- complete.cases(Duke.df)
Duke.df.clean <- Duke.df[Duke.ok,]
View(Duke.df.clean)


# Since we will use a few columns for the analysis, let extract the columns:
# 
library(tidyverse)
Duke.df.base <- Duke.df.clean %>% select("Season", "TeamName", "WScore", "WTeamID", "WFGA", "WFGM", "LScore", "LTeamID", "LFGA", "LFGM")
view(Duke.df.base)

GeneralStatistics = function(x){
  m = mean(x)
  md = median(x)
  n = length(x)
  s = sd(x)
  Asimetry = skewness(x)
  kurtosis = kurtosis(x)
  return(c(n=n, mean = m, median = md, des.estandar = s, Asimetry = Asimetry, kurtosis = kurtosis))
}

view(Duke.df.base)
summary(Duke.df.base)


# Building the Score Columns 
Duke.df.base$Duke.Scoreself <- 0
Duke.df.base$Duke.Scoreoppo <- 0
Duke.df.base$Duke.Perc <- 0.0000000
Duke.df.base$Oppo.Perc <- 0.0000000

# Unifiying the columns
Duke.df.base$Duke.Scoreself <- with( Duke.df.base, ifelse( TeamName == "Duke", WScore, LScore ))
Duke.df.base$Duke.Scoreoppo <- with( Duke.df.base, ifelse( TeamName != "Duke", WScore, LScore ))

# Calculating the percentage for the Winned Field Goal Attempted vs Field Goal Made
Duke.df.base$Duke.Perc <- with( Duke.df.base, ifelse( TeamName == "Duke", WFGM/WFGA, LFGM/LFGA ))
Duke.df.base$Oppo.Perc <- with( Duke.df.base, ifelse( TeamName != "Duke", WFGM/WFGA, LFGM/LFGA ))

Duke.df.base$Diff <- (Duke.df.base$Duke.Perc - Duke.df.base$Oppo.Perc)

view(Duke.df.base)


install.packages("quantmod")
library(quantmod)
install.packages("tseries")
library(tseries)
install.packages("fImport")
library(fImport)
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

# Checking The histogram for the Percentage for Duke
hist(Duke.df.base$Duke.Perc)
abline(v=median(Duke.df.base$Duke.Perc), col="red")

plot(Duke.df.base$Duke.Perc,Duke.df.base$Duke.Scoreself)
abline(lsfit(Duke.df.base$Duke.Perc,Duke.df.base$Duke.Scoreself))
Duke.modelo = lm(data = data.frame(Duke.df.base),  Duke.Perc ~ Duke.Scoreself)

summary(Duke.modelo)

# Ecuación lineal del modelo
# y = 0.174016 + 0.003709*X1

Duke.modelo$residuals
shapiro.test(Duke.modelo$residuals)
plot(Duke.modelo$residuals)

Duke.modelo.glm = glm(Duke.Perc ~ Duke.Scoreself, data = data.frame(Duke.df.base), family = "binomial")
plot(Duke.modelo.glm)






