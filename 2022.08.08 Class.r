# List
(milista <- list(nombre="pepe", no.hijos=3, edades.hijos= c(4, 7, 9)))
milista
# Accessing list id's
milista$edades.hijos

# accessing list index
milista[1]
milista[2]
milista[3]

# dataframe
x = c(1,2,3)
y = c(4,5,6)
z = c(7,8,9)

x = c(10000,12000,15000)
y = c(30,37,50)
z = c(.15,.17,.20)

# option 1
df = data.frame(x, y, z)
view(df)

# option 2
df = data.frame(salario=x, edad=y, tasaISR=z)
view(df)

# dataframe structure
str(df)

df[1,2]
df[-1]
df[2]

# operando con dataframes
df[-3,1:3]
df["salario"]<c(12000)

# Media
mean(df$tasaISR)

# summary
summary(df)

# Dataset dimension
dim(df)

# reading dataframes
setwd("D:/Downloads")
df2 = read.csv("bestsellers with categories.csv")
View(df2)
# ultimas lineas del dataset
tail(df2)
head(df2)
mean(df2[,5])
mean(df2$Price)

var(df2$Price)
dist(df)

short.view = df2[1:50,]
# write a file
write.csv(short.view,"short.view.csv")

dim(netflix)
str(netflix)

next.2015 = netflix[netflix$release_year > 2015,]

df_netflix_sorted = sort(netflix$release_year,TRUE)
head(df_netflix_sorted, 2000)
net.2015 = netflix$release_year>2015
net.2015 = df_netflix_sorted["release_year" > 2015,]
net.2015 = df_netflix_sorted[df_netflix_sorted$released_year > 2015,]
write.csv(net.2015, "net.2015.csv")
extract(netflix,"released_year",)
net.2015 = filter(netflix,release_year > 2015)
view(test)

vec = c(2000, 4000, 6000,8000, 10000)
mean(vec)
median(vec)

vector2 = c(0:100)
quantile(vector2, 0.25)

quantile(vector2, seq(0.1, 0.90, by = 0.01))

# variance
var(vector2)

# standard deviation
sd(vector2)
