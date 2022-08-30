# Class 08 Aug 2022
url1 <- "https://data.humdata.org/hxlproxy/data/download/time_series_covid19_confirmed_global_narrow.csv?dest=data_edit&filter01=explode&explode-header-att01=date&explode-value-att01=value&filter02=rename&rename-oldtag02=%23affected%2Bdate&rename-newtag02=%23date&rename-header02=Date&filter03=rename&rename-oldtag03=%23affected%2Bvalue&rename-newtag03=%23affected%2Binfected%2Bvalue%2Bnum&rename-header03=Value&filter04=clean&clean-date-tags04=%23date&filter05=sort&sort-tags05=%23date&sort-reverse05=on&filter06=sort&sort-tags06=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv"
url2 <- "https://data.humdata.org/hxlproxy/data/download/time_series_covid19_deaths_global_narrow.csv?dest=data_edit&filter01=explode&explode-header-att01=date&explode-value-att01=value&filter02=rename&rename-oldtag02=%23affected%2Bdate&rename-newtag02=%23date&rename-header02=Date&filter03=rename&rename-oldtag03=%23affected%2Bvalue&rename-newtag03=%23affected%2Binfected%2Bvalue%2Bnum&rename-header03=Value&filter04=clean&clean-date-tags04=%23date&filter05=sort&sort-tags05=%23date&sort-reverse05=on&filter06=sort&sort-tags06=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_deaths_global.csv"

download.file(url = url1, destfile = "confirmados.csv", mode = "wb")
download.file(url = url2, destfile = "muertes.csv", mode = "wb")

conf = read.csv("confirmados.csv")
mu = read.csv("muertes.csv")

View(conf)
View(mu)

head(mu[,6], 50)

# dplyr
# get the head of the dataframe
dplyr::slice_head(mu, n = 50)
dplyr::slice_tail(mu, n = 50)
fields = c("Country.Region", "Value")
dplyr::select(mu, 2, 6)
dplyr::select(mu, Country.Region, Value)
dplyr::select(mu, fields)

# Rename fields as result, without renaming the original dataframe
conf = dplyr::rename(conf, pais = Country.Region, confirmados = Value)
conf = dplyr::transmute(conf, pais = Country.Region, confirmados = Value)
new.var = dplyr::transmute(conf, pais = Country.Region, confirmados = Value)
View(conf)
View(new.var)

# Fechas en R
# 1. El operador principal es el simbolo %
# 2. El separador de componente de fecha es -
conf = dplyr::mutate(conf, Fecha = as.Date(Date, "%Y-%m-%d"), confirmados = as.numeric(confirmados))

# cambios precisos al Dataframe
Mexico = dplyr::filter(conf, Country.Region == "Mexico" | Country.Region == "Mexico")
View(Mexico)

# Valores calculados
mexico_acum = dplyr::mutate(conf, acumulago_lag = c(-1, diff(confirmados)))
View(mexico_acum)
mexico_acum2 = dplyr::mutate(conf, acumulago_lag = c(-1, diff(Value)))
View(mexico_acum2)