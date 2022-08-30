# Tipos de variables
variable.string = "cadena.de.valor"
variable.numero <- 500

# La asignaciópn puede ser ejecutada con menor que

variable = "texto"

# Class permite saber la clase del objeto
class(variable)

# Tipo del dato, especializaci'on del objeto

typeof(variable)
typeof(variable.string)
typeof(variable.numero)

View(variable)

if (variable == FALSE)
  print("Probando");


v3 = c(1,2,3,4,5,6,7,8,9)
class(v3)
typeof(v3)

# union de vectores
a = c(2,4,6,8)
b = c(1,3,5,7)
c = c("a","b","c","d")
d = c(10,20,30,40)
a + as.numeric(c)
table(a)
rev(a)
append(a,b)

matriz = rbind(a,b)
print(matriz)

resultado = ((43012180)+10)/4.1
print(resultado)

