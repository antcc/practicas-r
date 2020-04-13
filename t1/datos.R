#!/usr/bin/Rscript
# Entrada, salida y manipulación de datos
# Antonio Coín Castro

# %% Entrada de datos

# Usamos read.csv con header = T (por defecto), pues los datos están
# separados por ',' y tienen una primera línea de cabecera.
datos <- read.csv("http://www.ugr.es/local/andresgc/Datos.txt", as.is = T)

# Usamos read.table para leer el segundo archivo, con los datos
# separados por espacios.
datos2 <-
  invisible(read.table(
    "http://www.ugr.es/local/andresgc/Datos2.txt",
    header = T, as.is = T))

# Vemos como tenemos nuevos objetos con los datos leídos
ls()
datos
datos2

# Podemos usar attach() para tener disponibles los objetos
# dentro de los datos leídos
attach(datos)
Peso
detach(datos)

# %% Estudio de los datos

# Nos muestra el mínimo, el máximo, la media, la mediana, y los
# cuartiles primero y tercero de los datos numéricos.
summary(datos)
summary(datos2)

# Realizamos un histograma de algunas columnas de los datos
hist(datos$Peso)
hist(datos2$Peso)
hist(datos2$Altura)

# Por último, podemos realizar algún diagrama de caja.
# Estos dividen los datos en 3 cuartiles, y se representan
# el mínimo, el máximo, la mediana, y los cuartiles primero y
# tercero.
boxplot(Altura ~ Sexo, data = datos)
boxplot(Peso ~ Edad, data = datos)

# %% Añadir funcionalidad mediante libros

# Devuelve los libros por defecto
library()

# Consultamos la ayuda de uno de ellos
library(help = "cluster")

# Cargamos un par de ellos y observamos que se añaden a la lista
library(cluster)
library(curl)
search()

# Podemos utilizar las funciones del libro 'cluster'.
# Por ejemplo, escribimos una función para aplicar
# K-medias a los datos que habíamos leído, y mostrar los resultados.

fit_kmeans <- function(data, k) {
  fit <- kmeans(data, k)

  # Esta función del libro 'cluster' muestra gráficamente
  # los clusters obtenidos, frente a las 2 primeras componentes
  # principales
  clusplot(
    data, fit$cluster, color = T,
    shade = T, labels = 2, lines = 0)
}

fit_kmeans(datos[c(1, 2, 3)], 4)

# Descargamos el libro 'curl' (que ocupa la segunda posición)
detach()
# Descargamos el libro 'cluster'
detach()
