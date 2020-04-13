#!/usr/bin/Rscript
# Antonio Coín Castro

# %% Vectores

# Todos los elementos son del mismo tipo. Si se modifica algún elemento,
# el resto es modificado para convertirse a su tipo.
x <- (1:10)^2
x
x[1] <- "a"
x

# Slicing
x[c(3, 5, 7)]

# Se repite el vector hasta que alcance la longitud
x + (1:5)

# Da un warning porque la longitud no es múltiplo
x + (2:5)

# Se repite la cadena FTF hasta completar la
# longitud del vector
x[c(F, T, F)]

# Acceso con condiciones (vector de Bool)
x > 10
x[x > 10]
x[1:length(x) %% 2 == 1]

# %% Matrices

# Crea una matriz 4x5 de 0s
matrix(0, 4, 5)

# Matriz 5 x 2 con los números del 1 al 10 (por columnas)
m1 <- matrix(1:10, 5)
m1

datos <- c(
  77, 1.63, 23,
  58, 1.63, 23,
  75, 1.74, 25,
  65, 1.65, 25
)
m2 <- matrix(datos, ncol = 3, byrow = T)

m2[, 1]  # Primera columna
m2[1, ]  # Primera fila
m2[4, 3]

# Operaciones elemento a elemento: +, -, *, /, %%, %*% (producto matricial)
m1 + m1

# Repite [1,2,3,4,5] y crea una matriz por columnas
m1 * (1:5)

# Producto exterior
x <- 1:3
y <- 1:4
outer(x, y)

# La operación M'M se puede conseguir con `crossprod`
m3 <- crossprod(m1)
m3
t(m1) %*% m1

# Podemos invertir una matriz cuadrada con `solve`
solve(m3) %*% m3

# También podemos resolver sistemas de ecuaciones
solve(matrix(c(3, 2, 1, -1), ncol = 2, byrow = T), c(5, 0))

# Podemos calcular los autovalores y autovectores con `eigen`.
# @arg symmetric indica que la matriz es simétrica. Si es F se inspecciona
# para comprobarlo.
# @arg only.values (=F) controla si se calculan autovectores.
eigen(m3)$values
eigen(m3)$vectors  # Matriz de autovectores por columnas
determinante <- function(x) prod(eigen(x)$values)
determinante(m3)

# %% Arrays

x <- array(1:6, 2:3)  # Matriz 2x3
tri <- array(1: (2 * 3 * 5), c(2, 3, 5))  # Array 2x3x5
tri[1, , ]

# Permutación: un vector que indica las variables que permutan
xt <- aperm(x, c(2, 1))
xtt <- aperm(x, c(2, 1), F)
xt
xtt

# %% Listas
l <-list(A=1:10, B=c("1", "a"))
l
l$A
l[[2]]
l[c(1,2)]

# %% Dataframes

h1 <- data.frame(Peso=c(90,87,60), Altura=c(1.85, 1.87, 1.63))
h1
h2 <- data.frame(h1, Sexo=c("H", "H", "M"))
h2[,3]
h2[3]
h2[[3]]

# La función I() permite introducir un vector sin transformarlo en factores
h3 <- data.frame(h2, Nombre=I(c("Pepe", "Juan", "Antonia")))
h3

# Subconjuntos y transformaciones
subset(h3,select=c(Sexo,Nombre))
subset(h3,subset=(Sexo=="H"))

transform(h3,Peso=log(Peso))
transform(h3,LogPeso=log(Peso))

# Comprobar tipo de objetos
x <- 1:50
x <- as.data.frame(x)
head(x)

# %% Funciones y estructuras

# if - else es escalar
# ifelse es vectorial
inverso <- function(x) ifelse(x==0, NA, 1/x)
inverso(-1:2)
