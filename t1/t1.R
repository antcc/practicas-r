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
