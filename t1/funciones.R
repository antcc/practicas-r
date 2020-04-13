#!/usr/bin/Rscript
# Ejemplos de funciones en R
# Antonio Coín Castro

# %% Consdieraciones generales

# Establecemos una semilla para garantizar reproducibilidad
# en los cálculos aleatorios
set.seed(2020)

# %% Extracción de cartas de una baraja

#
# Esta función simula la extracción de cartas de una baraja de
# 52 cartas, con reemplazamiento. El objetivo es sacar los cuatro
# ases en un número concreto de intentos. Estos se encuentran en las
# posiciones 1, 14, 27 y 40. Devuelve los nombres de las cartas que
# se van sacando.
#
#   mostrar: controla si se muestra información al terminar.
#   max_intentos: número máximo de extracciones permitidas.
#
cuatro_ases_con <- function(mostrar = F, max_intentos = 1000) {
  intentos <- 0
  res <- vector(length = max_intentos)
  nombres <- vector(length = max_intentos)
  ases <- c(0, 0, 0, 0)

  # Nombres de los palos y las cartas
  palos <- c("Oros", "Copas", "Espadas", "Bastos")
  cartas <- c("As", "Dos", "Tres", "Cuatro", "Cinco", "Seis",
              "Siete", "Ocho", "Nueve", "Diez", "Sota", "Caballo", "Rey")

  # Vamos sacando cartas
  while (intentos < max_intentos) {
    # Sacamos una carta
    extraida <- sample(52, 1)
    intentos <- intentos + 1
    res[intentos] <- extraida

    # Calculamos el palo y la carta que es
    palo <- (extraida - 1) %/% 13 + 1
    carta <- (extraida - 1) %% 13 + 1
    nombres[intentos] <- paste(cartas[carta], "de", palos[palo])

    if (extraida %% 13 == 1) { # Si es un as
      ases[palo] <- 1
      if (sum(ases) == 4) { # Si tenemos 4 ases
        length(res) <- length(nombres) <- intentos
        if (mostrar)
          cat("He necesitado", intentos,
              "intentos para obtener cuatro ases.\n")
        return(list(e = intentos, r = res, n = nombres, conseguido = T))
      }
    }
  }

  if (mostrar)
    cat("No se han podido obtener cuatro ases en",
        intentos, "extracciones.\n")
  return(list(e = NA, r = res, n = nombres, conseguido = F))
}

# Probamos a extraer ases
cuatro_ases_con(T, 10)
cuatro_ases_con(T, 500)

#
# Esta función simula 'n' juegos de extracción de ases
# como el de arriba, de forma que podamos estudiar la distribución.
# Devuelve un vector con los intentos necesarios en cada juego.
#
distri_ases_con <- function(n = 5, max_intentos = 1000) {
  res <- vector(length = n)
  for (i in 1:n)
    res[i] <- cuatro_ases_con(F, max_intentos)$e
  res
}

# Repetimos el experimento 1000 veces y estudiamos la distribución
# de intentos.
distribucion_con <- distri_ases_con(1000)
summary(distribucion_con)
hist(distribucion_con)

#
# Esta función es similar a la anterior, pero simula la extracción
# de cartas sin reemplazamiento. Obtiene una permutación pseudoaleatoria
# de la baraja y luego extrae las cartas secuencialmente.
#
cuatro_ases_sin <- function(mostrar = F) {
  ases <- 0
  res <- sample(52) # Permutación de la baraja
  for (i in 1:52) {
    if (res[i] %% 13 == 1) { # Si es un as
      ases <- ases + 1
      if (ases == 4)
        break
    }
  }

  if (mostrar)
    cat("He necesitado", i,
        "intentos para obtener cuatro ases.\n")
  return(list(e = i, r = res[1:i]))
}

# Probamos a extraer ases
cuatro_ases_sin(T)

#
# Esta función simula 'n' juegos de extracción de ases
# sin reemplazamiento. Devuelve un vector con los intentos
# necesarios en cada uno.
#
distri_ases_sin <- function(n = 5) {
  res <- vector(length = n)
  for (i in 1:n)
    res[i] <- cuatro_ases_sin(F)$e
  res
}

# Realizamos el mismo estudio de la distribución que antes.
# Observamos que se necesitan unos 43 intentos de media.
distribucion_sin <- distri_ases_sin(1000)
summary(distribucion_sin)
hist(distribucion_sin)

# %% Lanzamiento de monedas

#
# Esta función simula el lanzamiento de monedas no cargadas
# hasta obtener un número determinado de caras seguidas.
#
n_caras <- function(n = 10, mostrar = F, max_intentos = 1000) {
  caras <- 0
  intentos <- 0
  res <- 1:max_intentos

  while (caras < n) {
    intentos <- intentos + 1
    lanzamiento <- sample(2, 1)  # '1' es cara, '2' es cruz
    res[intentos] <- ifelse(lanzamiento == 1, "Cara", "Cruz")
    if (lanzamiento == 1) { # Si es cara
      caras <- caras + 1
      if (caras == n) {
        length(res) <- intentos
        if (mostrar)
          cat("He necesitado", intentos, "lanzamientos para obtener",
              n, "caras seguidas.\n")
        return(list(l = intentos, r = res, conseguido = T))
      }
    }
    else { # Se corta la racha
      caras <- 0
    }
  }

  return(list(l = NA, r = res, conseguido = F))
}

# Tiramos algunas monedas
n_caras(3, T)

#
# Esta función simula 'm' juegos de lanzamiento de monedas
# hasta obtener 'n' caras. Devuelve un vector con los
# lanzamientos necesarios en cada uno.
#
distri_caras <- function(m = 5, n = 3, max_intentos = 1000) {
  res <- vector(length = m)
  for (i in 1:m)
    res[i] <- n_caras(n, F, max_intentos)$l
  res
}

# Estudiamos la distribución del número de lanzamientos
distribucion_caras <- distri_caras(1000, 5)
summary(distribucion_caras)
hist(distribucion_caras)

# %% Suma de potencias

#
# Esta función acepta un vector y devuelve las
# distintas sumas de las potencias señaladas del mismo.
#
sumpot <- function(x, pot = 2:3) {
  l <- length(pot)

  # Hacemos pre-allocation de una lista
  res <- vector("list", length = l)
  for (i in 1:l)
    res[[i]] <- sum(x^pot[i])

  return(res)
}

sumpot(1:10, c(3, 5, 7))
sumpot(1:100, c(2, 5))[[2]]

# %% Cuadrícula de distancias

#
# Esta función genera una cuadrícula sobre un conjunto de
# ordenadas (x) y abscisas (y), y para cada punto, calcula la distancia
# al origen.
#
distancia_origen <- function(x, y) {
  # Evitamos escribir el doble bucle usando el producto externo
  outer(x, y, function(x, y) sqrt(x^2 + y^2))
}

distancia_origen(1:3, -2:2)

# Podemos visualizar la superficie dada por la función
# f(X, Y) = sqrt(X^2 + Y^2)
persp(distancia_origen(-40:40, -40:40), col = "lightblue")

# %% Procesos no lineales

dong1 <- function(n = 100) {
  ini <- Sys.time()  # medimos el tiempo de ejecución

  x <- vector(mode = "numeric", length = n)
  y <- vector(mode = "numeric", length = n)
  x[1] <- y[1] <- 1

  for (i in 2:n) {
    m <- ifelse(sample(2, 1) == 2, 1, -1)
    x[i] <- 0.5 * x[i - 1] + 0.5 * y[i - 1] + m
    y[i] <- -0.5 * x[i - 1] + 0.5 * y[i - 1] + m
  }

  return(list(x = x[2:n], y = y[2:n], tiempo = Sys.time() - ini))
}

dong1_mejorada <- function(n = 100) {
  ini <- Sys.time()

  x <- vector(mode = "numeric", length = n)
  y <- vector(mode = "numeric", length = n)
  x[1] <- y[1] <- 1

  # Extraemos todos los números aleatorios de una tirada
  m <- sample(c(-1, 1), n, T)

  for (i in 2:n) {
    x[i] <- 0.5 * x[i - 1] + 0.5 * y[i - 1] + m[i]
    y[i] <- -0.5 * x[i - 1] + 0.5 * y[i - 1] + m[i]
  }

  return(list(x = x[2:n], y = y[2:n], tiempo = Sys.time() - ini))
}

dong2 <- function(n = 100) {
  x <- vector(mode = "numeric", length = n)
  y <- vector(mode = "numeric", length = n)
  x[1] <- y[1] <- 1

  # Creamos vectores para evitar varias sentencias 'if'
  xx <- c(0, 0.5, 0.25)
  yy <- c(0, 0, 0.5)

  for (i in 2:n) {
    a <- sample(3, 1)
    x[i] <- 0.5 * x[i - 1] + xx[a]
    y[i] <- 0.5 * y[i - 1] + yy[a]
  }

  return(list(x = x[2:n], y = y[2:n]))
}

dong3 <- function(n = 100) {
  x <- vector(mode = "numeric", length = n)
  y <- vector(mode = "numeric", length = n)
  x[1] <- y[1] <- 1

  for (i in 2:n) {
    # Aprovechamos el parámetro 'p' de la función 'sample'
    # para extraer números con distinta probabilidad:
    # 1 -> 0.01, 2 -> 0.85, 3 -> 0.07, 4 -> 0.07
    a <- sample(4, 1, p = c(1, 85, 7, 7))
    switch(a,
    { # Caso a=1
      x[i] <- 0
      y[i] <- 0.25 * y[i - 1]
    },
    { # Caso a=2
      x[i] <- 0.85 * x[i - 1] + 0.04 * y[i - 1]
      y[i] <- -0.04 * x[i - 1] + 0.85 * y[i - 1] + 1.6
    },
    { # Caso a=3
      x[i] <- 0.2 * x[i - 1] - 0.26 * y[i - 1]
      y[i] <- 0.26 * x[i - 1] + 0.22 * y[i - 1]
    },
    { # Caso a=4
      x[i] <- -0.15 * x[i - 1] + 0.28 * y[i - 1]
      y[i] <- 0.26 * x[i - 1] + 0.24 * y[i - 1] + 1
    }
    )
  }
  return(list(x = x[2:n], y = y[2:n]))
}

# Vemos que la versión mejorada tiene un tiempo de ejecución
# significativamente menor

dong1(1000)
dong1_mejorada(1000)
dong2()
dong3()
