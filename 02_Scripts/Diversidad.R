# Shannon


abund1 <- c (80, 20)
abund2 <- c (50, 50, 50)

total1 <- sum (abund1)
total2 <- sum (abund2)

frec1 <- abund1/total1
frec2 <- abund2/total2

mult1 <- (log(frec1))*frec1
mult2 <- (log(frec2))*frec2

shannon1 <- -(sum(mult1))
shannon2 <- -(sum(mult2))

print (shannon1)
print (shannon2)

log (2)
log (3)


Shannon <- function (abund) {
  
  riqueza <- length (abund)
  total <- sum (abund)
  frec <- abund/total
  mult <- (log(frec))*frec
 # shannon <- -(sum(mult)) # Si el objeto tiene el mismo nombre que la función pueden haber problemas, mejor cambiarle el nombre
  s <- -(sum(mult))
 print (s) # Esto solo lo imprime
  p <- s/log(riqueza)
  
  message ("Índice de Shannon")
  print (s) # Esto solo lo imprime
 # return (s) # Esto hace que el objeto se cree con la función, así no solo se ve  
  
  message ("Pielou")
  print (p)
}

Shannon (abund2)

####################################################################################################

# Simpson

abu <- c (4, 7, 10, 1)

simpson <- function (abu){ # aproximación de simpson, debe usarse solo en muestras con n y N grande
  
  riq <- length (abu)
  tot <- sum (abu)
  
  suma <- c()
  
  for (i in 1:riq) {
    
    E <- ((abu[i])/tot)^2
    suma [i] <- E
    
  }
  
  simp <- sum (suma)
  return (simp)
}

simpson (abu) 

# Ejercicio

# 1. n1 = 500, n2 = 500
a1 <- c (500, 500)
simpson (a1)
# 2. n1 = 500, n2 = 50
a2 <- c (600, 400)
simpson (a2)
# 3. n1 = 500, n2 = 500
a3 <- c (750, 250)
simpson (a3)
# 4. n1 = 500, n2 = 500
a4 <- c (900, 100)
simpson (a4)
# 5. n1 = 500, n2 = 500
a5 <- c (990, 10)
simpson (a5)


simpson_inv <- function (abundancia) {
  
  simp <- simpson (abundancia)
  inv <- 1/simp
  
  return (inv)
  
}

simpson_inv (a5)

gini_simpson <- function (abundancia) {
  
  gini <- 1 - (simpson_inv (abundancia))
  return (gini)
}

gini_simpson (a5)


###################################################################################################

# Rank-abundance

abu <- c (4, 7, 10, 1)

rank_abundance <- function (abu) {
  
  ordenado <- sort (abu, decreasing = T)
  plot (ordenado)
  
}

rank_abundance (abu)

experimento <- rep (4, 100) # Se espera que si todas las abundancias son iguales, la línea sería recta horizontal
rank_abundance (experimento) # Se cumple
