# TEORÍA DE REDES

install.packages ("igraph")
library (igraph)

######################################################
# CREACIÓN DE GRAFO

# Crear un grafo dirigido vacío con 5 nodos
r <- make_empty_graph(n = 31, directed = F) # Una red vacía no tiene conexiones, solo nodos
# A partir de 31 nodos, se vuelve esférica la red, antes es una línea

# Asignar atributos visuales a los nodos
V(r)$color <- "pink"

# Visualizar
plot(r, 
     main = "Red vacía con 31 nodos", # Título
     vertex.size = 20)


# Para seguir trabajando usaré una matriz más chiquita
g <- make_empty_graph(n = 4, directed = T)

V(g)$color <- "pink"
V(g)$label.color <- "black"
E(g)$color <- "darkgreen" # Solo funciona si se corre después de que se hagan conexiones

plot(g, 
     main = "Red vacía con 4 nodos", # Título
     vertex.size = 50)


############################################################
# CONEXIONES DEL GRAFO

# Agregar conexiones: 1→2, 1→3, 2→4, 3→4, 4→5, mediante vectores
g <- add_edges(g, c(1,2, 1,3, 3,2, 2,4, 4,1)) # Si se vuelve a correr esto sin cambiar el nombre del objeto, pondrá de nuevo las conexiones, será como si tuviera 2.

plot(g, 
     main = "Red con conexiones",
     vertex.size = 30,
     edge.arrow.size = 0.6)

#############################################################
# ATRIBUTOS DEL NODO

# Agregar un nodo nuevo con color rojo
g <- add_vertices(g, 1, color = "purple") # si se agregan nuevos vertices o nodos, se pierden los datos visuales a menos que se vuelvan a especificar (colores)
# Conectar el nuevo nodo (nodo 5)
g <- add_edges(g, c(5,3, 4,5))

plot(g, 
     main = "Nuevo nodo y conexiones",
     vertex.size = 30,
     edge.arrow.size = 0.6)

###################################################################
# NOMBRES

# Asignar nombres a los vértices/nodos
V(g)$name <- LETTERS[1:5]

plot(g, 
     main = "Nodos con nombres A–E",
     vertex.size = 30,
     edge.arrow.size = 0.6)

###################################################################
# MODIFICAR CONEXIONES

# Eliminar la segunda conexión (B→D) y agregar D→B
g <- delete_edges(g, 4) # quita B→D (2,4)
g <- add_edges(g, c(4, 2)) # D→B
g <- add_edges(g, c(2, 5)) # C→A

plot(g, 
     main = "Cambio en dirección de conexión",
     vertex.size = 30,
     edge.arrow.size = 0.6)

####################################################################
# CÁLCULO Y VISUALIZACIÓN DEL GRADO

print (g) # Muestra las conexiones de los elementos de la red

degree(g) # Total
degree(g, mode = "in") # Entrada
degree(g, mode = "out") # Salida

plot(g, 
     layout = layout_nicely(g),
     vertex.size = degree(g, mode = "in") * 10 + 10, # Cambia el tamaño según si entra o salen las flechas
     vertex.label.dist = 0.5,
     edge.arrow.size = 0.5,
     main = "Tamaño proporcional al grado (degree) de entrada")

plot(g, 
     layout = layout_nicely(g),
     vertex.size = degree(g, mode = "out") * 10 + 10,
     vertex.label.dist = 0.5,
     edge.arrow.size = 0.5,
     main = "Tamaño proporcional al grado (degree) de entrada")


###################################################################
# REDES ESPECIALES

# igraph tiene funciones específicas para generar ejemplos de redes especiales

plot (make_star(10)) # Estrella, todos unidos a un nodo
plot (make_ring(10)) # Anillo
plot (make_tree(10)) # Árbol
plot (make_lattice(c(4, 4))) # Rejilla

#######################################################################
# EJERCICIOS

# 1) Crea una red no dirigida de 7 nodos con colores aleatorios de una paleta, conéctalos formando una cadena (path) y visualiza el resultado

e1 <- make_empty_graph(n = 7, directed = F)
e1<- graph_from_literal(1-2-3-4-5-6-7)

install.packages("viridis") # Genera vectores con paletas de colores para daltonismo

paleta <- c(colores) # Pendiente de terminar esta y las siguientes dos líneas
V(e1)$color <- paleta
V(e1)$name <- nombres

# 2) Construye manualmente una red donde los nodos representen 5 estados mexicanos y las conexiones indiquen si comparten frontera estatal. Muestra la matriz de adyacencia.

e2 <- make_empty_graph(n = 5, directed = F)
V(e2)$name <- c("Guanajuato", "Querétaro", "Hidalgo", "Estado de México", "Michoacán")

e2 <- add_edges(e2, c(1,2, 1,5, 2,3, 2,4, 2,5, 3,4, 4,5))
degree(e2)

plot(e2, 
     main = "Ejercicio 2",
     vertex.size = 90)

# 3) Personas que se sientan a los lados, enfrente y atrás

e3 <- make_empty_graph(n = 13, directed = F)
V(e3)$name <- c("Mariana", "Alonso", "Leydi", "Josué", "Mayra", "Celina", "Fernanda", "Diego", "Paola", "Andrea", "Gabriela", "César", "Samuel")

e3 <- add_edges(e3, c("Mariana","Mayra",
                      "Mariana","Celina",
                      "Mariana","Gabriela",
                      "Mariana","Andrea",
                      "Mariana", "Paola",
                      "Alonso", "Mayra",
                      "Alonso", "Celina",
                      "Leydi", "Josué",
                      "Leydi", "Fernanda",
                      "Leydi", "Diego",
                      "Fernanda", "Diego",
                      "Fernanda", "Paola",
                      "Fernanda", "Andrea",
                      "Diego", "Paola",
                      "Diego", "Andrea",
                      "Josué", "Paola",
                      "Josué", "Andrea",
                      "Josué", "Mayra",
                      "Josué", "Celina",
                      "Gabriela", "Mayra",
                      "Gabriela", "Celina",
                      "César", "Paola",
                      "César", "Andrea",
                      "César", "Samuel"))

V(e3)$color <- "pink"
degree(e3)

plot(e3, 
     main = "Ejercicio 3",
     vertex.size = 30)

# Ejercicio con pistas 1: Crea una red vacía de 7 nodos no dirigidos. Agrega conexiones para formar un triángulo entre los nodos 1, 2 y 3, y otro triángulo entre 5, 6 y 7. Conecta ambos triángulos con una conexión entre 3 y 5. Visualiza el resultado y calcula el grado (degree) de cada nodo.

ep1 <- make_empty_graph(n = 7, directed = F)
ep1 <- add_edges(ep1, c(1,2, 2,3, 3,1, 5,6, 6,7, 7,5, 3,5))

plot(ep1, 
     main = "Ejercicio con pistas 1",
     vertex.size = 30)

degree(ep1)


# Ejercicio con pistas 3: ¿Qué pasa si agregas un self-loop (conexión de un nodo a sí mismo) y una conexión duplicada? Crea una red, agrégalos, visualiza, y luego “limpia” la red con simplify(). Compara el antes y después.

ep3 <- make_empty_graph(n = 3, directed = F)
ep3 <- add_edges(ep3, c(1,1, 1,2, 1,2))

plot (ep3, 
     main = "Ejercicio con pistas 3",
     vertex.size = 30)
degree (ep3) # Si un nodo se conecta consigo mismo, se cuenta como si tuviera dos conexiones

ep3s <- simplify (ep3, remove.multiple = T, remove.loops = T)

plot (ep3s, 
      main = "Ejercicio con pistas 3",
      vertex.size = 30)
degree (ep3s)

# Ejercicio propuesto 1: Crea una red estrella de 15 nodos. Calcula su grado (degree) máximo y mínimo. ¿Cuál es la densidad de conexiones (edge_density())?

epr1 <- make_star(15)
epr1

plot (epr1, 
       main = "Ejercicio con pistas 3",
       vertex.size = 30, 
      vertex.color = "lightblue")

max_degree(epr1)
min_degree(epr1) # No existe

degree (epr1) # 14 y 1

edge_density (epr1) # Divide las conexiones que hay entre el máximo de conexiones que podría haber.

# Ejercicio propuesto 4: Crea una función en R llamada resumen_red() que reciba un grafo g y devuelva un data frame con las columnas: nodo, grado_total, grado_entrada, grado_salida. Pruébala con una red dirigida de al menos 10 nodos.

resumen_red <- function (g) {
  
   # nodo, grado_total, grado_entrada, grado_salida
} # PENDIENTE



##########################################################################################
# RED DE AMIGOS

data <- read.csv ("01_Raw_data/matriz_adyacencia_amigxs_2026.csv")
View(data)
row.names(data) <- data [,1]
data <- data[,-1]
class(data) # es un data.frame

data <- as.matrix(data)
class(data) # ya es una matriz

amigos <- graph_from_adjacency_matrix (data, mode = "directed") # convierte la matriz a una red
plot (amigos, 
      main = "Ejercicio con pistas 3",
      vertex.size = 30, 
      vertex.color = "lightblue",
      edge.arrow.size = 0.5)

degree (amigos)
tkplot(amigos) # Genera una ventana interactiva que permite visualizar fácilmente la red

degree (amigos, mode = "in") # Personas que te consideran su amigo
degree (amigos, mode = "out") # Personas que tú consideras tus amigos

degree (amigos, mode = "in") - degree (amigos, mode = "out")

amigos_mariana <- which (data ["MARIANA",] == 1) # Esto regresa a los amigos que yo consideré, y el número de nodo que les corresponde
mean (degree (amigos, amigos_mariana, mode = "out")) # Esto calcula solo el promedio del degree de los elementos de la matriz amigos que aparecen en la matriz amigos_mariana


soy_popular <- function (nombre) {
  nombre <- nombre
  mis_amigxs <- which(data[nombre,]==1)
  popularidad <- length(mis_amigxs)
  cuantos <- mean(degree(amigos, mis_amigxs, mode = "out"))
  #print("Soy ", "nombre", "y tengo", "popularidad", "amigos. Mis amigos tienen en promedio", "cuantos", "amigos")
} #algo falla con el print
soy_popular("MARIANA")

######################################################################
# MÉTRICAS DE LA RED

mean_distance(amigos) # 1.344583
diameter(amigos) # 3
get_diameter (amigos)
camino <- shortest_paths(g, from = "N1", to = "N6")$vpath[[1]]

distances(amigos)
is_connected(amigos)

View(amigos)

transitivity (amigos)
eccentricity (amigos)

# EJERCICIOS

# Ejercicio resuelto 1: Compara la distancia media de tres redes: anillo, estrella y grafo completo, todos con 30 nodos. ¿Cuál sería la mejor para difundir una noticia rápidamente? Visualiza los resultados con un gráfico de barras.

estrella <- make_star (30, mode = "undirected")
plot (estrella)
pe <- mean_distance (estrella)

anillo <- make_ring (30)
plot (anillo)
pa <- mean_distance (anillo)

completo <- make_full_graph (30)
plot (completo)
pc <- mean_distance (completo)

comparacion <- data.frame(Red = c("Estrella", "Anillo", "Completo"),
                          Dist_media = c (pe, pa, pc))
comparacion

barplot(comparacion$Dist_media,
        names.arg = comparacion$Red,
        col = c("pink2", "lavender", "lightblue"),
        main = "Distancia media (n = 30)",
        ylab = "Distancia media",
        ylim = c(0, max(comparacion$Dist_media) * 1.2) # Esta hace que el eje y se extienda un poco más arriba de la distancia más grande
       )

text(x = c(0.7, 1.9, 3.1), y = comparacion$Dist_media + 0.3,
     labels = comparacion$Dist_media, cex = 0.9, font = 2)

# EJERCICIO

transitivity (estrella, type = "global") # Se refiere a si los vecinos se conectan entre sí, 0 es que no se conectan, 1 es que se conectan por completo.
eccentricity (estrella) # Se refiere a cual es la distancia máxima de un nodo a cualquier otro nodo

transitivity (anillo, type = "global")
eccentricity (anillo)

transitivity (completo, type = "global")
eccentricity (completo)


##############################################################################
# CLUSTERING Y TRANSITIVIDAD

ejemplo <- graph_from_literal( # esta funcion permite agregar nodos y conexiones en un solo paso
  A -- B, A -- C, A -- D, A -- E, B -- C, D -- E, E -- F)
ejemplo


#################################################################################
# GRADO Y DISTRIBUCIÓN

# Ejercicio 1: Genera redes BA con 100, 500, 1000 y 5000 nodos. ¿Cómo cambia el grado del hub más conectado a medida que crece la red? Haz un gráfico de dispersión de n (tamaño de red) vs. grado máximo.

EJ1 <- sample_gnp(500, p = 0.01)
V(EJ1)$name <- paste0("ER_", 1:vcount(EJ1))

plot(EJ1, 
     vertex.size = degree(EJ1) * 1.5 + 3, # Esto se hace para exagerar el tamaño de los nodos y que se note bien la diferencia
     vertex.label = NA,
     vertex.color = "hotpink2",
     edge.color = adjustcolor("gray50", alpha = 0.4),
     edge.size = 100,
     main = "Red Aleatoria (Erdős–Rényi)")

p <- 0.01
n <- 500
grado_teorico <- p * (n - 1)
grado_teorico
grado_empirico <- degree (EJ1)
mean (grado)

hist (grado,
      main = "Ejercicio 1",
      ylab = "Frecuencia",
      xlab = "Grado",
      col = "pink")
k_vals <- 0:max(grado)
lines(k_vals, dpois(k_vals, lambda = grado_teorico), # dpois es la función de la distribución Poisson
      col = "navy", lwd = 2.5, type = "b", pch = 16, cex = 0.8)

# Ejercicio resuelto 2: Genera una red BA con 1000 nodos y ajusta una ley de potencia con poweRlaw. Reporta el exponente gamma y el valor de Xmin.

g_ba <- sample_pa(100, directed = FALSE)
V(g_ba)$name <- paste0("BA_", 1:vcount(g_ba))

#################################################################
# DATOS LISTOS PARA HACER PRUEBAS

install.packages("igraphdata")
library (igraphdata)

data ("karate") # Esta es una red muy popular utilizada para proponer modelos de clusterización
plot (karate, 
      main = "Karate")

hist (sort (degree (karate), decreasing = T))
karate_sinH <- delete_vertices (karate, c("John A", "Mr Hi", "Actor 33", "Actor 3", "Actor 2", "Actor 4", "Actor 32", "Actor 9", "Actor 14", "Actor 24"))
plot (karate_sinH)

hubs_k <- sort (degree (karate), decreasing = T)[1:10]
karate_sinH <- delete_vertices(karate, hubs_k) # con esta y la línea anterior, se pueden eliminar los hubs sin tener que escribirlo uno por uno


data ("yeast") # Esta ya es una red muy grande, no hacer plot pq sino se traba
hist (sort (degree (yeast), decreasing = T))

hubs_y <- sort (degree (yeast), decreasing = T)[1:2000]
yeast_sinH <- delete_vertices (yeast, hubs_y)

hist (sort (degree (yeast_sinH), decreasing = T)) # No cambia mucho, quién sabe por qué


##########################################################################
# ROBUSTEZ DE REDES

plot (amigos, 
      main = "Red de amigos",
      vertex.size = 30, 
      vertex.color = "lightblue",
      edge.arrow.size = 0.5)

# que pasa si me quito a mí

amigos_sin_mariana <- delete_vertices(amigos, "MARIANA")
plot (amigos_sin_mariana, 
      main = "Red de amigos, sin Mariana",
      vertex.size = 30, 
      vertex.color = "pink",
      edge.arrow.size = 0.5)

mean_distance (amigos) # 1.364583
mean_distance (amigos_sin_mariana) # 1.487179
diameter (amigos)
get_diameter(amigos)
diameter (amigos_sin_mariana)
get_diameter(amigos_sin_mariana)

# que pasa si quitamos los hubs

hubs <- sort (degree(amigos), decreasing = T)[1:5] # Muestra solo los primeros 5
hubs # total
hubs_out <- sort (degree(amigos, mode = "out"), decreasing = T)
hubs_out
hubs_in <- sort (degree(amigos, mode = "in"), decreasing = T)
hubs_in

amigos_sin_hubs <- delete_vertices(amigos, c("MARIANA", "CESAR", "MAYRA"))
plot (amigos_sin_hubs, 
      main = "Red de amigos, sin hubs",
      vertex.size = 30, 
      vertex.color = "pink",
      edge.arrow.size = 0.5)

mean_distance (amigos_sin_hubs) # 1.47619
diameter (amigos_sin_hubs) # 3

# ahora que pasa si quitamos a los que no son hubs

amigos_con_hubs <- delete_vertices(amigos, c("FERNANDA", "DIEGO", "SAMUEL"))
plot (amigos_con_hubs, 
      main = "Red de amigos, con hubs",
      vertex.size = 30, 
      vertex.color = "pink",
      edge.arrow.size = 0.5)

mean_distance (amigos_con_hubs) # 1.388889
diameter (amigos_con_hubs) # 3

# Componentes

gigante <- function(g) {
  comp <- components(g) # Da la cantidad de componentes y cuantos nodos tiene cada uno
  max(comp$csize) / vcount(g) # Esto indica la proporción de nodos que tiene el componente gigante
} # vcount es el total de vertices (nodos)

gigante (amigos)
gigante (amigos_sin_mariana)
gigante (amigos_sin_hubs)
gigante (yeast)

# Simulación de fallos

simular_fallos <- function(g, fraccion_eliminar = 0.5, semilla = 42) { # Poner un igual permite indicar cuales son los valores por default, la función trabajará con eso si no se especifíca otro, pero quien use la función la puede cambiar
  set.seed(semilla) # Los números no pueden ser realmente aleatorios, seed es una lista de números de las cuales toma uno el programa, hay muchas listas, se puede cambiar el seed
  n_original <- vcount(g) # número de vertices
  n_eliminar <- floor(n_original * fraccion_eliminar) # Floor redondea hacia abajo los números, ya que para eliminar nodos se deben dar números enteros
  
  # Seleccionar nodos a eliminar en orden aleatorio
  orden_eliminacion <- sample(V(g)$name, n_eliminar) # toma nombres aleatorios (basado en la semilla) para decidir el orden
  
  # Registrar métricas en cada paso
  resultados <- data.frame( # pone los resultados en un data.frame
    eliminados = 0:n_eliminar,
    fraccion_eliminada = (0:n_eliminar) / n_original,
    comp_gigante = numeric(n_eliminar + 1),
    dist_media = numeric(n_eliminar + 1),
    diametro = numeric(n_eliminar + 1)
  )
  
  g_temp <- g # Para no modificar la red original, copiamos la red en otro objeto
  
  for (i in 0:n_eliminar) { 
    if (i > 0) { # va eliminando los vertices uno por uno
      g_temp <- delete_vertices(g_temp, orden_eliminacion[i])
    }
    
    resultados$comp_gigante[i + 1] <- gigante(g_temp) # usa la función que se generó antes
    
    # Calcular distancia media solo en el componente gigante
    comp <- components(g_temp)
    giant_id <- which.max(comp$csize)
    giant_nodes <- which(comp$membership == giant_id)
    if (length(giant_nodes) > 1) {
      g_giant <- induced_subgraph(g_temp, giant_nodes)
      resultados$dist_media[i + 1] <- mean_distance(g_giant)
      resultados$diametro[i + 1] <- diameter(g_giant)
    } else {
      resultados$dist_media[i + 1] <- 0
      resultados$diametro[i + 1] <- 0
    }
  }
  
  resultados
}

simular_fallos(yeast, fraccion_eliminar = 0.002)


simular_ataques <- function(g, fraccion_eliminar = 0.5, semilla = 42) { # Esta es de ataque dirigidos, elimina los nodos con mayor degree primero, por lo que después de cada ataque, recalcula los degrees para volver a eliminar el que tenga mayor degree
  set.seed(semilla)
  n_original <- vcount(g)
  n_eliminar <- floor(n_original * fraccion_eliminar)
  
  resultados <- data.frame(
    eliminados = 0:n_eliminar,
    fraccion_eliminada = (0:n_eliminar) / n_original,
    comp_gigante = numeric(n_eliminar + 1),
    dist_media = numeric(n_eliminar + 1),
    diametro = numeric(n_eliminar + 1)
  )
  
  g_temp <- g
  
  for (i in 0:n_eliminar) {
    if (i > 0) {
      # Identificar el nodo con mayor grado (recalculado)
      deg <- degree(g_temp)
      hub <- V(g_temp)$name[which.max(deg)]
      g_temp <- delete_vertices(g_temp, hub)
    }
    
    resultados$comp_gigante[i + 1] <- gigante(g_temp)
    
    # Calcular distancia media solo en el componente gigante
    comp <- components(g_temp)
    giant_id <- which.max(comp$csize)
    giant_nodes <- which(comp$membership == giant_id)
    if (length(giant_nodes) > 1) {
      g_giant <- induced_subgraph(g_temp, giant_nodes)
      resultados$dist_media[i + 1] <- mean_distance(g_giant)
      resultados$diametro[i + 1] <- diameter(g_giant)
    } else {
      resultados$dist_media[i + 1] <- 0
      resultados$diametro[i + 1] <- 0
    }
  }
  
  resultados
}

simular_ataques (yeast, fraccion_eliminar = 0.002)


#############################################################################
# MEDIDAS DE CENTRALIDAD

deg <- degree (amigos)
lay <- layout_with_fr(amigos) #esto para qué?
plot(amigos, layout = lay,
     vertex.size = deg * 1.5 + 3,
     vertex.color = "pink",
     vertex.label.cex = 0.5,
     edge.color = adjustcolor("gray50", 0.4),
     main = "Centralidad de grado para red de Amigos")

btw_raw <- betweenness(amigos)
btw_scaled <- (btw_raw - min(btw_raw)) / (max(btw_raw) - min(btw_raw)) * 20 + 5

plot(amigos, layout = lay,
     vertex.size = btw_scaled,
     vertex.color = "mediumpurple",
     vertex.label.cex = 0.5,
     edge.color = adjustcolor("gray50", 0.4),
     main = "Centralidad de intermediación (betweenness) para red de Amigos")

eig <- eigen_centrality(amigos)$vector
eig_scaled <- eig * 20 + 5

plot(amigos, layout = lay,
     vertex.size = eig_scaled,
     vertex.color = "steelblue",
     vertex.label.cex = 0.5,
     edge.color = adjustcolor("gray50", 0.4),
     main = "Centralidad de vector propio (eigenvector)")

clo <- closeness(amigos, normalized = TRUE)
clo_scaled <- (clo - min(clo)) / (max(clo) - min(clo)) * 20 + 5

plot(amigos, layout = lay,
     vertex.size = clo_scaled,
     vertex.color = "tomato",
     vertex.label.cex = 0.5,
     edge.color = adjustcolor("gray50", 0.4),
     main = "Centralidad de cercanía (closeness)")

# Para verlas todas juntas:

colores <- c("pink", "lavender", "mediumpurple", "steelblue")
titulos <- c("Grado", "Cercanía", "Intermediación", "Vector propio")

medidas <- list( # Normalizar todas las medidas a [0, 1] para escalar visualmente
  degree(amigos, normalized = TRUE),
  closeness(amigos, normalized = TRUE),
  betweenness(amigos, normalized = TRUE),
  eigen_centrality(amigos)$vector
)

par(mfrow = c(2, 2), mar = c(1, 1, 3, 1)) #esto para qué?

for (i in 1:4) {
  m <- medidas[[i]]
  m_scaled <- m / max(m) * 20 + 5
  
  plot(amigos, layout = lay,
       vertex.size = m_scaled,
       vertex.color = colores[i],
       vertex.label.cex = 0.45,
       edge.color = adjustcolor("gray50", 0.3),
       main = titulos[i])
}

# Comparación de centralidad con distintas medidas:

lambda_max <- max(Re(eigen(as_adjacency_matrix(amigos, sparse = FALSE))$values)) # Recalcular alpha con parámetro seguro
alpha_seguro <- 1 / (lambda_max + 1) # Esto es otra medida de centralidad, no es necesario para que corra, solo hay que borrar las líneas de eso

centralidades <- data.frame(
  Nodo = V(amigos)$name,
  Grado = degree(amigos, normalized = TRUE),
  Cercanía = closeness(amigos, normalized = TRUE),
  Intermediación = betweenness(amigos, normalized = TRUE),
  Eigenvector = eigen_centrality(amigos)$vector,
  PageRank = page_rank(amigos)$vector,
  Armónica = harmonic_centrality(amigos, normalized = TRUE),
  Subgrafo = subgraph_centrality(amigos),
  Hub = hub_score(amigos)$vector,
  Alpha = alpha_centrality(amigos, alpha = alpha_seguro, exo = 1)
)


top10 <- order(centralidades$Grado, decreasing = TRUE)[1:10] # Mostrar top 10 por grado

tabla_top10 <- centralidades[top10, ]
tabla_top10[, -1] <- round(tabla_top10[, -1], 4)

knitr::kable(
  tabla_top10,
  caption = "Medidas de centralidad para los 10 nodos con mayor grado",
  row.names = FALSE
)

# Heatmap para saber si las medidas de centralidad están "de acuerdo" entre sí

cor_mat <- cor(centralidades[, -1], use = "complete.obs") # Matriz de correlación (solo columnas numéricas)

heatmap(cor_mat, # Visualizar como heatmap
        col = hcl.colors(20, palette = "RdBu", rev = TRUE),
        scale = "none",
        margins = c(10, 10),
        main = "Correlación entre medidas de centralidad")

#########################################################################################
# COMUNIDADES

# Calcula, usando R, el logaritmo de 10,000,100,000 y 1,000,000. Calcula, también, el logaritmo del logaritmo de los números anteriores.

log (10000)
log (100000)
log (1000000)

# En igraph genera una red de 10,000 nodos y 100,000 nodos con los modelos free-scale, aleatoria y small-world. (¡¡¡¡NO, NO y NO las grafiques !!!!!) o 1000 y 100

ER <- sample_gnp(10000, p = 0.04)
SW <- sample_smallworld(1, 10000, nei = 3, p = 0.1)
BA <- sample_pa(10000, directed = FALSE)

# Usa igraph para calcular el diámetro y distancias promedio de cada una de las redes anteriores.

diameter (ER)
diameter (SW)
diameter (BA)

# Métodos de clusterización

algoritmos <- list(
  "Edge betweenness"   = cluster_edge_betweenness(amigos),
  "Walktrap"           = cluster_walktrap(amigos),
  "Leading eigenvector" = cluster_leading_eigen(amigos),
  "Label propagation"  = cluster_label_prop(amigos),
  "Infomap"            = cluster_infomap(amigos),
  "Optimal"            = cluster_optimal(amigos)
)

seleccion <- c("Edge betweenness", "Optimal", "Label propagation", "Leading eigenvector", "Infomap", "Walktrap")

par(mfrow = c(2, 3), mar = c(1, 1, 3, 1))

for (nombre in seleccion) {
  c_alg <- algoritmos[[nombre]]
  q_alg <- modularity(amigos, membership(c_alg))
  plot(c_alg, amigos, layout = lay,
       vertex.size = 12, vertex.label.cex = 0.45,
       edge.arrow.size = 0.2,
       main = paste0(nombre, "\nQ = ", round(q_alg, 4),
                     " | k = ", length(c_alg)))
}
