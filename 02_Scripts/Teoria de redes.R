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
     vertex.size = 50)


# Para seguir trabajando usaré una matriz más chiquita
g <- make_empty_graph(n = 4, directed = T)
V(g)$color <- "pink"
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
g <- add_vertices(g, 1, color = "purple")
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
  
    nodo, grado_total, grado_entrada, grado_salida
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
