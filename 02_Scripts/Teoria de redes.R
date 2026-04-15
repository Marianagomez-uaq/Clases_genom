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
