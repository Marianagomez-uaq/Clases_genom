
# Para visualizar:

install.packages("networkD3")
install.packages("visNetwork")
library (networkD3)
library (visNetwork)

# Usaré la red de amigos del otro script

wc <- cluster_walktrap(amigos)
members <- membership(wc)

# Convert to object suitable for networkD3
amigos_d3 <- igraph_to_networkD3(amigos, group = members)

# Create force directed network plot
forceNetwork(Links = amigos_d3$links, Nodes = amigos_d3$nodes,
             Source = 'source', Target = 'target',
             NodeID = 'name', Group = 'group')  

library(visNetwork)
visIgraph(amigos) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visInteraction(navigationButtons = TRUE)
