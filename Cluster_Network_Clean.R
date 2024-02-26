Network_analysisFunc <- function(t, s1, data2){
  library(coop)
  library(igraph)
  
  water.active <- t
  
  #  library cosine similarity instead of correlation
  mat_water = cosine(t(water.active),use = 'pairwise.complete.obs')
  
  #  correlation below s1 ignore
  mat_water[mat_water<s1]=0
  
  
  #  network analysis using igraph
  network_water = graph_from_adjacency_matrix(mat_water, weighted=T, mode="undirected", diag=F)
  clp_water<- cluster_louvain(network_water)
  
  #  add communities to network_water data
  V(network_water)$community <- clp_water$membership
  data2$m <- clp_water$membership
  
  return(list(network_water, clp_water, water.active,data2, mat_water))
  
}
