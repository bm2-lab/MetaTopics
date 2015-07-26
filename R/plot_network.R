plot_network <-
function(model,prob=0.05){
  require(igraph)
  term_matrix <- model@beta
  term_matrix <- exp(term_matrix)
  colnames(term_matrix)<-model@terms
  result=lapply(1:nrow(term_matrix),topic_i_term,term_matrix=term_matrix,prob=0.05)
  edgelist = as.data.frame(do.call(rbind, result), stringsAsFactors =F)
  g <-graph.data.frame(edgelist,directed=F )
  l<-layout.auto(g)
  #edge.color="black"
  nodesize = centralization.degree(g)$res 
  V(g)$size = log( centralization.degree(g)$res )
  
  nodeLabel = V(g)$name
  E(g)$color =  sample(colors()[26:137], 15)[edgelist$i]
  
  plot(g, vertex.label= nodeLabel,  edge.curved=TRUE, 
       vertex.label.cex =0.4,  edge.arrow.size=0.2, layout=l )
}
