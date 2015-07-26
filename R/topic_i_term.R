topic_i_term <-
function(i,term_matrix,prob=0.01){
  x=term_matrix[i,]
  x=round(x[x>=prob],digits=2)
  if(length(x)>2){
    ri=data.frame(embed(names(x), 2)[, 2:1],i) 
  }else if(length(x)==2){
    ri=data.frame(X1=names(x)[1],X2=names(x)[2],i=i)
  }else{
    ri=data.frame(X1=names(x),X2=paste('topic',i,sep=''),i=i)
  }
  return(ri)
}
