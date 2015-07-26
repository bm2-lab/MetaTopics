micro.abundance <-
function(data,dimi=1){
  y_sum=apply(data,dimi,sum)
  if(dimi==1){
    y_sum_matrix=matrix(rep(y_sum,ncol(data)),ncol=ncol(data))
  }else if(dimi==2){
    y_sum_matrix=matrix(rep(y_sum,each=nrow(data)),nrow=nrow(data))
  }
  return(data/y_sum_matrix)
}
