abundance.plot <-
function(abundancedata,classification='',col='grey28',main='',xlab='',ylab='',first_n=30,las=2,legend.cex=1,sort='decreasing',...){
  n_class = length(unique(classification))
  mean.y = apply(abundancedata,2,mean)
  order.y = order(mean.y,decreasing = ifelse(sort=='decreasing',TRUE,FALSE))
  abundancedata=abundancedata[,order.y]
  
  if(length(col)==1){
    col=col
  }else{
    col=col[order.y]
    classification=classification[order.y]
  }
  
  y = abundancedata[,1:min(first_n,ncol(abundancedata))]
  
  boxplot(y,ylim=c(0,1),las=las,xlab=xlab,ylab=ylab,main=main,
          col=col[1:min(first_n,ncol(abundancedata))],...)
  
  if (n_class>1){
    col_system = cbind(classification,col)
    col_system=col_system[!duplicated(col_system),]
    legend('topright',col_system[,1],col=col_system[,2],cex=legend.cex,pch=16)
  }
}
