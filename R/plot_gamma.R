plot_gamma <-
function(model,lls,prob=0.05){  
  require(reshape)
  require(ggplot2)
  ll=data.frame(label=lls,sample=names(lls))
  my_gamma <- model@gamma
  rownames(my_gamma)<- model@documents
  colnames(my_gamma)<- paste('Topic',1:ncol(my_gamma),sep='')
  my_gamma <- data.frame(my_gamma,sample=rownames(my_gamma))
  my_gamma <- my_gamma[names(lls),]
  data_plot=merge(my_gamma,ll,by='sample')
  data_plot=melt(data_plot,id=c('sample','label'))
  data_plot$sample <- factor(data_plot$sample,levels=names(sort(lls)))
  data_plot=subset(data_plot,value>=prob)
  p <- ggplot(data_plot,aes(sample,variable)) +
    geom_point(aes(colour = as.factor(label),size=value)) +
    theme_bw()+
    labs(title = paste("topic vs. sample probability distribution\n (points probability <",prob,
                       " filtered)",sep=''),x='Samples',y='Topics',color='Disease Group') +
    theme(axis.text.x=element_text(angle=45,size=7))
  p}
