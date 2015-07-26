plot_beta <-
function(model,prob=0.01){
  require(reshape)
  my_beta <- model@beta
  colnames(my_beta) <- model@terms
  rownames(my_beta) <- paste('Topic',1:nrow(my_beta),sep='')
  my_beta <- exp(my_beta)
  my_beta <- data.frame(my_beta,topics=rownames(my_beta))
  data_plot <- melt(my_beta,id='topics')
  data_plot=subset(data_plot,value>=prob)
  p <- ggplot(data_plot,aes(topics,variable)) +
    geom_point(colour = 'blue',aes(size=value)) +
    theme_bw()+
    labs(title = paste("topic vs. bacteria probability distribution\n (points probability <",prob," filtered)"),
         x='topic',y='bacteria') +
    theme(axis.text.x=element_text(angle=45,size=7))+
    theme(axis.text.y=element_text(size=8))
  p
}
