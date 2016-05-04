plot_perplexity <-
function(model,k){
  m_per = apply(model[[1]],1,mean,na.rm=T)
  m_log=apply(model[[2]],1,mean,na.rm=T)
  df = model[[1]]  # perplexity matrix
  par(mfrow=c(2,3))
  plot(k,m_per,type='l',xlab='',ylab='Mean of Perplexity')
  
  matplot(k, df, type = c("b"), xlab = "Number of topics", 
          ylab = "Perplexity", pch=1:10,col = 1, main = '')       
  legend("bottomright", legend = paste("fold", 1:5), col=1, pch=1:5)
  t_perplex = t(model$perplex)
  colnames(t_perplex)=NULL
  boxplot(t_perplex,xlab='',ylab='Box Distribution of Perplexity',las=2)
  #--------------------------------------------------------------------------
  plot(k,m_log,type='l',xlab='',ylab='Mean of loglik')
  
  matplot(k, model$loglik, type = c("b"), xlab = "Number of topics", 
          ylab = "loglik", pch=1:10,col = 1, main = '')       
  legend("bottomright", legend = paste("fold", 1:5), col=1, pch=1:5)
  t_perplex = t(model$loglik)
  colnames(t_perplex)=NULL
  boxplot(t_perplex,xlab='',ylab='Box Distribution of loglik',las=2)  
}
