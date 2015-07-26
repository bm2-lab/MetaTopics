selectK <-
function(dtm,kv,SEED,cross,sp,method,...) # change 60 to 15
{
  per_ctm=NULL
  log_ctm=NULL
  for (k in kv)
  {
    per=NULL
    loglik=NULL
    for (i in 1:cross)  #only run for 3 replications# 
    {
      cat("R is running for", "topic", k, "fold", i,"\n")
      te=sp[[i]]
      tr=setdiff(1:nrow(dtm),te)
      if (method=='Gibbs'){
        Gibbs_result = LDA(dtm[tr,], k = k, method = "Gibbs",...)
        per=c(per,perplexity(Gibbs_result,newdata=dtm[te,]))
        loglik=c(loglik,logLik(Gibbs_result,newdata=dtm[te,]))
      }else if(method == 'CTM'){
        CTM_result = CTM(dtm[tr,], k = k,...)
        per=c(per,perplexity(CTM_result,newdata=dtm[te,]))
        loglik=c(loglik,logLik(CTM_result,newdata=dtm[te,]))
        
      }else if(method == 'VEM'){
        VEM_result = LDA(dtm[tr, ], method='VEM',k = k,...)
        per=c(per,perplexity(VEM_result,newdata=dtm[te,]))
        loglik=c(loglik,logLik(VEM_result,newdata=dtm[te,]))
        #VEM_fixed = LDA(dtm[tr,], k = k, control = list(estimate.alpha = FALSE, seed = SEED)),
      }else{
        cat("Error, you must select a method like VEM, CTM or Gibbs")}
    }
    per_ctm=rbind(per_ctm,per)
    log_ctm=rbind(log_ctm,loglik)
  }
  return(list(perplex=per_ctm,loglik=log_ctm))
}
