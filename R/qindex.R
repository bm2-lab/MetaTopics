qindex <-
function(model,lls,prob=0.05){
  require(reshape)
  require(dplyr)
  #row for samples
  #col for topics
  pmatrix <- model@gamma
  rownames(pmatrix)<- model@documents
  colnames(pmatrix)<- paste('Topic',1:ncol(pmatrix),sep='')
  pmatrix <- pmatrix[names(lls),]
  
  p.matrix <- data.frame(pmatrix,samples=rownames(pmatrix),label=lls)
  p.matrix <- melt(p.matrix,id=c('samples','label'))
  p.step1=p.matrix %>%
    group_by(label,variable)%>%
    summarise(number=sum(value>=prob))
  total_number=sum(p.step1$number)
  p.step2=p.step1 %>%
    group_by(label)%>%
    summarise(bylabel=sum(number))
  p.step1=merge(p.step1,p.step2,by='label')
  p.step3=p.step1 %>%
    group_by(variable)%>%
    summarise(bytopic=sum(number)/total_number)
  p.step1=merge(p.step1,p.step3,by='variable') 
  p.step1$quelete = (p.step1$number/p.step1$bylabel-p.step1$bytopic)/p.step1$bytopic
  p.step1[,c('label','variable','quelete')]%>%arrange(label,variable)
}
