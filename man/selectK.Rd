\name{selectK}
\alias{selectK}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to select the appropriate topic number
}
\description{
This function could be used to apply cross validation on topicmodels. The returned perplexity or likelihood can help to make a decision of the topic number.
}
\usage{
selectK(dtm, kv, SEED, cross, sp, method, ...)
}

\arguments{
  \item{dtm}{
  	The data which consists of counts of a microbiome in each individual. It should be an object of class "simple_triplex_matrix".
  	}
  \item{kv}{
A vector with integer as the number of topics.
}
  \item{SEED}{

}
  \item{cross}{
The number for cross validation
}
  \item{sp}{
See \code{smp}
}
  \item{method}{
The method should be one of 'CTM','Gibbs' and 'VEM', which come from the package topicmodels. Please refer to the help of this package. 
}
  \item{\dots}{
please refer to the options of the method you selected in package topicmodels. 
}
}
\details{

}
\value{
A list which contains values of perplexity and likelihood of cross validation for each topic number.
}
\references{

}
\author{

}
\note{

}



\seealso{

}
\examples{
	library(BiotypeR)
	library(topicmodels)
	library(slam)
	data.denoized=noise.removal(t(meta_counts), percent=0.01)
	samples=colnames(data.denoized)
	bacteria=rownames(data.denoized)
	data.final=meta_counts[samples,bacteria]
	dtm=as.simple_triplet_matrix(data.final)
	seed_num=2014
	fold_num=5
	kv_num = c(2:30)
	sp=smp(cross=fold_num,n=nrow(dtm),seed=seed_num)
	control = list(seed = seed_num, burnin = 1000,thin = 100, iter = 1000)
	#not run: system.time((ctmK=selectK(dtm=dtm,kv=kv_num,SEED=seed_num,cross=fold_num,sp=sp,method='Gibbs',control=control)))
	#not run: plot_perplexity(ctmK,kv_num)}
