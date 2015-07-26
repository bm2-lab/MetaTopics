\name{Gibbs_model_example}
\alias{Gibbs_model_example}
\docType{data}
\title{
A topic model of the data meta_counts
}
\description{
This is a model returned from topic model with Gibbs as the method and 10 as the topic number. The data is meta_counts.  
}
\usage{data("Gibbs_model_example")}
\format{
  The format is:
Formal class 'LDA_Gibbs' [package "topicmodels"] with 16 slots
  ..@ seedwords      : NULL
  ..@ z              : int [1:117100] 2 2 9 9 9 9 9 9 9 9 ...
  ..@ alpha          : num 5
  ..@ call           : language LDA(x = dtm, k = K, method = "Gibbs", control = list(seed = seed_num,      burnin = 1000, thin = 100, iter = 1000))
  ..@ Dim            : int [1:2] 39 88
  ..@ control        :Formal class 'LDA_Gibbscontrol' [package "topicmodels"] with 14 slots
  .. .. ..@ delta        : num 0.1
  .. .. ..@ iter         : int 100
  .. .. ..@ thin         : int 100
  .. .. ..@ burnin       : int 1000
  .. .. ..@ initialize   : chr "random"
  .. .. ..@ alpha        : num 5
  .. .. ..@ seed         : int 2014
  .. .. ..@ verbose      : int 0
  .. .. ..@ prefix       : chr "/var/folders/b1/3cq936r56c17gqhb1g8xy7580000gn/T//Rtmp2299Tp/file3e117b94067"
  .. .. ..@ save         : int 0
  .. .. ..@ nstart       : int 1
  .. .. ..@ best         : logi TRUE
  .. .. ..@ keep         : int 0
  .. .. ..@ estimate.beta: logi TRUE
  ..@ k              : int 10
  ..@ terms          : chr [1:88] "Acholeplasma" "Actinomyces" "Aggregatibacter" "Atopobium" ...
  ..@ documents      : chr [1:39] "NO2" "NO6" "NO8" "NO9" ...
  ..@ beta           : num [1:10, 1:88] -11.7 -6.31 -9.62 -11.74 -11.67 ...
  ..@ gamma          : num [1:39, 1:10] 0.1772 0.0434 0.0474 0.4577 0.0172 ...
  ..@ wordassignments:List of 5
  .. ..$ i   : int [1:1112] 1 1 1 1 1 1 1 1 1 1 ...
  .. ..$ j   : int [1:1112] 1 2 3 4 5 6 7 8 9 10 ...
  .. ..$ v   : num [1:1112] 2 9 6 9 9 8 2 6 7 2 ...
  .. ..$ nrow: int 39
  .. ..$ ncol: int 88
  .. ..- attr(*, "class")= chr "simple_triplet_matrix"
  ..@ loglikelihood  : num -85072
  ..@ iter           : int 100
  ..@ logLiks        : num(0) 
  ..@ n              : int 117100
}
\details{
	library(topicmodels)
	library(slam)
	library(BiotypeR)
	data.denoized=noise.removal(t(meta_counts), percent=0.01)
	samples=colnames(data.denoized)
	bacteria=rownames(data.denoized)
	data.final=meta_counts[samples,bacteria]
	dtm=as.simple_triplet_matrix(data.final)
	seed_num=2014
	K=10
	Gibbs_model_example = LDA(dtm, k = K, method = "Gibbs",
	control = list(seed = seed_num, burnin = 1000,thin = 100, iter = 1000))
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Gibbs_model_example)
## maybe str(Gibbs_model_example) ; plot(Gibbs_model_example) ...
}
\keyword{datasets}
