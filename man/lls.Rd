\name{lls}
\alias{lls}
\docType{data}
\title{
Disease type annotations for the individuals of data meta_counts
}
\description{
This is a vector containing the disease type for each individual. 
}
\usage{data("lls")}
\format{
  The format is:
 Named chr [1:39] "NOT_OLP" "NOT_OLP" "NOT_OLP" "NOT_OLP" ...
 - attr(*, "names")= chr [1:39] "NO2" "NO6" "NO8" "NO9" ...
}
\details{
This is a vector containing the disease type for each individual. Each individual is assigned to one of NOT_OLP, OLP_no-erosive or OLP_erosive. 
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
See data \code{meta_counts}
}
\examples{
data(lls)
## maybe str(lls) ; plot(lls) ...
}
\keyword{datasets}
