\name{meta_counts}
\alias{meta_counts}
\docType{data}
\title{
An oral metagenomic data
}
\description{
This is an oral metagenomic data from 39 individuals. The columns are the names of the microbial with genus level and the rownames are the ID annotations for the samples. It is a matrix containing the counts of the microbial in each individual mapped to a certain microbiome.
}
\usage{data("meta_counts")}
\format{
  The format is:
 int [1:39, 1:129] 0 0 0 0 0 0 0 0 0 0 ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:39] "NO2" "NO6" "NO8" "NO9" ...
  ..$ : chr [1:129] "Abiotrophia" "Acholeplasma" "Achromobacter" "Acidaminococcus" ...
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(meta_counts)
## maybe str(meta_counts) ; plot(meta_counts) ...
}
\keyword{datasets}
