\name{genus_2_phylum}
\alias{genus_2_phylum}
\docType{data}
\title{
Microbial annotations with genus level and phylum level
}
\description{
This is a data frame containing the annotation for the microbial in data meta_counts. 
}
\usage{data("genus_2_phylum")}
\format{
  A data frame with 129 observations on the following 4 variables.
  \describe{
    \item{\code{genus}}{a character vector}
    \item{\code{phylum}}{a character vector}
    \item{\code{presence}}{a numeric vector}
    \item{\code{colour}}{a character vector}
  }
}
\details{
All the microbial in column \code{genus} are the same as data meta_counts. The column \code{phylum} has the corresponding phylum level annotation for each microbiome. The column \code{colour} is used for \code{abundance.plot} as an identification of the phylum level. 
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(genus_2_phylum)
## maybe str(genus_2_phylum) ; plot(genus_2_phylum) ...
}
\keyword{datasets}
