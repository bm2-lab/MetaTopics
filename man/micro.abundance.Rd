\name{micro.abundance}
\alias{micro.abundance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Microbiome Aabundance
}
\description{
This function is used to convert the count data to abundance percentage of the microbial in each individual
}
\usage{
micro.abundance(data, dimi = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
The data of meta genomics with the counts of the microbial in each individuals. By default, the columns should be bacteria and the rows should be individuals
}
  \item{dimi}{
If your data has rows as the microbial, you could change dim to be 2.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A matrix with the same dimension will be returned and each value will be a persentage of the bacteriom in the individual.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
meta_abundance <- micro.abundance(meta_counts,1)
apply(meta_abundance,1,sum)
}

