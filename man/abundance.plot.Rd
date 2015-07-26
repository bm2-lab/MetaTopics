\name{abundance.plot}
\alias{abundance.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot the Abundance Data
}
\description{
This is a function to plot the abundance distribution over samples of microbial. You can specific the classification of each microbiome by previous knowledge and identify them by different colors
}
\usage{
abundance.plot(abundancedata, classification = "", col = "grey28", main = "", xlab = "", ylab = "", first_n = 30, las = 2, legend.cex = 1, sort = "decreasing", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{abundancedata}{
A matrix containing the abundance data, which represents the percentage of the microbial in each individual, with columns as microbial and rows as individuals.
}
  \item{classification}{
Your classification for each microbiome. It could be Species, Genus and so on.
}
  \item{col}{
The specified colors for each microbiome. This is used to represent the classification you have defined. Thus its length should be the same as the parameter classification
}
  \item{main}{
See function \code{plot}
}
  \item{xlab}{
See function \code{plot}
}
  \item{ylab}{
See function \code{plot}
}
  \item{first_n}{
This is used to selecte the top microbial for the plotting. According to the long tail of the distribution, it is not suggested to plot all the microbial. You can sort all of them and then plot the ones you are interested in. 
}
  \item{las}{
See function \code{plot}
}
  \item{legend.cex}{
See \code{legend.cex}
}
  \item{sort}{
Wether you want to sort the plot by decreasing or ascending
}
  \item{\dots}{
See \code{plot}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
	head(genus_2_phylum)
	genus_2_phylum=genus_2_phylum[colnames(meta_abundance),]
	abundance.plot(meta_abundance,classification = genus_2_phylum$phylum,col=genus_2_phylum$colour)
	}
