\name{plot_network}
\alias{plot_network}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot connections among microbial and topics.
}
\description{
This function uses package igraph to creat connections among microbial and topics. The meaningful microbial in each topic will be connected by a line, which could be identified by color.
}
\usage{
plot_network(model, prob = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
The model returned from topic model.
}
  \item{prob}{
The probability cutoff for the meaningful microbial.
}
}
\details{
In each topic, all the microbial with probability not less than probability cutoff would be connected by a single line. Please note that the order means nothing. If two topics have the same meaningful microbial, these two topics will be connected by the shared microbial and edges.
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
plot_network(Gibbs_model_example,prob=0.05)
}
