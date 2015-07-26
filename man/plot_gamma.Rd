\name{plot_gamma}
\alias{plot_gamma}

\title{
Visualization of the gamma matrix returned from the topic model
}
\description{
Gamma matrix returned from the topic model represents the probability or estimated perentage of a topic in a certain individual. This function will act as a visulization tool for this matrix.
}
\usage{
plot_gamma(model, lls, prob = 0.05)
}

\arguments{
  \item{model}{
Your topic model returned from package topicmodels.
}
  \item{lls}{
This is should be a vector, which contains the disease class of the individuals so that the individuals can be clustered.
}
  \item{prob}{
This is a cutoff used to restrict the number of points on the plots. If a topic has a probability smaller than the cutoff in an individual, it will not be shown on this topic.
}
}
\details{
This plot is based on ggplot2 and you can customise it. Please help for ggplot2
}
\value{

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
library(ggplot2)
plot_gamma(Gibbs_model_example,lls,prob=0.05)}
