\name{plot_beta}
\alias{plot_beta}

\title{
Visualization for beta matrix returned from the topic model
}
\description{
Beta matrix returned from the topic model represents the probability or estimated perentage of a microbiome in a certain topic. This function will act as a visulization tool for this matrix.
}
\usage{
plot_beta(model, prob = 0.01)
}

\arguments{
  \item{model}{
	Your topic model returned from package topicmodels
}
  \item{prob}{
	This is a cutoff used to restrict the number of points on the plots. If a microbiome has a probability smaller than the cutoff in a topic, it will not be shown in this topic.
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
plot_beta(Gibbs_model_example,prob=0.01)}
