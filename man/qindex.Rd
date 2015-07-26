\name{qindex}
\alias{qindex}

\title{
Quelete Index computing
}
\description{
This function is used to compute the Quelete Index from the topic model 
}
\usage{
qindex(model, lls, prob = 0.05)
}

\arguments{
  \item{model}{
The result of your topic model
}
  \item{lls}{
A vector which includes the disease annotations for each individual.
}
  \item{prob}{
A probability cutoff used to identify a meaningful sub-community observation. For a certain individual, the topics with probability no smaller than prob will be thought as a meaningful pbservation in this individual.
}
}
\details{
A matrix which consists of the Qindex values of each topic over all the disease types.
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
qindex(Gibbs_model_example,lls,0.05)
}
