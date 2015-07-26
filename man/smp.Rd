\name{smp}
\alias{smp}

\title{
Generate the data set for cross validation used in function selectK
}
\description{
This function is used to generate a list which contains the index of samples used for validation in each fold of cross validation.
}
\usage{
smp(cross, n, seed)
}

\arguments{
  \item{cross}{
the number of cross validation
}
  \item{n}{
the number of samples
}
  \item{seed}{

}
}
\details{

}
\value{
A list, whose length is the number of fold for the cross validation. Each element of the list is a vector containing the index of the sample used in as the validation samples in each fold of cross validation.
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
#See Example in function selectK
}
