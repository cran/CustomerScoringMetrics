\name{response}
\alias{response}
\docType{data}
\title{response data
}
\description{
    Predicted customer reponse probabilities and true responses for a customer scoring model. Includes results for two data samples: a test sample (\code{response$test}) and a validation sample (\code{response$val}).
}
\usage{data(response)}
\format{
  A list with two elements: \code{response$test} and \code{response$val}, both are data frames with data for 2 variables: \code{preds} and \code{dep}.
 }
 \author{
  Authors: Koen W. De Bock
  Maintainer: \email{kdebock@audencia.com}
}
\examples{
# Load data
data(response)
# Calculate incidence in test sample
print(sum(response$test[,1]=="cl1")/nrow(response$test))
}
\keyword{datasets}
