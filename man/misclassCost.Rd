% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misclassCost.R
\name{misclassCost}
\alias{misclassCost}
\title{Calculate misclassification cost}
\usage{
misclassCost(predTest, depTest, costType = c("costRatio", "costMatrix",
  "costVector"), costs = NULL, cutoff = 0.5, dyn.cutoff = FALSE,
  predVal = NULL, depVal = NULL)
}
\arguments{
\item{predTest}{Vector with predictions (real-valued or discrete)}

\item{depTest}{Vector with real class labels}

\item{costType}{An argument that specifies how the cost information is provided. This
should be either \code{"costRatio"} or \code{"costMatrix"}. In the former case, a
single value is provided which reflects the cost ratio (the ratio of the cost
associated with a false negative to the cost associated with a false positive). In
the latter case, a full (4x4) misclassification cost matrix should be provided in the
form \code{rbind(c(0,3),c(15,0))} where in this example 3 is the cost for a false
positive, and 15 the cost for a false negative case.}

\item{costs}{see \code{costType}}

\item{cutoff}{Threshold for converting real-valued predictions into class predictions.
Default 0.5.}

\item{dyn.cutoff}{Logical indicator to enable dynamic threshold determination using
validation sample predictions. In this case, the function determines, using validation
data, the indidicence (occurrence percentage of the customer behavior or characterstic
of interest) and chooses a cutoff value so that the number of predicted positives is
equal to the number of true positives. If \code{TRUE}, then the value for the cutoff
parameter is ignored.}

\item{predVal}{Vector with predictions (real-valued or discrete). Only used if
\code{dyn.cutoff} is \code{TRUE}.}

\item{depVal}{Optional vector with true class labels for validation data. Only
used if \code{dyn.cutoff} is \code{TRUE}.}
}
\value{
A list with the following elements:
\item{misclassCost}{Total misclassification cost value}
\item{cutoff}{the threshold value used to convert real-valued predictions to class
predictions}
}
\description{
Calculates the absolute misclassification cost value for a set of predictions.
}
\examples{
## Load response modeling data set
data("response")
## Generate cost vector
costs <- runif(nrow(response$test), 1, 100)
## Apply misclassCost function to obtain the misclassification cost for the
## predictions for test sample. Assume a cost ratio of 5.
emc<-misclassCost(response$test[,2],response$test[,1],costType="costVector", costs=costs)
print(emc$EMC)

}
\references{
Witten, I.H., Frank, E. (2005): Data Mining: Practical Machine Learning
Tools and Techniques, Second Edition. Chapter 5. Morgan Kauffman.
}
\seealso{
\code{\link{dynConfMatrix}},\code{\link{expMisclassCost}},\code{\link{dynAccuracy}}
}
\author{
Koen W. De Bock, \email{kdebock@audencia.com}
}
