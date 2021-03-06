% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/liftTable.R
\name{liftTable}
\alias{liftTable}
\title{Calculate lift table}
\usage{
liftTable(predTest, depTest, resolution = 1/10)
}
\arguments{
\item{predTest}{Vector with predictions (real-valued or discrete)}

\item{depTest}{Vector with true class labels}

\item{resolution}{Value for the determination of percentile intervals. Default 1/10 (10\%).}
}
\value{
A lift table.
}
\description{
Calculates a lift table, showing for different percentiles of predicted scores how much
more the characteristic or action of interest occurs than for the overall sample.
}
\examples{
## Load response modeling predictions
data("response")
## Apply liftTable function to obtain lift table for test sample results and print
## results
lt<-liftTable(response$test[,2],response$test[,1])
print(lt)

}
\references{
Berry, M.J.A. and Linoff, G.S. (2004): "Data Mining Techniques: For Marketing, Sales, and
Customer Relationship Management - Second Edition". John Wiley & Sons.
}
\seealso{
\code{\link{topDecileLift}}, \code{\link{liftIndex}}, \code{\link{liftChart}}
}
\author{
Koen W. De Bock, \email{kdebock@audencia.com}
}
