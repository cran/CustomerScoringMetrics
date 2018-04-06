#' Plot a cumulative gains chart
#'
#' Visualize gain through a cumulative gains chart.
#'
#' @param predTest Vector with predictions (real-valued or discrete)
#' @param depTest Vector with true class labels
#' @param resolution Value for the determination of percentile intervals. Default 1/10 (10\%).
#' @export
#' @author Koen W. De Bock, \email{kdebock@@audencia.com}
#' @references Linoff, G.S. and Berry, M.J.A (2011): "Data Mining Techniques: For Marketing, Sales, and
#' Customer Relationship Management - Third Edition". John Wiley & Sons.
#' @seealso \code{\link{topDecileLift}}, \code{\link{liftIndex}}, \code{\link{liftChart}}
#' @examples
#' ## Load response modeling predictions
#' data("response")
#' ## Apply cumGainschart function to visualize cumulative gains of a customer response model
#' cumGainsChart(response$test[,2],response$test[,1])
#'
cumGainsChart <- function(predTest,depTest,resolution=1/10) {
  checkDepVector(depTest)
  gt<-cumGainsTable(predTest,depTest,resolution=resolution)
  plot(c(0,gt[,1]),c(0,gt[,4]),type="b",xlab="Percentile",ylab="Cumulative Gain %", main="Cumulative Gains Chart")
  lines(seq(0,1,resolution)*100,seq(0,1,resolution)*100,type="l")
  }
