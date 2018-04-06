#' Generate a lift chart
#'
#' Visualize lift through a lift chart.
#'
#' @param predTest Vector with predictions (real-valued or discrete)
#' @param depTest Vector with true class labels
#' @param resolution Value for the determination of percentile intervals. Default 1/10 (10\%).
#' @export
#' @author Koen W. De Bock, \email{kdebock@@audencia.com}
#' @references Berry, M.J.A. and Linoff, G.S. (2004): "Data Mining Techniques: For Marketing, Sales, and
#' Customer Relationship Management - Second Edition". John Wiley & Sons.
#'
#' Blattberg, R.C., Kim, B.D. and Neslin, S.A. (2008): "Database Marketing: Analyzing and Managing Customers". Springer.
#' @seealso \code{\link{topDecileLift}}, \code{\link{liftIndex}}, \code{\link{liftChart}}
#' @examples
#' ## Load response modeling predictions
#' data("response")
#' ## Apply liftChart function to visualize lift table results
#' liftChart(response$test[,2],response$test[,1])
#'
liftChart<-function(predTest,depTest,resolution=1/10){
  checkDepVector(depTest)
  lt<-liftTable(predTest,depTest)
  plot(lt[,1],lt[,2],type="b",xlab="Percentile",ylab="Lift", main="Lift Chart")
}
