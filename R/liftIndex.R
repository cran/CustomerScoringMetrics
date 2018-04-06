#' Calculate lift index
#'
#' Calculates lift index metric.
#'
#' @param predTest Vector with predictions (real-valued or discrete)
#' @param depTest Vector with true class labels
#' @return Lift index value
#' @export
#' @author Koen W. De Bock, \email{kdebock@@audencia.com}
#' @references Berry, M.J.A. and Linoff, G.S. (2004): "Data Mining Techniques: For Marketing, Sales, and
#' Customer Relationship Management - Second Edition". John Wiley & Sons.
#' @seealso \code{\link{liftTable}}, \code{\link{topDecileLift}}, \code{\link{liftChart}}
#' @examples
#' ## Load response modeling predictions
#' data("response")
#' ## Calculate lift index for test sample results
#' li<-liftIndex(response$test[,2],response$test[,1])
#' print(li)
#'
liftIndex<-function(predTest,depTest){
  checkDepVector(depTest)
  lt<-liftTable(predTest,depTest)
  LI = t(as.matrix(lt[,5]))%*%as.matrix(1+lt[1,1]/100-(lt[,1]/100)) / sum(lt[,5])
  return(LI)
}
