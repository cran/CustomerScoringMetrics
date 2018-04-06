#' Calculate top-decile lift
#'
#' Calculates top-decile lift, a metric that expresses how the incidence in the 10\% customers
#' with the highest model predictions compares to the overall sample incidence. A top-decile
#' lift of 1 is expected for a random model. A top-decile lift of 3 indicates that in the 10\%
#' highest predictions, 3 times more postive cases are identified by the model than would be expected
#' for a random selection of instances.  The upper boundary of the metric depends on the sample
#' incidence and is given by 100\% / Indidence \%. E.g. when the incidence is 10\%, top-decile
#' lift can be no higher than 10.
#'
#' @param predTest Vector with predictions (real-valued or discrete)
#' @param depTest Vector with true class labels
#' @return Top-decile lift value
#' @references Berry, M.J.A. and Linoff, G.S. (2004): "Data Mining Techniques: For Marketing, Sales, and
#' Customer Relationship Management - Second Edition". John Wiley & Sons.
#' @export
#' @author Koen W. De Bock, \email{kdebock@@audencia.com}
#' @seealso \code{\link{liftTable}}, \code{\link{liftIndex}}, \code{\link{liftChart}}
#' @examples
#' ## Load response modeling predictions
#' data("response")
#' ## Calculate top-decile lift for test sample results
#' tdl<-topDecileLift(response$test[,2],response$test[,1])
#' print(tdl)
#'
topDecileLift<-function(predTest,depTest){
  checkDepVector(depTest)
  lt<-liftTable(predTest,depTest)
  TDL<-lt[which(lt[,1]==10),2]
  return(TDL)
}
