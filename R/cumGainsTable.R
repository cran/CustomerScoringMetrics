#' Calculates cumulative gains table
#'
#' Calculates a cumulative gains (cumulative lift) table, showing for different percentiles
#' of predicted scores the percentage of customers with the behavior or characterstic of
#' interest is reached.
#'
#' @param predTest Vector with predictions (real-valued or discrete)
#' @param depTest Vector with true class labels
#' @param resolution Value for the determination of percentile intervals. Default 1/10 (10\%).
#' @return A gain percentage table.
#' @export
#' @author Koen W. De Bock, \email{kdebock@@audencia.com}
#' @references Linoff, G.S. and Berry, M.J.A (2011): "Data Mining Techniques: For Marketing, Sales, and
#' Customer Relationship Management - Third Edition". John Wiley & Sons.
#' @seealso \code{\link{topDecileLift}}, \code{\link{liftIndex}}, \code{\link{liftChart}}
#' @examples
#' ## Load response modeling predictions
#' data("response")
#' ## Apply cumGainsTable function to obtain cumulative gains table for test sample results
#' ## and print results
#' cgt<-cumGainsTable(response$test[,2],response$test[,1])
#' print(cgt)
#'
cumGainsTable <- function (predTest,depTest,resolution=1/10) {
  checkDepVector(depTest)
  lt<-liftTable(predTest,depTest,resolution=resolution)
  N<-sum(lt$nrel)
  cumGainsTable <- cbind(lt[,1],N/nrow(lt),cumsum(lt$nrel),cumsum(lt$nrel)/N*100)
  colnames(cumGainsTable)<-c("Percentile","expectedNbrPos","trueNbrPos","cumGainsPercentage")
  return(cumGainsTable)
}
