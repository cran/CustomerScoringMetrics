#' Perform check on the true class label vector
#'
#' Perform check on the true class label vector.
#'
#' @param depTest Vector with true data labels (outcome values)
#' @export
#' @author Koen W. De Bock, \email{kdebock@@audencia.com}
#' @examples
#' ## Load response modeling predictions
#' data("response")
#' ## Apply checkDepVector checking function
#' checkDepVector(response$test[,1])
#'
checkDepVector<-function(depTest){
  tmp <- unique(depTest)
  depvalues <- tmp[order(tmp)]
  if (length(depvalues)>2){
    stop("The outcome variable has more than two classes")
  } else if (length(depvalues)==1) {
    stop("The outcome variable takes a constant value")
  }
}
