#' Calculate accuracy
#'
#' Calculates accuracy (percentage correctly classified instances) for real-valued
#' classifier predictions, with the optional ability to dynamically determine an
#' incidence-based cutoff value using validation sample predictions
#'
#' @param predTest Vector with predictions (real-valued or discrete)
#' @param depTest Vector with real class labels
#' @param cutoff Threshold for converting real-valued predictions into class predictions.
#' Default 0.5.
#' @param dyn.cutoff Logical indicator to enable dynamic threshold determination using
#' validation sample predictions. In this case, the function determines, using validation
#' data, the indidicence (occurrence percentage of the customer behavior or characterstic
#' of interest) and chooses a cutoff value so that the number of predicted positives is
#' equal to the number of true positives. If \code{TRUE}, then the value for the cutoff
#' parameter is ignored.
#' @param predVal Vector with predictions (real-valued or discrete). Only used if
#' \code{dyn.cutoff} is \code{TRUE}.
#' @param depVal Optional vector with true class labels for validation data. Only used
#' if \code{dyn.cutoff} is \code{TRUE}.
#' @return Accuracy value
#' \item{accuracy}{accuracy value}
#' \item{cutoff}{the threshold value used to convert real-valued predictions to class
#' predictions}
#' @export
#' @author Koen W. De Bock, \email{kdebock@@audencia.com}
#' @seealso \code{\link{dynConfMatrix}},\code{\link{confMatrixMetrics}}
#' @examples
#' ## Load response modeling data set
#' data("response")
#' ## Apply dynAccuracy function to obtain the accuracy that is achieved on the test sample.
#' ## Use validation sample predictions to dynamically determine a cutoff value.
#' acc<-dynAccuracy(response$test[,2],response$test[,1],dyn.cutoff=TRUE,predVal=
#' response$val[,2],depVal=response$val[,1])
#' print(acc)
#'
dynAccuracy<-function(predTest,depTest,dyn.cutoff=FALSE,cutoff=0.5,predVal=NULL,depVal=NULL){
  checkDepVector(depTest)
  cm<-dynConfMatrix(predTest,depTest,cutoff=cutoff,dyn.cutoff=dyn.cutoff,predVal=predVal,depVal=depVal)
  ACC<-sum(diag(cm$confMatrix))/sum(cm$confMatrix)
  ans<-list(accuracy=ACC,cutoff=cutoff)
  ans
}
