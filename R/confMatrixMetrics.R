#' Obtain several metrics based on the confusion matrix
#'
#' Calculates a range of metrics based upon the confusion matrix: accuracy, true positive
#' rate (TPR; sensitivity or recall), true negative rate (specificity), false postive
#' rate (FPR), false negative rate (FPR), F1-score , with the optional ability to
#' dynamically determine an
#' incidence-based cutoff value using validation sample predictions.
#'
#' @param predTest Vector with predictions (real-valued or discrete)
#' @param depTest Vector with real class labels
#' @param cutoff Threshold for converting real-valued predictions into class predictions.
#' Default 0.5.
#' @param dyn.cutoff Logical indicator to enable dynamic threshold determination using
#' validation sample predictions. In this case, the function determines, using validation
#' data, the incidence (occurrence percentage of the customer behavior or characterstic
#' of interest) and chooses a cutoff value so that the number of predicted positives is
#' equal to the number of true positives. If \code{TRUE}, then the value for the
#' \code{cutoff} parameter is ignored.
#' @param predVal Vector with predictions (real-valued or discrete). Only used if
#' \code{dyn.cutoff} is \code{TRUE}.
#' @param depVal Optional vector with true class labels for validation data. Only used
#' if \code{dyn.cutoff} is \code{TRUE}.
#' @return A list with the following items:
#' \item{accuracy}{accuracy value}
#' \item{truePostiveRate}{TPR or true positive rate}
#' \item{trueNegativeRate}{TNR or true negative rate}
#' \item{falsePostiveRate}{FPR or false positive rate}
#' \item{falseNegativeRate}{FNR or false negative rate}
#' \item{F1Score}{F1-score}
#' \item{cutoff}{the threshold value used to convert real-valued predictions to class
#' predictions}
#' @export
#' @author Koen W. De Bock, \email{kdebock@@audencia.com}
#' @references Witten, I.H., Frank, E. (2005): Data Mining: Practical Machine Learning
#' Tools and Techniques, Second Edition. Chapter 5. Morgan Kauffman.
#' @seealso \code{\link{dynConfMatrix}},\code{\link{dynAccuracy}}
#' @examples
#' ## Load response modeling data set
#' data("response")
#' ## Apply confMatrixMetrics function to obtain confusion matrix-based performance metrics
#' ## achieved on the test sample. Use validation sample predictions to dynamically
#' ## determine a cutoff value.
#' cmm<-confMatrixMetrics(response$test[,2],response$test[,1],dyn.cutoff=TRUE,
#' predVal=response$val[,2],depVal=response$val[,1])
#' ## Retrieve F1-score
#' print(cmm$F1Score)
#'
confMatrixMetrics <- function(predTest,depTest,cutoff=0.5,dyn.cutoff=FALSE,predVal=NULL,depVal=NULL){
  checkDepVector(depTest)
  cm<-dynConfMatrix(predTest,depTest,cutoff=cutoff,dyn.cutoff=dyn.cutoff,predVal=predVal,depVal=depVal)
  ACC<-sum(diag(cm$confMatrix))/sum(cm$confMatrix)
  TP = cm$confMatrix[2,2]
  TN = cm$confMatrix[1,1]
  FP = cm$confMatrix[1,2]
  FN = cm$confMatrix[2,1]
  TPR = TP/(TP+FN) #(sensitivity)
  TNR = TN/(FP+TN) #(specificity)
  FPR = FP/(TP+FN)
  FNR = FN/(FP+TN)
  if (TP+FN>0) REC = TP/(TP+FN) else REC=NA
  if (TP+FP>0) PRE = TP/(TP+FP) else PRE=NA
  if (is.na(PRE)==FALSE && is.na(REC)==FALSE) F1 = 2*PRE*REC/(PRE+REC) else F1 = NA
  ans<-list(accuracy=ACC,truePositiveRate=TPR,trueNegativeRate=TNR,falsePostiveRate=FPR,falseNegativeRate=FPR,F1Score=F1,cutoff=cutoff)
  ans
}

