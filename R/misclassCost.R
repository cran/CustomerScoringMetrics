#' Calculate misclassification cost
#'
#' Calculates the absolute misclassification cost value for a set of predictions.
#'
#' @param predTest Vector with predictions (real-valued or discrete)
#' @param depTest Vector with real class labels
#' @param costType An argument that specifies how the cost information is provided. This
#' should be either \code{"costRatio"} or \code{"costMatrix"}. In the former case, a
#' single value is provided which reflects the cost ratio (the ratio of the cost
#' associated with a false negative to the cost associated with a false positive). In
#' the latter case, a full (4x4) misclassification cost matrix should be provided in the
#' form \code{rbind(c(0,3),c(15,0))} where in this example 3 is the cost for a false
#' positive, and 15 the cost for a false negative case.
#' @param costs see \code{costType}
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
#' @param depVal Optional vector with true class labels for validation data. Only
#' used if \code{dyn.cutoff} is \code{TRUE}.
#' @return A list with the following elements:
#' \item{misclassCost}{Total misclassification cost value}
#' \item{cutoff}{the threshold value used to convert real-valued predictions to class
#' predictions}
#' @export
#' @author Koen W. De Bock, \email{kdebock@@audencia.com}
#' @references Witten, I.H., Frank, E. (2005): Data Mining: Practical Machine Learning
#' Tools and Techniques, Second Edition. Chapter 5. Morgan Kauffman.
#' @seealso \code{\link{dynConfMatrix}},\code{\link{expMisclassCost}},\code{\link{dynAccuracy}}
#' @examples
#' ## Load response modeling data set
#' data("response")
#' ## Generate cost vector
#' costs <- runif(nrow(response$test), 1, 100)
#' ## Apply misclassCost function to obtain the misclassification cost for the
#' ## predictions for test sample. Assume a cost ratio of 5.
#' emc<-misclassCost(response$test[,2],response$test[,1],costType="costVector", costs=costs)
#' print(emc$EMC)
#'
misclassCost<-function(predTest,depTest,costType=c("costRatio","costMatrix","costVector"),costs=NULL,cutoff=0.5,dyn.cutoff=FALSE,predVal=NULL,depVal=NULL){
  checkDepVector(depTest)
  cm<-dynConfMatrix(predTest=predTest,depTest=depTest,cutoff=cutoff,dyn.cutoff=dyn.cutoff,predVal=predVal,depVal=depVal,returnClassPreds=TRUE)
  if (costType=="costVector") {
    COST<- sum((depTest!=cm$classPreds)*costs)
  } else if (costType=="costRatio") {
    COST <- cm$confMatrix[1,2] + costs*cm$confMatrix[2,1]
  } else if (costType=="costMatrix") {
    COST <- cm$confMatrix[1,2]*costs[2,1]+ cm$confMatrix[2,1]*costs[1,2];
  }
  ans<-list(misclassCost=COST,cutoff=cm$cutoff)
  ans
}
