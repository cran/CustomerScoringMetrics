#' Calculate expected misclassification cost
#'
#' Calculates the expected misclassification cost value for a set of predictions.
#'
#' @param predTest Vector with predictions (real-valued or discrete)
#' @param depTest Vector with real class labels
#' @param costType An argument that specifies how the cost information is provided. This
#' should be either \code{"costRatio"} or \code{"costMatrix"}. In the former case, a single
#' value is provided which reflects the cost ratio (the ratio of the cost associated with a
#' false negative to the cost associated with a false positive). In the latter case, a full
#' (4x4) misclassification cost matrix should be provided in the form
#' \code{rbind(c(0,3),c(15,0))} where in this example 3 is the cost for a false positive,
#' and 15 the cost for a false negative case.
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
#' @param depVal Optional vector with true class labels for validation data. Only used
#' if \code{dyn.cutoff} is \code{TRUE}.
#' @return A list with
#' \item{EMC}{expected misclassification cost value}
#' \item{cutoff}{the threshold value used to convert real-valued predictions to class
#' predictions}
#' @export
#' @author Koen W. De Bock, \email{kdebock@@audencia.com}
#' @seealso \code{\link{dynConfMatrix}},\code{\link{misclassCost}}
#' @examples
#' ## Load response modeling data set
#' data("response")
#' ## Apply expMisclassCost function to obtain the misclassification cost for the
#' ## predictions for test sample. Assume a cost ratio of 5.
#' emc<-expMisclassCost(response$test[,2],response$test[,1],costType="costRatio", costs=5)
#' print(emc$EMC)
#'
expMisclassCost<-function(predTest,depTest,costType=c("costRatio","costMatrix"),costs=NULL,cutoff=0.5,dyn.cutoff=FALSE,predVal=NULL,depVal=NULL){
  checkDepVector(depTest)
  cm<-dynConfMatrix(predTest,depTest,cutoff=cutoff,dyn.cutoff=dyn.cutoff,predVal=predVal,depVal=depVal)
  if (costType=="costRatio") {
    EMC=(cm$confMatrix[1,2] + costs*cm$confMatrix[2,1])/sum(diag(cm$confMatrix));
  } else if (costType=="costMatrix") {
    EMC=(costs[1,2]*cm$confMatrix[1,2] + costs[2,1]*cm$confMatrix[2,1])/sum(diag(cm$confMatrix))
  }
  ans<-list(EMC=EMC,cutoff=cm$cutoff)
  ans
}

