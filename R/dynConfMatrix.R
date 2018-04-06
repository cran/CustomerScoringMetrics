#' Calculate a confusion matrix
#'
#' Calculates a confusion matrix for real-valued classifier predictions, with the optional
#' ability to dynamically determine an
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
#' equal to the number of true positives. If TRUE, then the value for the cutoff parameter
#' is ignored.
#' @param predVal Vector with predictions (real-valued or discrete). Only used if
#' \code{dyn.cutoff} is TRUE.
#' @param depVal Optional vector with true class labels for validation data. Only used
#' if \code{dyn.cutoff} is \code{TRUE}.
#' @param returnClassPreds Boolean value: should class predictions (using \code{cutoff})
#' be returned?
#' @return A list with two elements:
#' \item{confMatrix}{a confusion matrix}
#' \item{cutoff}{the threshold value used to convert real-valued predictions to class
#' predictions}
#' \item{classPreds}{class predictions, if requested using \code{returnClassPreds}}
#' @export
#' @author Koen W. De Bock, \email{kdebock@@audencia.com}
#' @references Witten, I.H., Frank, E. (2005): Data Mining: Practical Machine Learning
#' Tools and Techniques, Second Edition. Chapter 5. Morgan Kauffman.
#' @seealso \code{\link{dynAccuracy}}, \code{\link{confMatrixMetrics}}
#' @examples
#' ## Load response modeling data set
#' data("response")
#' ## Apply dynConfMatrix function to obtain a confusion matrix. Use validation sample
#' ## predictions to dynamically determine an incidence-based cutoff value.
#' cm<-dynConfMatrix(response$test[,2],response$test[,1],dyn.cutoff=TRUE,
#' predVal=response$val[,2],depVal=response$val[,1])
#' print(cm)
#'
dynConfMatrix<-function(predTest,depTest,cutoff=0.5,dyn.cutoff=FALSE,predVal=NULL,depVal=NULL,returnClassPreds=FALSE){
  checkDepVector(depTest)
  tmp <- unique(depTest)
  depvalues <- tmp[order(tmp)]
  yp <- data.frame(cbind(as.factor(depTest),predTest))

  if (length(depTest) != length(predTest))
    stop("Input vectors predTest and depTest should have the same length")
  if (!(length(cutoff) == 1 & cutoff[1] <= 1 & cutoff[1] >= 0))
    stop("inappropriate cutoff value... must be a single value between 0 & 1.")
  n = length(depTest)
  if (length(which(is.na(c(depTest, predTest)))) > 0) {
    na = union(which(is.na(depTest)), which(is.na(predTest)))
    warning(length(na), " data points removed due to missing data in either predTest or depTest")
    depTest = depTest[-na]
    predTest = predTest[-na]
  }
  if (dyn.cutoff==TRUE) {
    N = sum((yp[,1]==2)*1)
    cutoff <- (as.numeric(as.matrix(predVal[order(predVal)][length(predVal)-(N-1)]))+ as.numeric(as.matrix(predVal[order(predVal)][length(predVal)-(N)])))/2
  }
  if (cutoff == 0) {
    predTest3 <- (predTest>cutoff)*1
  } else {
    if (cutoff==0.5) cutoff<-0.50001
    predTest3 <- (predTest>=cutoff)*1
  }
  Pred <- factor(predTest3,labels=depvalues)
  Real <- factor(depTest,levels=depvalues)
  confMatrix = table(Real, Pred)

  if (length(rownames(confMatrix))==1) {
    if (rownames(confMatrix)==depvalues[2]) {
      confMatrix <- rbind(confMatrix,c(0,0))
      rownames(confMatrix)[2] <- depvalues[1]
    } else if (rownames(confMatrix)==depvalues[1]) {
      confMatrix <- rbind(c(0,0),confMatrix)
      rownames(confMatrix)[1] <- depvalues[2]
    }
  }
  if (returnClassPreds==TRUE) {
    classPreds<-Pred
  } else classPreds=NULL
  ans<-list(confMatrix=confMatrix,cutoff=cutoff,classPreds=classPreds)
  ans
}
