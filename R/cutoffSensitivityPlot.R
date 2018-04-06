#' Plot a sensitivity plot for cutoff values
#'
#' Visualize the sensitivity of a chosen metric to the choice of the threshold (cutoff) value
#' used to transform continuous predictions into class predictions.
#'
#' @param predTest Vector with predictions (real-valued or discrete)
#' @param depTest Vector with true class labels
#' @param metric Which metric to assess. Should be one of the following values:
#' \code{"accuracy"}, \code{"misclassCost"} or \code{"expMisclassCost"}.
#' @param resolution Value for the determination of percentile intervals. Default 1/10 (10\%).
#' @param costType An argument that specifies how the cost information is provided.
#' This should be either \code{"costRatio"} or \code{"costMatrix"} when \code{metric}
#' equals \code{"expMisclassCost"}; or \code{"costRatio"}, \code{"costVector"} or
#' \code{"costMatrix"} when \code{metric} equals \code{"MisclassCost"}. In the former
#' case, a single value is provided which reflects the cost ratio (the ratio of the cost
#' associated with a false negative to the cost associated with a false positive). In the
#' latter case, a full (4x4) misclassification cost matrix should be provided in the form
#' \code{rbind(c(0,3),c(15,0))} where in this example 3 is the cost for a false positive,
#' and 15 the cost for a false negative case.
#' @param costs see \code{costType}
#' @import stats graphics
#' @export
#' @author Koen W. De Bock, \email{kdebock@@audencia.com}
#' @seealso \code{\link{dynAccuracy}}, \code{\link{misclassCost}}, \code{\link{expMisclassCost}}
#' @examples
#' ## Load response modeling predictions
#' data("response")
#' ## Apply cutoffSensitivityPlot function to visualize how the cutoff value influences
#' ## accuracy.
#' cutoffSensitivityPlot(response$test[,2],response$test[,1],metric="accuracy")
#' ## Same exercise, but in function of misclassification costs
#' costs <- runif(nrow(response$test), 1, 50)
#' cutoffSensitivityPlot(response$test[,2],response$test[,1],metric="misclassCost",
#' costType="costVector",costs=costs, resolution=1/10)
#'
cutoffSensitivityPlot <- function(predTest,depTest,metric=c("accuracy","expMisclassCost","misclassCost"),costType=c("costRatio","costMatrix","costVector"),costs=NULL,resolution=1/50) {
  checkDepVector(depTest)
  if (metric=="expMisclassCost" && costType=="costVector") stop("Cost vectors are not supported for the misclassification cost. Use expMisclassCost.")
  tmp <- unique(depTest)
  depvalues <- tmp[order(tmp)]
  yp = cbind(as.factor(depTest),predTest)
  perc_list <- seq(0,1,resolution)[-1]
  perf = mat.or.vec(length(perc_list),1)

  for (perc_idx in 1:length(perc_list)) {
    if (metric=="accuracy") {
      perf[perc_idx]<-dynAccuracy(predTest,depTest,dyn.cutoff=FALSE,cutoff=perc_list[perc_idx])[[1]]
      multiplier<-1
      } else if (metric=="expMisclassCost") {
      perf[perc_idx]<-expMisclassCost(predTest,depTest,costType=costType, costs=costs,cutoff=perc_list[perc_idx])[[1]]
      multiplier<- -1
    } else if (metric=="misclassCost") {
      perf[perc_idx]<-misclassCost(predTest,depTest,costType=costType, costs=costs,cutoff=perc_list[perc_idx])[[1]]
      multiplier<- -1
    }
  }
  plot(perc_list,perf,type="b",xlab="Cutoff",ylab=metric, main="Cutoff sensitivity Chart")
  perf2<-multiplier*perf
  lines(perc_list[which(perf2==max(perf2))],rep(max(perf2),length(perc_list[which(perf2==max(perf2))])),type="b",col=34)
  lines(perc_list[which(perf2==max(perf2))],rep(max(perf2),length(perc_list[which(perf2==max(perf2))])),type="h",col=34)
  text(perc_list[which(perf2==max(perf2))][1],abs(min(perf)), paste("optimal cutoff =",perc_list[which(perf2==max(perf2))][1]), cex=1)
}

