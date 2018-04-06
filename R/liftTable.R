#' Calculate lift table
#'
#' Calculates a lift table, showing for different percentiles of predicted scores how much
#' more the characteristic or action of interest occurs than for the overall sample.
#'
#' @param predTest Vector with predictions (real-valued or discrete)
#' @param depTest Vector with true class labels
#' @param resolution Value for the determination of percentile intervals. Default 1/10 (10\%).
#' @return A lift table.
#' @import stats
#' @export
#' @author Koen W. De Bock, \email{kdebock@@audencia.com}
#' @references Berry, M.J.A. and Linoff, G.S. (2004): "Data Mining Techniques: For Marketing, Sales, and
#' Customer Relationship Management - Second Edition". John Wiley & Sons.
#' @seealso \code{\link{topDecileLift}}, \code{\link{liftIndex}}, \code{\link{liftChart}}
#' @examples
#' ## Load response modeling predictions
#' data("response")
#' ## Apply liftTable function to obtain lift table for test sample results and print
#' ## results
#' lt<-liftTable(response$test[,2],response$test[,1])
#' print(lt)
#'
liftTable <- function (predTest,depTest,resolution=1/10) {
  checkDepVector(depTest)
  tmp <- unique(depTest)
  depvalues <- tmp[order(tmp)]
  yp = cbind(as.factor(depTest),predTest)
  perc_list <- seq(0,1,resolution)[-1]
  lift = array(0,length(perc_list))
  ratio = array(0,length(perc_list))
  nr_per_percentile <- array(0,length(perc_list))
  P<- sum((yp[,1]==2)*1)
  N<- sum((yp[,1]==1)*1)
  yp_s <- yp[cbind(order(yp[,2],decreasing=TRUE)),]
  yp_s <- as.data.frame(yp_s[complete.cases(yp_s),])
  for (perc_idx in 1:length(perc_list)) {
    #[tmp,idx] = sort(yp(:,2),'descend');
    lift_percentage <- perc_list[perc_idx];
    cut_value <- yp_s[round(nrow(yp_s)*lift_percentage),2];
    #rm(idx);
    consideration_table_part1 <- yp_s[(yp_s[,2]>cut_value),];
    consideration_table_part2 <- yp_s[yp_s[,2]==cut_value,];
    idx <- sample(1:nrow(consideration_table_part2),nrow(consideration_table_part2),replace=FALSE);
    consideration_table_part2_s <- consideration_table_part2[idx,];
    consideration_table <- rbind(consideration_table_part1,consideration_table_part2_s);
    rm(consideration_table_part1)
    rm(consideration_table_part2)
    rm(consideration_table_part2_s)
    ratio[perc_idx] <- sum((consideration_table[1:round(nrow(yp_s)*lift_percentage),1]==2)*1,na.rm=TRUE)/floor(nrow(yp_s)*lift_percentage);
    #if (ratio == 0)
    # ratio = sum(yp_s(yp_s(:,2)==max(yp_s(:,2)),1)==1)/sum(yp_s(:,2)==max(yp_s(:,2)),1);
    #end
    lift[perc_idx] <- (ratio[perc_idx] / (P/(P+N)));
    nr_per_percentile[perc_idx] = sum(consideration_table[ceiling(nrow(yp_s)*(lift_percentage-perc_list[1])+0.000001):floor(nrow(yp_s)*lift_percentage),1]==2);
  }
  liftTable<- as.data.frame(cbind(perc_list*100,lift,(P/(P+N)),ratio,nr_per_percentile))
  colnames(liftTable)<-c("Percentile","TopPercentileLift","expectedIncidence","trueIncidence","nrel")
  return(liftTable)
}
