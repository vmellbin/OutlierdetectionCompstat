#' Detection of outliers based on standard deviation
#'
#' Detects observations that are more than a given number of standard devations away from the mean
#'
#' @param vector The vector where we detect outliers.
#' @param std The number of standard deviations for outliers to be detected
#'
#' @return Returns S3 object containing the index of the outliers
#'
#' @examples vector <- 1:10
#' Outliers<-OutlierVec(vector, std=1)
#' Outliers
#' @export
OutlierVec<- function(vector, std=2){
  Mean <-mean(vector)
  Antalstd=std
  Stddeviation <- sd(vector)
  lvector<-1:length(vector)
  vector1 <-(vector > Mean-Antalstd*Stddeviation & vector < Mean+Antalstd*Stddeviation)
  Outliervecdetect<-which(vector1 %in% 0)
  z<- list()
  class(z)<- c("Outliervec")
  z$mean<- Mean
  z$std<- Stddeviation
  z$Numberofstd<- Antalstd
  z$outliers<- Outliervecdetect
  z$Svector<- vector
  z$vector1<-vector1
  z
}


#' @export
plot.Outliervec<- function(x){
  outside_indices <- x$outliers
  Mean <-x$mean
  Stddeviation <- x$std
  Antalstd<- x$Numberofstd
  vector<- x$Svector
  vector1<- x$vector1
  plot(1:length(vector), vector, col=ifelse(vector1, "green", "red"),main="Outliers based on std deviation",xlab="index",ylab="")
  abline(h=Mean, col="black")
  text(outside_indices, vector[outside_indices], labels = outside_indices, pos = 2, cex = 0.8, col = "black")
  abline(h=Mean+Antalstd*Stddeviation, col="red",lty=2)
  abline(h=Mean-Antalstd*Stddeviation, col="red",lty=2)
}

#' @export
summary.Outliervec<- function(x) {
  sum<- list()
  class(sum)<- c("summary.Outliervec")
  sum$Numberofoutliers<- length(x$outliers)
  sum$Indexofoutliers<- x$outliers
  sum$Confidenceinterval<- paste("These outliers lie outside of the range (",x$mean-x$std*x$Numberofstd,"," ,x$mean+x$std*x$Numberofstd,")")
  sum
}

#' @export 
print.Outliervec<- function(x) {
  cat("\nCall:\n",
      paste("the index of the outliers in the given vector are", "\n"), x$outliers)
  cat("\n")
  invisible(x)
}






