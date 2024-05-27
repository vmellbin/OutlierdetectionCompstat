#' Detects observations that are more than a given number of standard devations away from the mean
#'
#' @param vector The vector where we detect outliers.
#' @param std The standard deviation for outliers to be detected
#'
#' @return Returns S3 object containing the index of the outliers
#' @export
#'
#' @examples vector <- 1:10
#' OutlierVec(vector)
#' 
#'
OutlierVec<- function(vector, std){
  Mean <-mean(vector)
  Stddeviation <- sd(vector)
  lvector<-1:length(vector)
  vector1 <-(vector > Mean-Antalstd*Stddeviation & vector < Mean+Antalstd*Stddeviation)
  Outliervecdetect<-which(vector1 %in% 0)
  class(z)<- c("Outliervec")
  z$mean<- Mean
  z$std<- Stddeviation
  z$outliers<- Outliervecdetect
  z
}

document()



