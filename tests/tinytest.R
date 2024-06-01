
if ( requireNamespace("tinytest", quietly=TRUE) ){
  tinytest::test_package("OutlierdetectionCompstat")
}

set.seed(1000)
vector<- rnorm(100)
Outliers<-OutlierVec(vector, 2)




expect_equal(Outliers$outliers, c(19,21,59,88,90))
