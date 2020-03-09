#' Change of data form
#'
#' Taking input as original data and output as another form that is easy to handle.
#'
#' @param original data
#' @return Reshaped data
#' @examples
#'    change(data)

change <- function (data) {
  data1 <- read.table(data, stringsAsFactors=F, sep="\t")
  var.name <- data1[ , 1]
  data1 <- data1[ , -1]
  data1 <- as.data.frame( t(data1) )
  rownames(data1) <- c(1 : 5)
  colnames(data1) <- var.name
  colnames(data1)[1] <- "Year"
  data1
}
