# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

change <- function (data) {
  var.name <- data[ , 1]
  data <- data[ , -1]
  data <- as.data.frame( t(data) )
  rownames(data) <- c(1 : 5)
  colnames(data) <- var.name
  colnames(data)[1] <- "Year"
  data
}
