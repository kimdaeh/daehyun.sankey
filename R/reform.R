#' wide form data to long form
#'
#' Taking data resulted from the function 'change' and produce a data frame that can be used for plotting the diagram.
#'
#' @param changed data
#' @return reformatted data
#' @examples
#'    reform(data)
#' @export


reform <- function (data) {		# data: original format of the data
  data1 <- change(data)		# reshape the data by the function1
  year <- c( rep("1990" , 5) , rep("1995" , 5) , rep("2000" , 5) , rep("2005" , 5) , rep("2010" , 5) )
  condition <- rep( colnames(data1)[2:6] , 5 )
  value <- c( data1[1 , ][2 : 6], data1[2 , ][2 : 6], data1[3 , ][2 : 6], data1[4 , ][2 : 6], +
                data1[5 , ][2 : 6] )
  value <- unlist( value )

  # List the name of the disease in the order of the value of the fraction as of 1990. (In order to make y axis label)
  ordering0 <- rbind( rank( data1[1 , c(2:6)]), rank( data1[2 , c(2:6)]), +
                        rank( data1[3 , c(2:6)]), rank( data1[4 , c(2:6)]), rank(data1[5, c(2:6)]) )
  ordering <- c( ordering0[1 , ], ordering0[2 , ], ordering0[3 , ], ordering0[4 , ], ordering0[5 , ] )
  data.for.graph <- data.frame( year , condition , value, ordering)

  # fakename: I found that the order of layers (flow) drawn on the diagram is in the alphabetical order of the disease name. However, the diagram in the Excel file is not so. Therefore, I reorder the sequence of the disease in accordance with the order in the original diagram.
  temp = data.for.graph$condition
  fakename = ifelse( temp=="Obesity", "A",
                     ifelse (temp=="Hypercholesterolemia", "B",
                             ifelse (temp=="Diabetes", "C",
                                     ifelse (temp=="Smoking", "D", "E"))))
  data.for.graph <- data.frame(data.for.graph, fakename)

  # fakevalue: The height of the stratum for a disease which has very small value of fraction is too short so the diagram drawn does not display such disease (e.g., Obesity in 1990). Therefore, I adjusted the value of fraction of each disease in a way the plotted diagram can display it.
  data.for.graph$fakevalue = ifelse (data.for.graph$value<=0.001, 0.025, data.for.graph$value)
  data.for.graph$fakevalue = ifelse (data.for.graph$value<0.02 & data.for.graph$value>=0.01, +
                                       0.03, data.for.graph$fakevalue)

  data.for.graph
}
