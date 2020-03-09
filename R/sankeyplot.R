#' Sankey Diagram
#'
#' Plotting versions of Sankey diagrams
#'
#' @param data, vertical white line (0 or 1), horizontal white line (0 or 1)
#' @return diagram
#' @examples
#'    reform(data, 1, 0)
#' @export




# (a,b) stands for the version of the diagram (in terms of white lines)
sankeyplot <-function(data, a, b) {
  data.for.graph <- reform(data)	# reformat the original data according to function 2

  # Colors of the each flow and stratum are in accordance with the original diagram in the Excel file ( "Obesity: A" / "Hypercholesterolemia: B" / "Diabetes: C" / " Smoking: D" / " Hypertension: E" )
  cols <- c(A ="#4D6D94", B ="#D88F54", C="#D9654E", D="#7EACC5", E="#8ABA8F")

  p = ggplot( data.for.graph,  aes(x = year, y = fakevalue,
                                   fill = reorder(fakename, -ordering), alluvium = fakename , stratum = fakename))
  p = p + scale_fill_manual(values = cols)


  # Diagram title and other formats
  p = p + theme_bw() +
    ggtitle("Risk Factors for Stroke in Blacks") +
    theme( plot.title = element_text(size=15, hjust = 0.5 , margin=margin(0,0,30,0)),
           panel.border = element_blank(),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           legend.position = "none",
           axis.title=element_blank(),
           axis.ticks=element_blank(),
           axis.text.y=element_blank(),
           axis.text.x=element_text(size=13) ) +
    scale_x_discrete(position = "top")


  ## Plotting the alluvial diagram (Four cases)
  # [a==0 & b==0] Vertical White (x), Horizontal White (x)
  if (a==0 & b==0) { p = p+geom_alluvium(width = 0.7, knot.pos = 0, alpha = 1,
                                         decreasing = F, fill = c( cols, rep(NA,20) ) ) }

  # [a==1 & b==0] Vertical White (o), Horizontal White (x)
  else if (a==1 & b==0) { p = p+geom_alluvium(width = 0.7, knot.pos = 0, alpha = 1,
                                              decreasing = F, fill = c( cols, rep(NA,20) ) )
  x <- c(1990, 1995, 2000, 2005, 2010)
  for (val in x) {
    p = p + geom_bar(width = 0.7, colour = 'white', size =1.5,
                     position="stack", stat="identity",
                     data = data.for.graph[data.for.graph$year == val , ])
  }

  for (val in x) {
    p = p + geom_bar(width = 0.67, size =1.5, position="stack",
                     stat="identity",
                     data = data.for.graph[data.for.graph$year == val , ] )
  }
  }

  # [a==0 & b==1] Vertical White (x), Horizontal White (o)
  else if (a==0 & b==1) { p = p+geom_alluvium(colour = "white", size = 1.5,
                                              width = 0.7, knot.pos = 0, alpha = 1, decreasing = F, fill = c( cols, rep(NA,20) ) ) }

  # [a==1 & b==1] Vertical White (o), Horizontal White (o)
  else if (a==1 & b==1) { p = p+geom_alluvium(colour = "white", size = 1.5,
                                              width = 0.7, knot.pos = 0, alpha = 1, decreasing = F,
                                              fill = c( cols, rep(NA,20) ) ) +
    geom_stratum(color="white", size =1.5, width = 0.7, decreasing = F) }


  # Adding labels (values) on each stratum (box)
  p = p+ geom_text(aes(label = sprintf( "%0.2f", round(value+0.00001, digits = 2) )),
                   stat = "stratum", color="white", decreasing = F, size=4)


  # Draw an invisible bar around y axis to secure the space for adding y axis label
  p = p+ geom_bar(width = 0.8, position="stack", stat="identity",
                  data= data.for.graph[data.for.graph$year == 1990 , ] , aes( x=0.3), alpha = 0 )


  # Adding y-axis label
  p = p + geom_text( data= data.for.graph[data.for.graph$year == 1990 , ],
                     aes(label = condition , x=0.4), size = 3.5, color = "black",
                     position = position_stack(vjust = 0.5))


  # Error message
  if ( (a!=0 & a!=1) | (b!=0 & b!=1) )
  {print("Error: the arguments 'a' and 'b' of the function 'sankey.plot(data, a, b)' should have the value of either 0 or 1.") }
  else {p}
}
