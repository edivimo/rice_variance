# Supporting functions & graphs

# Loading packages
library(readr)
library(ggplot2)

# Loading data
IR8 <- read_csv(file = "data/rice_long.csv",
                col_types = cols(
                  Row = col_character(),
                  Column = col_character(),
                  value = col_double()) )

CR5272 <- read_csv(file = "data/cr_rice_long.csv",
                   col_types = cols(
                     Row = col_character(),
                     Column = col_character(),
                     value = col_double()) )

# Rice plot graphic
gr_rice_plot <- function(rice_var, gradset){
  rice_pl <- ggplot(data = rice_var,
                    mapping = aes(x = Column,
                                  y = Row,
                                  fill = value)) +
    geom_tile() + scale_fill_gradient(low = gradset[1], high = gradset[2]) +
    ggtitle(deparse(substitute(rice_var)))
  
  return(rice_pl)
}


gr_rice_plot(IR8, c("palegreen", "darkgreen"))
gr_rice_plot(CR5272, c("cyan", "darkblue"))
