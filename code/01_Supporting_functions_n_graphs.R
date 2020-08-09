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

# Convert to numeric values
con_number <- function(rice_var){
  rice_var$Row <- as.numeric(substr(rice_var$Row, start = 3, stop = 4))
  rice_var$Column <- as.numeric(substr(rice_var$Column, start = 3, stop = 4))
  return(rice_var) 
}

# Rice plot graphic
gr_rice_plot <- function(rice_var, gradset){
  rice_var2 <- rice_var
  
  rice_var2$Row <- factor(rice_var$Row,
                      levels = sort(unique(rice_var$Row), decreasing = TRUE))
  
  rice_var2$Column <- factor(rice_var$Column,
                            levels = sort(unique(rice_var$Column)))
  
  rice_pl <- ggplot(data = rice_var2,
                    mapping = aes(x = Column,
                                  y = Row,
                                  fill = value)) +
    geom_tile() + scale_fill_gradient(low = gradset[1], high = gradset[2]) +
    ggtitle(deparse(substitute(rice_var)))
  
  return(rice_pl)
}

# Table of plot size and shape of the plot
shape_plot <- function(rice_var){
  rice_var_num <- con_number(rice_var)
  rice_col <- length(unique(rice_var_num$Column))
  rice_row <- length(unique(rice_var_num$Row))
  
  tab_var <- expand.grid(Width = 1:(rice_row/2), Length=1:(rice_col/2))
  tab_var$op_plot <- ifelse(((rice_row %% tab_var$Width) ==  0) &
                              ((rice_col %% tab_var$Length) ==  0), TRUE, FALSE)
  
  tab_var2 <- tab_var[tab_var$op_plot,]
  tab_var2$Size <- tab_var2$Width * tab_var2$Length
  tab_var2$Plots_num <- rice_col * rice_row / tab_var2$Size
  
  tab_var2 <- tab_var2[, c('Size', 'Width', 'Length', 'Plots_num')]

  row.names(tab_var2) <- NULL
  
  return(tab_var2)
  
}

shape_plot(CR5272)


subplot_tab <- function(c_width, c_length, rice_var){
  rice_var_num <- con_number(rice_var)
  
  rice_var_num$New_row <- (rice_var_num$Row %/% c_width) + (rice_var_num$Row %% c_width)
  rice_var_num$New_col <- (rice_var_num$Column %/% c_length) + (rice_var_num$Column %% c_length)
  
  
  # sum_rice_var_num <- aggregate(rice_var_num$value, 
  #                               by = list(rice_var_num$New_row, rice_var_num$New_col),
  #                               FUN = "sum")
  
  return(rice_var_num)
}

View(subplot_tab(3,2, IR8))


# Subplot simulation by each with and length combination

subplot_sim <- function(c_width, c_length, rice_var){
  rice_var_num <- con_number(rice_var)
  
  rice_var_num$New_row <- (rice_var_num$Row %/% c_width) + (rice_var_num$Row %% c_width)
  rice_var_num$New_col <- (rice_var_num$Column %/% c_length) + (rice_var_num$Column %% c_length)
  
  # mean_rice_var_num <- aggregate(rice_var_num[, c("New_row", "New_col", "value")], 
  #                                by = list(rice_var_num$New_row, rice_var_num$New_col),
  #                                FUN = "mean")
  sum_rice_var_num <- aggregate(rice_var_num[, c("New_row", "New_col", "value")], 
                                 by = list(rice_var_num$New_row, rice_var_num$New_col),
                                 FUN = "sum")
  
  Var_x <- var(sum_rice_var_num$value)
  Var_x_unit <- Var_x/(c_width*c_length)^2
  
  resdf <- data.frame(Var_x = Var_x, 
                      Var_x_unit = Var_x_unit,
                      CV = sqrt(Var_x_unit)/mean(rice_var_num$value)*100)
  
  return(resdf)
}

subplot_sim(3,2, IR8)






# Uniformity table

raw_unif_table <- function(rice_var){
  shape_df <- shape_plot(rice_var)
  
  var_list <- apply(shape_df, MARGIN = 1,
                     FUN = function(x) {subplot_sim(x[2], x[3], rice_var)}) 
    var_table <- as.data.frame(t(sapply(var_list, FUN = cbind)))
  
  res_table <- cbind(shape_df, var_table)
  
  return(res_table)
}

raw_unif_table(CR5272)


# subplot_sim
subplot_test <- function(size_plot, rice_var, raw_unif){
  rice_var_num <- con_number(rice_var)
  filt_raw <- raw_unif[raw_unif$Size == size_plot, c('Width', 'Length')]
  
  
  return(filt_raw)
}

subplot_test(6, IR8, raw_unif_table(IR8))




unif_sel(raw_unif_table(IR8), 6)

unif_sel <- function(raw_unif, size_plot){
  
  filt_raw <- raw_unif[raw_unif$Size == size_plot,]
  
  if(nrow(filt_raw) == 1){
    sel_line <- filt_raw
  }
  
  if(nrow(filt_raw) == 2){
    sel_line <- filt_raw
  } else {
    sel_line <- filt_raw  
  }
  
  return(sel_line)
}


unif_sel(raw_unif_table(IR8), 6)
unif_sel(raw_unif_table(IR8), 2)
unif_sel(raw_unif_table(IR8), 1)
