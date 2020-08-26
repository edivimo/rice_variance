# Supporting functions & graphs

# Loading packages

library(ggplot2)


IR8 <- read.csv(file = "data/rice_long.csv")
CR5272 <- read.csv(file = "data/cr_rice_long.csv")

# Convert to numeric values
con_number <- function(rice_var){
  rice_var$Row <- as.numeric(substr(rice_var$Row, start = 3, stop = 4))
  rice_var$Column <- as.numeric(substr(rice_var$Column, start = 3, stop = 4))
  return(rice_var) 
}

# Table of plot size and shape of the plot

shape_plot <- function(rice_var){
  rice_var_num <- con_number(rice_var)
  rice_col <- length(unique(rice_var_num$Column))
  rice_row <- length(unique(rice_var_num$Row))
  
  tab_var <- expand.grid(Width = 1:floor(rice_row/2),
                         Length=1:floor(rice_col/2))
  tab_var$op_plot <- (rice_col*rice_row)%%(tab_var$Width*tab_var$Length) == 0
  
  
  tab_var2 <- tab_var[tab_var$op_plot,]
  tab_var2$Size <- tab_var2$Width * tab_var2$Length
  tab_var2$Plots_num <- rice_col * rice_row / tab_var2$Size
  
  tab_var2 <- tab_var2[, c('Size', 'Width', 'Length', 'Plots_num')]

  row.names(tab_var2) <- NULL
  
  return(tab_var2)
  
}


# Function to transform the plot to a bigger subplot

subplot_tab <- function(c_width, c_length, rice_var){
  rice_var_num <- con_number(rice_var)
  
  rice_var_num$New_row <- ceiling(rice_var_num$Row/c_width)
  rice_var_num$New_col <- ceiling(rice_var_num$Column/c_length)
  
  
  sum_rice_var_num <- 
    aggregate(rice_var_num$value,
              by = list(rice_var_num$New_row, rice_var_num$New_col),
              FUN = "sum")
  
  return(sum_rice_var_num)
}


# Subplot simulation by each with and length combination

subplot_sim <- function(c_width, c_length, rice_var){
  rice_var_num <- con_number(rice_var)
  
  sum_rice_var_num <- subplot_tab(c_width, c_length, rice_var)
  
  Var_x <- var(sum_rice_var_num$x)
  
  Var_x_unit <- Var_x/(c_width*c_length)^2
  
  resdf <- data.frame(Var_x = Var_x, 
                      Var_x_unit = Var_x_unit,
                      CV = sqrt(Var_x_unit)/mean(rice_var_num$value)*100)
  
  return(resdf)
}

# Uniformity table

raw_unif_table <- function(rice_var){
  shape_df <- shape_plot(rice_var)
  
  var_list <- apply(shape_df, MARGIN = 1,
                    function(x) {subplot_sim(x[2], x[3], rice_var)}) 
    var_table <- as.data.frame(t(sapply(var_list, FUN = cbind)))
  
  res_table <- cbind(shape_df, var_table)
  
  return(res_table)
}


# Makes an F-test or a Bartlett-test and returns
# the proper variance
subplot_test <- function(size_plot, rice_var, raw_unif){
  rice_var_num <- con_number(rice_var)
  filt_raw <- raw_unif[raw_unif$Size == size_plot,]
  
  shape_list <- lapply(1:nrow(filt_raw), function(dim_row){
    res_shape <- subplot_tab(filt_raw$Width[dim_row],
                             filt_raw$Length[dim_row],
                             rice_var)
    res_shape$Shape <- paste0(filt_raw$Width[dim_row],
                              "x",
                              filt_raw$Length[dim_row])
    return(res_shape)
  }  )
  
  shape_red <- Reduce(rbind, shape_list)
  
  # Test section
  if(length(unique(shape_red$Shape)) > 2) {
    res_test <- bartlett.test(x ~ Shape, data = shape_red)
  } else {
    res_test <- var.test(x ~ Shape, data = shape_red)
  }
  
  # Selection of the proper value of variance

  if(res_test$p.value >= 0.05) {
    # res_df <- filt_raw
    res_df <- filt_raw[1,]
    res_df$Var_x[[1]] <- mean(unlist(filt_raw$Var_x))
    res_df$Var_x_unit[[1]] <- mean(unlist(filt_raw$Var_x_unit))
    res_df$CV[[1]] <- mean(unlist(filt_raw$CV))
      
  } else {
    plain_Var_X <- unlist(filt_raw$Var_x)
    res_df <- filt_raw[plain_Var_X == min(plain_Var_X),]
  }
  
  return(res_df)
}


# Reducing Raw uniformity calculations to low variance shapes

unif_sel <- function(rice_var){
  
  raw_unif <- raw_unif_table(rice_var)
  vec_size <- unique(raw_unif$Size)
  sel_unif <- lapply(vec_size, 
                     function(size_shape) {
                       if(sum(raw_unif$Size == size_shape) == 1) {
                         raw_unif[raw_unif$Size == size_shape,]
                       } else {
                         subplot_test(size_shape, rice_var, raw_unif) 
                       }
                     })
  sel_df <- Reduce(rbind, sel_unif)
  
  return(sel_df)
}


# Rice plot graphic

gr_rice_plot <- function(rice_var, gradset){
  
  rice_var2 <- con_number(rice_var)
  
  
  rice_pl <- ggplot(data = rice_var2,
                    mapping = aes(x = Column,
                                  y = Row,
                                  fill = value)) +
    geom_raster() + 
    scale_y_reverse() +
    scale_fill_gradient(name = expression((g/m^2)), 
                        low = gradset[1], high = gradset[2]) +
    ggtitle(paste("Grain yield: ", deparse(substitute(rice_var))))
  
  
  return(rice_pl)
}

gr_rice_plot(IR8, c("LightGreen", "DarkGreen"))
gr_rice_plot(CR5272, c("LightGreen", "DarkGreen"))


# Create a scatterplot with an lm smooth

gr_lm <- function(unif_data){
  
  # Modeling Part
  unif_data$Var_x_unit <- unlist(unif_data$Var_x_unit)
  mod_smith <- lm(log(Var_x_unit) ~ log(Size), data = unif_data)
  pred_vec <- predict(mod_smith, interval = "prediction")
  unif_data <- cbind(unif_data, exp(pred_vec))
  
  # Equation labeling
  # label_eq <- 
  #   paste("V(x) = ",
  #         expression(exp(mod_smith$coefficients[1])/
  #                      X^(exp(mod_smith$coefficients[2])) ) )
  
  label_eq <-
    substitute(V[(x)] == over(b, x^m),
               list(b = unname(round(exp(coef(mod_smith)[1]), digits = 2)),
                    m = unname(round(exp(coef(mod_smith)[2]), digits = 2))
                    ))
  
  # Graphing part
  var_base <- ggplot(unif_data, 
                      mapping = aes(x = Size, y = Var_x_unit))
  
  var_final <- var_base + geom_point() +
    geom_ribbon(mapping = aes(ymin = lwr, ymax = upr),
                fill = "darkgreen", alpha = 0.2) +
    geom_line(mapping = aes(y = fit)) +
    ylab("Var(x)") +
    # geom_label(mapping = aes(x = max(Size)*(3/4),
    #                          y = max(Var_x_unit)*(7/8),
    #                          label = as.character(as.expression(label_eq))))
    annotate("text",
             x = max(unif_data$Size)*(3/4),
             y = max(unif_data$Var_x_unit)*(9/10),
             label = as.character(as.expression(label_eq)), parse = TRUE)
  
  return(var_final)
  # return(label_eq)
}

gr_lm(unif_sel(IR8)) + ggtitle("IR8")
gr_lm(unif_sel(CR5272))


