# Converting datasets from wide to long format

# Loading packages
library(agricolae)
library(readr)
library(tidyr)

# These is the original data, IR8 and CR5272.
# Sources are Gomez and Gomez for IR8 and Villalobos for CR5272.

data(rice)
cr_rice <- read_csv(file = "data/Uniformidad.csv")

# Standardizing the names and converting to long format
# names(cr_rice) <- 
#   sapply(X = 1:length(names(cr_rice)), FUN = function(x) paste0("C_", x))
# names(rice) <- sapply(X = 1:length(names(rice)),
#                       FUN = function(x) paste0("C_", x))
# cr_rice$Row <- sapply(X = 1:nrow(cr_rice), FUN = function(x) paste0("R_", x))
# rice$Row <- sapply(X = 1:nrow(rice), FUN = function(x) paste0("R_", x))

names(cr_rice) <- sapply(X = formatC(1:length(names(cr_rice)),
                                     width = 2, format = "d", flag = "0"),
                         FUN = function(x) paste0("C_", x))

names(rice) <- sapply(X = formatC(1:length(names(rice)),
                                  width = 2, format = "d", flag = "0"),
                      FUN = function(x) paste0("C_", x))

cr_rice$Row <- sapply(X = formatC(1:nrow(cr_rice),
                                  width = 2, format = "d", flag = "0"),
                      FUN = function(x) paste0("R_", x))

rice$Row <- sapply(X = formatC(1:nrow(rice),
                               width = 2, format = "d", flag = "0"),
                   FUN = function(x) paste0("R_", x))

cr_rice_long <- pivot_longer(cr_rice, cols = -Row, names_to = "Column")
rice_long <- pivot_longer(rice, cols = -Row, names_to = "Column")

# Saving the files
write_csv(cr_rice_long, path = "data/cr_rice_long.csv")
write_csv(rice_long, path = "data/rice_long.csv")

1:12
formatC(1:14, width = 2, format = "d", flag = "0")

