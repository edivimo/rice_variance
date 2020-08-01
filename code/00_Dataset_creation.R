# Converting dataset from wide to long format

library(agricolae)
library(readr)
library(tidyr)
data(rice)

cr_rice <- read_csv(file = "data/Uniformidad.csv")

names(cr_rice) <- sapply(X = LETTERS[1:length(names(cr_rice))], FUN = function(x) paste0("C_", x))
names(rice) <- sapply(X = LETTERS[1:length(names(rice))], FUN = function(x) paste0("C_", x))

cr_rice$Row <- sapply(X = LETTERS[1:nrow(cr_rice)], FUN = function(x) paste0("R_", x))
rice$Row <- sapply(X = LETTERS[1:nrow(rice)], FUN = function(x) paste0("R_", x))

cr_rice_long <- pivot_longer(cr_rice, cols = -Row, names_to = "Column")
rice_long <- pivot_longer(rice, cols = -Row, names_to = "Column")

write_csv(cr_rice_long, path = "data/cr_rice_long.csv")
write_csv(rice_long, path = "data/rice_long.csv")

