## quick summary statistics

# base R
summary(iris)

# tidyverse
devtools::install_github("hadley/precis")
library(precis)
precis(iris)
precis(iris, histogram = TRUE)
