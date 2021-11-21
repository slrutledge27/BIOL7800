### Functions to put in R package for Data Science Fall 2021 HW_07

### Generate matrix for functions to operate on
set.seed(123)
x = matrix(rpois(100, 0.1), 10, 10)

### Write function to remove all rows in matrix w/ 0's
rm_0s_by_row <- function(x){x[rowSums(x[])>0,]}
rm_0s_by_row(x)

#' to remove all rows in matrix that have 0's in all columns
#'
#' @param x Input vector, must be a matrix/data frame/array.
#' @export


### Write function to remove all columns in matrix w/ 0's
rm_0s_by_col <- function(x){x[,colSums(x)!=0]}
rm_0s_by_col(x)

#' to remove all columns in matrix that have 0's in all rows
#'
#' @param x Input vector, must be a matrix/data frame/array.
#' @export