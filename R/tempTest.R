#' Test R file for package build process
#'
#' @param col The color of points. Must be "red", "green" or "blue".  Default is "red".
#' @return Output the plot
#' @description Test Plot
#' @examples \dontrun{testPlot("blue")}
#' @export
testPlot <- function (col=c('red','green','blue')) {
  col <- match.arg(col)
  mtcars %>% ggvis(~mpg, ~wt, fill := col)
}
