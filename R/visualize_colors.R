

#' Visualize Colors
#' @description This function plots supplied colors in a barplot so the user can get a quick sense of what they look like
#'
#' @param color_pal a vector of character corresponding to colors to plot
#'
#' @importFrom graphics barplot
#' @importFrom stats runif
#' @examples
#'
#' visualize_colors(c("dodgerblue", "darkgrey"))
#'
#' \dontrun{
#' # for those who use viridis
#' visualize_colors(viridis::viridis(5))
#' }
#'
#' @return a barplot (graphics version, not ggplot)
#' @export


visualize_colors <- function(color_pal){
  num_colors <- length(color_pal)
  return(graphics::barplot(runif(num_colors, min = 3, max = 10), col = color_pal))
}
