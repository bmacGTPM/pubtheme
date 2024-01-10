#' A density plot used in `pubtheme`
#'
#' A density plot that is a modified version of GGally::ggally_density that has more transparent filled areas under the density lines. 
#' @param data The `data` as in `ggplot`
#' @param mapping The `mapping` as in `ggplot` 
#' @param ... Additional arguments
#' @return When used in conjunction with ggplot, it returns a plot formatted using theme_pub.
#' @import plotly
#' @rawNamespace import(ggplot2, except = 'last_plot')
#' @examples
#' #See the `Pairs Plot` section of `https://github.com/bmacGTPM/pubtheme` for examples.

## This makes the density plots more transparent
pub.density = function (data, mapping, ...){
  p = ggplot(data = data, 
            mapping = mapping) + 
    geom_density(alpha = 0.3, ...)
  p
}
