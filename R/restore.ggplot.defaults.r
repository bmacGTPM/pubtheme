#' Restore ggplot defaults
#'
#' A function for restoring ggplot defaults that were changed by the pubtheme package.
#' @import plotly
#' @rawNamespace import(ggplot2, except = 'last_plot')
#' @export
#' @examples
#'
#' ## Load required packages
#' library(plotly) ## loads ggplot2, and we'll need plotly eventually
#' ggplot(mtcars, aes(x=wt, y=mpg, color=as.factor(cyl))) + 
#'     geom_point() ## default discrete colors (red, green, blue)
#'
#' ## pubtheme discrete colors (snred, snblue, snmediumgray)
#' library(pubtheme)
#' ggplot(mtcars, aes(x=wt, y=mpg, color=as.factor(cyl))) + 
#'     geom_point() + 
#'     theme_pub()
#'
#' ## Still pubtheme discrete colors (snred, snblue, snmediumgray), even though theme_pub isn't used
#' ggplot(mtcars, aes(x=wt, y=mpg, color=as.factor(cyl))) + geom_point()
#'
#' ## Restore defaults. Now the original colors are used again.
#' restore.ggplot.defaults()
#' ggplot(mtcars, aes(x=wt, y=mpg, color=as.factor(cyl))) + geom_point()
#' ggplot(mtcars, aes(x=wt, y=mpg, color=          cyl )) + geom_point()

restore.ggplot.defaults <- function(){

  ## color scales
  ## defaults were given here https://ggplot2-book.org/scale-colour.html#robust-recipes
  ## and here https://ggplot2-book.org/scale-colour.html#colour-discrete
  options(ggplot2.continuous.colour = scale_colour_gradient)
  options(ggplot2.continuous.fill   = scale_fill_gradient)
  options(ggplot2.discrete.colour   = scale_colour_hue)
  options(ggplot2.discrete.fill     = scale_fill_hue)

  ## geom defaults
  ## obtained by restarting R and using, for example,
  ## ggplot2:::check_subclass("point", "Geom")$default_aes
  update_geom_defaults("bar"    , list(colour = NA       , alpha = NA  , size=0.5 , linetype = 1, fill = 'gray35'))
  update_geom_defaults("col"    , list(colour = NA       , alpha = NA  , size=0.5 , linetype = 1, fill = 'gray35'))
  update_geom_defaults("line"   , list(colour = 'black'  , alpha = NA  , size=0.5 , linetype = 1))
  update_geom_defaults("segment", list(colour = 'black'  , alpha = NA  , size=0.5 , linetype = 1))
  update_geom_defaults("smooth" , list(colour = "#3366FF", alpha = -0.4, size=1   , linetype = 1, fill = 'gray60', weight = 1))
  update_geom_defaults("point"  , list(colour = 'black'  , alpha = NA  , size=1.5 ,               fill = NA, 
                                       stroke = 0.5, 
                                       shape  = 19))
  
  update_geom_defaults("text"   , list(colour = 'black'  , alpha = NA  , size=3.88, 
                                       angle      = 0, 
                                       hjust      = 0.5, 
                                       vjust      = 0.5, 
                                       family     = '', 
                                       fontface   = 1, 
                                       lineheight = 1.2))
  ## put the original geom_col back
  #rm(geom_col)
  
  
}



