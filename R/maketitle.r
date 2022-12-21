#' A function used by layout.pub for formating a title with optional subtitle. 
#'
#' This function is a way to add a subtitle with lighter gray font color to a figure using layout.pub
#' @param title Text with the desired title in Upper Lower.
#' @param subtitle Text with an optional subtitle in Upper Lower. Default is NULL.
#' @param base_size Font size. Default is 18. Should be set to the font size being used in `layout.pub`.
#' @return Returns an HTML-formatted title (and optional subtitle) for use by layout.pub and plot_ly.
#' @import plotly
#' @export
#' @exportPattern "^[[:alpha:]]+" ## exports all objects like colors
#' @examples
#' #See `https://github.com/bmacGTPM/pubtheme` for examples.
maketitle = function(title, subtitle=NULL, base_size=18){
  paste0('<b>', title, 
         '</b><br><span style=\"font-size: ',
         42*base_size/36, 'px; color:', pubmediumgray,'\">', subtitle, '</span>')
  
}