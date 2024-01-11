#' Function for reverse date axes 
#' 
#' Copied from https://github.com/tidyverse/ggplot2/issues/4014
#' Use this with scale_y_continuous below (and use _continuous instead of _date)
#' @export
reverse2_trans <- function() {
  trans_new(
    "reverse2",
    function(x) -1 * as.numeric(x), # Force values to be numeric for Date objects
    function(x) -1 * as.numeric(x)
  )
}