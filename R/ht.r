#' ht: quickly show both `head()` and `tail()` for a data.frame
#'
#' A convenience function that shows by default both the `head` and `tail` of a data frame or other objects supported by `head` and `tail`. 
#' @param x An R object, usually a data frame, as in `head`.
#' @param n An integer vector as in `head`. 
#' @param keepnums Same as in `head`.
#' @return When used with a data frame, returns the first `n` and last `n` rows. 
#' @examples
#' ht(mtcars)

ht = function(x, 
              n = 2, 
              keepnums = FALSE, 
              ...){

  headn = head(x, n)
  tailn = tail(x, n)
  
  data = rbind(headn, 
               tailn)
  print(headn)
  cat('  ...\n')
  print(tailn)
  invisible(data)
}