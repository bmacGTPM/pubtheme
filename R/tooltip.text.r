#' Function for creating HTML code for a formatted tooltip
#'
#' A function for generating HTML code to be used to create a formatted tooltip. The tooltip will have one line per column supplied to the function. The first column given will be listed in the first line and will be bolded.
#' @param ... Unquoted names of columns to include in the tooltip.  
#' @return HTML code for a tooltip where the first line is bold. 
#' @import tidyverse
#' @import rlang
#' @export
#' @examples
#' #See `https://github.com/bmacGTPM/pubtheme` for examples.

tooltip.text = function(...){
  uq = ensyms(...) 
  #print(uq)
  
  q = purrr::map(uq, rlang::as_string) %>%
    unlist()
  #print(q)
  
  m = matrix(rep(paste0('<br>', q, ': '), 
                 each = nrow(data.frame(...))), 
             ncol = length(q))
  #print(head(m,2))
  
  temp.df = data.frame(..., m)
  #print(temp.df[1:2,])
  
  cols = matrix(1:(length(q)*2), 
                ncol=length(q), 
                byrow = T) %>% 
    data.frame() 
  cols
  
  cols = cols %>%
    arrange(desc(.data$X1)) %>%
    unlist() %>% 
    as.numeric()
  cols
  
  #print(cols)
  temp.df = temp.df[,cols]
  #print(temp.df[1:2,])
  
  ## is there a column called name? 
  ## If so, make it bold, and remove the "name:"
  name.col = which(colnames(temp.df)=='name')
  #print(name.col)
  if(length(name.col)!=0){
    name.cols = c(name.col-1, name.col)
    new.cols = c(name.cols, 
                 setdiff(1:ncol(temp.df), 
                         name.cols))
    temp.df = temp.df[,new.cols]
  }
  
  temp.df[,1] = gsub('<br>name: ', '<b>', temp.df[,1])
  temp.df[,2] = paste0(temp.df[,2], '</b>')
  #print(temp.df[1:2,])

  #temp.df = temp.df[]
  temp.df2 = apply(temp.df, 2, paste)
  temp.df3 = apply(temp.df2, 1, paste, collapse = '')
  temp.df3

}

# mtcars %>% 
#   rownames_to_column('name') %>%
#   mutate(text = tooltip.text(mpg, cyl, wt, name)) %>% 
#   head(2) %>%
#   print()

