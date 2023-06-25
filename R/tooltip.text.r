#' @export 
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
    data.frame() %>% 
    arrange(desc(X1)) %>%
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
  .Internal(paste(temp.df, 
                  sep="", 
                  collapse=NULL, 
                  recycle0=FALSE))
  
}

# mtcars %>% 
#   rownames_to_column('name') %>%
#   mutate(text = tooltip.text(mpg, cyl, wt, name)) %>% 
#   head(2) %>%
#   print()

