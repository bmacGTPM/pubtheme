#' Prepare plot for publication
#'
#' This function adds theme_pub to a ggplot and changes other non-theme related options like `scale_*` and `coord_*`.
#' @param g A `ggplot` object. 
#' @param type Text indicating the type of plot the theme is being used for.  Supported types are currently 'line', 'bar', 'hist', 'grid', 'scatter' which can be used for plots with `geom_line`, `geom_bar`, `geo_histogram`, `geom_tile`, and `geom_point`.  Other plot types will be added later.
#' @param base_size base font size, given in pts. For saving images, 36 is required. For viewing in RStudio, something much smaller like 36/3 = 12 (the default) is recommended.
#' @param xlim (Optional) A vector of length two, specifying the lower and upper limit of the x-axis
#' @param ylim (Optional) A vector of length two, specifying the lower and upper limit of the y-axis
#' @param xbreaks (Optional) A vector of length $n$, specifying the breaks for the x-axis
#' @param ybreaks (Optional) A vector of length $n$, specifying the breaks for the y-axis
#' @param xlabels (Optional) A vector of length $n$, specifying the labels for the x-axis
#' @param ylabels (Optional) A vector of length $n$, specifying the labels for the y-axis
#' @param int If TRUE, make the plot interactive using `ggplotly` and `layoutpub`. Default is FALSE.
#' @param tooltip If `int=TRUE` then this `tooltip` argument is passed to `ggplotly`. If `int=FALSE`, this is ignored. If left blank, this default is 'all', which is the default for `ggplotly`.
#' @param subtitle (Same as in `layoutpub`.) Is there a subtitle? Default is FALSE. This affects spacing. Ignored if `int=FALSE`.
#' @param legend.rows (Same as in `layoutpub`.) How many rows are needed for the legend? Default is 0. This affects spacing. Ignored if `int=FALSE`.
#' @param caption (Same as in `layoutpub`.) Is there a caption? Default is FALSE. This affects spacing. Ignored if `int=FALSE`.
#' @param ... Other arguments passed to `theme_pub`

pub = function (g, 
                type      = 'scatter',
                base_size = 36/3,
                xlim      = NULL, 
                ylim      = NULL, 
                xbreaks   = NULL,
                ybreaks   = NULL,
                xlabels   = NULL,
                ylabels   = NULL,
                xtrans    = NULL, 
                ytrans    = NULL,
                int       = FALSE, 
                tooltip   = 'all',
                subtitle  = F, 
                caption   = F, 
                legend.rows = 0, 
                ...
                # base_family = "sans",
                # base_line_size=base_size*.35/36*3,
                # base_rect_size=base_size*.35/36,
                # facet=F,
                # colors = 'default'
                ){
 
  #source('R/reverse2_trans.r')
  
  reverse2_trans <- function() {
    trans_new(
      "reverse2",
      function(x) -1 * as.numeric(x), # Force values to be numeric for Date objects
      function(x) -1 * as.numeric(x)
    )
  }
  
  #if(type!='hist'){ ## don't expand x axis for hist
  g$coordinates$expand  = TRUE ## why didn't I say FALSE?
  g$coordinates$clip    = 'off'
  g$coordinates$default = TRUE
  
  if(inherits(layer_scales(g)$x, 'ScaleContinuousPosition')){x.axis.type = 'cont'}
  if(inherits(layer_scales(g)$y, 'ScaleContinuousPosition')){y.axis.type = 'cont'}
  if(inherits(layer_scales(g)$x, 'ScaleContinuousDate'    )){x.axis.type = 'date'}
  if(inherits(layer_scales(g)$y, 'ScaleContinuousDate'    )){y.axis.type = 'date'}
  if(inherits(layer_scales(g)$x, 'ScaleDiscretePosition'  )){x.axis.type = 'dis'}
  if(inherits(layer_scales(g)$y, 'ScaleDiscretePosition'  )){y.axis.type = 'dis'}  
  #if(x.axis.type=='date' & type=='timeline'){x.axis.type='cont'}
  #if(y.axis.type=='date' & type=='timeline'){y.axis.type='cont'}
  
  ## Partly from 
  ## https://coolbutuseless.github.io/2019/04/26/reverse-engineer-the-ggplot-call-from-a-ggplot-object/
  ## I found it to be cool but useFUL
  
  geoms = g$layers %>% 
    lapply(function(x) ggplot2:::snakeize(class(x$geom))[1]) %>% 
    unlist()
  
  text = ifelse(sum(grepl('geom_text', geoms) > 0), TRUE, FALSE)
  text
  
  ## Is there a bar plot with light gray background bars?
  ## If so, there is no need to expand
  bg.bars = ifelse(sum(grepl('geom[_]col|geom[_]bar', 
                             geoms)) > 1, 
                   TRUE, 
                   FALSE)
  bg.bars
  
  ## Based on type, and axis type, choose a bunch of settings.
  expandx = expansion(mult = c(0, 0))
  expandy = expansion(mult = c(0, 0))
  
  if(type=='hist'                    ){expandx = expansion(mult = c(0.05, 0.05))}
  if(type=='bar' & text==T & !bg.bars){expandx = expansion(mult = c(0   , 0.1 ))}
  if(type=='timeline'                ){expandy = expansion(mult = c(0.1 , 0.1 ))}
  if(type=='slope'                   ){expandx = expansion(mult = c(0.5 , 0.5 ))
                                       expandy = expansion(mult = c(0.1 , 0.1 ))}
  
  ## Modify the scales
  ## Determine type of x-axis and y-axis

  ## Can specify xlim and ylim in pub().
  ## But if lims(x=c(), y=c()) is used, can we use that without
  ## generating warnings every time there is a new `scale` used?
  ## Would need to be able to modify breaks, oob, and labels
  ## Otherwise, just specify xlim and ylim
  ## I would probably have to use ggplot_build, at least
  ## and then print using ggplot_gtable and grid.draw
  ## https://ggplot2-book.org/internals.html#sec-plot-method
  ## b = ggplot_build(g)
  ## t = ggplot_gtable(b)
  
  ## check if there were limits supplied
  #if(!is.null(g$scales$scales[[1]]$limits)){xlim=g$scales$scales[[1]]$limits}
  #if(!is.null(g$scales$scales[[2]]$limits)){ylim=g$scales$scales[[2]]$limits}
  #if(!is.null(g$scales$has_scale('x'))){xlim=g$scales$scales[[1]]$limits}
  #if(!is.null(g$scales$scales[[2]]$limits)){ylim=g$scales$scales[[2]]$limits}
  

  ## scales
  #breaks=trans_breaks('identity', identity, 2), ## weird right bound
  if(type %in% c('scatter', 'line', 'hist', 'bar', 
                 'grid', 'pop', 'map', 'slope')){
    
    ### if axis type is cont use scale_x_continuous 
    if(x.axis.type == 'cont'){
    
      scalesx = scale_x_continuous(
        oob    = squish, 
        labels = if(is.null(xlabels)) comma else xlabels, 
        expand = expandx,
        limits = if(is.null(xlim)) NULL else xlim, 
        breaks = 
          if(is.null(xbreaks) & is.null(xlim)) 
            waiver() 
        else if (is.null(xbreaks) & !is.null(xlim)) 
          c(xlim[1],
            mean(xlim), 
            xlim[2])
        else xbreaks
        )
      
    }
    
    
    if(x.axis.type=='dis'){
      scalesx = 
        scale_x_discrete(expand   = expandx, 
                         breaks   = if(is.null(xbreaks)) waiver() else xbreaks, 
                         labels   = if(is.null(xlabels)) waiver() else xlabels, 
                         position = ifelse(type %in% c('grid', 'slope'), 
                                           'top', 
                                           waiver()))
    }
    
    ## Similar to continuous except has scale_x_date
    ## and has slightly different defaults for breaks
    if(x.axis.type=='date'){
      scalesx = scale_x_date(                             
        oob    = squish, 
        labels = if(is.null(xlabels)) waiver() else xlabels, 
        expand = expandx,
        breaks =
          if(is.null(xbreaks) & is.null(xlim)) 
            waiver() 
        else if(is.null(xbreaks) & !is.null(xlim)) 
          c(xlim[1],
            mean(xlim), 
            xlim[2])
        else xbreaks
        )
    }
    
    
    if(y.axis.type=='cont'){
 
        scalesy = scale_y_continuous(
          oob    = squish, 
          labels = if(is.null(ylabels)) comma else ylabels, 
          expand = expandy,
          limits = if(is.null(ylim)) NULL else ylim, 
          breaks = 
            if(is.null(ybreaks) & is.null(ylim)) 
              waiver() 
          else if (is.null(ybreaks) & !is.null(ylim)) 
            c(ylim[1],
              mean(ylim), 
              ylim[2])
          else ybreaks
        ) 
        

      }
    
    if(y.axis.type=='dis'){
      scalesy = 
        scale_y_discrete(expand = c(0,0),
                         breaks = if(is.null(ybreaks)) waiver() else ybreaks, 
                         labels = if(is.null(ylabels)) waiver() else ylabels)
    }

    
    ## Similar to continuous except has scale_y_date
    ## and has slightly different defaults for breaks
    if(y.axis.type=='date'){
      scalesy = scale_y_date(                             
        oob    = squish, 
        labels = if(is.null(ylabels)) waiver() else ylabels, 
        expand = expandy,
        breaks =
          if(is.null(ybreaks) & is.null(ylim)) 
            waiver() 
        else if(is.null(ybreaks) & !is.null(ylim)) 
          c(ylim[1],
            mean(ylim), 
            ylim[2])
        else ybreaks
      )
    }
    
    
    sizes = scale_size(range=c(3,18)*base_size/36)
    linewidths = NULL
    
    if(type == 'pop'){
      sizes      = scale_size(     range = c( 3,  3)*3*base_size/36)
      linewidths = scale_linewidth(range = c(.5, .5)*3*base_size/36)
    }
    
    if(type == 'slope'){
      sizes      = scale_size(     range = c(3.5, 3.5)*3*base_size/36)
      linewidths = scale_linewidth(range = c(1  , 1  )*3*base_size/36)
    }
    # theme.and.scales = list(scalesx, scalesy, coords, sizes)
    
  }
  
  ## Doesn't work because the pairs plot object is not a ggplot object
  ## Would need extra work to implement this. 
  ## But it's probably not necessary because all of the default scales etc 
  ## are typically good/best. 
  # if(type=='pairs'){
  #   ## xlim and ylim won't be specified, so use defaults
  #   scalesx = scale_x_continuous(oob = squish, labels = comma, expand = expandx)
  #   scalesy = scale_y_continuous(oob = squish, labels = comma, expand = expandy)
  #   sizes = scale_size(range=c(1,3)*base_size/36)
  # }
  
  
  if(type=='timeline'){
    scalesx = scale_x_continuous(
      oob    = squish,
      labels = if(is.null(xlabels)) comma else xlabels,
      expand = expandx,
      limits = if(is.null(xlim)) NULL else xlim,
      breaks =
        if(is.null(xbreaks) & is.null(xlim))
          waiver()
      else if (is.null(xbreaks) & !is.null(xlim))
        c(xlim[1],
          mean(xlim),
          xlim[2])
      else xbreaks
    )
    scalesy = scale_y_continuous(
      oob    = squish, 
      trans  = if(is.null(ytrans)) "reverse2" else ytrans,
      breaks = as.numeric(ybreaks),
      labels = format(ybreaks, '%b %d, %Y'),
      expand = expandy)
    sizes = scale_size(range = c(3,18) * base_size/36)
    linewidths = NULL
  }
  
  # # For slope graph, make changes to x-axis
  # if(type == 'slope'){
  #   
  #     scalesx = 
  #       scale_x_discrete(expand   = c(0,0), 
  #                        breaks   = if(is.null(xbreaks)) waiver() else xbreaks, 
  #                        labels   = if(is.null(xlabels)) waiver() else xlabels, 
  #                        position = 
  #                                          'top', 
  #                                          waiver()))
  #   }
  # }
  
  g = g + scalesx + scalesy + sizes + linewidths
  
  ## How much should the left margin of the legend shift to the left?
  ## Shift the legend based on 
  ## max number of characters in y-axis labels and
  ## whether or not the axis title exists. 
  ## This is kinda hacky but works pretty well. 
  ## Might be better to access coordinates in gtable, 
  ## but I had trouble doing that 

  ## Get the labels that will be used for the y axis
  y.labels = ggplot_build(g)$layout$panel_params[[1]]$y$breaks
  y.labels = y.labels[!is.na(y.labels)]
  y.labels
  cols = paste0('lab', 1:length(y.labels))
  arial.widths[,cols] = NA
  
  ## rescale so a zero is 20 pixels wide 
  zero.width = arial.widths$V2[arial.widths$V1 == 0]
  zero.width
  arial.widths = arial.widths %>%
    mutate(V2 = V2/zero.width*20)
  head(arial.widths)
  
  for(j in 1:nrow(arial.widths)){
    n.chars = str_count(y.labels, fixed(arial.widths[j,1]))
    arial.widths[j,cols] = n.chars
  }
  
  widths = arial.widths %>% 
    mutate(across(all_of(cols), ~.x*V2)) %>%
    summarise(across(starts_with('lab'), sum))
    
  max.width = max(widths)
  #max.width %>% print()
  
  y.title = g$labels$y
  title.width = 0
  if(!is.null(y.title)){title.width = 30 + 50} ## 50 is fudging it
  #title.width %>% print()
  
  tick.width = if(type %in% c('bar', 'grid')) -3 else 20
  #print(tick.width)
  #browser()
 
   legend.shift = 
    tick.width + ## axis.ticks
    max.width  + ## axis text
    title.width  ## axis title
 #print(legend.shift)
  ## Add the theme

  g = g + 
    theme_pub(type = type, 
              base_size = base_size, 
              legend.shift = -legend.shift,
              ...
              # base_family = base_family,
              # base_line_size=base_line_size,
              # base_rect_size=base_rect_size,
              # facet=facet,
              # colors = colors
              ) 

  if(type=='bar' & text==T){
    g = g + 
      theme(axis.text.x = element_blank())
  }
    
  g$type = type
  
  if(int == T){
    g = g %>% 
      ggplotly(width   = 1440*base_size/36, 
               height  = 1440*base_size/36, 
               tooltip = tooltip) %>%
      layoutpub(type      = type, 
                base_size = base_size, 
                subtitle  = subtitle, 
                caption   = caption, 
                legend    = ifelse(legend.rows == 0, F, T)) %>%
      style(marker.sizeref = 1.5)
    
    }
  
  return(g)
   
}
