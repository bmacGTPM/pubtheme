#' A plotly theme that matches the ggplot theme theme_pub
#'
#' A plotly theme for making publication quality interactive visualizations with options for organization-specific colors and logos.
#' @param p A plotly object created using plot_ly. Future versions will allow plotly objects created using `ggplotly`.
#' @param type Text indicating the type of plot the theme is being used for.  Supported types are currently 'line', 'bar', 'hist', 'grid', 'scatter'. Other plot types will be added later.
#' @param base_size base font size, given in pts. For viewing in RStudio, something like 18 (the default) or 12 is recommended.
#' @param subtitle Is there a subtitle? Default is FALSE. This affects spacing.
#' @param legend.rows How many rows are needed for the legend? Default is 0. This affects spacing. 
#' @param caption Is there a caption? Default is FALSE. This affects spacing.
#' @param facet Not implemented yet. Indicates whether or not `facet_wrap` or `facet_grid` are being used for this plot.  The default is `facet=FALSE`.
#' @return When used in conjunction with plot_ly, it returns a plot formatted using layout.pub.
#' @import plotly
#' @import scales
#' @export
#' @exportPattern "^[[:alpha:]]+" ## exports all objects like colors
#' @examples
#' #See `https://github.com/bmacGTPM/pubtheme` for examples.
#' 
layoutpub = function(p, 
                     type        = 'scatter', 
                     base_size   = 12, 
                     subtitle    = F, 
                     caption     = F, 
                     legend.rows = 0, 
                     facet       = F){
  
  scale       = base_size/36
  subtitle.pt = 0
  caption.pt  = 0
  legend.pt   = 0
  
  if(subtitle    == T){subtitle.pt = 46}
  if(caption     == T){ caption.pt = 36 + 20} ## text plus 20 in spacing
  if(legend.rows != 0){  legend.pt = 36*legend.rows + 20*legend.rows}
  
  custom.layout = 
    layout(p,
           autosize = T,
           font = list(family = "Arial",
                       size   = base_size,
                       color  = pubtextgray),
           title = list(font = list(family = "Arial", 
                                    size   = 50*scale, 
                                    color  = pubdarkgray),
                        pad = list(t = (70+20)*scale, 
                                   b = (70+20)*scale,
                                   l = 50*scale, 
                                   r = 50*scale),
                        x       = 0, 
                        xanchor = 'left',
                        xref    = 'container',
                        y       = 1, 
                        yanchor = 'top', 
                        yref    = 'container'),
           ## margin means plotting area, seems not to include titles, etc
           ## need to put padding above/below titles, etc
           margin = list(t = 70*scale + 
                           50*48/36*scale + ## title (50 font converted to pixels)
                           subtitle.pt*48/36*scale + ## subtitle (46 font)
                           legend.pt*scale +
                           40*scale, ## bonus
                         r = 50*scale, 
                         b = 50*scale +    
                           ## below title
                           36*48/36*scale +  ## title (36 point converted to pixels)
                           20      *scale +  ## above title
                           36*48/36*scale +  ## axis text (36 font converted to pixels)
                           20      *scale +  ## above axis text
                           caption.pt*48/36*scale +
                           0*scale,          ## a little more
                         
                         l = 50    *scale +
                           36*48/36*scale +  ## title (36 point converted to pixels)
                           20      *scale +  ## above title
                           36*48/36*scale +  ## axis text (36 font converted to pixels)
                           20      *scale,   ## above axis text 
                         
                         pad = 0*scale),       ## don't want space between plot and ticks
           
           xaxis = list(title = list(standoff = 30*scale, 
                                     font     = list(size   = 36*scale, 
                                                     family = 'Arial', 
                                                     color  = pubtextgray)), 
                        tickfont = list(size   = 36*scale, 
                                        family = 'Arial', 
                                        color  = pubtextgray),
                        ticks  = 'outside', ## or inside, or remove for no ticks
                        nticks = 3, 
                        tick0  = 0, 
                        linecolor = pubtextgray,
                        tickcolor = pubtextgray,
                        gridcolor = publightgray, 
                        linewidth = 2*scale,
                        tickwidth = 2*scale,
                        gridwidth = 2*scale,
                        zeroline  = F, ## line at zero
                        showline  = T, ## axis line
                        mirror    = T, ## show on top too
                        ticklabelstep = 1, ## show tick label every n ticks
                        ticklen = 20*scale, 
                        layer   = 'below traces'),
           
           yaxis = list(title = list(standoff = 20*scale, 
                                     font     = list(size   = 36*scale, 
                                                     family = 'Arial', 
                                                     color  = pubtextgray)),
                        tickfont = list(size   = 36*scale, 
                                        family = 'Arial', 
                                        color  = pubtextgray),
                        ticks = 'outside', 
                        #ticks = 3, 
                        tick0 = 0, 
                        linecolor = pubtextgray,
                        tickcolor = pubtextgray,
                        gridcolor = publightgray, 
                        linewidth = 2*scale,
                        tickwidth = 2*scale,
                        gridwidth = 2*scale,
                        zeroline  = F,
                        showline  = T,
                        mirror    = T, # show on right side too
                        ticklabelstep = 1, ## show tick label every n ticks
                        ticklen   = 20*scale, 
                        layer     = 'below traces'), 
           
           legend = list(font = list(family = "Arial",
                                     size   = base_size,
                                     color  = pubtextgray),
                         title = list(font = list(family = "Arial",
                                                  size   = base_size,
                                                  color  = pubtextgray)),
                         grouptitlefont = list(family = "Arial",
                                               color  = pubtextgray),
                         orientation = 'h', 
                         itemclick   = 'toggleothers', 
                         itemdoubleclick = 'toggle',
                         itemsizing = 'constant',
                         itemwidth = 10*scale,
                         xref    = 'paper',
                         x       = -0.105, #-0.117, 
                         xanchor = 'left', ## -0.018 with title
                         y       = 1.03, 
                         yanchor = 'bottom', 
                         bgcolor = pubbackgray, 
                         tracegroupgap = 20*scale, 
                         valign = 'top'),
  
           hoverlabel = list(bgcolor = pubbackgray#, 
                             #font=list(size=36*scale, color=pubtextgray),
                             #bordercolor = 'transparent'
                             ),
           #hovermode = "x unified",
           #dragmode = 'select', 
           #selectdirection = 'h', ## for selecting an x range
           paper_bgcolor = pubbackgray,
           plot_bgcolor  = pubbackgray, 
           colorway      = default.pal, ## not tested
           colorscale    = c(publightgray, pubblue) ## doesn't seem to work
           )
  
  if(type == 'bar'){
    th = th + theme(axis.line          = element_blank(),
                    axis.ticks         = element_blank(),
                    axis.ticks.length  = unit(0, "pt"),
                    #axis.text.x=element_blank(),
                    panel.grid.major.y = element_blank())
  }
  
  if(type == 'pop'){
    th = th + theme(axis.line           = element_blank(),
                    axis.ticks          = element_blank(),
                    axis.ticks.length.y = unit(0, "pt"))
  }
  
  if(type == 'hist'){
    th = th + theme(axis.ticks.x       = element_blank(),
                    panel.grid.major.x = element_blank()) 
  }
  
  if(type == 'grid'){
    custom.layout = custom.layout %>% 
      layout(xaxis = list(ticks     = '', 
                          linewidth = 0, 
                          gridwidth = 0,
                          side      = 'top', 
                          autotypenumbers = 'category'),
             yaxis = list(ticks     = '', 
                          linewidth = 0, 
                          gridwidth = 0,
                          side      = 'left'))
      
    # th = th + theme(axis.ticks        = element_blank(),
    #                 axis.ticks.length = unit(0, "pt"),
    #                 axis.line         = element_blank(),
    #                 strip.background  = element_blank(),
    #                 axis.title.x.top  = element_text(
    #                   margin = margin(t = -6*px, 
    #                                   b = 30*px, 
    #                                   unit = 'in'), 
    #                   vjust = 0),
    #                 panel.grid.major = element_blank(), 

    #)
  }
  
  if(type == 'timeline'){
    th = th + theme(axis.ticks.x        = element_blank(),
                    axis.ticks.length.x = unit(0, "pt"),
                    axis.line           = element_blank(),
                    panel.grid.major    = element_blank(), 
                    axis.text.x         = element_blank())
  }
  
  if(type == 'map'){
    th = th + theme(axis.text  = element_blank(),
                    axis.title = element_blank(), 
                    axis.line  = element_blank(), 
                    axis.ticks = element_blank(), 
                    panel.grid = element_blank())
  }
  if(facet == T){
    th = th + theme(panel.border = element_rect(color = pubtextgray, 
                                                fill  = NA),
                    strip.background = element_rect(color = pubtextgray, 
                                                    fill  = publightgray))
  }
  ## changes based on type
  ##### not implemented yet #####
  
  ## changes when using faceting
  ##### not implemented yet #####
  
  ## changes when linking plots
  ##### not implemented yet #####
  #%>% highlight(on = "plotly_click", off = "plotly_doubleclick")
  
  return(custom.layout)
}




         