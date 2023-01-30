#' A ggplot theme
#'
#' A ggplot theme for making publication quality visualizations with options for organization-specific colors and logos.
#' @param type Text indicating the type of plot the theme is being used for.  Supported types are currently 'line', 'bar', 'hist', 'grid', 'scatter' which can be used for plots with `geom_line`, `geom_bar`, `geo_histogram`, `geom_tile`, and `geom_point`.  Other plot types will be added later.
#' @param base_size base font size, given in pts. For saving images, 36 is required. For viewing in RStudio, something much smaller like 36/3 = 12 (the default) is recommended.
#' @param base_family base font family.  The default is `'sans'`, which is available cross-platform by default.
#' @param base_line_size base size for line elements. Default is `base_size*.35/36`, and should typically not be changed.
#' @param base_rect_size base size for rect elements. Default is `base_size*.35/36`, and should typically not be changed.
#' @param facet Indicates whether or not `facet_wrap` or `facet_grid` are being used for this plot.  The default is `facet=FALSE`.
#' @param colors Choose the color palette. The default, colors='default", is reds, blues and grays commonly used in journalism.  'cb14' is a colorblind friendly palette with 14 colors. Choosing 'yourorgname' will use specific reds, blues, and grays from the organizations color palette, if they have been implement. This assumes you have installed the package `orgthemes`
#' @return When used in conjunction with ggplot, it returns a plot formatted using theme_pub.
#' @import plotly
#' @import scales
#' @rawNamespace import(ggplot2, except = 'last_plot')
#' @import tidyverse
#' @import ggrepel
#' @export
#' @exportPattern "^[[:alpha:]]+" ## exports all objects like colors
#' @examples
#' #See `https://github.com/bmacGTPM/pubtheme` for examples.


theme_pub <- function (type='line',
                       base_size = 36/3,
                       base_family = "sans",
                       base_line_size=base_size*.35/36*3,
                       base_rect_size=base_size*.35/36,
                       facet=F,
                       colors = 'default'){


  #base_size <- base_size
  px <- 1/1440*20*base_size/36 ## Number of inches in one pixel. Assumes 72dpi and 20x20 image

  ## default geom settings
  ## Use the 0.35 conversion for points to mm here for geom_text.
  ## Necessary because geom_text and themes define font sizes differently.
  ## save default settings, then update defaults, then return to old settings at the end of the
  update_geom_defaults("point"  , list(size=       7*base_size/36, color=pubtextgray))
  update_geom_defaults("line"   , list(linewidth=  3*base_size/36, color=pubtextgray))
  update_geom_defaults("smooth" , list(linewidth=  3*base_size/36, color=pubtextgray))
  update_geom_defaults("segment", list(linewidth=  4*base_size/36, color=pubtextgray))
  update_geom_defaults("text"   , list(size=     .35*base_size   , color=pubtextgray, family=base_family))
  update_geom_defaults("label_repel"   , list(size=     .35*base_size   , color=pubtextgray, family=base_family))
  update_geom_defaults("bar"    , list(                            color=pubtextgray)) ## does width even work?

  ## this changes the default scale_size range
  ## unfortunately scale_size() still needed to be called.
  ## See https://github.com/bmacGTPM/themesn/issues/12
  #formals(scale_size, envir = environment(scale_size))$range <<- c(6,21)*base_size/36

  ## define colors if needed
  ## do it so that colors defined in orgthemes can be used without changing
  ## any code in pubtheme. 
  
  if(length(colors)==1){
    if(colors == 'default'){pal = default.pal}
    if(colors == 'cb14'   ){pal =      cb.pal}
  }
  if(length(colors)>1){pal=colors}
  #if(colors == 'yale'   ){pal =   pal} # not yet implemented
  #if(colors == 'cmu'    ){pal =   pal} # not yet implemented

  ## redefine default color palettes
  options(ggplot2.continuous.colour = function() scale_colour_gradient(low=publightgray, high=pubblue)) ## use different blue
  options(ggplot2.continuous.fill   = function() scale_fill_gradient(low=publightgray, high=pubblue)) ## use different blue
  options(ggplot2.discrete.colour   = pal)
  options(ggplot2.discrete.fill     = pal)
  #options(ggplot2.continuous.size   = function() scale_size_continuous(limits=c(5, 10))) ## don't think this is an option

  th = theme(line = element_line(colour = pubtextgray,
                                 linewidth = base_line_size,
                                 linetype = 1,
                                 lineend = "butt"),
             rect = element_rect(fill = pubbackgray,
                                 colour = NA,
                                 linewidth = base_rect_size,
                                 linetype = 1),
             text = element_text(family = base_family,
                                 face = "plain",
                                 colour = pubtextgray,
                                 size = base_size,
                                 lineheight = 0.9,
                                 hjust = 0.5, vjust = 0.5, angle = 0,
                                 margin = margin(10,10,10,10,'pt'),
                                 debug = FALSE),
             title = NULL,
             aspect.ratio = NULL,
             axis.title         = NULL,
             axis.title.x       = element_text(margin = margin(t = 30*px, unit='in'), vjust = 1),
             axis.title.x.top   = element_text(margin = margin(b = 30*px, unit='in'), vjust = 0),
             axis.title.x.bottom= NULL,
             axis.title.y       = element_text(margin = margin(r = 30*px, unit='in'), vjust = 1, angle =  90),
             axis.title.y.left  = NULL,
             axis.title.y.right = element_text(margin = margin(l = 30*px, unit='in'), vjust = 0, angle = -90),
             axis.text         = NULL,
             axis.text.x       = element_text(margin = margin(t = 20*px, unit='in'), vjust = 1),
             axis.text.x.top   = element_text(margin = margin(b = 20*px, unit='in'), vjust = 0),
             axis.text.x.bottom= NULL,
             axis.text.y       = element_text(margin = margin(r = 20*px, unit='in'), hjust = 1),
             axis.text.y.left  = NULL,
             axis.text.y.right = element_text(margin = margin(l = 20*px, unit='in'), hjust = 0),
             axis.ticks = element_line(colour = pubtextgray),
             axis.ticks.x        = NULL,
             axis.ticks.x.top    = NULL,
             axis.ticks.x.bottom = NULL,
             axis.ticks.y        = NULL,
             axis.ticks.y.left   = NULL,
             axis.ticks.y.right  = NULL,
             axis.ticks.length          = unit(20*px, "in"),
             axis.ticks.length.x        = NULL,
             axis.ticks.length.x.top    = NULL,
             axis.ticks.length.x.bottom = NULL,
             axis.ticks.length.y        = NULL,
             axis.ticks.length.y.left   = NULL,
             axis.ticks.length.y.right  = NULL,
             axis.line          = element_line(linewidth=base_line_size),
             axis.line.x        = NULL,
             axis.line.x.top    = NULL,
             axis.line.x.bottom = NULL,
             axis.line.y        = NULL,
             axis.line.y.left   = NULL,
             axis.line.y.right  = NULL,
             legend.background = element_rect(),
             legend.margin     = margin(0, 20*px, 0, 0, 'in'),
             legend.spacing    = NULL,
             legend.spacing.x  = unit(20*px, 'in'),
             legend.spacing.y  = NULL,
             legend.key        = element_rect(fill = NA, colour = NA),
             legend.key.size   = unit(30*px, "in"),
             legend.key.height = unit(30*px, "in"), 
             legend.key.width  = unit(36*px, "in"),
             legend.text       = element_text(size = base_size, vjust=0.5, hjust=0.5,
                                              margin = margin(0, 50*px, 0, 0, 'in')),
             legend.text.align = 0.5,
             legend.title      = element_text(size=base_size, vjust=0.5,
                                              margin = margin(0, 20*px, 0, 0, 'in')),
             legend.title.align= NULL,
             legend.position   = "top",
             legend.direction  = NULL,
             legend.justification = "left",
             legend.box            = NULL,
             legend.box.just       = 'left',
             legend.box.margin     = margin(0, 0, 50*px, -140*px, "in"),
             legend.box.background = element_blank(),
             legend.box.spacing    = NULL,
             panel.background = element_rect(fill=pubbackgray, color=NA),
             panel.border     = element_blank(),
             panel.spacing = unit(50*px, "in"),
             panel.spacing.x = NULL,
             panel.spacing.y = NULL,
             panel.grid         = NULL,
             panel.grid.major   = NULL,
             panel.grid.minor   = element_blank(),
             panel.grid.major.x = element_blank(),
             panel.grid.major.y = element_line(colour = publightgray),
             panel.grid.minor.x = NULL,
             panel.grid.minor.y = NULL,
             panel.ontop        = FALSE,
             plot.background = element_rect(),
             plot.title    = element_text(size = 50/36*base_size, hjust = 0, vjust = 1, margin = margin(0,0,b = 70*px, 0, unit='in'), color=pubdarkgray, face='bold'),
             plot.title.position = 'plot',
             plot.subtitle = element_text(size = 42/36*base_size, hjust = 0, vjust = 1, margin = margin(-40*px,0,b = 70*px, 50*px, unit='in'), color=pubmediumgray), ## black is not an option
             plot.caption  = element_text(size = 33/36*base_size, hjust = 0, vjust = 1, margin = margin(50*px, 0, 0, 0, 'in'), color=pubmediumgray),
             plot.caption.position = 'plot',
             plot.tag      = element_text(size =       base_size, hjust = 0.5, vjust = 0.5),
             plot.tag.position = "topleft",
             plot.margin = margin(t = 70*px,
                                  r = 50*px,
                                  b = 50*px,
                                  l = 50*px, unit='in'),
             strip.background   = NULL,
             strip.background.x = NULL,
             strip.background.y = NULL,
             strip.placement    = "outside",
             strip.placement.x  = NULL,
             strip.placement.y  = NULL,
             strip.text = element_text(colour = pubtextgray, size = base_size,
                                       margin = margin(20*px, 20*px, 20*px, 20*px, unit='in')),
             strip.text.x = NULL,
             strip.text.y = element_text(angle = -90),
             strip.switch.pad.grid = unit(base_size/4, "pt"),
             strip.switch.pad.wrap = unit(base_size/4, "pt"),
             complete = TRUE,
             validate = TRUE)

  ## Change the base theme based on the type of plot specified by the user.
  ## Use axis.ticks.length = unit(0, "pt") so  blank ticks don't take up whitespace
  if(type=='scatter'){th = th + theme(panel.grid.major.x = element_line(colour = publightgray))}
  if(type=='pairs'  ){th = th + theme(panel.grid.major.x = element_line(colour = publightgray),
                                      panel.border = element_rect(color=pubtextgray, fill=NA),
                                      strip.background   = element_rect(color=pubtextgray, fill=publightgray))}
  if(type=='line'   ){th = th}
  if(type=='bar'    ){th = th + theme(axis.line  = element_blank(),
                                      axis.ticks = element_blank(),
                                      axis.ticks.length = unit(0, "pt"),
                                      axis.text.x=element_blank(),
                                      panel.grid.major.y = element_blank()) }
  if(type=='pop'    ){th = th + theme(axis.line  = element_blank(),
                                      axis.ticks = element_blank(),
                                      axis.ticks.length.y = unit(0, "pt")
                                      )}
  if(type=='hist'   ){th = th + theme(axis.ticks.x = element_blank(), panel.grid.major.x = element_blank()) }
  if(type=='grid'   ){th = th + theme(axis.ticks = element_blank(),
                                      axis.ticks.length = unit(0, "pt"),
                                      axis.line  = element_blank(),
                                      strip.background = element_blank(),
                                      axis.title.x.top   = element_text(margin = margin(t = -6*px, 
                                                                                        b = 30*px, 
                                                                                        unit='in'), 
                                                                        vjust = 0),
                                      panel.grid.major = element_blank(), 
                                      legend.title     = element_text(size=base_size, vjust=0.5,
                                                                       margin = margin(0, 0, 0, 50*px, 'in')),
                                      legend.key.width = unit(60*px, "in")
                                      )}
  if(type=='timeline'){th = th +theme(axis.ticks.x = element_blank(),
                                      axis.ticks.length.x = unit(0, "pt"),
                                      axis.line  = element_blank(),
                                      panel.grid.major = element_blank(), 
                                      axis.text.x = element_blank())}
  if(type=='map'){th = th + theme(axis.text = element_blank(), 
                                   axis.title = element_blank(), 
                                   axis.line = element_blank(), 
                                   axis.ticks = element_blank(), 
                                   panel.grid = element_blank())}
  if(facet==T       ){th = th + theme(panel.border = element_rect(color=pubtextgray, fill=NA),
                                      strip.background   = element_rect(color=pubtextgray, fill=publightgray))}

  return(th)
}

