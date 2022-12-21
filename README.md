
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pubtheme

<!-- badges: start -->
<!-- badges: end -->

The package `pubtheme` contains a `ggplot` theme `theme_pub` for
creating data journalism-style data visualizations with color palettes
and formatting similar to those used by media organizations like BBC, NY
Times, and ESPN. Several templates for scatter plot, line plots, etc.,
are provided below for easy copying/pasting.

Organization-specific color palettes and logos can be used as well via
the package `orgthemes`. See <https://github.com/bmacGTPM/orgthemes>.

## Installation

If you don’t have the package `devtools`, install it using
`install.packages('devtools')`. If you have `devtools`, you can install
the GitHub version of `pubtheme` with:

``` r
devtools::install_github("bmacGTPM/pubtheme")
```

If you get an error about download method, try changing this option
before installing.

``` r
options(download.file.method = 'libcurl')
```

Load the package using

``` r
library(pubtheme)
```

as usual. The theme will change some of your ggplot defaults the first
time you use it. To change them back, restart R, or use

``` r
restore.ggplot.defaults()
```

at any time. It is recommended that you update your version of
`tidyverse` and especially `ggplot2` to use this package.

## Scatter plot

``` r
dg = mtcars %>% 
  select(wt, mpg, cyl)

title = "Title in Upper Lower" 
g = ggplot(dg, aes(x=wt, y=mpg, color=as.factor(cyl)))+
  geom_point(aes(size=mpg))+
  labs(title    = title,
       subtitle = 'Optional Subtitle In Upper Lower',
       caption  = "Optional caption, giving additional info or Twitter handle",
       x = 'Horizontal Axis Label in Upper Lower',
       y = 'Vertical Axis Label in Upper Lower')+
  scale_x_continuous(limits=c(0, 6), breaks=c(0, 3, 6), oob=squish, labels=comma)+
  scale_y_continuous(limits=c(0,40), breaks=c(0,20,40), oob=squish, labels=comma)+
  coord_cartesian(clip='off', expand=FALSE)+
  scale_size(range=c(2,6))+
  theme_pub(type='scatter', base_size = 36/3) 
print(g)

## Save to a file
gg = g +
  scale_size(range=c(6,18)) +
  theme_pub(type='scatter', base_size=36)
  

ggsave(filename=paste0("img/", gsub("%", " Perc", title), ".jpg"), 
      plot=gg,
      width=20,   
      height=20,  
      units='in', 
      dpi=72)  

g.scatter=g ## save for later
```

<img src="man/figures/README-unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

You must have a subfolder called `img` in order for the `ggsave` chunk
above to work.

Note that we used `base_size=12` when viewing the image in RStudio. Use
`ggsave` and `base_size=36` when saving an image instead of exporting
from the RStudio viewer. Do not adjust the `width=20` in ggsave.

Do not change `width=20`, `units='in'`, or `dpi=72`. Height can be
adjusted if desired. A square image is often preferred, so when in
doubt, keep height at 20.

Upper Lower means First letter of each word is capitalized. The option
`expand=FALSE` removes the default padding. The option `breaks=c(0,3,6)`
sets 3 lines at left, middle, and right. You can certainly add lines if
there is a reason to, but when in doubt you can stick with just 3 lines
(left/middle/right) only. Similarly, for the y-axis, top/middle/bottom
only.

You’ll notice a `scale_size(range=c(6,18)` when preparing the plot to be
saved. If you are using `size` inside `aes()`, you’ll need that change
the scale, otherwise the points will be too small.

## Pairs plot

``` r
library(GGally) ## Needed for ggpairs function
dg = mtcars %>%
  select(mpg, cyl, wt, carb) %>%
  mutate(cyl=factor(cyl))

title = 'Title in Upper Lower'
g = ggpairs(dg, 
            columns = c('mpg', 'wt', 'carb'),
            aes(color = cyl, 
                fill  = cyl), 
            diag = list(continuous = pub.density)) +
  labs(title    = title,
       subtitle = 'Optional Subtitle In Upper Lower',
       caption  = "Optional caption, giving additional info or Twitter handle",
       x = 'Horizontal Axis Label in Upper Lower',
       y = 'Vertical Axis Label in Upper Lower')+
  coord_cartesian(clip='off', expand=FALSE)+
  theme_pub(type='pairs', base_size=36/3)
print(g)

## Save to a file
gg = g +
  theme_pub(type='pairs', base_size=36)

ggsave(filename=paste0("img/", gsub("%", " Perc", title), ".jpg"), 
      plot=gg,
      width=20,   
      height=20,  
      units='in', 
      dpi=72)  
```

<img src="man/figures/README-unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

## Line plot

Since `name` and `value` will be more common column names when using
these in the wild, we’ll rename some columns to be name and value, to
simplify copying and pasting later.

Continuous variables for `x` seem to be more common, so we’ll convert
date to days for this example, so that copying and pasting later becomes
easier. We’ll use `toupper()` to avoid using variables with all
lowercase letters in the legend.

``` r
dg = economics_long %>%
  mutate(name = toupper(variable), 
         days = as.numeric(date - min(date)), 
         value= value01) %>%
  select(days, name, value)
head(dg)
#> # A tibble: 6 × 3
#>    days name     value
#>   <dbl> <chr>    <dbl>
#> 1     0 PCE   0       
#> 2    31 PCE   0.000265
#> 3    62 PCE   0.000762
#> 4    92 PCE   0.000471
#> 5   123 PCE   0.000916
#> 6   153 PCE   0.00157

title = "Title in Upper Lower" 
g = ggplot(dg, aes(x=days, y=value, color=name))+
  geom_line()+
  labs(title    = title,
       subtitle = 'Optional Subtitle In Upper Lower',
       caption  = "Optional caption, giving additional info or Twitter handle",
       x = 'Horizontal Axis Label in Upper Lower', 
       y = 'Vertical Axis Label in Upper Lower')+  
  scale_x_continuous(labels=comma) + 
  scale_y_continuous(labels=comma, limits=c(0,1), breaks=c(0, .5, 1))+ 
  coord_cartesian(clip='off', expand=FALSE)+
  theme_pub(type='line', base_size=36/3) 
print(g)

## Save to a file
gg = g + 
  theme_pub(type='line', base_size=36)

ggsave(filename=paste0("img/", gsub("%", " Perc", title), ".jpg"), ## must have a subfolder called 'img'
       plot=gg,
       width=20,   
       height=20,  
       units='in', 
       dpi=72)     
```

<img src="man/figures/README-unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

Note that once again we set breaks for the y-axis at the top, middle,
and bottom.

## Histogram

``` r
dg = economics %>%
  filter(date<='2008-01-01') %>%
  rename(value=unemploy)

title = "Title in Upper Lower" 
g  = ggplot(dg, aes(x=value))+
  geom_histogram(fill=pubred, color=pubbackgray, binwidth=500) + ## set a reasonable binwidth
  labs(title    = title,
       subtitle = 'Optional Subtitle In Upper Lower',
       caption  = "Optional caption, giving additional info or Twitter handle",
       x = 'Horizontal Axis Label in Upper Lower', ## Required.
       y = 'Count')+  ## Often don't need to change.
  scale_x_continuous(labels=comma)+
  scale_y_continuous(labels=comma, expand = c(0,0))+
  theme_pub(type='hist', base_size=36/3) 
print(g)

gg = g + 
  theme_pub(type='hist', base_size=36)

ggsave(filename=paste0("img/", gsub("%", " Perc", title), ".jpg"), 
       plot = gg,
       width=20,   ## do not change
       height=20,  
       units='in', ## do not change
       dpi=72)     ## do not change
```

<img src="man/figures/README-unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

Here `binwidth` will almost surely need to be changed for your data.

## Bar plot

We’ll use the `mtcars` data again, with some modifications. For
cylinder, we’ll add `-cylinder` to the numbers so it looks nice. We’ll
then convert it a factor so that we can specify the order. We’ll also
create a column called `max` which controls the length of the lightgray
bars in the background. Finally, we’ll rename `cyl` and `mpg` to `name`
and `value`, which will be convenient when copying and pasting this
code.

``` r
dg = mtcars %>%
  group_by(cyl) %>%
  summarise(mpg = mean(mpg)) %>%
  mutate(cyl = paste0(cyl, '-cylinder'),
         cyl = factor(cyl, levels=c('8-cylinder', '6-cylinder', '4-cylinder')), 
         max = 35) %>% ## for background cars
  rename(name  = cyl, 
         value = mpg) 

title = "Title in Upper Lower" 
g = ggplot(dg, aes(x=value, y=name))+
  geom_bar(stat='identity', aes(x=max), color=NA, fill=publightgray, width=0.8)+ ## optional background bars. 
  geom_bar(stat='identity', fill=pubred, color=NA, width=0.8)+ 
  geom_text(aes(label=round(value,2)), hjust=-0.1)+ ## optional numbers with reasonable number of digits
  labs(title    = title,
       subtitle = 'Optional Subtitle In Upper Lower',
       caption  = "Optional caption, giving additional info or Twitter handle",
       x = 'Horizontal Axis Label in Upper Lower', ## Optional. 
       y = NULL)+  ## Optional. Upper Lower.
  scale_x_continuous(limits=c(0,35))+ 
  theme_pub(type='bar', base_size=36/3) 
print(g)

gg = g + 
  theme_pub(type='bar', base_size=36)

ggsave(filename=paste0("img/", gsub("%", " Perc", title), ".jpg"), ## must have a subfolder called 'img'
       plot = gg,
       width=20,   ## do not change
       height=15,  
       units='in', ## do not change
       dpi=72)     ## do not change
```

<img src="man/figures/README-unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

If you are using digits next to the bars, you can increase the `max` so
the text fits.

You may want to remove `x` and `y` axis titles. If not used, use
`x=NULL` and/or `y=NULL` above. Do not use `x=''` and/or `y=''`. If you
do use axes titles, they should be in Upper Lower.

## Grid plot

``` r
dg = airquality %>%
  mutate(Month= month.abb[Month],
         Month = factor(Month, levels=rev(month.abb))) %>% 
  rename(name  = Day,
         name2 = Month,
         value = Temp)

title = "Title in Upper Lower" 
g = ggplot(dg, aes(x=name, y=name2, fill=value))+
  geom_tile(linewidth=0.4, show.legend = F) +
  scale_fill_gradient(low = pubbackgray,
                      high = pubred,
                      na.value = 'white',
                      oob=squish) +
  labs(title    = title,
       subtitle = 'Optional Subtitle In Upper Lower',
       caption  = "Optional caption, giving additional info or Twitter handle",
       x = 'Day (Optional Axis Label in Upper Lower)', 
       y = NULL)+  ## Optional. 
  geom_vline(xintercept=1:(length(unique(dg$name ))+1)-.5, color=pubdarkgray, linewidth=0.2)+ # vert  lines between each square
  geom_hline(yintercept=1:(length(unique(dg$name2))+1)-.5, color=pubdarkgray, linewidth=0.2)+ # horiz lines 
  scale_x_continuous(expand = c(0, 0), position='top', breaks=seq(2,30,by=2))+
  scale_y_discrete(  expand = c(0, 0)) +
  theme_pub(type='grid', base_size=36/3) 

print(g)

gg = g + 
  theme_pub(type='grid', base_size=36)

ggsave(filename=paste0("img/", gsub("%", " Perc", title), ".jpg"), 
       plot=gg,
       width=20,   ## do not change
       height=10,  
       units='in', ## do not change
       dpi=72)     ## do not change
```

<img src="man/figures/README-unnamed-chunk-9-1.png" style="display: block; margin: auto;" />

## Lollipop plot

We’ll make a horizontal lollipop plot. It can be used an alternative to
a bar plot, and it is often preferred, especially when the visualization
will eventually be printed since the lollipop plot used far less
ink/toner.

``` r
dg = airquality %>%
  mutate(Month= month.abb[Month],
         Month = factor(Month, levels=rev(month.abb))) %>% 
  group_by(Month) %>%
  summarise(Temp=mean(Temp)) %>%
  rename(name  = Month,
         value = Temp)


title = "Title in Upper Lower" 
g = ggplot(dg, aes(x=value, y=name))+
  geom_point(color=pubred)+ 
  geom_segment(aes(x=0, xend=value, y=name, yend=name), color=pubred)+
  geom_text(aes(label=round(value,2)), hjust=-0.3)+ ## optional numbers with reasonable number of digits
  labs(title    = title,
       subtitle = 'Optional Subtitle In Upper Lower',
       caption  = "Optional caption, giving additional info or Twitter handle",
       x = 'Horizontal Axis Label in Upper Lower', ## Optional. 
       y = NULL)+  ## Optional. Upper Lower.
  scale_x_continuous(limits=c(0,120), expand=c(0,0))+
  coord_cartesian(clip='off', expand=FALSE)+
  theme_pub(type='pop', base_size=36/3) 
print(g)

gg = g + 
  theme_pub(type='pop', base_size=36)

ggsave(filename=paste0("img/", gsub("%", " Perc", title), ".jpg"), ## must have a subfolder called 'img'
       plot = gg,
       width=20,   ## do not change
       height=15,  
       units='in', ## do not change
       dpi=72) 
```

<img src="man/figures/README-unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

## Lollipop for discrete distributions

This still uses `type='pop'` but we’ll use a different template for
copying/pasting.

``` r
dg = data.frame(name=0:10, value=dbinom(0:10, 10, .5))

title = "Title in Upper Lower" 
g = ggplot(dg, aes(x=name, y=value))+
  geom_point(color=pubred)+ 
  geom_segment(aes(x=name, xend=name, y=0, yend=value), color=pubred)+
  geom_text(aes(label=round(value,2)), vjust=-0.5)+ ## optional numbers with reasonable number of digits
  labs(title    = title,
       subtitle = 'Optional Subtitle In Upper Lower',
       caption  = "Optional caption, giving additional info or Twitter handle",
       x = 'Horizontal Axis Label in Upper Lower', ## Optional. 
       y = NULL)+  ## Optional. Upper Lower.
  scale_x_continuous(limits=c(0,10), expand=c(0,0), breaks=0:10)+
  scale_y_continuous(limits=c(0,.3), breaks=c(0, .1, .2, .3))+
  coord_cartesian(clip='off', expand=FALSE)+
  theme_pub(type='pop', base_size=36/3) 
print(g)

gg = g + 
  theme_pub(type='pop', base_size=36)

ggsave(filename=paste0("img/", gsub("%", " Perc", title), ".jpg"), ## must have a subfolder called 'img'
       plot = gg,
       width=20,   ## do not change
       height=15,  
       units='in', ## do not change
       dpi=72) 
```

<img src="man/figures/README-unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

## Barbell plot

This still uses `type='pop'` but we’ll use a different template for
copying/pasting.

``` r
dg = airquality %>%
  mutate(Month= month.abb[Month],
         Month = factor(Month, levels=rev(month.abb))) %>% 
  group_by(Month) %>%
  summarise(x   =min(Temp), 
            xend=max(Temp)) %>%
  rename(name  = Month)


title = "Title in Upper Lower" 
g = ggplot(dg, aes(y=name))+
  geom_point(aes(x=x   ), color=pubred)+ 
  geom_point(aes(x=xend), color=pubred)+ 
  geom_segment(aes(x=x, xend=xend, yend=name), color=pubred)+
  geom_text(aes(x=x,    label=round(x   ,2)), hjust=1.3)+ ## optional numbers with reasonable number of digits
  geom_text(aes(x=xend, label=round(xend,2)), hjust=-0.3)+ ## optional numbers with reasonable number of digits
  labs(title    = title,
       subtitle = 'Optional Subtitle In Upper Lower',
       caption  = "Optional caption, giving additional info or Twitter handle",
       x = 'Horizontal Axis Label in Upper Lower', ## Optional. 
       y = NULL)+  ## Optional. Upper Lower.
  scale_x_continuous(limits=c(0,120), expand=c(0,0))+
  coord_cartesian(clip='off', expand=FALSE)+
  theme_pub(type='pop', base_size=36/3) 
print(g)

gg = g + 
  theme_pub(type='pop', base_size=36)

ggsave(filename=paste0("img/", gsub("%", " Perc", title), ".jpg"), ## must have a subfolder called 'img'
       plot = gg,
       width=20,   ## do not change
       height=15,  
       units='in', ## do not change
       dpi=72) 
```

<img src="man/figures/README-unnamed-chunk-12-1.png" style="display: block; margin: auto;" />

## Dot and Whiskers Plot

It is often desirable to visualize regression coefficients using a dot
and whiskers plot instead of showing a table of coefficients and
standard errors. We’ll make up a regression model and demonstrate that
here.

This still uses `type='pop'` but we’ll use a different template for
copying/pasting.

``` r
## standardize predictors so they are roughly the same scale
d = mtcars %>%
  mutate(wt = scale(wt),  
         cyl=scale(cyl), 
         disp=scale(disp), 
         hp = scale(hp))

m1 = lm(mpg ~  wt + cyl + disp + hp, data=d)
#summary(m1)

dg = summary(m1)$coefficients %>% 
  as.data.frame() %>%
  rownames_to_column(var = 'var') %>%
  rename(coef=Estimate, se=`Std. Error`) %>%
  select(var, coef, se) %>%
  mutate(var = toupper(gsub('[(]|[)]', '', var))) %>%
  filter(var!='INTERCEPT')

title = "Title in Upper Lower" 
g = ggplot(dg, aes(x=coef, y=var))+
  geom_segment(aes(x    = coef-se, 
                   xend = coef+se,
                   y    = var, 
                   yend = var), color=pubred)+
  geom_point(color=pubred) +
  geom_vline(xintercept=0, color=pubmediumgray)+
  labs(title    = title,
       subtitle = 'Optional Subtitle In Upper Lower',
       caption  = "Optional caption, giving additional info or Twitter handle",
       x = 'Coefficient', 
       y = NULL)+  ## Optional. 
  scale_x_continuous(limits=c(-5,5))+
  scale_y_discrete(expand=c(0,0)) +
  coord_cartesian(clip='off')+
  theme_pub(type='pop', base_size=36/3) 

print(g)

gg = g + 
  theme_pub(type='pop', base_size=36)

ggsave(filename=paste0("img/", gsub("%", " Perc", title), ".jpg"), 
       plot=gg,
       width=20,   ## do not change
       height=10,  
       units='in', ## do not change
       dpi=72)     ## do not change
```

<img src="man/figures/README-unnamed-chunk-13-1.png" style="display: block; margin: auto;" />

## Faceting

We’ll use our scatter plot example, but with `facet_wrap` to make
separate plots for each `cyl`.

``` r
dg = mtcars %>%
  select(wt, mpg, cyl) %>%
  mutate(cyl = paste0(cyl, '-cylinder')) %>%
  rename(name = cyl)

title = "Title in Upper Lower" ## to be used by ggplot and ggsave
g = ggplot(dg, aes(x=wt, y=mpg))+
  geom_point(aes(size=mpg, color=name), show.legend=F)+
  facet_wrap(~name, nrow=1) +
  labs(title    = title,
       subtitle = 'Optional Subtitle In Upper Lower',
       caption  = "Optional caption, giving additional info or Twitter handle",
       x = 'Horizontal Axis Label in Upper Lower',
       y = 'Vertical Axis Label in Upper Lower')+
  scale_x_continuous(limits=c(0, 6), breaks=c(0, 3, 6), oob=squish, labels=comma_format(accuracy = 1))+
  scale_y_continuous(limits=c(0,40), breaks=c(0,20,40), oob=squish, labels=comma)+
  coord_cartesian(clip='off', expand=FALSE)+
  theme_pub(type='scatter', base_size=36/3, facet=T)
print(g)

gf = g # save this 'g' with 'f'aceting for later

gg = g +
  theme_pub(type='scatter', base_size=36, facet=T)

ggsave(filename=paste0("img/", gsub("%", " Perc", title), ".jpg"), ## must have a subfolder called 'img'
       plot=gg, ## change range=c(6,18) when base_size=36
       width=20,   ## do not change
       height=13,  ## can change if desired. Here, 14 was chosen so that each subplot is square
       units='in', ## do not change
       dpi=72)     ## do not change
```

<img src="man/figures/README-unnamed-chunk-14-1.png" style="display: block; margin: auto;" />

## Timeline

Let’s make up some data. Let’s also define a new “`reverse_trans`”
function called `reverse2_trans`.

``` r
current.year = as.numeric(format(Sys.Date(), "%Y"))
dg = data.frame(date=as.Date(c(paste0(current.year, 
                                   c('-01-01', 
                                     '-07-04', 
                                     '-12-25', 
                                     '-12-31')),
                             paste0(current.year+1, 
                                    c('-01-01', 
                                      '-07-04', 
                                      '-12-25', 
                                      '-12-31')))), 
                text=c("New Year's Day", 
                       "Independence Day",
                       "Christmas Day",
                       "New Year's Eve", 
                       "New Year's Day", 
                       "Independence Day",
                       "Christmas Day",
                       "New Year's Eve")) %>%
  mutate(text = paste0(text, ', ', date), 
         name = case_when(grepl('Christmas', text) ~ 'Christmas', 
                          grepl('Indep'    , text) ~ 'Indep', 
                          TRUE ~ 'Other'), 
         name = factor(name, levels=c('Christmas', 'Indep', 'Other')))
dg
#>         date                         text      name
#> 1 2022-01-01   New Year's Day, 2022-01-01     Other
#> 2 2022-07-04 Independence Day, 2022-07-04     Indep
#> 3 2022-12-25    Christmas Day, 2022-12-25 Christmas
#> 4 2022-12-31   New Year's Eve, 2022-12-31     Other
#> 5 2023-01-01   New Year's Day, 2023-01-01     Other
#> 6 2023-07-04 Independence Day, 2023-07-04     Indep
#> 7 2023-12-25    Christmas Day, 2023-12-25 Christmas
#> 8 2023-12-31   New Year's Eve, 2023-12-31     Other

## Now make the timeline using ggrepel for the text
library(ggrepel) ## for  geom_text_repel() or geom_label_repel()

## Function for reverse date axes 
## Copied from https://github.com/tidyverse/ggplot2/issues/4014
## Use this with scale_y_continuous below (and use _continuous instead of _date)
reverse2_trans <- function() {
  trans_new(
    "reverse2",
    function(x) -1 * as.numeric(x), # Force values to be numeric for Date objects
    function(x) -1 * as.numeric(x)
  )
}

## Define breaks, title, and plot
breaks = as.Date(c('2022-01-01', '2023-01-01', '2024-01-01'))
title = "Title in Upper Lower" ## to be used by ggplot and ggsave
g = ggplot(dg, aes(x=0, y=date, color=name))+
  geom_segment(aes(x=0, xend=0, y=min(date), yend=max(date)), 
               show.legend = F, 
               color=publightgray)+
  geom_point(show.legend=F)+
  geom_label_repel(aes(label=text),
                   nudge_x = 1,
                   hjust = 0, 
                   direction = 'y',
                   show.legend = F)+
  labs(title    = title,
       subtitle = 'Optional Subtitle In Upper Lower',
       caption  = "Optional caption, giving additional info or Twitter handle",
       x = '',
       y = '')+
  scale_x_continuous(limits=c(0, 5), expand=c(0,0))+
  scale_y_continuous(trans=c('reverse2'),
                     breaks=as.numeric(breaks), 
                     labels=format(breaks, '%b %d, %Y'), 
                     expand=c(.1, .1))+ ## expand in case more room is needed
  coord_cartesian(clip='off')+
  theme_pub(type='timeline', base_size=12)
  
g

gg = g +
  theme_pub(type='timeline', base_size=36)

ggsave(filename=paste0("img/", gsub("%", " Perc", title), ".jpg"), ## must have a subfolder called 'img'
       plot=gg, ## change range=c(6,18) when base_size=36
       width=20,   ## do not change
       height=30,  ## can change if desired. Here, 14 was chosen so that each subplot is square
       units='in', ## do not change
       dpi=72)     ## do not change
```

<img src="man/figures/README-unnamed-chunk-15-1.png" style="display: block; margin: auto;" />

## Default colors

This theme changes your default ggplot colors to those found in
`colors.r`. The palette for discrete color scales
`scale_colour_discrete` and `scale_fill_discrete` consists of red, blue,
gray, light red, and light blue, as seen in the Line Plot above. Recall
that if you want to undo the changes made by this theme, you can use
`restore.ggplot.defaults()` at any time.

If more than 5-6 colors are needed, a 14-color colorblind friendly
palette `cb.pal` can be used by adding
`+ scale_color_manual(values=cb.pal)` or
`+ scale_fill_manual( values=cb.pal)` to a plot. For example,

``` r
gf + scale_color_manual(values=cb.pal)
```

<img src="man/figures/README-unnamed-chunk-16-1.png" style="display: block; margin: auto;" />

## Plotly

You can use `layout_pub` to get similar formatting for `plotly` figures
that use `ggplotly` or `plot_ly`. This is under development. Only using
`type='scatter'` with `plot_ly` is currently supported. Others will be
added “soon”. Example using `plot_ly`:

``` r
library(plotly)
library(xml2)
dg = mtcars %>% 
  select(wt, mpg, cyl) %>%
  mutate(cyl = factor(cyl)) %>%
  rownames_to_column('name')

base_size=18 ## c
p=plot_ly(data=dg, 
          width=1440*base_size/36, 
          height=1440*base_size/36) %>%
  add_trace(type='scatter', 
            mode='markers',
            x=~wt, 
            y=~mpg, 
            color=~cyl, 
            text=~name,
            colors=default.pal[1:3], ## change 3 to number of categories
            marker = list(alpha=1, size=30*base_size/36),
            hovertemplate = paste0( 
              "<b>%{text}</b><br>",
              "%{yaxis.title.text}: %{y:,.3f}<br>",
              "%{xaxis.title.text}: %{x:,.2f}<br>",
              "<extra></extra>")) %>%
  layoutpub(type='scatter', base_size=base_size, subtitle=T, caption=F, legend=T) %>%
  layout(title = list(text = maketitle(title='Title In Upper Lower',
                                       subtitle='Optional Subtitle in Upper Lower',
                                       base_size=base_size)), 
         xaxis = list(title = list(text='Wt'),
                      range=c(0,6),
                      tickvals = c(0,3,6)),
         yaxis = list(title = list(text='MPG'), 
                      range=c(0,40), 
                      tickvals = c(0,20,40))) 
  
print(p)

#htmltools::save_html(partial_bundle(p), 
#           file=paste0("img/", gsub("%", " Perc", title), ".html"))
#xml2::write_html(partial_bundle(p), 
#                 file=paste0("img/", gsub("%", " Perc", title), ".html"))
htmlwidgets::saveWidget(widget = p, 
                        file = paste0("img/", gsub("%", " Perc", title), ".html"), 
                        selfcontained = F, 
                        libdir = "lib")
```

``` r
htmltools::includeHTML(paste0("img/", gsub("%", " Perc", title), ".html"))
```

<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8"/>
<style>body{background-color:white;}</style>
<script src="lib/htmlwidgets-1.5.3/htmlwidgets.js"></script>
<script src="lib/plotly-binding-4.10.1/plotly.js"></script>
<script src="lib/setprototypeof-0.1/setprototypeof.js"></script>
<script src="lib/typedarray-0.1/typedarray.min.js"></script>
<script src="lib/jquery-3.5.1/jquery.min.js"></script>
<link href="lib/crosstalk-1.1.1/css/crosstalk.css" rel="stylesheet" />
<script src="lib/crosstalk-1.1.1/js/crosstalk.min.js"></script>
<link href="lib/plotly-htmlwidgets-css-2.11.1/plotly-htmlwidgets.css" rel="stylesheet" />
<script src="lib/plotly-main-2.11.1/plotly-latest.min.js"></script>
  <title>plotly</title>
</head>
<body>
<div id="htmlwidget_container">
  <div id="htmlwidget-511d30348e03b1bee50f" style="width:720px;height:720px;" class="plotly html-widget"></div>
</div>
<script type="application/json" data-for="htmlwidget-511d30348e03b1bee50f">{"x":{"visdat":{"3d6015de74c4":["function () ","plotlyVisDat"]},"cur_data":"3d6015de74c4","attrs":{"3d6015de74c4":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter","mode":"markers","x":{},"y":{},"color":{},"text":{},"colors":["#C81428","#058CC8","#646464"],"marker":{"alpha":1,"size":15},"hovertemplate":"<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>","inherit":true}},"layout":{"width":720,"height":720,"margin":{"b":93,"l":93,"t":147,"r":25,"pad":0},"autosize":true,"font":{"family":"Arial","size":18,"color":"#646464"},"title":{"font":{"family":"Arial","size":25,"color":"#1E1E1E"},"pad":{"t":45,"b":45,"l":25,"r":25},"x":0,"xanchor":"left","y":1,"yanchor":"top","text":"<b>Title In Upper Lower<\/b><br><span style=\"font-size: 21px; color:#878787\">Optional Subtitle in Upper Lower<\/span>"},"xaxis":{"domain":[0,1],"automargin":true,"title":{"standoff":15,"font":{"size":18},"text":"Wt"},"ticks":"outside","nticks":3,"tick0":0,"linecolor":"#646464","tickcolor":"#646464","gridcolor":"#D2D2D2","linewidth":1,"tickwidth":1,"gridwidth":1,"zeroline":false,"showline":true,"mirror":true,"ticklabelstep":1,"ticklen":10,"range":[0,6],"tickvals":[0,3,6]},"yaxis":{"domain":[0,1],"automargin":true,"title":{"standoff":10,"font":{"size":18},"text":"MPG"},"ticks":"outside","tick0":0,"linecolor":"#646464","tickcolor":"#646464","gridcolor":"#D2D2D2","linewidth":1,"tickwidth":1,"gridwidth":1,"zeroline":false,"showline":true,"mirror":true,"ticklabelstep":1,"ticklen":10,"range":[0,40],"tickvals":[0,20,40]},"showlegend":true,"legend":{"orientation":"h","itemclick":"toggle","itemdoubleclick":"toggleothers","x":-0.13,"xanchor":"left","y":1.12,"yanchor":"top"},"hoverlabel":{"bgcolor":"#F0F0F0"},"paper_bgcolor":"#F0F0F0","plot_bgcolor":"#F0F0F0","colorway":["#C81428","#058CC8","#646464","#E68C8C","#82B4D2"],"colorscale":["#D2D2D2","#058CC8"],"hovermode":"closest"},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"type":"scatter","mode":"markers","x":[2.32,3.19,3.15,2.2,1.615,1.835,2.465,1.935,2.14,1.513,2.78],"y":[22.8,24.4,22.8,32.4,30.4,33.9,21.5,27.3,26,30.4,21.4],"text":["Datsun 710","Merc 240D","Merc 230","Fiat 128","Honda Civic","Toyota Corolla","Toyota Corona","Fiat X1-9","Porsche 914-2","Lotus Europa","Volvo 142E"],"marker":{"color":"rgba(200,20,40,1)","alpha":1,"size":15,"line":{"color":"rgba(200,20,40,1)"}},"hovertemplate":["<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>","<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>","<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>","<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>","<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>","<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>","<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>","<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>","<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>","<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>","<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>"],"name":"4","textfont":{"color":"rgba(200,20,40,1)"},"error_y":{"color":"rgba(200,20,40,1)"},"error_x":{"color":"rgba(200,20,40,1)"},"line":{"color":"rgba(200,20,40,1)"},"xaxis":"x","yaxis":"y","frame":null},{"type":"scatter","mode":"markers","x":[2.62,2.875,3.215,3.46,3.44,3.44,2.77],"y":[21,21,21.4,18.1,19.2,17.8,19.7],"text":["Mazda RX4","Mazda RX4 Wag","Hornet 4 Drive","Valiant","Merc 280","Merc 280C","Ferrari Dino"],"marker":{"color":"rgba(5,140,200,1)","alpha":1,"size":15,"line":{"color":"rgba(5,140,200,1)"}},"hovertemplate":["<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>","<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>","<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>","<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>","<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>","<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>","<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>"],"name":"6","textfont":{"color":"rgba(5,140,200,1)"},"error_y":{"color":"rgba(5,140,200,1)"},"error_x":{"color":"rgba(5,140,200,1)"},"line":{"color":"rgba(5,140,200,1)"},"xaxis":"x","yaxis":"y","frame":null},{"type":"scatter","mode":"markers","x":[3.44,3.57,4.07,3.73,3.78,5.25,5.424,5.345,3.52,3.435,3.84,3.845,3.17,3.57],"y":[18.7,14.3,16.4,17.3,15.2,10.4,10.4,14.7,15.5,15.2,13.3,19.2,15.8,15],"text":["Hornet Sportabout","Duster 360","Merc 450SE","Merc 450SL","Merc 450SLC","Cadillac Fleetwood","Lincoln Continental","Chrysler Imperial","Dodge Challenger","AMC Javelin","Camaro Z28","Pontiac Firebird","Ford Pantera L","Maserati Bora"],"marker":{"color":"rgba(100,100,100,1)","alpha":1,"size":15,"line":{"color":"rgba(100,100,100,1)"}},"hovertemplate":["<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>","<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>","<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>","<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>","<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>","<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>","<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>","<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>","<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>","<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>","<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>","<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>","<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>","<b>%{text}<\/b><br>%{yaxis.title.text}: %{y:,.3f}<br>%{xaxis.title.text}: %{x:,.2f}<br><extra><\/extra>"],"name":"8","textfont":{"color":"rgba(100,100,100,1)"},"error_y":{"color":"rgba(100,100,100,1)"},"error_x":{"color":"rgba(100,100,100,1)"},"line":{"color":"rgba(100,100,100,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
<script type="application/htmlwidget-sizing" data-for="htmlwidget-511d30348e03b1bee50f">{"viewer":{"width":720,"height":720,"padding":0,"fill":false},"browser":{"width":720,"height":720,"padding":0,"fill":false}}</script>
</body>
</html>

See bmacgtpm.github.io/pubtheme/img/Title in Upper Lower.html to view
the interactive plot.
