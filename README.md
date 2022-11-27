
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
the package `orgthemes`.

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
source('R/colors.r')
```

as usual. The theme will change some of your ggplot defaults the first
time you use it. To change them back, restart R, or use

``` r
restore.ggplot.defaults()
```

at any time.

## Scatter plot

``` r
dg = mtcars %>% 
  select(wt, mpg)

title = "Title in Upper Lower" 
g = ggplot(dg, aes(x=wt, y=mpg))+
  geom_point(aes(size=mpg), color=pubred)+
  labs(title    = title,
       subtitle = 'Optional Subtitle In Upper Lower',
       caption  = "Optional caption, giving additional info or twitter handle",
       x = 'Horizontal Axis Label in Upper Lower',
       y = 'Vertical Axis Label in Upper Lower')+
  scale_x_continuous(limits=c(0, 6), breaks=c(0, 3, 6), oob=squish, labels=comma)+
  scale_y_continuous(limits=c(0,40), breaks=c(0,20,40), oob=squish, labels=comma)+
  coord_cartesian(clip='off', expand=FALSE)+
  theme_pub(type='scatter', base_size = 12) 
print(g)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

``` r

## Save to a file
gg = g +
  theme_pub(type='scatter', base_size=36)+
  scale_size(range=c(6,18)) 

ggsave(filename=paste0("img/", gsub("%", " Perc", title), ".jpg"), 
      plot=gg,
      width=20,   
      height=20,  
      units='in', 
      dpi=72)     
```

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
       caption  = "Optional caption, giving additional info or twitter handle",
       x = 'Horizontal Axis Label in Upper Lower', 
       y = 'Vertical Axis Label in Upper Lower')+  
  scale_x_continuous(labels=comma) + 
  scale_y_continuous(labels=comma, limits=c(0,1), breaks=c(0, .5, 1))+ 
  coord_cartesian(clip='off', expand=FALSE)+
  theme_pub(type='line', base_size=36/3) 
print(g)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

``` r

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
       caption  = "Optional caption, giving additional info or twitter handle",
       x = 'Horizontal Axis Label in Upper Lower', ## Required.
       y = 'Count')+  ## Often don't need to change.
  scale_x_continuous(labels=comma)+
  scale_y_continuous(labels=comma, expand = c(0,0))+
  theme_pub(type='hist', base_size=36/3) 
print(g)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

``` r

gg = g + 
  theme_pub(type='hist', base_size=36)

ggsave(filename=paste0("img/", gsub("%", " Perc", title), ".jpg"), 
       plot = gg,
       width=20,   ## do not change
       height=20,  
       units='in', ## do not change
       dpi=72)     ## do not change
```

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
       caption  = "Optional caption, giving additional info or twitter handle",
       x = 'Horizontal Axis Label in Upper Lower', ## Optional. 
       y = NULL)+  ## Optional. Upper Lower.
  scale_x_continuous(limits=c(0,35))+ 
  theme_pub(type='bar', base_size=36/3) 
print(g)
```

<img src="man/figures/README-unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

``` r

gg = g + 
  theme_pub(type='bar', base_size=36)

ggsave(filename=paste0("img/", gsub("%", " Perc", title), ".jpg"), ## must have a subfolder called 'img'
       plot = gg,
       width=20,   ## do not change
       height=15,  
       units='in', ## do not change
       dpi=72)     ## do not change
```

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
  geom_tile(size=0.4, show.legend = F) +
  scale_fill_gradient(low = pubbackgray,
                      high = pubred,
                      na.value = 'white',
                      oob=squish) +
  labs(title    = title,
       subtitle = 'Optional Subtitle In Upper Lower',
       caption  = "Optional caption, giving additional info or twitter handle",
       x = 'Day (Optional Axis Label in Upper Lower)', 
       y = NULL)+  ## Optional. 
  geom_vline(xintercept=1:(length(unique(dg$name ))+1)-.5, color=pubdarkgray, size=0.2)+ # vert  lines between each square
  geom_hline(yintercept=1:(length(unique(dg$name2))+1)-.5, color=pubdarkgray, size=0.2)+ # horiz lines 
  scale_x_continuous(expand = c(0, 0), position='top', breaks=seq(2,30,by=2))+
  scale_y_discrete(  expand = c(0, 0)) +
  theme_pub(type='grid', base_size=36/3) 

print(g)
```

<img src="man/figures/README-unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

``` r

gg = g + 
  theme_pub(type='grid', base_size=36)

ggsave(filename=paste0("img/", gsub("%", " Perc", title), ".jpg"), 
       plot=gg,
       width=20,   ## do not change
       height=10,  
       units='in', ## do not change
       dpi=72)     ## do not change
```

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
       caption  = "Optional caption, giving additional info or twitter handle",
       x = 'Horizontal Axis Label in Upper Lower', ## Optional. 
       y = NULL)+  ## Optional. Upper Lower.
  scale_x_continuous(limits=c(0,120), expand=c(0,0))+
  coord_cartesian(clip='off', expand=FALSE)+
  theme_pub(type='pop', base_size=36/3) 
print(g)
```

<img src="man/figures/README-unnamed-chunk-9-1.png" style="display: block; margin: auto;" />

``` r

gg = g + 
  theme_pub(type='pop', base_size=36)

ggsave(filename=paste0("img/", gsub("%", " Perc", title), ".jpg"), ## must have a subfolder called 'img'
       plot = gg,
       width=20,   ## do not change
       height=15,  
       units='in', ## do not change
       dpi=72) 
```

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
       caption  = "Optional caption, giving additional info or twitter handle",
       x = 'Horizontal Axis Label in Upper Lower', ## Optional. 
       y = NULL)+  ## Optional. Upper Lower.
  scale_x_continuous(limits=c(0,10), expand=c(0,0), breaks=0:10)+
  scale_y_continuous(limits=c(0,.3), breaks=c(0, .1, .2, .3))+
  coord_cartesian(clip='off', expand=FALSE)+
  theme_pub(type='pop', base_size=36/3) 
print(g)
```

<img src="man/figures/README-unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

``` r

gg = g + 
  theme_pub(type='pop', base_size=36)

ggsave(filename=paste0("img/", gsub("%", " Perc", title), ".jpg"), ## must have a subfolder called 'img'
       plot = gg,
       width=20,   ## do not change
       height=15,  
       units='in', ## do not change
       dpi=72) 
```

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
       caption  = "Optional caption, giving additional info or twitter handle",
       x = 'Horizontal Axis Label in Upper Lower', ## Optional. 
       y = NULL)+  ## Optional. Upper Lower.
  scale_x_continuous(limits=c(0,120), expand=c(0,0))+
  coord_cartesian(clip='off', expand=FALSE)+
  theme_pub(type='pop', base_size=36/3) 
print(g)
```

<img src="man/figures/README-unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

``` r

gg = g + 
  theme_pub(type='pop', base_size=36)

ggsave(filename=paste0("img/", gsub("%", " Perc", title), ".jpg"), ## must have a subfolder called 'img'
       plot = gg,
       width=20,   ## do not change
       height=15,  
       units='in', ## do not change
       dpi=72) 
```

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
       caption  = "Optional caption, giving additional info or twitter handle",
       x = 'Coefficient', 
       y = NULL)+  ## Optional. 
  scale_x_continuous(limits=c(-5,5))+
  scale_y_discrete(expand=c(0,0)) +
  coord_cartesian(clip='off')+
  theme_pub(type='pop', base_size=36/3) 

print(g)
```

<img src="man/figures/README-unnamed-chunk-12-1.png" style="display: block; margin: auto;" />

``` r

gg = g + 
  theme_pub(type='pop', base_size=36)

ggsave(filename=paste0("img/", gsub("%", " Perc", title), ".jpg"), 
       plot=gg,
       width=20,   ## do not change
       height=10,  
       units='in', ## do not change
       dpi=72)     ## do not change
```

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
       caption  = "Optional caption, giving additional info or twitter handle",
       x = 'Horizontal Axis Label in Upper Lower',
       y = 'Vertical Axis Label in Upper Lower')+
  scale_x_continuous(limits=c(0, 6), breaks=c(0, 3, 6), oob=squish, labels=comma_format(accuracy = 1))+
  scale_y_continuous(limits=c(0,40), breaks=c(0,20,40), oob=squish, labels=comma)+
  coord_cartesian(clip='off', expand=FALSE)+
  theme_pub(type='scatter', base_size=36/3, facet=T)
print(g)
```

<img src="man/figures/README-unnamed-chunk-13-1.png" style="display: block; margin: auto;" />

``` r

gg = g +
  theme_pub(type='scatter', base_size=36, facet=T)
  #scale_size(range=c(6,18))

ggsave(filename=paste0("img/", gsub("%", " Perc", title), ".jpg"), ## must have a subfolder called 'img'
       plot=gg, ## change range=c(6,18) when base_size=36
       width=20,   ## do not change
       height=13,  ## can change if desired. Here, 14 was chosen so that each subplot is square
       units='in', ## do not change
       dpi=72)     ## do not change
```

## Default colors

This theme changes your default ggplot colors to those found in
`colors.r`. The palette for discrete color scales
`scale_colour_discrete` and `scale_fill_discrete` consists of red, blue,
gray, light red, and light blue, as seen in the Line Plot above. Recall
that if you want to undo the changes made by this theme, you can use
`restore.gglot.defaults()` at any time.

If more than 5-6 colors are needed, the 14-color colorblind friendly
palette `cb14` can be used by adding
`+ scale_color_manual(values=cb.pal)` or
`+ scale_fill_manual( values=cb.pal)` to a plot. For example,

``` r
g + scale_color_manual(values=cb.pal)
```

<img src="man/figures/README-unnamed-chunk-14-1.png" style="display: block; margin: auto;" />
