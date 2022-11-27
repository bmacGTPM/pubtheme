## colors.r

## Default colors
pubred         = rgb(200,  20,  40, max=255) ## red for highlighted data
publightred    = rgb(230, 140, 140, max=255) ## red for background data
pubdarkgray    = rgb( 30,  30,  30, max=255) ## gray for highlighted data (dark)
pubmediumgray  = rgb(135, 135, 135, max=255) ## medium gray for subtitle and caption
publightgray   = rgb(210, 210, 210, max=255) ## gray for background data
pubtextgray    = rgb(100, 100, 100, max=255) ## dark gray for text
pubbackgray    = rgb(240, 240, 240, max=255) ## very light gray for image background
pubblue        = rgb(  5, 140, 200, max=255) ## blue for highlighted data
publightblue   = rgb(130, 180, 210, max=255) ## blue for background data
default.pal = c(pubred, pubblue, pubtextgray,  publightred, publightblue)

## colorblind friendly color palette
## from https://jacksonlab.agronomy.wisc.edu/2016/05/23/15-level-colorblind-friendly-palette/
cb14 =  c("#000000","#004949","#009292","#ff6db6","#ffb6db",
          "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
          "#920000","#924900","#db6d00","#24ff24","#ffff6d")
cb14[15] = 'lightgray' ## replace the yellow with gray, since yellow is almost impossible to see.
cb.pal = cb14[-2] ## remove this since it is so close to the next one


