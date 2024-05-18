
## choose order of variables
cols = sort(unique(colnames(mtcars)))

corr <- round(cor(mtcars[,cols]), 2)
head(corr,2)


title = "Title in Upper Lower"
g = ggcorrplot(corr) + 
  scale_fill_gradientn(colors = c('red4', 
                                  pubred, 
                                  publightred, 
                                  pubbackgray, 
                                  publightblue, 
                                  pubblue, 
                                  'navy'),
                       na.value = pubmediumgray, ## same color as below
                       oob      = squish,
                       breaks   = c(-1, 0, 1),
                       limits   = c(-1,    1)) +
  labs(title    = title,
       subtitle = 'Optional Subtitle In Upper Lower',
       #caption  = "Optional caption giving more info, X handle, or shameless promotion of pubtheme",
       x    = 'Day (Optional Axis Label in Upper Lower)', 
       y    = NULL, ## Optional
       fill = 'Value') #+ 
g

g %>% 
  pub(type    = 'grid', base_size = 10) + 
  theme(axis.text.x.top = element_text(angle = 90, 
                                       hjust = 0, 
                                       vjust = 0.7))


dg = corr %>%
  as.data.frame() %>%
  
  ## longer format
  rownames_to_column(var = 'x') %>%
  pivot_longer(cols      = -x, 
               names_to  = 'y', 
               values_to = 'value') %>%
  
  ## order them however you'd like
  mutate(x = factor(x, levels = rev(sort(unique(x)))), 
         y = factor(y, levels = sort(unique(y))))

title = "Title in Upper Lower"
g = ggplot(dg, 
           aes(x    = x, 
               y    = y, 
               fill = value)) + 
  geom_tile(linewidth   = 0.4, 
            show.legend = T, 
            color       = pubdarkgray) +
  scale_fill_gradientn(colors = c('red4', 
                                  pubred, 
                                  publightred, 
                                  pubbackgray, 
                                  publightblue, 
                                  pubblue, 
                                  'navy'),
                       na.value = pubmediumgray, ## same color as below
                       oob      = squish,
                       breaks   = c(-1, 0, 1),
                       limits   = c(-1,    1)) +

  labs(title    = title,
       subtitle = 'Optional Subtitle In Upper Lower',
       caption  = "Optional caption giving more info, X handle, or shameless promotion of pubtheme",
       x    = 'Day (Optional Axis Label in Upper Lower)', 
       y    = NULL, ## Optional
       fill = 'Value') 

g %>% 
  pub(type    = 'grid') + 
  theme(axis.text.x.top = element_text(angle = 90, 
                                   hjust = 0, 
                                   vjust = 0.3))

gg = g %>%
  pub(type      = 'grid', 
      base_size = 36)

ggsave(filename = paste0("img/", gsub("%", " Perc", title), ".jpg"), 
       plot   = gg,
       width  = 20,   ## do not change
       height = 24,   ## can change from 20 if desired. We use 12 here to make the tiles square
       units  = 'in', ## do not change
       dpi    = 72)   ## do not change
