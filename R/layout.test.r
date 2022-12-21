# 
# library(plotly)
# dg = mtcars %>% 
#   select(wt, mpg, cyl) %>%
#   mutate(cyl = factor(cyl)) %>%
#   rownames_to_column('name')
# 
# base_size=18
# p=plot_ly(data=dg, width=1440*base_size/36, height=1440*base_size/36) %>%
#   add_trace(type='scatter', 
#             mode='markers',
#             x=~wt, 
#             y=~mpg, 
#             color=~cyl, 
#             text=~name,
#             colors=default.pal[1:3], ## change 3 to number of categories
#             marker = list(alpha=1, size=30*base_size/36),
#             # https://plotly.com/r/hover-text-and-formatting/#customize-tooltip-text-with-a-hovertemplate
#             hovertemplate = paste0( 
#               "<b>%{text}</b><br>",
#               "%{yaxis.title.text}: %{y:,.3f}<br>",
#               "%{xaxis.title.text}: %{x:,.2f}<br>",
#               "<extra></extra>"
#             )
#   ) %>%
#   layout.pub(base_size=base_size, subtitle=T, caption=F, legend=T) %>%
#   layout(title = list(text = format.title(title='Title In Upper Lower', 
#                                           subtitle='Optional Subtitle in Upper Lower',
#                                           base_size=base_size)), 
#          xaxis = list(title = list(text='Wt'),
#                       range=c(0,6),
#                       tickvals = c(0,3,6)),
#          yaxis = list(title = list(text='MPG'), 
#                       range=c(0,40), 
#                       tickvals = c(0,20,40)))
# 
# print(p)
# 
# # layout(font = list(family = "Arial", 
# #                    size=18, 
# #                    color=pubtextgray), 
# #        title = list(font = list(size=48)))
# 
# 
# # ggplotly(g) %>%
# #   publayout(18) %>%
# #   layout(title = list(font= list(family='Arial'))) ## need some extra stuff when using ggplotly for some reason
