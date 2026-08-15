#' An echarts4r theme that matches `theme_pub`
#'
#' Apply pubtheme colors, fonts, and spacing to an [echarts4r::e_charts()]
#' figure. Call this last in the pipeline, after series, titles, and axes
#' have been added.
#'
#' @param e An `echarts4r` object from [echarts4r::e_charts()].
#' @param type Text indicating the type of plot. Supported types match
#'   `theme_pub`: `'line'`, `'bar'`, `'hist'`, `'grid'`, `'scatter'`,
#'   `'pop'`, `'dot'`, `'map'`, `'slope'`, `'timeline'`, `'cal'`, `'pairs'`.
#' @param base_size Base font size, in px. For viewing, 12 (the default)
#'   matches `theme_pub`. Use 36 for large exported images.
#' @param base_family Base font family. The default is `'sans'`, mapped to
#'   CSS `'sans-serif'` so it is available cross-platform. `'serif'` and
#'   `'mono'` are mapped the same way; any other string is passed through
#'   (for example `'Arial'`).
#' @param facet Indicates whether the chart uses [echarts4r::e_facet()]
#'   or should otherwise get a panel border. Default is `FALSE`.
#' @param colors Color palette. `'default'` is the pubtheme reds, blues,
#'   and grays. `'cb14'` is the colorblind-friendly palette. A character
#'   vector of colors is used as-is.
#' @param caption Optional caption text. ECharts has no caption slot, so
#'   this is added as a second title at the bottom left, in the same
#'   medium gray and size as `theme_pub` captions.
#' @return The `echarts4r` object, with pubtheme styling applied.
#' @seealso [theme_pub()], [layoutpub()], [pub.echarts.theme()]
#' @export
#' @examples
#' # See https://github.com/bmacGTPM/pubtheme for examples.
#' \dontrun{
#' library(echarts4r)
#' mtcars |>
#'   e_charts(wt) |>
#'   e_scatter(mpg) |>
#'   e_title("Title in Upper Lower", "Optional Subtitle in Upper Lower") |>
#'   e_x_axis(name = "Horizontal Axis Label in Upper Lower") |>
#'   e_y_axis(name = "Vertical Axis Label in Upper Lower") |>
#'   e_theme_pub(type = "scatter")
#' }

e_theme_pub = function(e,
                       type = "scatter",
                       base_size = 12,
                       base_family = "sans",
                       facet = FALSE,
                       colors = "default",
                       caption = NULL) {
  if (!requireNamespace("echarts4r", quietly = TRUE)) {
    stop(
      "Package echarts4r is required for e_theme_pub(). ",
      "Install it with install.packages(\"echarts4r\").",
      call. = FALSE
    )
  }
  if (!inherits(e, "echarts4r")) {
    stop(
      "e_theme_pub() expects an echarts4r object from e_charts().",
      call. = FALSE
    )
  }

  pal = .pub.pal(colors)
  family = .pub.font.family(base_family)
  scale = base_size / 36

  theme.json = pub.echarts.theme(
    base_size = base_size,
    base_family = base_family,
    colors = colors
  )
  # e_theme_custom() always inits the widget with theme = "custom",
  # so the registered name must match or the JSON theme is ignored.
  e = echarts4r::e_theme_custom(e, theme = theme.json, name = "custom")
  e = echarts4r::e_color(e, color = pal, background = pubbackgray)
  e = echarts4r::e_text_style(
    e,
    fontFamily = family,
    fontSize = base_size,
    color = pubtextgray
  )

  has.title = .pub.has.title(e)
  has.subtitle = .pub.has.subtitle(e)
  has.caption = .pub.has.caption(caption)
  legend.show = .pub.legend.show(e)

  title.h = if (has.title) 50 * scale * 48 / 36 + 20 * scale else 0
  subtitle.h = if (has.subtitle) 42 * scale * 48 / 36 + 10 * scale else 0
  legend.h = if (legend.show) 36 * scale + 20 * scale else 0
  caption.h = if (has.caption) 33 * scale * 48 / 36 + 20 * scale else 0

  title.top = 20 * scale
  legend.top = title.top + title.h + subtitle.h
  grid.top = 70 * scale + title.h + subtitle.h + legend.h
  # containLabel covers tick labels, not axis names. Leave room for the
  # rotated y-axis name on the left and the x-axis name plus caption below.
  grid.bottom = 56 * base_size / 12 + caption.h
  grid.left = 72 * base_size / 12
  grid.right = 28 * base_size / 12

  e = .pub.style.titles(
    e,
    family = family,
    base_size = base_size,
    scale = scale,
    caption = caption,
    title.top = title.top
  )
  e = .pub.style.grid(
    e,
    type = type,
    facet = facet,
    left = grid.left,
    right = grid.right,
    top = grid.top,
    bottom = grid.bottom
  )
  e = .pub.style.axes(
    e,
    type = type,
    facet = facet,
    family = family,
    base_size = base_size,
    scale = scale
  )
  e = .pub.style.legend(
    e,
    type = type,
    family = family,
    base_size = base_size,
    scale = scale,
    top = legend.top
  )
  e = .pub.style.tooltip(e, type = type, family = family, base_size = base_size)
  e = .pub.style.visualmap(e, family = family, base_size = base_size)
  e = .pub.style.series(e, base_size = base_size)

  e
}

#' pubtheme as an ECharts theme JSON
#'
#' Build the pubtheme ECharts theme as a JSON string. Use this with
#' [echarts4r::e_theme_register()] in Shiny or R Markdown so the theme is
#' registered once, then apply it with \code{e_theme(e, "pub")}.
#' For a single chart, prefer [e_theme_pub()], which also sets type-specific
#' axis, grid, and legend spacing.
#'
#' @inheritParams e_theme_pub
#' @return A JSON string suitable for `e_theme_custom()` or `e_theme_register()`.
#' @seealso [e_theme_pub()]
#' @export
#' @examples
#' # See https://github.com/bmacGTPM/pubtheme for examples.
#' \dontrun{
#' library(echarts4r)
#' library(shiny)
#' ui = fluidPage(
#'   e_theme_register(pub.echarts.theme(), name = "pub"),
#'   echarts4rOutput("chart")
#' )
#' }

pub.echarts.theme = function(base_size = 12,
                             base_family = "sans",
                             colors = "default") {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop(
      "Package jsonlite is required for pub.echarts.theme(). ",
      "Install it with install.packages(\"jsonlite\").",
      call. = FALSE
    )
  }

  pal = .pub.pal(colors)
  family = .pub.font.family(base_family)
  scale = base_size / 36
  line.width = max(1, 2 * base_size / 12)
  point.size = 8 * base_size / 12
  tick.len = 6 * base_size / 12
  label.margin = 8 * base_size / 12
  name.gap = 48 * base_size / 12

  axis = list(
    axisLine = list(
      show = TRUE,
      lineStyle = list(color = pubtextgray, width = line.width)
    ),
    axisTick = list(
      show = TRUE,
      length = tick.len,
      lineStyle = list(color = pubtextgray, width = line.width)
    ),
    axisLabel = list(
      color = pubtextgray,
      fontSize = base_size,
      fontFamily = family,
      margin = label.margin
    ),
    nameLocation = "middle",
    nameGap = name.gap,
    nameTextStyle = list(
      color = pubtextgray,
      fontSize = base_size,
      fontFamily = family
    ),
    splitLine = list(
      show = TRUE,
      lineStyle = list(color = publightgray, width = 1)
    ),
    splitArea = list(show = FALSE)
  )

  value.axis = axis
  category.axis = axis
  category.axis$splitLine$show = FALSE

  theme = list(
    color = pal,
    backgroundColor = pubbackgray,
    textStyle = list(
      fontFamily = family,
      fontSize = base_size,
      color = pubtextgray
    ),
    title = list(
      left = 0,
      itemGap = 8 * base_size / 12,
      textStyle = list(
        fontFamily = family,
        fontSize = 50 * scale,
        color = pubdarkgray,
        fontWeight = "bold"
      ),
      subtextStyle = list(
        fontFamily = family,
        fontSize = 42 * scale,
        color = pubmediumgray,
        fontWeight = "normal"
      )
    ),
    legend = list(
      orient = "horizontal",
      left = 0,
      itemWidth = 14 * base_size / 12,
      itemHeight = 10 * base_size / 12,
      itemGap = 16 * base_size / 12,
      textStyle = list(
        color = pubtextgray,
        fontSize = base_size,
        fontFamily = family
      )
    ),
    tooltip = list(
      backgroundColor = pubbackgray,
      borderColor = publightgray,
      borderWidth = 1,
      textStyle = list(
        color = pubtextgray,
        fontSize = base_size,
        fontFamily = family
      )
    ),
    grid = list(
      containLabel = TRUE,
      borderColor = pubtextgray,
      borderWidth = 0
    ),
    categoryAxis = category.axis,
    valueAxis = value.axis,
    logAxis = value.axis,
    timeAxis = value.axis,
    line = list(
      symbol = "circle",
      symbolSize = max(4, point.size / 2),
      lineStyle = list(width = 3 * base_size / 12),
      smooth = FALSE
    ),
    bar = list(
      itemStyle = list(borderWidth = 0, borderColor = "transparent")
    ),
    scatter = list(
      symbol = "circle",
      symbolSize = point.size
    ),
    effectScatter = list(
      symbol = "circle",
      symbolSize = point.size
    ),
    heatmap = list(
      itemStyle = list(borderColor = pubdarkgray, borderWidth = 0.4)
    ),
    graph = list(
      color = pal
    ),
    visualMap = list(
      color = c(pubblue, publightgray),
      textStyle = list(
        color = pubtextgray,
        fontSize = base_size,
        fontFamily = family
      )
    ),
    toolbox = list(
      iconStyle = list(borderColor = pubtextgray),
      emphasis = list(iconStyle = list(borderColor = pubdarkgray))
    )
  )

  # as.character() strips the json class. If the class stays, htmlwidgets
  # embeds an object instead of a string and echarts4r's JSON.parse() fails.
  as.character(jsonlite::toJSON(theme, auto_unbox = TRUE, null = "null"))
}

.pub.pal = function(colors) {
  pal = default.pal
  if (length(colors) == 1) {
    if (colors == "cb14") pal = cb.pal
  }
  if (length(colors) > 1) pal = colors
  pal
}

.pub.font.family = function(base_family) {
  switch(
    base_family,
    sans = "sans-serif",
    serif = "serif",
    mono = "monospace",
    base_family
  )
}

.pub.has.title = function(e) {
  titles = e$x$opts$title
  if (is.null(titles) || length(titles) == 0) return(FALSE)
  text = titles[[1]]$text
  !is.null(text) && nzchar(text)
}

.pub.has.subtitle = function(e) {
  titles = e$x$opts$title
  if (is.null(titles) || length(titles) == 0) return(FALSE)
  text = titles[[1]]$subtext
  !is.null(text) && nzchar(text)
}

.pub.has.caption = function(caption) {
  !is.null(caption) && !isFALSE(caption) && nzchar(as.character(caption)[1])
}

.pub.legend.show = function(e) {
  legend = e$x$opts$legend
  if (is.null(legend)) return(FALSE)
  if (!is.null(legend$show) && !isTRUE(legend$show)) return(FALSE)
  data = legend$data
  !is.null(data) && length(data) > 0
}

.pub.style.titles = function(e, family, base_size, scale, caption, title.top) {
  titles = e$x$opts$title
  if (is.null(titles)) titles = list()

  if (length(titles) > 0) {
    for (i in seq_along(titles)) {
      titles[[i]]$left = 0
      if (is.null(titles[[i]]$top) && is.null(titles[[i]]$bottom)) {
        titles[[i]]$top = title.top
      }
      titles[[i]]$itemGap = 8 * base_size / 12
      titles[[i]]$textStyle = list(
        fontFamily = family,
        fontSize = 50 * scale,
        color = pubdarkgray,
        fontWeight = "bold"
      )
      titles[[i]]$subtextStyle = list(
        fontFamily = family,
        fontSize = 42 * scale,
        color = pubmediumgray,
        fontWeight = "normal"
      )
    }
  }

  if (.pub.has.caption(caption)) {
    titles = c(
      titles,
      list(list(
        text = as.character(caption)[1],
        left = 0,
        bottom = 0,
        textStyle = list(
          fontFamily = family,
          fontSize = 33 * scale,
          color = pubmediumgray,
          fontWeight = "normal"
        )
      ))
    )
  }

  if (length(titles) > 0) e$x$opts$title = titles
  e
}

.pub.style.grid = function(e, type, facet, left, right, top, bottom) {
  border.width = if (isTRUE(facet) || type %in% c("pairs")) 1 else 0
  grids = e$x$opts$grid

  style.one = function(g) {
    if (is.null(g)) g = list()
    if (is.null(grids) || length(grids) <= 1) {
      g$left = left
      g$right = right
      g$top = top
      g$bottom = bottom
    }
    g$containLabel = TRUE
    g$borderColor = pubtextgray
    g$borderWidth = border.width
    g
  }

  if (is.null(grids) || length(grids) == 0) {
    e$x$opts$grid = list(style.one(list()))
    return(e)
  }

  # A single unwrapped grid (has left/top at the top level) vs a list of grids
  if (!is.null(grids$left) || !is.null(grids$top) || !is.null(grids$containLabel)) {
    e$x$opts$grid = list(style.one(grids))
  } else {
    e$x$opts$grid = lapply(grids, style.one)
  }
  e
}

.pub.axis.style = function(which, type, facet, family, base_size, scale) {
  line.width = max(1, 2 * base_size / 12)
  tick.len = 6 * base_size / 12
  label.margin = 8 * base_size / 12
  # nameGap is from the axis line, so it must clear tick labels too
  name.gap = if (which == "y") 56 * base_size / 12 else 40 * base_size / 12

  show.line = TRUE
  show.tick = TRUE
  show.split = (which == "y")
  show.label = TRUE
  show.name = TRUE
  label.size = base_size

  if (type == "scatter") {
    show.split = TRUE
  }
  if (type == "line") {
    show.split = (which == "y")
  }
  if (type == "bar") {
    show.line = FALSE
    show.tick = FALSE
    show.split = FALSE
  }
  if (type %in% c("pop", "dot")) {
    show.line = FALSE
    show.tick = FALSE
    if (which == "y") show.tick = FALSE
    show.split = (which == "x")
  }
  if (type == "hist") {
    if (which == "x") {
      show.tick = FALSE
      show.split = FALSE
    }
    if (which == "y") show.split = TRUE
  }
  if (type %in% c("grid", "cal")) {
    show.line = FALSE
    show.tick = FALSE
    show.split = FALSE
  }
  if (type == "cal") {
    if (which == "y") {
      show.label = FALSE
      show.name = FALSE
    }
    if (which == "x") label.size = base_size * 0.75
  }
  if (type == "timeline") {
    show.line = FALSE
    show.split = FALSE
    if (which == "x") {
      show.tick = FALSE
      show.label = FALSE
    }
  }
  if (type == "map") {
    show.line = FALSE
    show.tick = FALSE
    show.split = FALSE
    show.label = FALSE
    show.name = FALSE
  }
  if (type == "slope") {
    show.line = FALSE
    show.tick = FALSE
    show.name = FALSE
    if (which == "y") {
      show.label = FALSE
      show.split = FALSE
    }
    if (which == "x") show.split = TRUE
  }
  if (type == "pairs" || isTRUE(facet)) {
    show.line = TRUE
  }

  list(
    axisLine = list(
      show = show.line,
      lineStyle = list(color = pubtextgray, width = line.width)
    ),
    axisTick = list(
      show = show.tick,
      length = if (show.tick) tick.len else 0,
      lineStyle = list(color = pubtextgray, width = line.width)
    ),
    axisLabel = list(
      show = show.label,
      color = pubtextgray,
      fontSize = label.size,
      fontFamily = family,
      margin = label.margin
    ),
    nameLocation = "middle",
    nameGap = name.gap,
    nameTextStyle = list(
      color = if (show.name) pubtextgray else "transparent",
      fontSize = base_size,
      fontFamily = family
    ),
    splitLine = list(
      show = show.split,
      lineStyle = list(color = publightgray, width = 1)
    ),
    splitArea = list(show = FALSE)
  )
}

.pub.map.axes = function(axes, style) {
  if (is.null(axes)) return(axes)

  apply.one = function(ax) {
    if (is.null(ax) || !is.list(ax)) return(ax)
    for (nm in names(style)) ax[[nm]] = style[[nm]]
    ax
  }

  # Single unwrapped axis vs list of axes
  if (!is.null(axes$type) || !is.null(axes$show) || !is.null(axes$name) ||
      !is.null(axes$min) || !is.null(axes$max) || !is.null(axes$data)) {
    return(apply.one(axes))
  }
  lapply(axes, apply.one)
}

.pub.style.axes = function(e, type, facet, family, base_size, scale) {
  x.style = .pub.axis.style("x", type, facet, family, base_size, scale)
  y.style = .pub.axis.style("y", type, facet, family, base_size, scale)

  if (!is.null(e$x$opts$xAxis)) {
    e$x$opts$xAxis = .pub.map.axes(e$x$opts$xAxis, x.style)
  }
  if (!is.null(e$x$opts$yAxis)) {
    e$x$opts$yAxis = .pub.map.axes(e$x$opts$yAxis, y.style)
  }
  e
}

.pub.style.legend = function(e, type, family, base_size, scale, top) {
  legend = e$x$opts$legend
  if (is.null(legend)) return(e)

  icon = switch(
    type,
    bar = "rect",
    hist = "rect",
    grid = "rect",
    cal = "rect",
    line = "circle",
    "circle"
  )

  legend$orient = "horizontal"
  legend$left = 0
  if (is.null(legend$top) || identical(legend$top, "auto")) {
    legend$top = top
  }
  legend$itemWidth = 14 * base_size / 12
  legend$itemHeight = 10 * base_size / 12
  legend$itemGap = 16 * base_size / 12
  legend$icon = icon
  legend$textStyle = list(
    color = pubtextgray,
    fontSize = base_size,
    fontFamily = family
  )
  e$x$opts$legend = legend
  e
}

.pub.style.tooltip = function(e, type, family, base_size) {
  tip = e$x$opts$tooltip
  if (is.null(tip)) tip = list()
  if (is.null(tip$trigger)) {
    tip$trigger = if (type %in% c("line", "hist")) "axis" else "item"
  }
  tip$backgroundColor = pubbackgray
  tip$borderColor = publightgray
  tip$borderWidth = 1
  tip$textStyle = list(
    color = pubtextgray,
    fontSize = base_size,
    fontFamily = family
  )
  e$x$opts$tooltip = tip
  e
}

.pub.style.visualmap = function(e, family, base_size) {
  vm = e$x$opts$visualMap
  if (is.null(vm)) return(e)

  style.one = function(v) {
    if (is.null(v) || !is.list(v)) return(v)
    if (is.null(v$inRange)) v$inRange = list()
    v$inRange$color = c(publightgray, pubblue)
    v$textStyle = list(
      color = pubtextgray,
      fontSize = base_size,
      fontFamily = family
    )
    v
  }

  if (!is.null(vm$type) || !is.null(vm$min) || !is.null(vm$max)) {
    e$x$opts$visualMap = list(style.one(vm))
  } else {
    e$x$opts$visualMap = lapply(vm, style.one)
  }
  e
}

.pub.style.series = function(e, base_size) {
  series = e$x$opts$series
  if (is.null(series) || length(series) == 0) return(e)

  line.width = 3 * base_size / 12
  point.size = 8 * base_size / 12

  style.one = function(s) {
    if (is.null(s) || !is.list(s) || is.null(s$type)) return(s)
    stype = s$type

    if (stype %in% c("line", "lines")) {
      if (is.null(s$lineStyle)) s$lineStyle = list()
      if (is.null(s$lineStyle$width)) s$lineStyle$width = line.width
      if (is.null(s$smooth)) s$smooth = FALSE
    }

    if (stype %in% c("scatter", "effectScatter")) {
      if (is.null(s$symbol)) s$symbol = "circle"
      if (is.null(s$symbolSize)) s$symbolSize = point.size
    }

    if (stype == "bar") {
      if (is.null(s$itemStyle)) s$itemStyle = list()
      if (is.null(s$itemStyle$borderWidth)) s$itemStyle$borderWidth = 0
    }

    if (stype == "heatmap") {
      if (is.null(s$itemStyle)) s$itemStyle = list()
      if (is.null(s$itemStyle$borderColor)) s$itemStyle$borderColor = pubdarkgray
    }

    s
  }

  e$x$opts$series = lapply(series, style.one)
  e
}
