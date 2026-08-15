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
#' @param base_size Base font size, matching `theme_pub` (pts). Converted
#'   to CSS pixels for ECharts (`1pt = 4/3px`). For viewing, 12 (the
#'   default) matches `theme_pub`. Use 36 for large exported images.
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
#' @param xlim,ylim Optional length-2 numeric limits for the x- and y-axis.
#'   When set, the axis is scaled to those limits with three grid lines:
#'   the two endpoints and the midpoint, matching `pub()`. Existing
#'   `min`/`max` on the chart are used the same way if `xlim`/`ylim`
#'   are left `NULL`.
#' @param xbreaks,ybreaks Optional break locations. Overrides the
#'   three-line default when limits are set.
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
#'   e_theme_pub(type = "scatter", xlim = c(0, 6), ylim = c(0, 40))
#' }

e_theme_pub = function(e,
                       type = "scatter",
                       base_size = 12,
                       base_family = "sans",
                       facet = FALSE,
                       colors = "default",
                       caption = NULL,
                       xlim = NULL,
                       ylim = NULL,
                       xbreaks = NULL,
                       ybreaks = NULL) {
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
  fs.body = .pub.fs(base_size)
  fs.title = .pub.fs(50 / 36 * base_size)
  fs.sub = .pub.fs(42 / 36 * base_size)
  fs.cap = .pub.fs(33 / 36 * base_size)

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
    fontSize = fs.body,
    color = pubtextgray
  )

  has.title = .pub.has.title(e)
  has.subtitle = .pub.has.subtitle(e)
  has.caption = .pub.has.caption(caption)
  legend.show = .pub.legend.show(e)

  # Same tokens as theme_pub (70/50/30/20), converted from inches to CSS
  # pixels on a 6in figure. All of these scale with base_size.
  pad.t = .pub.sp(70, base_size)
  pad.r = .pub.sp(70, base_size)
  pad.b = .pub.sp(50, base_size)
  pad.l = .pub.sp(50, base_size)
  gap.title.sub = .pub.sp(30, base_size)   # 70 below title + (-40) above subtitle
  gap.after.sub = .pub.sp(70, base_size)
  gap.after.legend = .pub.sp(50, base_size)
  gap.caption = .pub.sp(50, base_size)
  name.gap = .pub.axis.name.gap(base_size)

  # Title, subtitle, legend, and caption share the y-axis title's left edge.
  ink.left = pad.l
  grid.left = pad.l + fs.body + name.gap
  grid.right = pad.r

  header = pad.t
  title.top = pad.t
  if (has.title) {
    header = header + fs.title
    header = header + if (has.subtitle) gap.title.sub else gap.after.sub
  }
  if (has.subtitle) {
    header = header + fs.sub + gap.after.sub
  }
  legend.top = header
  if (legend.show) {
    # theme_pub key height is 30px; text is base_size. Then 50px to the panel.
    legend.h = max(fs.body, .pub.sp(30, base_size))
    header = header + legend.h + gap.after.legend
  }
  grid.top = header
  grid.bottom = pad.b + name.gap + fs.body
  if (has.caption) grid.bottom = grid.bottom + fs.cap + gap.caption

  e = .pub.style.titles(
    e,
    family = family,
    base_size = base_size,
    scale = scale,
    caption = caption,
    title.top = title.top,
    left = ink.left,
    bottom = pad.b
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
    scale = scale,
    xlim = xlim,
    ylim = ylim,
    xbreaks = xbreaks,
    ybreaks = ybreaks
  )
  e = .pub.style.legend(
    e,
    type = type,
    family = family,
    base_size = base_size,
    scale = scale,
    top = legend.top,
    left = ink.left
  )
  e = .pub.style.tooltip(e, type = type, family = family, base_size = base_size)
  e = .pub.style.visualmap(e, family = family, base_size = base_size)
  e = .pub.style.series(e, type = type, base_size = base_size)
  e = .pub.fill.background(e)

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
  fs.body = .pub.fs(base_size)
  line.width = .pub.axis.line.px(base_size)
  tick.len = .pub.sp(20, base_size)
  label.margin = .pub.tick.label.margin(base_size)
  name.gap = .pub.axis.name.gap(base_size)

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
      fontSize = fs.body,
      fontFamily = family,
      margin = label.margin
    ),
    nameLocation = "middle",
    nameGap = name.gap,
    nameTextStyle = list(
      color = pubtextgray,
      fontSize = fs.body,
      fontFamily = family
    ),
    splitLine = list(
      show = TRUE,
      lineStyle = list(color = publightgray, width = .pub.axis.line.px(base_size))
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
      fontSize = fs.body,
      color = pubtextgray
    ),
    title = list(
      left = .pub.sp(50, base_size),
      itemGap = .pub.sp(30, base_size),
      textStyle = list(
        fontFamily = family,
        fontSize = .pub.fs(50 * scale),
        color = pubdarkgray,
        fontWeight = "bold"
      ),
      subtextStyle = list(
        fontFamily = family,
        fontSize = .pub.fs(42 * scale),
        color = pubmediumgray,
        fontWeight = "normal"
      )
    ),
    legend = list(
      orient = "horizontal",
      left = .pub.sp(50, base_size),
      itemWidth = .pub.sp(36, base_size),
      itemHeight = .pub.sp(30, base_size),
      itemGap = .pub.sp(20, base_size),
      textStyle = list(
        color = pubtextgray,
        fontSize = fs.body,
        fontFamily = family
      )
    ),
    tooltip = list(
      backgroundColor = pubbackgray,
      borderColor = publightgray,
      borderWidth = 1,
      textStyle = list(
        color = pubtextgray,
        fontSize = fs.body,
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
      symbol = "none",
      lineStyle = list(width = .pub.geom.px(3, base_size)),
      smooth = FALSE
    ),
    bar = list(
      itemStyle = list(borderWidth = 0, borderColor = "transparent")
    ),
    scatter = list(
      symbol = "circle",
      symbolSize = .pub.geom.px(7, base_size)
    ),
    effectScatter = list(
      symbol = "circle",
      symbolSize = .pub.geom.px(7, base_size)
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
        fontSize = fs.body,
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

# Screen CSS pixels. ggplot theme sizes are pts (72 per inch); ECharts
# fontSize and line widths are CSS px (96 per inch).
.pub.dpi = 96

.pub.fs = function(size) size * .pub.dpi / 72

# theme_pub: px = 1/1440 * 20 * base_size/36 inches on a 6in figure.
.pub.sp = function(n, base_size) {
  n * (20 / 1440) * (base_size / 36) * .pub.dpi
}

# theme_pub geom sizes are in mm (mult * base_size/36).
.pub.mm.to.px = function(mm) mm / 25.4 * .pub.dpi

.pub.geom.px = function(mult, base_size) {
  .pub.mm.to.px(mult * base_size / 36)
}

.pub.axis.line.px = function(base_size) {
  # theme_pub base_line_size = base_size * 0.35 / 36 * 3 mm.
  # Do not floor at 1px — that stops scaling at small base_size.
  .pub.mm.to.px(base_size * 0.35 / 36 * 3)
}

# ggplot: ticks (20) then axis.text margin (20) then the number.
# ECharts axisLabel.margin is from the axis line, so it must include the tick.
.pub.tick.label.margin = function(base_size) {
  .pub.sp(20, base_size) + .pub.sp(20, base_size)
}

# From the axis line: ticks + label margin + label + axis-title margin (30).
.pub.axis.name.gap = function(base_size) {
  .pub.tick.label.margin(base_size) +
    .pub.fs(base_size) + .pub.sp(30, base_size)
}

.pub.fill.background = function(e) {
  css = paste0(
    "html,body{margin:0;padding:0;overflow:hidden;",
    "background-color:", pubbackgray, " !important;}",
    ".html-widget,.echarts4r,.html-widget-static-bound{",
    "background-color:", pubbackgray, " !important;",
    "border:0;outline:0;box-shadow:none;}"
  )
  if (requireNamespace("htmlwidgets", quietly = TRUE) &&
      requireNamespace("htmltools", quietly = TRUE)) {
    e = htmlwidgets::prependContent(e, htmltools::tags$style(css))
  }
  e
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

.pub.style.titles = function(e, family, base_size, scale, caption, title.top, left, bottom) {
  titles = e$x$opts$title
  if (is.null(titles)) titles = list()

  if (length(titles) > 0) {
    for (i in seq_along(titles)) {
      titles[[i]]$left = left
      if (is.null(titles[[i]]$top) && is.null(titles[[i]]$bottom)) {
        titles[[i]]$top = title.top
      }
      titles[[i]]$itemGap = .pub.sp(30, base_size)
      titles[[i]]$textStyle = list(
        fontFamily = family,
        fontSize = .pub.fs(50 * scale),
        color = pubdarkgray,
        fontWeight = "bold"
      )
      titles[[i]]$subtextStyle = list(
        fontFamily = family,
        fontSize = .pub.fs(42 * scale),
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
        left = left,
        bottom = bottom,
        textStyle = list(
          fontFamily = family,
          fontSize = .pub.fs(33 * scale),
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
    # Margins already include axis names and tick labels, matching theme_pub.
    g$containLabel = FALSE
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
  fs.body = .pub.fs(base_size)
  line.width = .pub.axis.line.px(base_size)
  tick.len = .pub.sp(20, base_size)
  label.margin = .pub.tick.label.margin(base_size)
  name.gap = .pub.axis.name.gap(base_size)

  show.line = TRUE
  show.tick = TRUE
  show.split = (which == "y")
  show.label = TRUE
  show.name = TRUE
  label.size = fs.body

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
    if (which == "x") label.size = .pub.fs(base_size * 0.75)
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
      fontSize = fs.body,
      fontFamily = family
    ),
    splitLine = list(
      show = show.split,
      lineStyle = list(color = publightgray, width = .pub.axis.line.px(base_size))
    ),
    splitArea = list(show = FALSE)
  )
}

.pub.walk.axes = function(axes, fn) {
  if (is.null(axes)) return(axes)

  # Single unwrapped axis vs list of axes
  if (!is.null(axes$type) || !is.null(axes$show) || !is.null(axes$name) ||
      !is.null(axes$min) || !is.null(axes$max) || !is.null(axes$data)) {
    return(fn(axes))
  }
  lapply(axes, fn)
}

.pub.map.axes = function(axes, style) {
  .pub.walk.axes(axes, function(ax) {
    if (is.null(ax) || !is.list(ax)) return(ax)
    for (nm in names(style)) ax[[nm]] = style[[nm]]
    ax
  })
}

.pub.series.y = function(e) {
  series = e$x$opts$series
  if (is.null(series)) return(numeric())
  vals = lapply(series, function(s) {
    d = s$data
    if (is.null(d)) return(numeric())
    unlist(lapply(d, function(pt) {
      if (is.list(pt) && !is.null(pt$value)) {
        v = pt$value
        if (length(v) >= 2) return(suppressWarnings(as.numeric(v[length(v)])))
        return(suppressWarnings(as.numeric(v)))
      }
      suppressWarnings(as.numeric(pt))
    }))
  })
  nums = unlist(vals)
  nums[is.finite(nums)]
}

.pub.comma.formatter = function() {
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) return(NULL)
  htmlwidgets::JS(
    "function(value){ var n = Number(value); if (isNaN(n)) return value; return n.toLocaleString('en-US'); }"
  )
}

.pub.apply.lims = function(ax, lim, breaks) {
  if (is.null(ax) || !is.list(ax)) return(ax)
  if (!is.null(ax$type) && ax$type %in% c("category", "time")) return(ax)

  if (!is.null(breaks) && length(breaks) >= 2) {
    ax$min = min(breaks)
    ax$max = max(breaks)
    ax$interval = (max(breaks) - min(breaks)) / (length(breaks) - 1)
    ax$splitNumber = length(breaks) - 1
  } else {
    if (is.null(lim) || length(lim) != 2) {
      if (!is.null(ax$min) && !is.null(ax$max)) {
        lim = c(as.numeric(ax$min), as.numeric(ax$max))
      } else {
        return(ax)
      }
    }
    ax$min = min(lim)
    ax$max = max(lim)
    ax$interval = (max(lim) - min(lim)) / 2
    ax$splitNumber = 2
  }
  ax
}

.pub.style.axes = function(e, type, facet, family, base_size, scale,
                           xlim = NULL, ylim = NULL,
                           xbreaks = NULL, ybreaks = NULL) {
  x.style = .pub.axis.style("x", type, facet, family, base_size, scale)
  y.style = .pub.axis.style("y", type, facet, family, base_size, scale)

  comma = .pub.comma.formatter()
  if (!is.null(comma)) {
    x.style$axisLabel$formatter = comma
    y.style$axisLabel$formatter = comma
  }

  drop.cat.formatter = function(ax) {
    if (is.null(ax$type) || is.null(ax$axisLabel)) return(ax)
    if (ax$type == "category") ax$axisLabel$formatter = NULL
    if (ax$type == "time") ax$axisLabel$formatter = "{yyyy}"
    ax
  }

  if (!is.null(e$x$opts$xAxis)) {
    e$x$opts$xAxis = .pub.map.axes(e$x$opts$xAxis, x.style)
    e$x$opts$xAxis = .pub.walk.axes(
      e$x$opts$xAxis,
      function(ax) .pub.apply.lims(ax, xlim, xbreaks)
    )
    e$x$opts$xAxis = .pub.walk.axes(e$x$opts$xAxis, drop.cat.formatter)
  }
  if (!is.null(e$x$opts$yAxis)) {
    e$x$opts$yAxis = .pub.map.axes(e$x$opts$yAxis, y.style)
    e$x$opts$yAxis = .pub.walk.axes(
      e$x$opts$yAxis,
      function(ax) .pub.apply.lims(ax, ylim, ybreaks)
    )
    e$x$opts$yAxis = .pub.walk.axes(e$x$opts$yAxis, drop.cat.formatter)
  }
  # ggplot line axes use expand=0 and scales::extended_breaks, not 0.
  if (type %in% c("line") && is.null(ylim) && !is.null(e$x$opts$yAxis)) {
    nums = .pub.series.y(e)
    if (length(nums) >= 2) {
      br = scales::extended_breaks(5)(range(nums))
      e$x$opts$yAxis = .pub.walk.axes(e$x$opts$yAxis, function(ax) {
        if (is.null(ax$min) && is.null(ax$max) && length(br) >= 2) {
          ax$min = min(br)
          ax$max = max(br)
          ax$interval = diff(br)[1]
        }
        ax
      })
    }
  }
  e
}

.pub.style.legend = function(e, type, family, base_size, scale, top, left) {
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
  legend$left = left
  if (is.null(legend$top) || identical(legend$top, "auto")) {
    legend$top = top
  }
  fs.body = .pub.fs(base_size)
  legend$itemWidth = .pub.sp(36, base_size)
  legend$itemHeight = .pub.sp(30, base_size)
  legend$itemGap = .pub.sp(20, base_size)
  legend$icon = icon
  legend$textStyle = list(
    color = pubtextgray,
    fontSize = fs.body,
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
    fontSize = .pub.fs(base_size),
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
      fontSize = .pub.fs(base_size),
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

.pub.style.series = function(e, type, base_size) {
  series = e$x$opts$series
  if (is.null(series) || length(series) == 0) return(e)

  line.width = .pub.geom.px(3, base_size)
  point.size = .pub.geom.px(7, base_size)

  style.one = function(s) {
    if (is.null(s) || !is.list(s) || is.null(s$type)) return(s)
    stype = s$type

    if (stype %in% c("line", "lines")) {
      if (is.null(s$lineStyle)) s$lineStyle = list()
      s$lineStyle$width = line.width
      if (is.null(s$smooth)) s$smooth = FALSE
      # theme_pub lines have no point symbols
      if (is.null(s$symbol) || identical(s$symbol, "emptyCircle")) {
        s$symbol = "none"
      }
    }

    if (stype %in% c("scatter", "effectScatter")) {
      if (is.null(s$symbol)) s$symbol = "circle"
      # echarts4r sets a tiny default; keep JS sizes from a size aesthetic
      if (!inherits(s$symbolSize, "JS_EVAL")) s$symbolSize = point.size
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
