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
#'   `Arial, Helvetica, Liberation Sans, sans-serif` so it matches ggplot2
#'   `"sans"` (not the browser's default `sans-serif`, which is often
#'   Segoe UI and reads larger). `'serif'` and `'mono'` are mapped the
#'   same way; any other string is passed through.
#' @param base_line_size Base size for line elements, in mm, matching
#'   `theme_pub`. Default is `base_size * 0.35 / 36 * 3`.
#' @param base_rect_size Base size for rect elements, in mm, matching
#'   `theme_pub`. Default is `base_size * 0.35 / 36`.
#' @param legend.shift Amount to shift the legend to the right, in the
#'   same `theme_pub` pixel units (`px`). Used the same way `pub()`
#'   passes `legend.shift` into `theme_pub`.
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
#'   three-line default when limits are set. Dates are converted to
#'   UTC milliseconds for time axes.
#' @param xlabels,ylabels Optional axis-label formatters. A character
#'   vector is used as category labels. An ECharts format string
#'   (for example `"{MMM} {yyyy}"`) is passed through. A function is
#'   ignored; convert it to a format string or JS formatter first.
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
                       base_line_size = base_size * 0.35 / 36 * 3,
                       base_rect_size = base_size * 0.35 / 36,
                       legend.shift = 0,
                       facet = FALSE,
                       colors = "default",
                       caption = NULL,
                       xlim = NULL,
                       ylim = NULL,
                       xbreaks = NULL,
                       ybreaks = NULL,
                       xlabels = NULL,
                       ylabels = NULL) {
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
  line.width = .pub.mm.to.px(base_line_size)
  rect.width = .pub.mm.to.px(base_rect_size)

  theme.json = pub.echarts.theme(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size,
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

  # Title, subtitle, and caption share the plot-left ink edge
  # (theme_pub plot.title.position = "plot"). legend.shift uses the
  # same px units as theme_pub and moves only the legend.
  ink.left = pad.l
  legend.left = pad.l + .pub.sp(legend.shift, base_size)
  show.y.name = .pub.axis.has.name(e$x$opts$yAxis) &&
    !type %in% c("map", "slope", "cal")
  show.x.name = .pub.axis.has.name(e$x$opts$xAxis) &&
    !type %in% c("map", "slope")
  show.y.lab = !type %in% c("map", "slope", "cal")
  show.x.lab = !type %in% c("map", "timeline")
  y.lab.w = if (show.y.lab) .pub.axis.label.width(e$x$opts$yAxis, base_size) else 0
  x.lab.h = if (show.x.lab) fs.body else 0
  y.name.w = if (show.y.name) fs.body + .pub.sp(30, base_size) else 0
  x.name.h = if (show.x.name) fs.body + .pub.sp(30, base_size) else 0
  # grid / cal / slope (and other tickless types) have no tick length, so
  # only reserve the axis.text margin. Including a phantom tick made the
  # top-axis title sit as close to the header as a bottom axis does.
  no.tick = type %in% c("grid", "cal", "bar", "slope", "map", "pop", "dot")
  tick.lab.pad = if (no.tick) .pub.sp(20, base_size) else .pub.tick.label.margin(base_size)
  if (type == "timeline") {
    y.lab.w = .pub.timeline.label.width(ybreaks, base_size)
  }
  grid.left = pad.l + y.name.w + y.lab.w + if (show.y.lab) tick.lab.pad else 0
  grid.right = pad.r
  # Room for bar-end labels (ggplot geom_text just past the data bar).
  if (type %in% c("bar", "pop")) grid.right = grid.right + fs.body * 7
  if (type == "timeline") grid.right = grid.right + fs.body * 4
  if (type == "map") {
    grid.left = pad.l
    grid.right = pad.r
  }

  # ECharts text boxes are taller than fontSize; 1.25 matches the
  # observed title/subtitle/legend occupancy versus ggplot.
  box = 1.25
  header = pad.t
  title.top = pad.t
  if (has.title) {
    header = header + fs.title * box
    header = header + if (has.subtitle) gap.title.sub else gap.after.sub
  }
  if (has.subtitle) {
    header = header + fs.sub * box + gap.after.sub
  }
  has.vm = !is.null(e$x$opts$visualMap)
  vm.top = header
  if (has.vm) {
    # Horizontal colourbar + its numeric labels + gap before top axis.
    header = header + fs.body * 2.2 + gap.after.legend
  }
  legend.top = header
  if (legend.show) {
    # theme_pub key + text, then 50px box margin, then ggplot's 5.5pt
    # spacer between the guide box and the panel.
    # theme_pub legend.key.height = 30*px, then 50*px box margin,
    # then ggplot's default 0.2cm (~5.5pt) spacer to the panel.
    legend.h = max(fs.body, .pub.sp(30, base_size)) * box
    header = header + legend.h + gap.after.legend + 5.5 * .pub.dpi / 72
  }
  grid.top = header

  widget.w = .pub.widget.px(e)
  cap.width = max(widget.w - ink.left - pad.r, 1)
  cap.h = .pub.caption.height(caption, cap.width, base_size)
  x.band = x.name.h + x.lab.h + if (show.x.lab) tick.lab.pad else 0
  # grid / cal / slope put the discrete x title and ticks above the panel.
  # Reserve that band (and the extra header gap) only when a top axis
  # actually exists — a name and/or tick labels. Types without one keep
  # the usual header-to-panel spacing.
  x.on.top = type %in% c("grid", "cal", "slope") &&
    (show.x.name || show.x.lab)
  if (x.on.top) {
    grid.top = header + x.band + gap.after.legend
    grid.bottom = pad.b
    if (has.caption) grid.bottom = grid.bottom + cap.h + gap.caption
  } else {
    grid.bottom = pad.b + x.band
    if (has.caption) grid.bottom = grid.bottom + cap.h + gap.caption
  }
  if (type == "map") {
    grid.top = header
    grid.bottom = pad.b
    if (has.caption) grid.bottom = grid.bottom + cap.h + gap.caption
  }

  e = .pub.style.titles(
    e,
    family = family,
    base_size = base_size,
    scale = scale,
    caption = caption,
    title.top = title.top,
    left = ink.left,
    bottom = pad.b,
    width = cap.width
  )
  e = .pub.style.grid(
    e,
    type = type,
    facet = facet,
    left = if (isTRUE(facet)) pad.l else grid.left,
    right = if (isTRUE(facet)) pad.r else grid.right,
    top = if (isTRUE(facet)) header else grid.top,
    bottom = if (isTRUE(facet)) {
      if (has.caption) pad.b + cap.h + gap.caption else pad.b
    } else {
      grid.bottom
    },
    rect.width = rect.width,
    widget.w = widget.w,
    widget.h = {
      wh = suppressWarnings(as.numeric(e$height)[1])
      if (!is.finite(wh) || wh <= 0) 360 else wh
    }
  )
  e = .pub.style.axes(
    e,
    type = type,
    facet = facet,
    family = family,
    base_size = base_size,
    scale = scale,
    line.width = line.width,
    xlim = xlim,
    ylim = ylim,
    xbreaks = xbreaks,
    ybreaks = ybreaks,
    xlabels = xlabels,
    ylabels = ylabels
  )
  if (isTRUE(facet)) e = .pub.style.facet.axes(e)
  e = .pub.style.legend(
    e,
    type = type,
    family = family,
    base_size = base_size,
    scale = scale,
    top = legend.top,
    left = legend.left,
    width = max(widget.w - legend.left - pad.r, 1)
  )
  e = .pub.style.tooltip(
    e,
    type = type,
    family = family,
    base_size = base_size,
    rect.width = rect.width
  )
  e = .pub.style.visualmap(
    e,
    family = family,
    base_size = base_size,
    top = vm.top,
    left = ink.left
  )
  e = .pub.style.series(e, type = type, base_size = base_size, family = family)
  if (type == "timeline") {
    e = .pub.timeline.repel(
      e,
      base_size = base_size,
      family = family,
      top = grid.top,
      bottom = grid.bottom
    )
  }
  if (type == "map") {
    e = .pub.style.geo(
      e,
      left = grid.left,
      right = grid.right,
      top = grid.top,
      bottom = grid.bottom
    )
  }
  e = .pub.fill.background(e)
  # Publication figures are static; animation also lets webshot
  # capture a half-drawn heatmap.
  e$x$opts$animation = FALSE

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
                             base_line_size = base_size * 0.35 / 36 * 3,
                             base_rect_size = base_size * 0.35 / 36,
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
  line.width = .pub.mm.to.px(base_line_size)
  tick.len = .pub.sp(20, base_size)
  label.margin = .pub.tick.label.margin(base_size)
  name.gap = .pub.axis.name.gap(base_size)
  rect.width = .pub.mm.to.px(base_rect_size)

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
      lineStyle = list(color = publightgray, width = line.width)
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
        lineHeight = 0.9 * fs.body,
        fontFamily = family
      )
    ),
    tooltip = list(
      backgroundColor = pubbackgray,
      borderColor = publightgray,
      borderWidth = rect.width,
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
      itemStyle = list(
        borderColor = .pub.heat.border.color(),
        borderWidth = .pub.heat.border.px()
      )
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
    # Match ggplot2 "sans" (Arial on Windows, Helvetica on macOS).
    # Bare CSS sans-serif is often Segoe UI, which has a larger x-height.
    sans = "Arial, Helvetica, Liberation Sans, sans-serif",
    serif = "Times New Roman, Times, serif",
    mono = "Courier New, Courier, monospace",
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

.pub.axis.line.px = function(base_size, base_line_size = NULL) {
  # theme_pub base_line_size = base_size * 0.35 / 36 * 3 mm.
  # Do not floor at 1px — that stops scaling at small base_size.
  if (is.null(base_line_size)) base_line_size = base_size * 0.35 / 36 * 3
  .pub.mm.to.px(base_line_size)
}

.pub.widget.px = function(e) {
  w = e$width
  w = suppressWarnings(as.numeric(w)[1])
  if (!is.finite(w) || w <= 0) return(360)
  w
}

# theme_pub caption has a 50*px top margin. At narrow widget widths the
# caption wraps; reserve every line so it does not sit on the x-axis title.
.pub.caption.height = function(caption, width, base_size) {
  if (!.pub.has.caption(caption)) return(0)
  fs = .pub.fs(33 / 36 * base_size)
  txt = as.character(caption)[1]
  cpl = max(8, floor(width / max(fs * 0.5, 1)))
  nlines = max(1, ceiling(nchar(txt) / cpl))
  nlines * fs * 1.25
}

.pub.axis.has.name = function(ax) {
  if (is.null(ax) || !is.list(ax)) return(FALSE)
  nm = ax$name
  if (is.null(nm) && is.list(ax[[1]])) nm = ax[[1]]$name
  !is.null(nm) && nzchar(as.character(nm)[1])
}

.pub.axis.is.cat = function(ax) {
  if (is.null(ax) || !is.list(ax)) return(FALSE)
  if (is.null(ax$type) && is.null(ax$data) && is.null(ax$name) &&
      is.list(ax[[1]])) {
    ax = ax[[1]]
  }
  (!is.null(ax$type) && identical(ax$type, "category")) || !is.null(ax$data)
}

.pub.axis.categories = function(ax) {
  if (is.null(ax) || !is.list(ax)) return(character())
  labs = ax$data
  if (is.null(labs) && is.list(ax[[1]])) labs = ax[[1]]$data
  if (is.null(labs)) return(character())
  as.character(unlist(labs))
}

# Approximate category / numeric tick-label width in CSS px.
.pub.axis.label.width = function(ax, base_size) {
  fs.body = .pub.fs(base_size)
  labs = .pub.axis.categories(ax)
  nch = if (length(labs) > 0) max(nchar(labs), na.rm = TRUE) else 4
  nch = max(nch, 1)
  nch * fs.body * 0.55
}

# Timeline y ticks are "%b %d, %Y" (e.g. "Jan 1, 2026"). The generic
# width helper only sees a value axis and reserves four characters.
.pub.timeline.label.width = function(ybreaks, base_size) {
  labs = "Sep 30, 2026"
  if (!is.null(ybreaks) && length(ybreaks) > 0) {
    if (inherits(ybreaks, "Date") || inherits(ybreaks, "POSIXt")) {
      labs = format(ybreaks, "%b %d, %Y")
    } else {
      nums = suppressWarnings(as.numeric(ybreaks))
      if (length(nums) > 0 && all(is.finite(nums)) && max(abs(nums)) < 1e6) {
        labs = format(as.Date(nums, origin = "1970-01-01"), "%b %d, %Y")
      }
    }
  }
  max(nchar(as.character(labs)), na.rm = TRUE) * .pub.fs(base_size) * 0.6
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

# ggplot geom_tile(linewidth = 0.4) is 0.4 mm, and adjacent tiles share
# one stroke. ECharts draws a full border inside every cell, so a 1px
# (or even 0.75px) width reads as a heavy double line. A 0.5px hairline
# matches the ggplot weight at the 360px compare size.
.pub.heat.border.px = function() {
  0.4
}

.pub.heat.border.color = function() {
  pubtextgray
}

.pub.fill.background = function(e) {
  # Style only the widget. Never set overflow on html/body — that
  # freezes scroll when the chart is embedded in a Quarto/Rmd page.
  css = paste0(
    ".html-widget,.echarts4r,.html-widget-static-bound{",
    "background-color:", pubbackgray, " !important;",
    "border:0;outline:0;box-shadow:none;overflow:hidden;}"
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

.pub.style.titles = function(e, family, base_size, scale, caption, title.top, left, bottom,
                             width = 280) {
  titles = e$x$opts$title
  if (is.null(titles)) titles = list()

  if (length(titles) > 0) {
    for (i in seq_along(titles)) {
      titles[[i]]$left = left
      if (is.null(titles[[i]]$top) && is.null(titles[[i]]$bottom)) {
        titles[[i]]$top = title.top
      }
      titles[[i]]$itemGap = .pub.sp(30, base_size)
      titles[[i]]$padding = 0
      titles[[i]]$textStyle = list(
        fontFamily = family,
        fontSize = .pub.fs(50 * scale),
        lineHeight = .pub.fs(50 * scale),
        color = pubdarkgray,
        fontWeight = "bold",
        width = width,
        overflow = "break"
      )
      titles[[i]]$subtextStyle = list(
        fontFamily = family,
        fontSize = .pub.fs(42 * scale),
        lineHeight = .pub.fs(42 * scale),
        color = pubmediumgray,
        fontWeight = "normal",
        width = width,
        overflow = "break"
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
        padding = 0,
        textStyle = list(
          fontFamily = family,
          fontSize = .pub.fs(33 * scale),
          lineHeight = .pub.fs(33 * scale),
          color = pubmediumgray,
          fontWeight = "normal",
          width = width,
          overflow = "break"
        )
      ))
    )
  }

  if (length(titles) > 0) e$x$opts$title = titles
  e
}

.pub.style.grid = function(e, type, facet, left, right, top, bottom,
                           rect.width = 1, widget.w = 360, widget.h = 360) {
  # theme_pub panel.border uses base_rect_size; pairs and facet = TRUE.
  border.width = if (isTRUE(facet) || type %in% c("pairs")) rect.width else 0
  grids = e$x$opts$grid

  pct = function(x) {
    if (is.null(x)) return(NA_real_)
    if (is.numeric(x)) return(x / 100)
    as.numeric(sub("%", "", as.character(x)[1])) / 100
  }

  style.one = function(g) {
    if (is.null(g)) g = list()
    is.facet.panel = isTRUE(facet) &&
      !is.null(g$top) &&
      (is.character(g$top) || (is.numeric(g$width) && !is.null(g$height)))
    if (is.facet.panel && !is.na(pct(g$top))) {
      inner.w = max(widget.w - left - right, 1)
      inner.h = max(widget.h - top - bottom, 1)
      g$left = left + pct(g$left) * inner.w
      g$top = top + pct(g$top) * inner.h
      g$width = pct(g$width) * inner.w
      g$height = pct(g$height) * inner.h
      g$containLabel = TRUE
    } else if (is.null(grids) || length(grids) <= 1) {
      g$left = left
      g$right = right
      g$top = top
      g$bottom = bottom
      g$containLabel = FALSE
    } else {
      g$containLabel = FALSE
    }
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

# ggplot facet_wrap: y labels on the left column, x labels on the
# bottom row, one axis title each. Inner panels should not reserve
# that space again.
.pub.style.facet.axes = function(e) {
  grids = e$x$opts$grid
  if (is.null(grids) || length(grids) <= 1) return(e)
  if (!is.null(grids$left) || !is.null(grids$top) && !is.null(grids$containLabel) &&
      is.null(grids[[1]]$left)) {
    return(e)
  }

  n = length(grids)
  lefts = vapply(grids, function(g) {
    v = suppressWarnings(as.numeric(g$left)[1])
    if (!is.finite(v)) Inf else v
  }, numeric(1))
  tops = vapply(grids, function(g) {
    v = suppressWarnings(as.numeric(g$top)[1])
    if (!is.finite(v)) Inf else v
  }, numeric(1))
  if (!all(is.finite(lefts)) || !all(is.finite(tops))) return(e)
  min.l = min(lefts)
  max.t = max(tops)
  min.t = min(tops)
  tol = 2

  hide.name = function(ax) {
    if (is.null(ax) || !is.list(ax)) return(ax)
    ax$name = ""
    if (is.null(ax$nameTextStyle)) ax$nameTextStyle = list()
    ax$nameTextStyle$color = "transparent"
    ax
  }
  hide.lab = function(ax) {
    if (is.null(ax) || !is.list(ax)) return(ax)
    if (is.null(ax$axisLabel)) ax$axisLabel = list()
    ax$axisLabel$show = FALSE
    ax
  }

  xa = e$x$opts$xAxis
  ya = e$x$opts$yAxis
  if (is.list(xa) && length(xa) == n && is.list(xa[[1]]) &&
      (is.null(xa$type) || !is.null(xa[[1]]$type) || !is.null(xa[[1]]$gridIndex))) {
    for (i in seq_len(n)) {
      left.col = abs(lefts[i] - min.l) <= tol
      bot.row = abs(tops[i] - max.t) <= tol
      top.left = left.col && abs(tops[i] - min.t) <= tol
      if (!left.col) {
        ya[[i]] = hide.name(ya[[i]])
        ya[[i]] = hide.lab(ya[[i]])
      } else if (!top.left) {
        ya[[i]] = hide.name(ya[[i]])
      }
      if (!bot.row) {
        xa[[i]] = hide.name(xa[[i]])
        xa[[i]] = hide.lab(xa[[i]])
      } else if (!left.col) {
        xa[[i]] = hide.name(xa[[i]])
      }
    }
    e$x$opts$xAxis = xa
    e$x$opts$yAxis = ya
  }
  e
}

# Fit the geo layer to the same panel box as the grid. layoutSize > 100%
# and layoutCenter zoom/shift crop the coasts.
.pub.style.geo = function(e, left, right, top, bottom) {
  geo = e$x$opts$geo
  if (is.null(geo)) return(e)

  style.one = function(g) {
    if (is.null(g) || !is.list(g)) return(g)
    g$left = left
    g$right = right
    g$top = top
    g$bottom = bottom
    g$layoutCenter = NULL
    g$layoutSize = NULL
    g$zoom = 1
    g
  }

  if (!is.null(geo$map) || !is.null(geo$boundingCoords) ||
      !is.null(geo$layoutCenter) || !is.null(geo$left)) {
    e$x$opts$geo = style.one(geo)
  } else {
    e$x$opts$geo = lapply(geo, style.one)
  }
  e
}

.pub.axis.style = function(which, type, facet, family, base_size, scale,
                           line.width = NULL) {
  fs.body = .pub.fs(base_size)
  if (is.null(line.width)) line.width = .pub.axis.line.px(base_size)
  tick.len = .pub.sp(20, base_size)
  label.margin = .pub.tick.label.margin(base_size)
  name.gap = .pub.axis.name.gap(base_size)

  # Default theme_pub: x grid off, y grid on, ticks and axis line on.
  show.line = TRUE
  show.tick = TRUE
  show.split = (which == "y")
  show.label = TRUE
  show.name = TRUE
  label.size = fs.body

  if (type == "scatter") {
    show.split = TRUE
  }
  if (type == "pairs") {
    show.split = TRUE
    show.line = TRUE
  }
  if (type == "line") {
    show.split = (which == "y")
  }
  if (type == "bar") {
    show.line = FALSE
    show.tick = FALSE
    show.split = FALSE
    # pub() blanks x tick labels when bar values are drawn as text
    if (which == "x") show.label = FALSE
  }
  if (type %in% c("pop", "dot")) {
    # theme_pub blanks all ticks and the axis line; y grid stays on.
    show.line = FALSE
    show.tick = FALSE
    show.split = (which == "y")
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
  # theme_pub puts discrete x on top for grid, slope, and calendar
  x.top = type %in% c("slope", "grid", "cal") && which == "x"
  if (isTRUE(facet)) {
    show.line = TRUE
  }
  if (!show.tick) {
    label.margin = .pub.sp(20, base_size)
    name.gap = label.margin + label.size + .pub.sp(30, base_size)
  }

  out = list(
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
      alignWithLabel = TRUE,
      lineStyle = list(color = publightgray, width = line.width)
    ),
    splitArea = list(show = FALSE)
  )
  if (isTRUE(x.top)) out$position = "top"
  out
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
    keep.fmt = !is.null(ax$axisLabel) &&
      inherits(ax$axisLabel$formatter, "JS_EVAL")
    old.fmt = if (keep.fmt) ax$axisLabel$formatter else NULL
    for (nm in names(style)) ax[[nm]] = style[[nm]]
    if (keep.fmt && !is.null(ax$axisLabel)) {
      ax$axisLabel$formatter = old.fmt
    }
    ax
  })
}

.pub.series.xy = function(e, which = "y") {
  series = e$x$opts$series
  if (is.null(series)) return(numeric())
  idx = if (which == "x") 1 else 2
  vals = lapply(series, function(s) {
    d = s$data
    if (is.null(d)) return(numeric())
    unlist(lapply(d, function(pt) {
      if (is.list(pt) && !is.null(pt$value)) {
        v = pt$value
        if (length(v) >= idx) return(suppressWarnings(as.numeric(v[idx])))
        return(suppressWarnings(as.numeric(v)))
      }
      if (is.list(pt) && length(pt) >= idx) {
        return(suppressWarnings(as.numeric(pt[[idx]])))
      }
      if (which == "x") return(NA_real_)
      suppressWarnings(as.numeric(pt))
    }))
  })
  nums = unlist(vals)
  nums[is.finite(nums)]
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

.pub.to.axis.num = function(x) {
  if (inherits(x, "Date")) {
    return(as.numeric(as.POSIXct(x, tz = "UTC")) * 1000)
  }
  if (inherits(x, "POSIXt")) return(as.numeric(x) * 1000)
  as.numeric(x)
}

.pub.apply.lims = function(ax, lim, breaks) {
  if (is.null(ax) || !is.list(ax)) return(ax)
  if (!is.null(ax$type) && ax$type == "category") return(ax)

  if (!is.null(breaks) && length(breaks) >= 2) {
    br = .pub.to.axis.num(breaks)
    ax$min = min(br)
    ax$max = max(br)
    ax$interval = (max(br) - min(br)) / (length(br) - 1)
    ax$splitNumber = length(br) - 1
  } else {
    if (is.null(lim) || length(lim) != 2) return(ax)
    lim = .pub.to.axis.num(lim)
    ax$min = min(lim)
    ax$max = max(lim)
    ax$interval = (max(lim) - min(lim)) / 2
    ax$splitNumber = 2
  }
  ax
}

# Keep every category band. Auto interval will otherwise drop later
# heatmap columns when day-number labels do not fit. Numeric labels
# match ggplot's even-day ticks; named labels keep hideOverlap.
.pub.grid.axis.labels = function(ax) {
  if (is.null(ax) || !is.list(ax)) return(ax)
  if (is.null(ax$axisLabel)) ax$axisLabel = list()
  ax$axisLabel$interval = 0
  labs = ax$data
  if (is.null(labs) || length(labs) == 0) {
    ax$axisLabel$hideOverlap = TRUE
    return(ax)
  }
  nums = suppressWarnings(as.numeric(as.character(unlist(labs))))
  if (length(nums) > 2 && all(is.finite(nums))) {
    ax$axisLabel$hideOverlap = FALSE
    if (requireNamespace("htmlwidgets", quietly = TRUE)) {
      ax$axisLabel$formatter = htmlwidgets::JS(
        "function(v){ var n=Number(v); return (isFinite(n) && n%2===0)?String(n):''; }"
      )
    }
  } else {
    ax$axisLabel$hideOverlap = TRUE
  }
  ax
}

.pub.apply.labels = function(ax, labels) {
  if (is.null(ax) || is.null(labels) || is.function(labels)) return(ax)
  if (is.null(ax$axisLabel)) ax$axisLabel = list()
  if (is.character(labels) && length(labels) == 1 && grepl("\\{", labels)) {
    ax$axisLabel$formatter = labels
    return(ax)
  }
  if (is.character(labels) && length(labels) > 1) {
    if (!is.null(ax$type) && ax$type == "category") {
      ax$data = labels
      ax$axisLabel$formatter = NULL
    }
  }
  ax
}

.pub.style.axes = function(e, type, facet, family, base_size, scale,
                           line.width = NULL,
                           xlim = NULL, ylim = NULL,
                           xbreaks = NULL, ybreaks = NULL,
                           xlabels = NULL, ylabels = NULL) {
  x.style = .pub.axis.style(
    "x", type, facet, family, base_size, scale, line.width = line.width
  )
  y.style = .pub.axis.style(
    "y", type, facet, family, base_size, scale, line.width = line.width
  )

  comma = .pub.comma.formatter()
  if (!is.null(comma)) {
    x.style$axisLabel$formatter = comma
    y.style$axisLabel$formatter = comma
  }

  drop.cat.formatter = function(ax) {
    if (is.null(ax$axisLabel)) return(ax)
    is.cat = (!is.null(ax$type) && ax$type == "category") ||
      !is.null(ax$data)
    if (is.cat) ax$axisLabel$formatter = NULL
    if (!is.null(ax$type) && ax$type == "time") {
      ax$axisLabel$formatter = "{yyyy}"
    }
    ax
  }

  force.cat = function(ax) {
    if (is.null(ax) || !is.list(ax)) return(ax)
    if (!is.null(ax$data)) ax$type = "category"
    ax
  }

  if (!is.null(e$x$opts$xAxis)) {
    e$x$opts$xAxis = .pub.map.axes(e$x$opts$xAxis, x.style)
    e$x$opts$xAxis = .pub.walk.axes(e$x$opts$xAxis, force.cat)
    e$x$opts$xAxis = .pub.walk.axes(
      e$x$opts$xAxis,
      function(ax) .pub.apply.lims(ax, xlim, xbreaks)
    )
    e$x$opts$xAxis = .pub.walk.axes(e$x$opts$xAxis, drop.cat.formatter)
    e$x$opts$xAxis = .pub.walk.axes(
      e$x$opts$xAxis,
      function(ax) .pub.apply.labels(ax, xlabels)
    )
    if (type %in% c("grid", "cal")) {
      e$x$opts$xAxis = .pub.walk.axes(e$x$opts$xAxis, .pub.grid.axis.labels)
    }
  }
  if (!is.null(e$x$opts$yAxis)) {
    e$x$opts$yAxis = .pub.map.axes(e$x$opts$yAxis, y.style)
    e$x$opts$yAxis = .pub.walk.axes(e$x$opts$yAxis, force.cat)
    e$x$opts$yAxis = .pub.walk.axes(
      e$x$opts$yAxis,
      function(ax) .pub.apply.lims(ax, ylim, ybreaks)
    )
    e$x$opts$yAxis = .pub.walk.axes(e$x$opts$yAxis, drop.cat.formatter)
    e$x$opts$yAxis = .pub.walk.axes(
      e$x$opts$yAxis,
      function(ax) .pub.apply.labels(ax, ylabels)
    )
    if (type %in% c("grid", "cal")) {
      e$x$opts$yAxis = .pub.walk.axes(e$x$opts$yAxis, .pub.grid.axis.labels)
    }
  }
  # ggplot line axes use expand=0 and scales::extended_breaks, not 0.
  apply.breaks = function(axes, nums, expand = 0) {
    if (is.null(axes) || length(nums) < 2) return(axes)
    rng = range(nums)
    if (expand > 0) rng = rng + c(-1, 1) * diff(rng) * expand
    br = scales::extended_breaks(5)(rng)
    br = br[br >= min(rng) - 1e-8 * diff(rng) & br <= max(rng) + 1e-8 * diff(rng)]
    interval = if (length(br) >= 2) stats::median(diff(br)) else diff(rng) / 2
    .pub.walk.axes(axes, function(ax) {
      if (!is.null(ax$type) && ax$type %in% c("category", "time")) return(ax)
      ax$min = min(rng)
      ax$max = max(rng)
      ax$interval = interval
      nice.max = min(rng) + interval * round((max(rng) - min(rng)) / interval)
      if (is.null(ax$axisLabel)) ax$axisLabel = list()
      if (abs(max(rng) - nice.max) > 1e-6 * max(1, abs(max(rng)))) {
        ax$axisLabel$showMaxLabel = FALSE
      }
      ax
    })
  }
  if (type %in% c("line") && is.null(ylim)) {
    e$x$opts$yAxis = apply.breaks(e$x$opts$yAxis, .pub.series.y(e))
  }
  if (type %in% c("line") && is.null(xlim)) {
    e$x$opts$xAxis = apply.breaks(e$x$opts$xAxis, .pub.series.xy(e, "x"))
  }
  if (type == "slope" && is.null(ylim)) {
    e$x$opts$yAxis = apply.breaks(e$x$opts$yAxis, .pub.series.y(e), expand = 0.1)
  }
  # Stems / whiskers sit on the tick. Default category boundaryGap
  # puts the grid between rows.
  if (type %in% c("pop", "dot")) {
    no.gap = function(ax) {
      if (is.null(ax) || !is.list(ax)) return(ax)
      if (.pub.axis.is.cat(ax)) ax$boundaryGap = FALSE
      ax
    }
    if (!is.null(e$x$opts$xAxis)) {
      e$x$opts$xAxis = .pub.walk.axes(e$x$opts$xAxis, no.gap)
    }
    if (!is.null(e$x$opts$yAxis)) {
      e$x$opts$yAxis = .pub.walk.axes(e$x$opts$yAxis, no.gap)
    }
  }
  e
}

.pub.style.legend = function(e, type, family, base_size, scale, top, left,
                             width = NULL) {
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
  legend$top = top
  legend$padding = 0
  fs.body = .pub.fs(base_size)
  legend$itemWidth = .pub.sp(36, base_size)
  legend$itemHeight = .pub.sp(30, base_size)
  legend$itemGap = .pub.sp(20, base_size)
  if (!is.null(width)) legend$width = width
  legend$icon = icon
  legend$textStyle = list(
    color = pubtextgray,
    fontSize = fs.body,
    lineHeight = fs.body,
    fontFamily = family
  )
  e$x$opts$legend = legend
  e
}

.pub.style.tooltip = function(e, type, family, base_size, rect.width = 1) {
  tip = e$x$opts$tooltip
  if (is.null(tip)) tip = list()
  if (is.null(tip$trigger)) {
    tip$trigger = if (type %in% c("line", "hist")) "axis" else "item"
  }
  tip$backgroundColor = pubbackgray
  tip$borderColor = publightgray
  tip$borderWidth = rect.width
  tip$textStyle = list(
    color = pubtextgray,
    fontSize = .pub.fs(base_size),
    fontFamily = family
  )
  e$x$opts$tooltip = tip
  e
}

.pub.style.visualmap = function(e, family, base_size, top = 0, left = 0) {
  vm = e$x$opts$visualMap
  if (is.null(vm)) return(e)
  fs.body = .pub.fs(base_size)

  style.one = function(v) {
    if (is.null(v) || !is.list(v)) return(v)
    if (is.null(v$inRange)) v$inRange = list()
    if (is.null(v$inRange$color)) v$inRange$color = c(publightgray, pubblue)
    v$orient = "horizontal"
    # Centered under the subtitle, not on the y-axis.
    v$left = "center"
    v$top = top
    v$bottom = NULL
    v$right = NULL
    # ECharts 4: itemWidth is thickness, itemHeight is length.
    v$itemWidth = max(8, fs.body * 0.7)
    v$itemHeight = .pub.sp(480, base_size)
    v$textStyle = list(
      color = pubtextgray,
      fontSize = fs.body,
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

.pub.style.series = function(e, type, base_size, family = "Arial, Helvetica, Liberation Sans, sans-serif") {
  series = e$x$opts$series
  if (is.null(series) || length(series) == 0) return(e)

  line.width = .pub.geom.px(3, base_size)
  point.size = .pub.geom.px(7, base_size)
  # theme_pub geom_text size = 0.35 * base_size mm ≈ base_size pt
  label.size = .pub.fs(base_size)
  heat.border = .pub.heat.border.px()

  if (type %in% c("pop", "dot")) {
    point.size = .pub.mm.to.px(3.5 * 3 * base_size / 36)
    line.width = .pub.mm.to.px(0.5 * 3 * base_size / 36)
  }
  # Vertical pop: category x, value y (stems up). Horizontal: value x,
  # category y (stems right). Match ggplot geom_text vjust / hjust.
  pop.vertical = type == "pop" &&
    .pub.axis.is.cat(e$x$opts$xAxis) &&
    !.pub.axis.is.cat(e$x$opts$yAxis)
  pop.label.pos = if (pop.vertical) "top" else "right"
  if (type == "slope") {
    point.size = .pub.mm.to.px(3.5 * 3 * base_size / 36)
    line.width = .pub.mm.to.px(1 * 3 * base_size / 36)
  }

  bar.idx = which(vapply(series, function(x) {
    is.list(x) && identical(x$type, "bar")
  }, logical(1)))
  n.bar = length(bar.idx)

  style.one = function(s, i) {
    if (is.null(s) || !is.list(s) || is.null(s$type)) return(s)
    stype = s$type

    if (stype %in% c("line", "lines")) {
      if (is.null(s$lineStyle)) s$lineStyle = list()
      s$lineStyle$width = line.width
      if (is.null(s$smooth)) s$smooth = FALSE
      # theme_pub lines have no point symbols unless type is slope
      if (type == "slope") {
        if (is.null(s$symbol) || identical(s$symbol, "emptyCircle")) {
          s$symbol = "circle"
        }
        if (!inherits(s$symbolSize, "JS_EVAL")) s$symbolSize = point.size
      } else if (is.null(s$symbol) || identical(s$symbol, "emptyCircle")) {
        s$symbol = "none"
      }
    }

    if (stype %in% c("scatter", "effectScatter")) {
      if (is.null(s$symbol) || identical(s$symbol, "emptyCircle")) {
        s$symbol = "circle"
      }
      # Keep a size aesthetic (JS) or an explicit path/hex symbolSize.
      if (!inherits(s$symbolSize, "JS_EVAL") &&
          (is.null(s$symbolSize) ||
            (is.numeric(s$symbolSize) && length(s$symbolSize) == 1 &&
              s$symbolSize <= 4))) {
        s$symbolSize = point.size
      }
      if (type == "pop") {
        if (is.null(s$label)) s$label = list()
        if (is.null(s$label$show)) s$label$show = TRUE
        s$label$position = pop.label.pos
      }
    }

    if (stype == "bar") {
      if (is.null(s$itemStyle)) s$itemStyle = list()
      if (is.null(s$itemStyle$borderWidth)) s$itemStyle$borderWidth = 0
      if (type == "hist") {
        s$barWidth = "88%"
        s$itemStyle$borderColor = pubbackgray
        s$itemStyle$borderWidth = 1
      }
      if (type %in% c("pop", "dot") && is.null(s$barWidth)) {
        s$barWidth = max(line.width, 1)
      }
      # theme_pub bar template: gray background geom_col + data
      # geom_col, both width 0.8, overlaid (not grouped).
      if (type == "bar") {
        this.bar = match(i, bar.idx)
        if (is.null(s$barWidth)) s$barWidth = "80%"
        if (!is.null(s$label) && !isFALSE(s$label$show)) {
          s$label$position = "right"
        }
        if (n.bar > 1) {
          s$barGap = "-100%"
          s$clip = FALSE
          if (is.null(s$z)) s$z = this.bar
          if (!is.na(this.bar) && this.bar < n.bar) {
            if (is.null(s$label)) s$label = list()
            s$label$show = FALSE
          } else if (!is.na(this.bar)) {
            if (is.null(s$label)) s$label = list()
            s$label$position = "right"
            s$label$show = TRUE
            s$label$hideOverlap = FALSE
            s$label$distance = 4
            if (is.null(s$label$formatter)) {
              s$label$formatter = htmlwidgets::JS(
                "function(p){ var v=p.value; var n=Array.isArray(v)?Number(v[0]):Number(v); if(!isFinite(n)) n=Array.isArray(v)?Number(v[v.length-1]):NaN; return isFinite(n)?(Math.round(n*100)/100).toString():''; }"
              )
            }
            s$data = lapply(s$data, function(pt) {
              if (!is.list(pt)) pt = list(value = pt)
              if (is.null(pt$label)) pt$label = list()
              pt$label$show = TRUE
              pt$label$position = "right"
              pt
            })
          }
        }
      }
    }

    if (stype == "heatmap") {
      if (is.null(s$itemStyle)) s$itemStyle = list()
      s$itemStyle$borderColor = .pub.heat.border.color()
      s$itemStyle$borderWidth = heat.border
      if (is.null(s$emphasis)) s$emphasis = list()
      if (is.null(s$emphasis$itemStyle)) s$emphasis$itemStyle = list()
      s$emphasis$itemStyle$borderColor = .pub.heat.border.color()
      s$emphasis$itemStyle$borderWidth = heat.border
      s$progressive = 0
      s$animation = FALSE
    }

    if (!is.null(s$label) && is.list(s$label)) {
      if (is.null(s$label$fontSize)) s$label$fontSize = label.size
      if (is.null(s$label$color)) s$label$color = pubtextgray
      if (is.null(s$label$fontFamily)) s$label$fontFamily = family
      if (is.null(s$label$lineHeight)) s$label$lineHeight = 0.9 * label.size
    }

    s
  }

  e$x$opts$series = Map(style.one, series, seq_along(series))
  .pub.heatmap.fill.na(e, heat.border)
}

# ECharts heatmap skips null z, and visualMap owns heatmap fill, so
# itemStyle.color cannot paint NA tiles. Draw missing cells as rect
# scatter (not a heatmap) with ggplot's na.value and the same border.
.pub.heatmap.fill.na = function(e, heat.border = .pub.heat.border.px()) {
  series = e$x$opts$series
  if (is.null(series) || length(series) == 0) return(e)

  xcats = .pub.axis.categories(e$x$opts$xAxis)
  ycats = .pub.axis.categories(e$x$opts$yAxis)

  pt.xy = function(pt) {
    if (!is.list(pt)) return(NULL)
    v = pt$value
    if (is.null(v) || length(v) < 2) return(NULL)
    c(as.character(v[1]), as.character(v[2]))
  }
  pt.na = function(pt) {
    if (!is.list(pt)) return(TRUE)
    v = pt$value
    if (is.null(v) || length(v) < 3) return(TRUE)
    z = suppressWarnings(as.numeric(v[length(v)]))
    !is.finite(z)
  }

  na.pts = list()
  heat.idx = integer()
  for (i in seq_along(series)) {
    s = series[[i]]
    if (!is.list(s) || !identical(s$type, "heatmap")) next
    if (!is.null(s$coordinateSystem) && !identical(s$coordinateSystem, "cartesian2d")) {
      next
    }
    heat.idx = c(heat.idx, i - 1L)
    data = s$data
    if (is.null(data)) data = list()
    keep = list()
    seen = character()
    for (pt in data) {
      xy = pt.xy(pt)
      if (is.null(xy)) {
        keep = c(keep, list(pt))
        next
      }
      seen = c(seen, paste(xy[1], xy[2], sep = "\r"))
      if (pt.na(pt)) {
        na.pts = c(na.pts, list(xy))
      } else {
        keep = c(keep, list(pt))
      }
    }
    if (length(xcats) > 0 && length(ycats) > 0) {
      for (xi in xcats) {
        for (yi in ycats) {
          key = paste(xi, yi, sep = "\r")
          if (!key %in% seen) na.pts = c(na.pts, list(c(xi, yi)))
        }
      }
    }
    s$data = keep
    series[[i]] = s
  }
  if (length(na.pts) == 0) {
    e$x$opts$series = series
    return(e)
  }

  w = .pub.widget.px(e)
  h = {
    hh = suppressWarnings(as.numeric(e$height)[1])
    if (!is.finite(hh) || hh <= 0) 360 else hh
  }
  g = e$x$opts$grid
  if (!is.null(g) && (is.null(g$left) && is.null(g$top))) g = g[[1]]
  left = if (is.numeric(g$left)) g$left else 0
  right = if (is.numeric(g$right)) g$right else 0
  top = if (is.numeric(g$top)) g$top else 0
  bottom = if (is.numeric(g$bottom)) g$bottom else 0
  nx = max(length(xcats), 1)
  ny = max(length(ycats), 1)
  band.w = max((w - left - right) / nx - heat.border, 2)
  band.h = max((h - top - bottom) / ny - heat.border, 2)

  na.data = lapply(na.pts, function(xy) {
    list(
      value = list(xy[1], xy[2]),
      itemStyle = list(
        color = pubmediumgray,
        borderColor = .pub.heat.border.color(),
        borderWidth = heat.border,
        opacity = 1
      )
    )
  })
  series = c(
    series,
    list(list(
      type = "scatter",
      data = na.data,
      symbol = "rect",
      symbolSize = c(band.w, band.h),
      silent = TRUE,
      tooltip = list(show = FALSE),
      animation = FALSE,
      legendHoverLink = FALSE,
      z = 10,
      itemStyle = list(
        color = pubmediumgray,
        borderColor = .pub.heat.border.color(),
        borderWidth = heat.border,
        opacity = 1
      )
    ))
  )
  e$x$opts$series = series

  vm = e$x$opts$visualMap
  if (is.null(vm) || length(heat.idx) == 0) return(e)
  set.idx = function(v) {
    if (is.null(v) || !is.list(v)) return(v)
    if (is.null(v$seriesIndex)) {
      # A lone 0 is falsy in JS (`if (seriesIndex)`), so visualMap
      # would apply to every series — including the NA tiles.
      idx = as.integer(heat.idx)
      if (length(idx) == 1) idx = c(idx, idx)
      v$seriesIndex = idx
    }
    v
  }
  if (!is.null(vm$type) || !is.null(vm$min) || !is.null(vm$max)) {
    e$x$opts$visualMap = list(set.idx(vm))
  } else {
    e$x$opts$visualMap = lapply(vm, set.idx)
  }
  e
}

# 1-D pack along y, then slide back toward the original positions.
.pub.repel.1d = function(y, min.gap, lo = NULL, hi = NULL) {
  n = length(y)
  if (n < 2 || !is.finite(min.gap) || min.gap <= 0) return(y)
  o = order(y)
  z = as.numeric(y[o])
  y0 = z
  for (i in 2:n) {
    if (is.finite(z[i]) && is.finite(z[i - 1]) && z[i] < z[i - 1] + min.gap) {
      z[i] = z[i - 1] + min.gap
    }
  }
  z = z + mean(y0 - z)
  if (!is.null(lo) && is.finite(lo) && z[1] < lo) z = z + (lo - z[1])
  if (!is.null(hi) && is.finite(hi) && z[n] > hi) z = z - (z[n] - hi)
  if (!is.null(lo) && is.finite(lo) && z[1] < lo) {
    z[1] = lo
    for (i in 2:n) z[i] = max(z[i], z[i - 1] + min.gap)
  }
  out = y
  out[o] = z
  out
}

.pub.pt.xy = function(pt) {
  if (is.list(pt) && !is.null(pt$value)) v = pt$value else v = pt
  if (is.null(v) || length(v) < 2) return(c(NA_real_, NA_real_))
  c(suppressWarnings(as.numeric(v[1])), suppressWarnings(as.numeric(v[2])))
}

.pub.pt.name = function(pt) {
  if (!is.list(pt)) return("")
  if (!is.null(pt$name) && nzchar(as.character(pt$name)[1])) {
    return(as.character(pt$name)[1])
  }
  if (!is.null(pt$label) && is.list(pt$label) &&
      is.character(pt$label$formatter) && !inherits(pt$label$formatter, "JS_EVAL")) {
    return(pt$label$formatter[1])
  }
  ""
}

# geom_label_repel(direction = "y"): keep points, slide labels along y,
# and draw a leader when a label has to move.
.pub.timeline.repel = function(e, base_size, family, top, bottom) {
  series = e$x$opts$series
  if (is.null(series) || length(series) == 0) return(e)

  xs = numeric()
  ys = numeric()
  labs = character()
  cols = character()
  src = integer()
  pti = integer()
  for (i in seq_along(series)) {
    s = series[[i]]
    if (!is.list(s) || !identical(s$type, "scatter")) next
    if (is.null(s$data)) next
    col = s$itemStyle$color
    if (is.null(col)) col = s$color
    if (is.null(col)) col = pubtextgray
    for (j in seq_along(s$data)) {
      xy = .pub.pt.xy(s$data[[j]])
      if (!all(is.finite(xy))) next
      lab = .pub.pt.name(s$data[[j]])
      if (!nzchar(lab)) next
      xs = c(xs, xy[1])
      ys = c(ys, xy[2])
      labs = c(labs, lab)
      cols = c(cols, as.character(col)[1])
      src = c(src, i)
      pti = c(pti, j)
    }
  }
  if (length(ys) < 2) return(e)

  h = {
    hh = suppressWarnings(as.numeric(e$height)[1])
    if (!is.finite(hh) || hh <= 0) 360 else hh
  }
  panel.h = max(h - top - bottom, 1)
  yr = diff(range(ys))
  if (!is.finite(yr) || yr <= 0) return(e)
  min.gap = yr / panel.h * .pub.fs(base_size) * 1.45
  ya = e$x$opts$yAxis
  if (!is.null(ya) && is.null(ya$min) && is.list(ya[[1]])) ya = ya[[1]]
  lo = if (!is.null(ya$min)) suppressWarnings(as.numeric(ya$min)[1]) else min(ys)
  hi = if (!is.null(ya$max)) suppressWarnings(as.numeric(ya$max)[1]) else max(ys)
  # Keep boxed labels inside the panel (first/last would clip at the ticks).
  pad = min.gap * 0.7
  if (is.finite(lo) && is.finite(hi) && hi - lo > 3 * pad) {
    lo = lo + pad
    hi = hi - pad
  }
  y2 = .pub.repel.1d(ys, min.gap, lo, hi)
  xa = e$x$opts$xAxis
  if (!is.null(xa) && is.null(xa$min) && is.list(xa[[1]])) xa = xa[[1]]
  xmin = if (!is.null(xa$min)) suppressWarnings(as.numeric(xa$min)[1]) else min(xs)
  xmax = if (!is.null(xa$max)) suppressWarnings(as.numeric(xa$max)[1]) else max(xs)
  if (!is.finite(xmin) || !is.finite(xmax) || xmax <= xmin) {
    xmin = 0
    xmax = 5
  }
  lab.x = pmax(xs, xmin) + 0.18 * (xmax - xmin)

  # Drop in-place labels so only the dodged boxes show.
  for (k in seq_along(src)) {
    pt = series[[src[k]]]$data[[pti[k]]]
    if (!is.list(pt)) pt = list(value = pt)
    if (is.null(pt$label)) pt$label = list()
    pt$label$show = FALSE
    series[[src[k]]]$data[[pti[k]]] = pt
    if (is.null(series[[src[k]]]$label)) series[[src[k]]]$label = list()
    series[[src[k]]]$label$show = FALSE
  }

  segs = lapply(seq_along(ys), function(k) {
    list(
      type = "line",
      data = list(list(xs[k], ys[k]), list(lab.x[k], y2[k])),
      symbol = "none",
      lineStyle = list(color = publightgray, width = 1),
      silent = TRUE,
      legendHoverLink = FALSE,
      z = 1
    )
  })
  boxes = lapply(seq_along(ys), function(k) {
    list(
      value = list(lab.x[k], y2[k]),
      name = labs[k],
      label = list(
        show = TRUE,
        position = "right",
        distance = 4,
        formatter = "{b}",
        color = pubtextgray,
        fontSize = .pub.fs(base_size),
        fontFamily = family,
        backgroundColor = pubbackgray,
        borderColor = cols[k],
        borderWidth = 1,
        padding = c(2, 4, 2, 4)
      ),
      itemStyle = list(color = "transparent")
    )
  })
  lab.s = list(
    type = "scatter",
    data = boxes,
    symbol = "circle",
    symbolSize = 1,
    label = list(
      show = TRUE,
      position = "right",
      distance = 4,
      formatter = "{b}",
      color = pubtextgray,
      fontSize = .pub.fs(base_size),
      fontFamily = family,
      backgroundColor = pubbackgray,
      borderWidth = 1,
      padding = c(2, 4, 2, 4)
    ),
    silent = TRUE,
    legendHoverLink = FALSE,
    z = 4
  )
  e$x$opts$series = c(series, segs, list(lab.s))
  e
}
