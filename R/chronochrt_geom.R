#' A chronological chart
#'
#' Computes and draws a chronological chart.
#'
#' This geom is special because no x and y coordinates are
#' provided in the input. Therefore, the following aesthetics must be provided
#' only in the \code{\link[ggplot2]{aes}} function: \code{region}, \code{level},
#' \code{start}, \code{end}, \code{add} (i.e. all required aesthetics).
#' \itemize{ \item \code{region} The title(s) of the section(s) the
#' chronological chart is subdivided into \item \code{name} The name(s) of the
#' chronological unit(s). To maintain compatibility with other geoms, the
#' aesthetic \code{label} can be used instead. \item \code{start, end} The start
#' and end date of a chronological unit, respectively. \item \code{level} The
#' levels of the chronological units. \item \code{add} Logical value indicating
#' whether chronological units within a \code{region} should be plotted in an
#' \emph{add}itional column.}
#'
#' Usually, the names of the chronological units are placed in their middle.
#' They can be arbitrarily placed by the aesthetics \code{name_x, name_y}:
#' \itemize{ \item \code{name_x} The horizontal position within an chronological
#' column, i.e. a value between 0 and 1 if \code{add = FALSE} and between 1 and
#' 2 if \code{add = TRUE}. \item \code{name_y} The vertical position given as a
#' year.}
#'
#' See \code{vignette("ChronochRt")} below for further details.
#'
#' The geom aims to preserve access to as much of the underlying aesthetics as
#' possible. To achieve this aim, ambiguous names were resolved (e.g.
#' \code{size} to \code{size_line} and \code{size_text}).
#'
#' @inheritParams ggplot2::layer
#' @param year_lim A numeric vector of length 2 to define the lower and upper
#'   limit of the chronological chart.
#' @param minimal Should chrons be optically separated by vertical lines? If
#'   \code{TRUE} only vertical lines between around the chronological columns
#'   will be drawn.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{layer}}. These
#'   are often aesthetics, used to set an aesthetic to a fixed value, like
#'   \code{hjust = 0.1}.
#'
#' @section Aesthetics: \code{geom_ChronochRt()} understands the following
#'   aesthetics (required aesthetics are in bold): \itemize{ \item
#'   \strong{\code{region}} \item \strong{\code{level}} \item
#'   \strong{\code{end}} \item \strong{\code{start}} \item \strong{\code{add}}
#'   \item \code{alpha} \item \code{angle} \item \code{colour} \item
#'   \code{family} \item \code{fill} \item \code{fontface} \item \code{group}
#'   \item \code{hjust} \item \code{lineheight} \item \code{name|label} \item
#'   \code{name_x} \item \code{name_y} \item \code{size_line} \item
#'   \code{size_text} \item \code{vjust} } See Details for how aesthetics
#'   specific for this geom work and learn more about setting aesthetics in
#'   \code{vignette("ggplot2-specs")}.
#'
#' @return Layer of a ggplot2 object.
#'
#' @export
#'
#' @examples
#'
#' # Create example data
#' library(ggplot2)
#'
#' chrons <- data.frame(region = c("A", "B", "B", "B", "A"),
#'                      name = c("a", "a", "1", "2", "b"),
#'                      start = c(-100, -100, -100, "0/50", "50_100"),
#'                      end = c("50_100", 150, "0/50", 150, 200),
#'                      level = c(1, 1, 2, 2, 1),
#'                      add = FALSE)
#'
#' ggplot(chrons) +
#' geom_chronochRt(aes(name = name, region = region, level = level,
#'                     start = start, end = end, add = add))
#'
#' ggplot(chrons, aes(name = name, region = region, level = level,
#'                    start = start, end = end, add = add)) +
#'    geom_chronochRt()
#'
#' # If more than one region should be plotted, they must be separated with
#' facet_grid:
#' ggplot(chrons) +
#'    geom_chronochRt(aes(name = name, region = region, level = level,
#'                        start = start, end = end, add = add)) +
#'    facet_grid(cols = vars(region), scales = "free_x", space = "free_x")
#'
#' # Adjust upper and lower end of a chronological chart with year_lim:
#' q <- ggplot(chrons, aes(name = name, region = region, level = level,
#'                         start = start, end = end, add = add)) +
#'    facet_grid(cols = vars(region), scales = "free_x", space = "free_x")
#'
#' q + geom_chronochRt(year_lim = c(-50, 100))
#'
#' # Change aesthetics of the plot:
#' q + geom_chronochRt(fill = "black", colour = "white")
#' q + geom_chronochRt(aes(fill = level, size_line = 3))
#'
#' # Change position of the names:
#' q + geom_chronochRt(aes(name_x = 0.75))
#'
#' # To remove vertical lines within a chronological column:
#' q + geom_chronochRt(minimal = TRUE)


geom_chronochRt <- function(mapping = NULL, data = NULL, inherit.aes = TRUE,
                            year_lim = NULL, minimal = FALSE, ...) {
  ggplot2::layer(
    geom = GeomChronochRt,
    mapping = mapping,
    data = data,
    stat = "identity",
    position = "identity",
    show.legend = FALSE,
    inherit.aes = inherit.aes,
    params = list(minimal = minimal, year_lim = year_lim, ...)
  )
}

GeomChronochRt <- ggplot2::ggproto("GeomChronochRt", ggplot2:::Geom, handle_na = function(self, data, params) {

  data
},

setup_data = function(self, data, params) {
  data <- ggplot2:::ggproto_parent(ggplot2:::Geom, self)$setup_data(data, params)

  data <- data %>%
    dplyr::mutate(
      boundary_start = dplyr::case_when(
        grepl("/", start) ~ "unsec",
        grepl("_", start) ~ "trans",
        TRUE ~ "sec"
      )
    ) %>%
    dplyr::mutate(
      boundary_end = dplyr::case_when(
        grepl("/", end) ~ "unsec",
        grepl("_", end) ~ "trans",
        TRUE ~ "sec"
      )
    ) %>%
    tidyr::separate(
      start, c("start", "start2"), sep = "/|_", fill = "right"
    ) %>%
    tidyr::separate(end, c("end", "end2"), sep = "/|_", fill = "right") %>%
    dplyr::mutate(dplyr::across(
      tidyselect::starts_with("start") | tidyselect::starts_with("end"),
      as.numeric
    )
    )

  if (!is.null(params$year_lim)) {
    if (!is.numeric(params$year_lim) || length(params$year_lim) != 2) {
      stop("Error in 'geom_chronochRt': 'year_min' must be a numeric vector of length 2.")
    }

    year_min <- min(params$year_lim, na.rm = TRUE)
    year_max <- max(params$year_lim, na.rm = TRUE)

    data <- data %>%
      dplyr::filter(!(start >= year_max & start2 >= year_max)) %>%
      dplyr::filter(!(end <= year_min & end2 <= year_min)) %>%
      dplyr::mutate(start = dplyr::if_else(start < year_min, year_min, start),
                    start2 = dplyr::if_else(start2 < year_min, year_min, start2),
                    end = dplyr::if_else(end > year_max, year_max, end),
                    end2 = dplyr::if_else(end2 > year_max, year_max, end2))
  }

  data <- data %>%
    dplyr::arrange(level, start) %>%
    dplyr::group_by(region, add) %>%
    dplyr::mutate(level = level - min(level) + 1) %>%
    dplyr::mutate(xmin = (level - 1) / max(level),
                  xmax = level / max(level)) %>%
    dplyr::mutate(x = xmin + ((xmax - xmin) / 2),
                  y = start + ((end - start) / 2)) %>%
    dplyr::mutate(xmax_uncorr = corr_xmax(start, end, xmax)) %>%
    dplyr::mutate(xmax = dplyr::if_else(xmax == xmax_uncorr, 1, xmax)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-xmax_uncorr) %>%
    dplyr::mutate(xmax = dplyr::if_else(add == TRUE, xmax + 1, xmax),
                  xmin = dplyr::if_else(add == TRUE, xmin + 1, xmin),
                  x = dplyr::if_else(add == TRUE, x + 1, x)) %>%
    dplyr::mutate(ymin = min(start, start2, na.rm = TRUE),
                  ymax = max(end, end2, na.rm = TRUE))

  if (!"name" %in% names(data)) {
    if ("label" %in% names(data)) {
      data <- dplyr::rename(data, name = label)
    } else {
      data$name <- ""
    }
  }
  data
},
draw_panel = function(data, panel_params, coord, minimal, year_lim) {

  data <- data %>%
    dplyr::mutate(x = if ("name_x" %in% names(.)) name_x else x,
                  y = if ("name_y" %in% names(.)) name_y else y)

  if ("trans" %in% data$boundary_start | "trans" %in% data$boundary_end) {

    data_line_d <- data %>%
      dplyr::select(-tidyselect::contains("2")) %>%
      dplyr::mutate(end = ifelse(boundary_end != "trans", NA, end),
                    start = ifelse(boundary_start != "trans", NA, start)) %>%
      tidyr::pivot_longer(c("start", "end"), names_to = "side",
                          values_to = "ystart", values_drop_na = TRUE) %>%
      dplyr::left_join(
        data %>%
          dplyr::select(-start, -end) %>%
          dplyr::mutate(end2 = ifelse(boundary_end != "trans", NA, end2),
                        start2 = ifelse(boundary_start != "trans", NA, start2)) %>%
          tidyr::pivot_longer(c("start2", "end2"), names_to = "side",
                              values_to = "yend", values_drop_na = TRUE) %>%
          dplyr::mutate(side = gsub("2", "", side)),
        by = c("region", "name", "level", "add", "PANEL", "group",
               "boundary_start", "boundary_end", "xmin", "xmax", "x", "y",
               "ymin", "ymax", "angle", "colour", "fill", "alpha", "size_line",
               "size_text", "hjust", "vjust", "family", "fontface",
               "lineheight", "side")
      ) %>%
      dplyr::group_by(region, level) %>%
      dplyr::mutate(xmax = min(xmax)) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(ystart, yend, xmin, xmax, .keep_all = TRUE) %>%
      dplyr::mutate(linetype = "solid")
  } else {
    data_line_d <- NULL
  }

  data_line_h <- data %>%
    dplyr::select(-tidyselect::contains("end")) %>%
    tidyr::pivot_longer(c("start", "start2"), names_to = "type",
                        values_to = "yend") %>%
    dplyr::rename(type_boundary = boundary_start) %>%
    dplyr::bind_rows(
      data %>%
        dplyr::select(-tidyselect::contains("start")) %>%
        tidyr::pivot_longer(c("end", "end2"), names_to = "type",
                            values_to = "yend") %>%
        dplyr::rename(type_boundary = boundary_end)
    ) %>%
    tidyr::drop_na(yend) %>%
    dplyr::filter(type_boundary != "trans") %>%
    dplyr::mutate(linetype = dplyr::if_else(type_boundary == "unsec",
                                            "dashed", "solid")) %>%
    dplyr::distinct(yend, xmax, linetype, .keep_all = TRUE)

  data_line_v <- data %>%
    tidyr::pivot_longer(c("xmin", "xmax"), names_to = "trash",
                        values_to = "xend") %>%
    tidyr::drop_na(xend) %>%
    dplyr::mutate(end = pmax(end, end2, na.rm = TRUE),
                  start = pmin(start, start2, na.rm = TRUE)) %>%
    dplyr::distinct(xend, end, .keep_all = TRUE) %>%
    dplyr::mutate(linetype = "solid")

  if (minimal == TRUE) {
    data_line_v <- dplyr::filter(data_line_v, xend %in% c(0, 1, 2))
  }

  rect_df <- ggplot2:::data_frame0(
    xmin = data$xmin,
    xmax = data$xmax,
    ymin = data$start,
    ymax = data$end,
    group = data$group,
    colour = NA,
    fill = data$fill,
    alpha = data$alpha,
    linetype = "blank",
    linewidth = 0
  )

  line_df_h <- ggplot2:::data_frame0(
    x = data_line_h$xmin,
    y = data_line_h$yend,
    xend = data_line_h$xmax,
    yend = data_line_h$yend,
    alpha = data_line_h$alpha,
    colour = data_line_h$colour,
    group = data_line_h$group,
    linetype = data_line_h$linetype,
    linewidth = data_line_h$size_line
  )

  line_df_v <- ggplot2:::data_frame0(
    x = data_line_v$xend,
    y = data_line_v$start,
    xend = data_line_v$xend,
    yend = data_line_v$end,
    alpha = data_line_v$alpha,
    colour = data_line_v$colour,
    group = data_line_v$group,
    linetype = data_line_v$linetype,
    linewidth = data_line_v$size_line
  )

  line_df_d <- ggplot2:::data_frame0(
    x = data_line_d$xmin,
    y = data_line_d$ystart,
    xend = data_line_d$xmax,
    yend = data_line_d$yend,
    alpha = data_line_d$alpha,
    colour = data_line_d$colour,
    group = data_line_d$group,
    linetype = data_line_d$linetype,
    linewidth = data_line_d$size_line
  )

  text_df <- ggplot2:::data_frame0(
    label = data$name,
    x = data$x,
    y = data$y,
    angle = data$angle,
    alpha = data$alpha,
    colour = data$colour,
    family = data$family,
    fontface = data$fontface,
    group = data$group,
    lineheight = data$lineheight,
    size = data$size_text,
    hjust = data$hjust,
    vjust = data$vjust
  )

  ggplot2:::ggname(
    "geom_chronochRt",
    grid::grobTree(
      ggplot2::GeomRect$draw_panel(data = rect_df,
                                   panel_params = panel_params,
                                   coord = coord),
      ggplot2::GeomSegment$draw_panel(data = line_df_h,
                                      panel_params = panel_params,
                                      coord = coord),
      ggplot2::GeomSegment$draw_panel(data = line_df_v,
                                      panel_params = panel_params,
                                      coord = coord),
      ggplot2::GeomSegment$draw_panel(data = line_df_d,
                                      panel_params = panel_params,
                                      coord = coord),
      ggplot2::GeomText$draw_panel(data = text_df,
                                   panel_params = panel_params,
                                   coord = coord)
    )
  )
},
required_aes = c("region", "level", "end", "start", "add"),

default_aes = ggplot2::aes(
  name = NULL,
  label = NULL,
  name_x = NULL,
  name_y = NULL,
  angle = 0,
  colour = "black",
  fill = "white", alpha = NA,
  size_line = 0.5,
  size_text = 3.88,
  hjust = 0.5,
  vjust = 0.5,
  family = "",
  fontface = 1,
  lineheight = 1.2
)
)

#' Determine maximum x value of parallel chrons
#'
#' This function determines the maximum x value of parallel chrons
#'
#' @param start,end Start and end of the chrons.
#' @param xmax The upper x value of the chrons.
#'
#' @return A vector
#'
#' @keywords internal
#' @export

corr_xmax <- function(start, end, xmax) {

  data <- xmax

  for (i in seq_along(start)) {
    for (j in seq_along(start)) {
      if (start[i] + 1  >= start[j] && start[i] + 1 <= end[j]) {
        data[i] <- max(xmax[i], xmax[j])
        data[j] <- max(xmax[i], xmax[j])
      }
    }
  }
  data
}
