#' Add image labels to plot
#'
#' Plot images in a ggplot2 object. Supported file types are: \code{.png},
#' \code{.jpg}, \code{.tif}, \code{.bmp.}, \code{.svg} (see
#' \code{\link[magick]{image_read}} for further details).
#'
#' The images are provided by their paths (local files or URLs) via the
#' aesthetic \code{image_path}. Rows with invalid file paths are silently
#' dropped, invalid URLs will throw an error.
#'
#' The absolute size in cm of the images can be specified via the aesthetics
#' \code{height} and \code{width}. If only one is specified, the image is scaled
#' under preservation of its aspect ratio. If both are given, the image might
#' appear distorted. See examples for further details.
#'
#' @inheritParams ggplot2::layer
#' @param ... Other arguments passed on to \code{\link[ggplot2]{layer}}. These are often aesthetics, used to set an aesthetic to a fixed value, like \code{height = 1}.
#'
#' @section Aesthetics: \code{geom_ChronochRtImage()} understands the following
#'   aesthetics (required aesthetics are in bold): \itemize{ \item
#'   \strong{\code{image_path}} \item \strong{\code{x}} \item \strong{\code{y}}
#'   \item \code{group} \item \code{height} \item \code{width} } See Details
#'   for how these aesthetics work.
#'
#' @author
#' This geom is a modified version of the \code{geom_custom()} from \href{https://github.com/baptiste}{Baptiste Auguié}'s \href{https://cran.r-project.org/package=egg}{egg package}.
#'
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#'
#' # Create example data
#' data <- data.frame(x = c(2, 4), y = c(2, 4),
#'                    image_path = "https://www.r-project.org/logo/Rlogo.png",
#'                    height = c(1,2), width = c(3,0.5))
#'
#' q <- ggplot(data) + lims(x = c(0, 6), y = c(0, 6))
#'
#' # Without size specifications
#' q + geom_chronochRtImage(aes(image_path = image_path, x = x, y = y))
#'
#' # Scale images to individual heights/widths by specifying one of them:
#' q + geom_chronochRtImage(aes(image_path = image_path, x = x, y = y, height = height))
#'
#' # Scale images to uniform height/width (i.e. independent of input data):
#' q + geom_chronochRtImage(aes(image_path = image_path, x = x, y = y, height = 1))
#'
#' # Specifying height and width might result in distorted images:
#' q + geom_chronochRtImage(aes(image_path = image_path, x = x, y = y, height = height, width = width))

geom_chronochRtImage <- function(mapping = NULL, data = NULL, inherit.aes = TRUE, ...) {
   ggplot2::layer(
      geom = GeomChronochRtImage,
      mapping = mapping,
      data = data,
      stat = "identity",
      position = "identity",
      show.legend = FALSE,
      inherit.aes = inherit.aes,
      params = list(...)
   )
  }

GeomChronochRtImage <- ggplot2::ggproto("GeomChronochRtImage", ggplot2:::Geom,

   handle_na = function(self, data, params) {
     data
   },
   setup_data = function(self, data, params) {
     data <- ggplot2:::ggproto_parent(ggplot2:::Geom, self)$setup_data(data, params)
     data
   },
   draw_panel = function(data, panel_scales, coord) {
     coords <- coord$transform(data, panel_scales)

     data <- data[file.exists(data$image_path) | grepl("http", data$image_path, fixed = TRUE), ]

     data$image <- lapply(data$image_path, function(x) magick::image_read(x))

     if (any(grepl("height", names(data)))) {data$height <- grid::unit(data$height, "cm")}
     if (any(grepl("width", names(data)))) {data$width <- grid::unit(data$width, "cm")}

     gl <- lapply(seq_along(data$image), function(i) {
       .g <- do.call(grid::rasterGrob, c(list(data$image[[i]]), height = list(data$height[[i]]), width = list(data$width[[i]])))
       grid::editGrob(.g,
                      x = grid::unit(coords$x[i], "native"),
                      y = grid::unit(coords$y[i], "native"))
       }
     )

     do.call(grid::grobTree, gl)
   },
   required_aes = c("image_path", "x", "y"),
   default_aes = ggplot2::aes(height = NULL, width = NULL)
)
