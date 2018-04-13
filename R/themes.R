#' SNAP color palettes
#'
#' Common SNAP color palettes.
#'
#' Obtain commonly used SNAP color palettes for styling plots.
#' Palettes can be specified by name or index.
#' By default, all colors are returned (\code{n = NULL}).
#' If \code{n} is less than the number of colors in \code{palette}, the first \code{n} colors are returned.
#'
#' @param palette character or integer, name or index of color palette.
#' @param n number of colors returned from \code{palette}. See details.
#'
#' @return a vector of hex colors.
#' @export
#'
#' @examples
#' snapalettes()
snapalettes <- function(palette = 1, n = NULL){
  x <- .palettes
  if(is.numeric(palette)){
    p <- switch(palette, "1" = x[[1]])
  } else {
    p <- switch(palette, "q1" = x[[1]])
  }
  if(is.null(n)) return(p)
  if(length(p) < n) stop(paste("The selected color palette contains fewer than", n, "colors."))
  if(n < length(p)) p <- p[1:n]
  p
}

.palettes <- list(
  q1 = c("#386cb0", "#fdb462", "#7fc97f", "#ef3b2c", "#662506", "#a6cee3", "#fb9a99", "#984ea3", "#ffff33")
)

#' SNAP plot themes
#'
#' A collection of SNAP custom styled ggplot themes.
#'
#' These themes are for ggplot objects, on which most stock SNAP plot functions are based.
#' The primary differences between available themes are the default fill and color options.
#'
#' @param base_size numeric, base font size.
#' @param base_family character, font family.
#' @param base_col default overall theme color.
#' @param base_fill default overall background/fill color.
#' @param grid_col default grid lines color.
#'
#' @return a ggplot theme.
#'
#' @name themes
#' @export
#'
#' @examples
#' library(ggplot2)
#' x <- c(rnorm(100), rnorm(100, 2))
#' d <- data.frame(x = x, grp = rep(LETTERS[1:2], each = 100))
#' g <- ggplot(d, aes(x, colour = grp, fill = grp)) +
#'   geom_density(size = 1, alpha = 0.5)
#' g + theme_snap()
#' g + theme_snapdark()

#' @export
#' @rdname themes
theme_snap <- function(base_size = 14, base_family = "", base_col = "black",
                       base_fill = "white", grid_col = "#F0F0F0") {
  .theme_prep(base_size = base_size, base_family = base_family,
              base_col = base_col, base_fill = base_fill) +
    ggplot2::theme(panel.background = ggplot2::element_rect(colour = NA),
                   plot.background = ggplot2::element_rect(colour = NA),
                   panel.border = ggplot2::element_rect(colour = NA),
                   panel.spacing.x = ggplot2::unit(0.25, "cm"),
                   plot.margin = ggplot2::unit(c(0.5, 1, 0.5, 0.5), "cm"),
                   axis.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1)),
                   axis.title.y = ggplot2::element_text(angle = 90, vjust = 2),
                   axis.title.x = ggplot2::element_text(vjust = -0.2),
                   axis.line = ggplot2::element_line(colour = base_col),
                   axis.ticks.length = ggplot2::unit(0.2, "cm"),
                   panel.grid.major = ggplot2::element_line(colour = grid_col),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.key = ggplot2::element_rect(colour = NA),
                   legend.position = "bottom",
                   legend.direction = "horizontal",
                   legend.title = ggplot2::element_text(face = "italic"),
                   strip.background = ggplot2::element_rect(colour = base_col, fill = grid_col),
                   strip.text = ggplot2::element_text(size = base_size, face = "bold")
     )
}

#' @export
#' @rdname themes
theme_snapdark <- function(base_size = 14, base_family = "", base_col = "white",
                           base_fill = "#242424", grid_col = "#333333"){
  theme_snap(base_size = base_size, base_family = base_family,
             base_col = base_col, base_fill = base_fill, grid_col = grid_col)
}

.theme_prep <- function(base_size = 12, base_family = "", base_col = "black", base_fill = "white"){
  x <- ggplot2::theme_gray(base_size = base_size, base_family = base_family)
  for (i in names(x)) {
    if ("colour" %in% names(x[[i]])) x[[i]]["colour"] <- list(NULL)
    if ("fill" %in% names(x[[i]])) x[[i]]["fill"] <- list(NULL)
  }
  x + ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA),
                     legend.background = ggplot2::element_rect(colour = NA),
                     line = ggplot2::element_line(colour = base_col),
                     rect = ggplot2::element_rect(fill = base_fill, colour = base_col),
                     text = ggplot2::element_text(colour = base_col))
}
