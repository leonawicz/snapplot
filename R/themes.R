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
  p <- switch(palette, 1 = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33"))
  if(is.null(n)) return(p)
  if(length(p) < n) stop(paste("The selected color palette contains fewer than", n, "colors."))
  if(n < length(p)) p <- p[1:n]
  p
}

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
theme_snap <- function(base_size = 14, base_family = "", base_col = "black", base_fill = "white", grid_col = "#F0F0F0") {
  (.theme_prep(base_size = base_size, base_family = base_family, base_col = base_col, base_fill = base_fill) +
     theme(plot.title = element_text(face = "bold", size = rel(1.2)),
           text = element_text(),
           panel.background = element_rect(colour = NA),
           plot.background = element_rect(colour = NA),
           panel.border = element_rect(colour = NA),
           axis.title = element_text(face = "bold", size = rel(1)),
           axis.title.y = element_text(angle = 90, vjust = 2),
           axis.title.x = element_text(vjust = -0.2),
           axis.text = element_text(),
           axis.line = element_line(colour = base_col),
           axis.ticks = element_line(),
           panel.grid.major = element_line(colour = grid_col),
           panel.grid.minor = element_blank(),
           legend.key = element_rect(colour = NA),
           legend.position = "bottom",
           legend.direction = "horizontal",
           legend.key.size = unit(0.2, "cm"),
           legend.margin = margin(0, 0, 0, 0, "cm"),
           legend.title = element_text(face = "italic"),
           plot.margin = unit(c(3, 1, 1, 1), "mm"),
           strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
           strip.text = element_text(face = "bold")
     ))
}

#' @export
#' @rdname themes
theme_snapdark <- function(base_size = 14, base_family = "", base_col = "white", base_fill = "#242424", grid_col = "#333333"){
  theme_snap(base_size = base_size, base_family = base_family, base_col = base_col, base_fill = base_fill, grid_col = grid_col)
}

.theme_prep <- function(base_size = 12, base_family = "", base_col = "black", base_fill = "white"){
  thm <- theme_gray(base_size = base_size, base_family = base_family)
  for (i in names(thm)) {
    if ("colour" %in% names(thm[[i]])) {
      thm[[i]]["colour"] <- list(NULL)
    }
    if ("fill" %in% names(thm[[i]])) {
      thm[[i]]["fill"] <- list(NULL)
    }
  }
  thm + theme(panel.border = element_rect(fill = NA), legend.background = element_rect(colour = NA),
              line = element_line(colour = base_col),
              rect = element_rect(fill = base_fill, colour = base_col), text = element_text(colour = base_col))
}
