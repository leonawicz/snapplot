#' Add Leaflet map widget for point location.
#'
#' Add a Leaflet map widget centered on a point to a static or interactive document or a Shiny app.
#'
#' This function generates a Leaflet map centered on \code{loc} where \code{loc} is the string name of a point location from the \code{snaplocs} package.
#' Alternatively, the function can be used more generally by providing a length-2 vector of longitude and lattitude.
#' This canned map widget uses two provider tile sets, each with default opacity, \code{op = 0.5}.
#'
#' @param loc character or numeric, name of available point location or specific coordinates. See details.
#' @param bottom character, bottom layer provider tile set.
#' @param top character, top layer provider tile set.
#' @param width character, map widget width, defaults to \code{"100\%"}.
#' @param op numeric, opacity for bottom and top tile sets, respectively. If length-1, will be repeated.
#' @param zoom integer, map zoom, defaults to 12.
#'
#' @return a Leaflet map.
#' @export
#'
#' @examples
#' leafloc("Anchorage")
#' leafloc(c(0, 52), zoom = 6)
leafloc <- function(loc, bottom = "Stamen.Watercolor", top = "Stamen.TonerLite", width = "100%", op = 0.5, zoom = 12){
  if(length(op == 1)) op <- rep(op, 2)
  if(is.character(loc)) loc <- as.numeric(snaplocs::get_coords(loc))
  if(!(is.numeric(loc) && length(loc) == 2 && all(loc >= -180 && loc <= 180))) stop("Invalid `loc`.")
  leaflet::leaflet(width = width) %>%
    leaflet::addProviderTiles(provider = bottom, options = leaflet::providerTileOptions(opacity = op[1])) %>%
    leaflet::addProviderTiles(provider = top, options = leaflet::providerTileOptions(opacity = op[2])) %>%
    leaflet::setView(lng = loc[1], lat = loc[2], zoom = zoom)
}
