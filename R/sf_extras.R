#' @title construct a polygon from a series of points
#'
#' @param pcoord1 The x-coordinate of the points
#' @param pcoord2 The y-coordinate of the points
#' @param pcrs (optional) Coordinate reference system to be assigned, default uses `WGS84`,
#' more details to see `sf::st_crs()`
#'
#' @return An `sfc` polygon object.
#' @export
#'
#' @examples
#' lon = c(126.626510,126.625261,126.626378,126.626541,126.626721,126.627732,126.626510)
#' lat = c(45.731596,45.729834,45.729435,45.729676,45.729604,45.730915,45.731596)
#' g = st_transform_cn(lon,lat)
#' st_point2polygon(g$lon,g$lat)
st_point2polygon = \(pcoord1,pcoord2,pcrs = NULL){
  if (is.null(pcrs)) {pcrs = sf::st_crs('EPSG:4326')}
  p_sfc = tibble::tibble(pcoord1 = pcoord1,
                         pcoord2 = pcoord2) %>%
    sf::st_as_sf(coords = c('pcoord1','pcoord2'),
                 crs = pcrs) %>%
    sf::st_combine() %>%
    sf::st_cast('POLYGON')
  return(p_sfc)
}
