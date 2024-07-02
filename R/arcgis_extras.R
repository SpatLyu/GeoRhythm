#' @title read simple features using arcgisbinding
#'
#' @param dsn The path of the source file
#'
#' @return object of class `sf` when a file was successfully read.
#' @export
#'
#' @examples
#' \dontrun{
#' gdbpath = system.file('extdata/xian.gdb',package = 'georhythm')
#' block = arc_read(paste0(gdbpath,'/urban'))
#' }
arc_read = \(dsn){
  arcgisbinding::arc.check_product()
  g = arcgisbinding::arc.open(dsn) %>%
    arcgisbinding::arc.select() %>%
    arcgisbinding::arc.data2sf()
  return(g)
}

#' @title read spatraster using arcgisbinding
#'
#' @param dsn The path of the source file
#'
#' @return object of class `SpatRaster` when a file was successfully read.
#' @export
#'
#' @examples
#' \dontrun{
#' gdbpath = system.file('extdata/xian.gdb',package = 'georhythm')
#' elev = arc_rast(paste0(gdbpath,'/elev'))
#' }
arc_rast = \(dsn){
  arcgisbinding::arc.check_product()
  g = arcgisbinding::arc.open(dsn) %>%
    arcgisbinding::arc.raster() %>%
    arcgisbinding::as.raster() %>%
    terra::rast()
  return(g)
}

#' @title write data to disk using arcgisbinding
#'
#' @note
#' The `IO` functions in `sf` and `terra` package can be used in most cases, and
#' `arc_write()` is not recommended unless you want to export `raster layer` to
#' a `gdb` file. Because `arc_write()` export takeng more time.
#'
#' @param obj input source objects
#' @param dsn full output path
#' @param overwrite (optional) overwrite existing dataset. default is `TEUE`.
#' @param ... (optional) other parameters passed to `arcgisbinding::arc.write()`
#'
#' @return An exported file in the disk.
#' @export
#'
arc_write = \(obj,dsn,overwrite = TRUE,...){
  arcgisbinding::arc.check_product()
  if (inherits(obj,"SpatRaster")){
    obj = raster::raster(obj)
  }
  if (inherits(obj,"sf")){
    if (ncol(obj) == 1) {
      obj = sf::st_geometry(obj)
    }
    obj = sf::st_make_valid(obj)
  }
  arcgisbinding::arc.write(dsn,obj,overwrite = TRUE,...)
}
