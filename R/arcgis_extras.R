#' @title read simple features using arcgisbinding
#'
#' @param dsn The path of the source file
#'
#' @return object of class `sf` when a file was successfully read.
#' @export
#'
read_arc = \(dsn){
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
rast_arc = \(dsn){
  arcgisbinding::arc.check_product()
  g = arcgisbinding::arc.open(dsn) %>%
    arcgisbinding::arc.raster() %>%
    arcgisbinding::as.raster() %>%
    terra::rast()
  return(g)
}

#' @title write data to disk using arcgisbinding
#'
#' @param obj input source objects
#' @param dsn full output path
#' @param overwrite (optional) overwrite existing dataset. default is `TEUE`.
#' @param ... (optional) other parameters passed to `arcgisbinding::arc.write()`
#'
#' @return An exported file in the disk.
#' @export
#'
write_arc = \(obj,dsn,overwrite = TRUE,...){
  arcgisbinding::arc.check_product()
  arcgisbinding::arc.write(dsn,obj,overwrite = TRUE,...)
}
