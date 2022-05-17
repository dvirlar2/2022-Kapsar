#' Get raster info from a file on disk
#'
#' This function populates a spatialRaster element with the
#' required elements by reading a local raster file in. The
#' `coord_name` argument can be found by examining the data.frame
#' that `get_coord_list()` returns against the proj4string of the
#' raster file.
#'
#' @param path (char) Path to a raster file
#' @param coord_name (char) horizCoordSysDef name
#' @param attributes (dataTable) attributes for raster
#'
#'
#' @export
eml_get_raster_metadata <- function(path, coord_name = NULL, attributes){
  
  raster_obj <- raster::raster(path)
  message(paste("Reading raster object with proj4string of ", raster::crs(raster_obj)@projargs))
  
  if (is.null(coord_name)){
    coord_name <- raster::crs(raster_obj)@projargs
  }
  
  
  if (identical(raster::origin(raster_obj), c(0,0))){
    raster_orig <- "Upper Left"
  } else if(!identical(raster::origin(raster_obj), c(0,0))){
    message("Raster origin not at 0,0")
    raster_orig <- "unknown"
  }
  
  raster_info <- list(entityName = basename(path),
                      attributeList = set_attributes(attributes),
                      spatialReference = list(horizCoordSysName = coord_name),
                      horizontalAccuracy = list(accuracyReport = "unknown"),
                      verticalAccuracy = list(accuracyReport = "unknown"),
                      cellSizeXDirection = raster::res(raster_obj)[1],
                      cellSizeYDirection = raster::res(raster_obj)[2],
                      numberOfBands = raster::nbands(raster_obj),
                      rasterOrigin = raster_orig,
                      rows = dim(raster_obj)[1],
                      columns = dim(raster_obj)[2],
                      verticals = dim(raster_obj)[3],
                      cellGeometry = "pixel")
  return(raster_info)
}
