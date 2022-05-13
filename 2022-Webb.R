# Testing new eml_get_raster function on webb dataset, because webb dataset has
# less files and will be easier to notice if things are correct

# libraries
library(dataone)
library(datapack)
library(uuid)
library(arcticdatautils)
library(EML)


# load test data
# load token

d1c <- dataone::D1Client("PROD", "urn:node:ARCTIC")

packageId <- "resource_map_urn:uuid:228574ef-9469-4728-aef6-70e47c337253"
dp <- dataone::getDataPackage(d1c, packageId, lazyLoad = TRUE, quiet = FALSE)


# Get metadata pid
xml <- selectMember(dp, name = "sysmeta@fileName", value = ".xml")

# Read in EML
doc <- read_eml(getObject(d1c@mn, xml))




# Creat object with filepath to folder containing all the tif files
raster_folder <- "~/Tickets-2022/2022-Kapsar/webb-tifs"

# create list of the file names
raster_names <- list.files(raster_folder, full.names = TRUE)


### How to order path names according to otherEntity list
# create list only consisting of the names in otherEntities
# ent_names <- vector(list, length(doc$dataset$otherEntity))

ent_names <- c()

for(i in 1:length(doc$dataset$otherEntity)){
  ent_names[i] <- doc$dataset$otherEntity[[i]]$entityName
}

# now order raster names to match order of ent_names object
# this will help ensure that the entity name of the spatialRaster object
# matches with the correct attribute tables. Not really necessary in datasets
# that have the same attribute in each raster, but you never know!
raster_names <- raster_names[order(ent_names)]



# Create empty vector length of raster_names
# We're going to use this to iterate through
spatialRaster <- vector("list", length(raster_names))


# run function
dvk_get_raster_metadata <- function(path, coord_name = NULL, attributeList){
  
  # define a raste object
  raster_obj <- raster::raster(path)
  message(paste("Reading raster object with proj4string of ", raster::crs(raster_obj)@projargs))
  
  # determine coordinates of raster
  if (is.null(coord_name)){
    coord_name <- raster::crs(raster_obj)@projargs
  }
  
  # determine coordinate origins of raster
  # if (identical(raster::origin(raster_obj), c(0,0))){
  #   raster_orig <- "Upper Left"
  # } else if(!identical(raster::origin(raster_obj), c(0,0))){
  #   message("Raster origin not at 0,0")
  #   raster_orig <- "unknown"
  # } 
  
  # determine coordinate origins of raster
  if (raster::origin(raster_obj)[1] > 0 & raster::origin(raster_obj)[2] > 0 ){
    # positive x, positive y
    raster_orig <- "Upper Right"
  } else if (raster::origin(raster_obj)[1] < 0 & raster::origin(raster_obj)[2] > 0 ){
    # negative x, positive y
    raster_origin <- "Upper Left"
  } else if (raster::origin(raster_obj)[1] < 0 & raster::origin(raster_obj)[2] < 0 ){
    # negative x, negative y
    raster_origin <- "Lower Left"
  } else if (raster::origin(raster_obj)[1] > 0 & raster::origin(raster_obj)[2] < 0 ) {
    # positive x, negative y
    raster_orig <- "Lower Right"
  }
  
  raster_info <- list(entityName = basename(path),
                      attributeList = attributeList,
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


for(i in 1:length(raster_names)){
  spatialRaster[[i]] <- dvk_get_raster_metadata(raster_names[i],
                                                       coord_name = NULL,
                                                       doc$dataset$otherEntity[[i]]$attributeList) 
  # NOTE::: Need to figure out how to make sure names of files match up
  # in the correct order of the entities
      # think this should incorporate an error message if the names don't line up,
      # but i think lining them up should happen outside of the function itself
}


doc$dataset$spatialRaster <- spatialRaster

eml_validate(doc)

doc$dataset$otherEntity <- NULL


## add physicals
# Get list of all pids and associated file names
all_pids <- get_package(d1c@mn, packageId, file_names = TRUE)
all_pids <- reorder_pids(all_pids$data, doc) #lines up pids w/correct file

# for loop to assign physicals for each file 
for (i in 1:length(all_pids)){
  doc$dataset$otherEntity[[i]]$physical <- pid_to_eml_physical(d1c@mn, all_pids[[i]])
}

eml_validate(doc)

###################
###################
# This is scratch

raster_obj <- raster::raster(raster_names[1])
raster::origin(raster_obj)
message(paste("Reading raster object with proj4string of ", raster::crs(raster_obj)@projargs))

raster_coord <- sf::st_crs(raster_obj)
raster_coord@datum

# determine coordinates of raster
if (is.null(coord_name)){
  coord_name <- raster::crs(raster_obj)@projargs
}


# Remove annotations from otherEntities so that it doesn't create an error message
annotations <- vector("list", length=23)
# assign annotations to an annotations object
for(i in c(1:23)){
  annotations[[i]] <- doc$dataset$otherEntity[[i]]$attributeList$attribute$annotation
}

# remove annotations
for(i in 1:length(doc$dataset$otherEntity)){
  doc$dataset$otherEntity[[i]]$attributeList$attribute$annotation <- NULL
}




# create spatial raster
for(i in 1:length(raster_names)){
  spatialRaster[[i]] <- dvk_get_raster_metadata(raster_names[i],
                                                coord_name = "GCS_WGS_1984",
                                                doc$dataset$otherEntity[[i]]$attributeList) 
  # NOTE::: Need to figure out how to make sure names of files match up
  # in the correct order of the entities
  # think this should incorporate an error message if the names don't line up,
  # but i think lining them up should happen outside of the function itself
}

# assign spatial raster
doc$dataset$spatialRaster <- spatialRaster
doc$dataset$otherEntity <- NULL


## add physicals
# Get list of all pids and associated file names
all_pids <- get_package(d1c@mn, packageId, file_names = TRUE)
all_pids <- reorder_pids(all_pids$data, doc) #lines up pids w/correct file

# for loop to assign physicals for each file 
for (i in 1:length(all_pids)){
  doc$dataset$spatialRaster[[i]]$physical <- pid_to_eml_physical(d1c@mn, all_pids[[i]])
}

eml_validate(doc)


eml_validate(doc)
annotations[[2]]
