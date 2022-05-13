# Kapsar Processing


## -- load libraries -- ##
# general
library(dataone)
library(datapack)
library(uuid)
library(arcticdatautils)
library(EML)



## -- read in data -- ##
# Set nodes
d1c <- dataone::D1Client("PROD", "urn:node:ARCTIC")

packageId <- "resource_map_urn:uuid:28c37466-9e94-41e7-adaa-2ef10fa687cf"
dp  <- getDataPackage(d1c, identifier = packageId, lazyLoad=TRUE, quiet=FALSE)


# Get the metadata id
xml <- selectMember(dp, name = "sysmeta@fileName", value = ".xml")


# get all versions of metadata
get_all_versions(d1c@mn, xml)


# load in the 2nd to last version
doc <- read_eml(getObject(d1c@mn, "urn:uuid:f9d644b9-90e1-4785-966a-938c038d600b"))
eml_validate(doc)

# status checks
length(doc$dataset$otherEntity)
doc$dataset$otherEntity[[420]]$entityName
doc$dataset$otherEntity[[420]]$entityDescription
doc$dataset$otherEntity[[420]]$attributeList

doc$dataset$otherEntity[[420]]$entityType ### Make sure format IDs are set correctly
# things look good


#-----------------------------------------------------------------------------#
# Let's create some spatialVectors
# collect all pids
all_pids <- get_package(d1c@mn, packageId, file_names = TRUE)
all_pids <- reorder_pids(all_pids$data, doc) #lines up pids w/correct file

# retrieve vector pids
vector_pids <- all_pids[c(265:336)]

# create entity to be manipulated with only shp files
vector_entity <- doc$dataset$otherEntity[c(265:336)]

# create empty spatialVector list
spatialVector <- vector("list", length = length(vector_entity))

# create spatialVector entities and assign to spatialVector object
for(i in seq_along(vector_entity)){ #length of vector pids
  spatialVector[[i]] <- 
    pid_to_eml_entity(d1c@mn,
                      vector_pids[[i]],
                      entity_type = "spatialVector",
                      entityName = vector_entity[[i]]$entityName,
                      entityDescription = vector_entity[[i]]$entityDescription,
                      attributeList = vector_entity[[i]]$attributeList,
                      geometry = "Polygon",
                      spatialReference = list(horizCoordSysName = "GCS_North_American_1983"))
}

# make sure entity names look correct
for(i in 1:72){
  print(spatialVector[[i]]$entityName)
}

# assign spatialVector entity to doc
doc$dataset$spatialVector <- spatialVector





#-----------------------------------------------------------------------------#
# From here, I'm going to walk through the steps of creating spatialRasters

# Create object with filepath to folder containing all the tif files
raster_folder <- "~/Tickets-2022/2022-Kapsar/all-tifs"

# create list of the file names
raster_names <- list.files(raster_folder, full.names = TRUE)

# empty vector for entity names
ent_names <- c()

# fill entity names
for(i in 1:length(doc$dataset$otherEntity)){
  ent_names[i] <- doc$dataset$otherEntity[[i]]$entityName
}

# there are shapefiles in the dataset, but we only want rasters. Get rid of the shapefile names
ent_names[c(1:264, 337:720)] 
ent_names <- c(ent_names[c(1:264)],
               ent_names[c(337:720)])

length(ent_names)

# re-order raster names so they match order of entity names
raster_names <- raster_names[order(ent_names)]

# Create empty vector length of raster_names.
# We're going to use this to iterate through
spatialRaster <- vector("list", length(raster_names))


# Load edited arcticdatautils::eml-get-raster function
dvk_get_raster_metadata <- function(path, coord_name = NULL, attributeList){
  
  # define a raste object
  raster_obj <- raster::raster(path)
  #message(paste("Reading raster object with proj4string of ", raster::crs(raster_obj)@projargs))
  
  # determine coordinates of raster
  if (is.null(coord_name)){
    coord_name <- raster::crs(raster_obj)@projargs
  }
  
  
  # determine coordinate origins of raster
  if (raster::origin(raster_obj)[1] > 0 & raster::origin(raster_obj)[2] > 0 ){
    # positive x, positive y
    raster_orig <- "Upper Right"
  } else if (raster::origin(raster_obj)[1] < 0 & raster::origin(raster_obj)[2] > 0 ){
    # negative x, positive y
    raster_orig <- "Upper Left"
  } else if (raster::origin(raster_obj)[1] < 0 & raster::origin(raster_obj)[2] < 0 ){
    # negative x, negative y
    raster_orig <- "Lower Left"
  } else if (raster::origin(raster_obj)[1] > 0 & raster::origin(raster_obj)[2] < 0 ){
    # positive x, negative y
    raster_orig <- "Lower Right"
  } else if (raster::origin(raster_obj)[1] == 0 & raster::origin(raster_obj)[2] < 0 ){
    raster_orig <- "Lower Left"
  } else if (raster::origin(raster_obj)[1] == 0 & raster::origin(raster_obj)[2] > 0 ){
    raster_orig <- "Upper Left"
  } else if (raster::origin(raster_obj)[1] > 0 & raster::origin(raster_obj)[2] == 0 ){
    raster_orig <- "Upper Right"
  } else if (raster::origin(raster_obj)[1] < 0 & raster::origin(raster_obj)[2] == 0 ){
    raster_orig <- "Upper Left"
  } else if (identical(raster::origin(raster_obj), c(0,0))){
    raster_orig <- "Upper Left"
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




# create empty list for tif attributeLists to go
tif_entities <- vector("list", length(raster_names))


# create entity list of only the tif files
tif_entities <- c(doc$dataset$otherEntity[c(1:264)], 
                   doc$dataset$otherEntity[c(337:720)])


# create spatial raster entities
for(i in 1:length(raster_names)){
  spatialRaster[[i]] <- dvk_get_raster_metadata(raster_names[i],
                                                coord_name = "GCS_North_American_1983",  # "GCS_WGS_1984" <- from 2022-Webb, # retrieved from get_coord_list
                                                tif_entities[[i]]$attributeList) 
}


# Assign newly created spatialRaster to the doc
doc$dataset$spatialRaster <- spatialRaster

# Null the otherEntities so that you don't have repeating IDs that would cause issues
doc$dataset$otherEntity <- NULL

# Moment of truth
eml_validate(doc)



## -- Update package -- ##
eml_path <- "~/Scratch/North_Pacific_and_Arctic_Marine_Vessel_Traffic.xml"
write_eml(doc, eml_path)

dp <- replaceMember(dp, xml, replacement = eml_path)

myAccessRules <- data.frame(subject="CN=arctic-data-admins,DC=dataone,DC=org", 
                            permission="changePermission")

newPackageId <- uploadDataPackage(d1c, dp, public = FALSE,
                                  accessRules = myAccessRules, quiet = FALSE)

# New PackageID: resource_map_urn:uuid:91c0e7ac-7d0f-47c6-be0e-e34ec127fdff

## -- set rights & access -- ##
# Manually set ORCiD
# kelly Kapsar
subject <- 'http://orcid.org/0000-0002-2048-5020'



# Get data pids
tif_pids <- selectMember(dp, name = "sysmeta@fileName", value = ".tif")
zip_pids <- selectMember(dp, name = "sysmeta@fileName",  value = ".zip")

# set rights
set_rights_and_access(d1c@mn,
                      pids = c(xml, tif_pids, zip_pids, packageId),
                      subject = subject,
                      permissions = c('read', 'write', 'changePermission'))
