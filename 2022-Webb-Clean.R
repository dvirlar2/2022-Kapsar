# Clean test for webb

# changing other entities to spatialRaster worked
# try adding physicals
# change formatID

d1c <- dataone::D1Client("PROD", "urn:node:ARCTIC")

packageId <- "resource_map_urn:uuid:7bb9d53f-f833-4fc1-88f3-0560351b2959"
dp <- dataone::getDataPackage(d1c, packageId, lazyLoad = TRUE, quiet = FALSE)

xml <- selectMember(dp, name = "sysmeta@fileName", value = ".xml")

doc <- read_eml(getObject(d1c@mn, xml))

#
#
#


# Creat object with filepath to folder containing all the tif files
raster_folder <- "~/Tickets-2022/2022-Kapsar/webb-tifs"

# create list of the file names
raster_names <- list.files(raster_folder, full.names = TRUE)

# empty list for entity names
ent_names <- c()

# fill entity names
for(i in 1:length(doc$dataset$otherEntity)){
  ent_names[i] <- doc$dataset$otherEntity[[i]]$entityName
}

# re-order raster names so they match order of entity names
raster_names <- raster_names[order(ent_names)]


# Create empty vector length of raster_names
# We're going to use this to iterate through
spatialRaster <- vector("list", length(raster_names))


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


# create spatial raster
for(i in 1:length(raster_names)){
  spatialRaster[[i]] <- dvk_get_raster_metadata(raster_names[i],
                                                coord_name = "GCS_WGS_1984",
                                                doc$dataset$otherEntity[[i]]$attributeList) 
}


doc$dataset$spatialRaster <- spatialRaster
doc$dataset$otherEntity <- NULL
eml_validate(doc)
  # TRUE 


## -- add physicals -- ##
# Do this after updating the package, otherwise I run into errors and can't validate doc

# Get list of all pids and associated file names
all_pids <- get_package(d1c@mn, packageId, file_names = TRUE)
all_pids <- reorder_pids(all_pids$data, doc) #lines up pids w/correct file

# for loop to assign physicals for each file
for (i in 1:length(all_pids)){
  doc$dataset$spatialRaster[[i]]$physical <- pid_to_eml_physical(d1c@mn, all_pids[[i]])
}

eml_validate(doc)
  # TRUE


## -- update package -- ##
eml_path <- "~/Scratch/Pan_Arctic_surface_water_yearly_and_trend_over.xml"
write_eml(doc, eml_path)

dp <- replaceMember(dp, xml, replacement = eml_path)

myAccessRules <- data.frame(subject="CN=arctic-data-admins,DC=dataone,DC=org", 
                            permission="changePermission")
packageId <- uploadDataPackage(d1c, dp, public = FALSE,
                               accessRules = myAccessRules, quiet = FALSE)


#------------------------------------------------------------------------------

# edit funding section
eml_award1 <- eml$award()
eml_award1$funderName <- "National Aeronautics and Space Administration"
eml_award1$awardNumber <- "80NSSC19K134"
eml_award1$title <- "Future Investigators in NASA Earth and Space Science and Technology"
eml_award1$funderIdentifier <- NULL
eml_award1$awardUrl <- NULL

doc$dataset$project$award <- NULL
doc$dataset$project$award <- eml_award1
eml_validate(doc)


## -- Change FormatIDs -- ## 
# Raster files -- physicals
for(i in 1:length(doc$dataset$spatialRaster)){
  doc$dataset$spatialRaster[[i]]$physical$dataFormat$externallyDefinedFormat$formatName <- "image/geotiff"
}


# Raster files -- sysmeta@fileType



## -- change amps -- ##
# Method Steps


doc$dataset$methods$methodStep[[5]]$description <- 
  str_replace(doc$dataset$methods$methodStep[[5]]$description,
                                             "&amp;amp;amp;gt;", ">")

# Sampling Steps
doc$dataset$methods$sampling$studyExtent$description$para[[2]] <-
  str_replace(doc$dataset$methods$sampling$studyExtent$description$para[[2]], "&amp;amp;amp;amp;", "&")

doc$dataset$methods$sampling$studyExtent$description$para[[9]] <-
  str_replace(doc$dataset$methods$sampling$studyExtent$description$para[[9]], "&amp;amp;amp;amp;", "&")

doc$dataset$methods$sampling$samplingDescription$para[[9]] <-
  str_replace(doc$dataset$methods$sampling$samplingDescription$para[[9]],
              "&amp;amp;amp;amp;", "&")

doc$dataset$methods$sampling$samplingDescription$para[[11]] <-
  str_replace(doc$dataset$methods$sampling$samplingDescription$para[[11]],
              "&amp;amp;amp;amp;", "&")


## -- update format IDs -- ##
# get pids of all the files
all_pids <- get_package(d1c@mn, packageId, file_names = TRUE)
all_pids <- reorder_pids(all_pids$data, doc) #lines up pids w/correct file

for(i in 1:length(all_pids)){
  sysmeta <- getSystemMetadata(d1c@mn, all_pids[[i]])
  sysmeta@formatId <- "image/geotiff"
  updateSystemMetadata(d1c@mn, all_pids[[i]], sysmeta)
}



## -- dataset annotations -- ##
doc <- eml_categorize_dataset(doc, "Hydrology")


## -- update package -- ##
eml_path <- "~/Scratch/Pan_Arctic_surface_water_yearly_and_trend_over.xml"
write_eml(doc, eml_path)

dp <- replaceMember(dp, xml, replacement = eml_path)

myAccessRules <- data.frame(subject="CN=arctic-data-admins,DC=dataone,DC=org", 
                            permission="changePermission")
packageId <- uploadDataPackage(d1c, dp, public = FALSE,
                               accessRules = myAccessRules, quiet = FALSE)


## -- set rights & access -- ##
# Manually set ORCiD
# Elizabeth Webb
subject <- 'http://orcid.org/0000-0001-5398-4478'

pids <- arcticdatautils::get_package(d1c@mn, packageId)

set_rights_and_access(d1c@mn,
                      pids = c(xml, pids$data, packageId),
                      subject = subject,
                      permissions = c('read', 'write', 'changePermission'))




## -- remove more amps -- ##
library(stringr)
doc$dataset$methods$sampling$studyExtent$description$para[[2]] <- 
  str_replace(doc$dataset$methods$sampling$studyExtent$description$para[[2]],
              "&amp;amp;amp;", "&")

doc$dataset$methods$sampling$studyExtent$description$para[[9]] <- 
  str_replace(doc$dataset$methods$sampling$studyExtent$description$para[[9]],
              "&amp;amp;", "&")

doc$dataset$methods$sampling$samplingDescription$para[[11]] <- 
  str_replace(
    doc$dataset$methods$sampling$samplingDescription$para[[11]],
    "&amp;amp;", "&"
  )

doc$dataset$methods$sampling$samplingDescription$para[[9]] <- 
  str_replace(
    doc$dataset$methods$sampling$samplingDescription$para[[9]],
    "&amp;amp;", "&"
  )
