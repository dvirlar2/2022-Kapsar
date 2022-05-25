# Realized that the spatial Rasters are all messed up. 
# Physicals are wrong. Attributes are wrong. 
# Need to start FRESH and try again. 


## copied from Spatial Raster Test


# To Do's
# [X] Check for funding number
      # F20AC10873-00 ; USFWS
# [X] Include funding number
# [X] Check for ethics statement
# [X] Include ethics statement 
      # "These data were collected remotely via satellite by exactEartch Ltd (now Spire Global). In accordance with exactEarth's privacy policy, they have been de-identified and presented in aggregated form to preserve the anonymity of individual vessels. Data do not contain information pertaining to human or animal subjects." 
# [X] Format IDs
# [X] Unique Descriptions
# [X] FAIR publishing
# [X] Carry over attributes
# [X] Convert to SpatialVectors
# [X] Convert to SpatialRasters
# [X] Add dataset annotations
# [X] Add physicals



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

packageId <- "resource_map_urn:uuid:b89c8e8f-fb19-488f-b9b2-46a37abfdee7"
dp  <- getDataPackage(d1c, identifier = packageId, lazyLoad=TRUE, quiet=FALSE)


# Get the metadata id
xml <- selectMember(dp, name = "sysmeta@fileName", value = ".xml")


doc <- read_eml(getObject(d1c@mn, xml))

eml_validate(doc)



## -- FAIR Principles -- ##
doc <- eml_add_publisher(doc)
doc <- eml_add_entity_system(doc)



## -- Discipline Annotations -- ##
doc <- eml_categorize_dataset(doc, "Human Geography")



## -- Change FormatIDs -- ## 
# Raster files
for(i in 1:720){
  if(doc$dataset$otherEntity[[i]]$entityType == "image/tiff"){
    doc$dataset$otherEntity[[i]]$entityType <- "image/geotiff"
  }
}


# Shapefiles
for(i in 1:720){
  if(doc$dataset$otherEntity[[i]]$entityType == "application/x-zip-compressed"){
    doc$dataset$otherEntity[[i]]$entityType <- "application/vnd.shp+zip"
  }
}



## -- Unique Entity Descriptions -- ## 
# Find only tif and shp files
Rasters <- which_in_eml(doc$dataset$otherEntity, "entityType", "image/geotiff")


Coastal <- which_in_eml(doc$dataset$otherEntity, "entityName", 
                        function(x) {
                          grepl("Coastal", x) # look for Coastal files only
                        })


# remove coastal from rasters
Rasters <- Rasters[!Rasters %in% Coastal]


Vectors <- which_in_eml(doc$dataset$otherEntity, "entityType", "application/vnd.shp+zip")


# Apply above to multiple entities
for(i in Rasters){
  t <- doc$dataset$otherEntity[[i]]$entityName
  t_split <- strsplit(t, split = "_|.tif")
  
  # put split string into entity description
  doc$dataset$otherEntity[[i]]$entityDescription <- 
    paste("Monthly vessel intensity within", t_split[[1]][[5]], "pixel of", 
          t_split[[1]][4], "vessels during", 
          month.name[as.numeric(t_split[[1]][3])], 
          t_split[[1]][2])
}


# Loop for Coastal data
for(i in Coastal){
  t <- doc$dataset$otherEntity[[i]]$entityName
  t_split <- strsplit(t, split = "_|.tif")
  
  # put split string into entity description
  doc$dataset$otherEntity[[i]]$entityDescription <- 
    paste("Monthly vessel intensity within 1km pixel of coastlines of the study area during", 
          month.name[as.numeric(t_split[[1]][3])], 
          t_split[[1]][2])
}


# Loop for Vector data
for(i in Vectors){
  t <- doc$dataset$otherEntity[[i]]$entityName
  t_split <- strsplit(t, split = "_|.zip")
  
  # put split string into entity description
  doc$dataset$otherEntity[[i]]$entityDescription <- 
    paste("Vessel data generated from satellite-based automatic identification system (AIS). Data include summaries of vessel speed, number of unique ships, and number of operating days (vessel x date combinations) aggregated by year, month, and ship type. This shapefile contains data during", month.name[as.numeric(t_split[[1]][3])], 
          t_split[[1]][2])
}

eml_validate(doc)
  # TRUE 



## -- Carry Over Attributes -- ##
## -- Carry Over Attributes -- ##
# Reference documents:
# Raster_2020_12_Tanker_25km.tif  --> for all 25km
# Raster_2020_12_Tanker_10km.tif  --> for all 10km
# Coastal_2020_12.tif             --> for all coastal
# SpeedHex_2020_12.zip            --> for all SpeedHex


which_in_eml(doc$dataset$otherEntity, "entityName", 
             function(x) {
               grepl("Raster_2020_12_Tanker_25km.tif", x) # find the reference files
             })


# Raster_2020_12_Tanker_25km.tif  --> [720]
twentyFiveKm <- which_in_eml(doc$dataset$otherEntity, "entityName", 
                             function(x) {
                               grepl("25km.tif", x) # find the reference files
                             })

# Remove reference file from above
twentyFiveKm <- head(twentyFiveKm, -1) # remove last value


# Raster_2020_12_Tanker_10km.tif  --> [1]
tenKm <- which_in_eml(doc$dataset$otherEntity, "entityName", 
                             function(x) {
                               grepl("10km.tif", x) # find the reference files
                             })

# Remove reference file from above
tenKm <- tail(tenKm, -1) # removing the first value


# Coastal_2020_12.tif             --> [193]
# Remove reference file
coast_ref <- tail(Coastal, -1)

# SpeedHex_2020_12.zip            --> [336]
# Remove reference file
vector_ref <- head(Vectors, -1) # remove the last value


# Assign 25km reference attributes
twentyFiveKm_attList <- doc$dataset$otherEntity[[720]]$attributeList


# Create reference id
doc$dataset$otherEntity[[720]]$attributeList$id <- "25km_attributes" 
# use any unique name for your id


for (i in twentyFiveKm){ # DON'T OVERWRITE THE REFERENCE ENTITY
  doc$dataset$otherEntity[[i]]$attributeList <- twentyFiveKm_attList
  doc$dataset$otherEntity[[i]]$attributeList <- list(references = "25km_attributes") # use the id you set above
}




# Assign 10km reference attributes
tenKm_attList <- doc$dataset$otherEntity[[1]]$attributeList


# Create reference id
doc$dataset$otherEntity[[1]]$attributeList$id <- "10km_attributes" 
# use any unique name for your id


for (i in tenKm){ # DON'T OVERWRITE THE REFERENCE ENTITY
  doc$dataset$otherEntity[[i]]$attributeList <- tenKm_attList
  doc$dataset$otherEntity[[i]]$attributeList <- list(references = "10km_attributes") # use the id you set above
}




# Assign coastal reference attributes
coastal_attList <- doc$dataset$otherEntity[[193]]$attributeList


# Create reference id
doc$dataset$otherEntity[[193]]$attributeList$id <- "coastal_attributes" 
# use any unique name for your id


for (i in coast_ref){ # DON'T OVERWRITE THE REFERENCE ENTITY
  doc$dataset$otherEntity[[i]]$attributeList <- coastal_attList
  doc$dataset$otherEntity[[i]]$attributeList <- list(references = "coastal_attributes") # use the id you set above
}




# Assign SpeedHex reference attributes
speedHex_attList <- doc$dataset$otherEntity[[336]]$attributeList
# SpeedHex files --> [265:336]


# Create reference id
doc$dataset$otherEntity[[336]]$attributeList$id <- "speedHex_attributes" 
# use any unique name for your id


for (i in vector_ref){ # DON'T OVERWRITE THE REFERENCE ENTITY
  doc$dataset$otherEntity[[i]]$attributeList <- speedHex_attList
  doc$dataset$otherEntity[[i]]$attributeList <- list(references = "speedHex_attributes") # use the id you set above
}

eml_validate(doc)
  # TRUE


## -- Spatial Entity Set Up -- ##
# Retrieve all pids, and line them up properly
# retrieve all pids
all_pids <- get_package(d1c@mn, packageId, file_names = TRUE)
all_pids <- reorder_pids(all_pids$data, doc) #lines up pids w/correct file
  # performed sanity checks, things seem to check out


# keep only the vector pids
vector_pids <- all_pids[Vectors]
  # sanity check: complete


# keep only raster pids
raster_pids <- all_pids[!all_pids %in% vector_pids]
  # sanity check: complete



## -- Create SpatialVector -- ##

vector_entity <- doc$dataset$otherEntity[Vectors]

spatialVector <- vector("list", length = length(vector_entity))

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


# Check to make sure names are in expected order
for(i in 1:72){
  print(spatialVector[[i]]$entityName)
}


# Check to make sure the pids are aligned
for(i in 1:72){
  if(vector_pids[[i]] == spatialVector[[i]]$id){
    print("TRUE")
  }
}

# Sanity check: spatialVectors create physicals... based on the pids assigned
spatialVector[[12]]$physical$objectName
  # sanity checks out... for now






## -- create spatialRasters -- ##
# load in the edited eml_raster function
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


# kapsar entity re-organizing


# -----------------------------------------------------------------------------

# Coastal
Coastal <- which_in_eml(doc$dataset$otherEntity, "entityName", 
                        function(x) {
                          grepl("Coastal", x) # look for Coastal files only
                        })

Coastal <- doc$dataset$otherEntity[Coastal]

# reverse the order so it will match the order of the file names
Coastal <- rev(Coastal)


# -----------------------------------------------------------------------------


# 10km
tenKm <- which_in_eml(doc$dataset$otherEntity, "entityName", 
                      function(x) {
                        grepl("10km.tif", x) # find the reference files
                      })


tenKm <- doc$dataset$otherEntity[tenKm]

# extract 10km names
tenKm_names <- vector()
for(i in 1:length(tenKm)){
  tenKm_names[[i]] <- tenKm[[i]]$entityName
}

# put entities in ascending order
tenKm_asc <- tenKm[order(tenKm_names)]







# -----------------------------------------------------------------------------

# 25km
twentyFiveKm <- which_in_eml(doc$dataset$otherEntity, "entityName", 
                             function(x) {
                               grepl("25km.tif", x) # find the reference files
                             })


twentyFiveKm <- doc$dataset$otherEntity[twentyFiveKm]

# extract 10km names
twentyFiveKm_names <- vector()
for(i in 1:length(twentyFiveKm)){
  twentyFiveKm_names[[i]] <- twentyFiveKm[[i]]$entityName
}

# put entities in ascending order
twentyFiveKm_asc <- twentyFiveKm[order(twentyFiveKm_names)]


# -----------------------------------------------------------------------------


# Combine separate but ordered rasters into one combined doc again
raster_combo <- c(Coastal, tenKm_asc, twentyFiveKm_asc)


# -----------------------------------------------------------------------------


# Load the coastal tifs
# Create object with file path to folder containing all the tif files
coastal_folder <- "~/Tickets-2022/2022-Kapsar/coastal-tifs"

# create list of the file names
coastal_names <- list.files(coastal_folder, full.names = TRUE)



# -----------------------------------------------------------------------------


# Load the 10km tifs
# Create object with file path to folder containing all the tif files
tenKm_folder <- "~/Tickets-2022/2022-Kapsar/10km-tifs"

# create list of the file names
tenKm_names <- list.files(tenKm_folder, full.names = TRUE)


# -----------------------------------------------------------------------------


# Load the 20km tifs
# Create object with file path to folder containing all the tif files
twentyKm_folder <- "~/Tickets-2022/2022-Kapsar/25km-tifs"

# create list of the file names
twentyKm_names <- list.files(twentyKm_folder, full.names = TRUE)


# -----------------------------------------------------------------------------


# Combine separate but ordered rasters into one combined doc again
raster_names <- c(coastal_names, tenKm_names, twentyKm_names)


# -----------------------------------------------------------------------------


# check to make sure raster_entity and raster_names are aligned
for(i in 201:400){
  if(str_replace_all(basename(raster_names[i]), "-", "_") == raster_combo[[i]]$entityName){
    print("TRUE")
  }
  else{
    print("FALSE")
    print(c(str_replace_all(basename(raster_names[i]), "-", "_"), raster_combo[[i]]$entityName))
  }
}

# SANITY CHECK: COMPLETE


# -----------------------------------------------------------------------------

# Create empty vector length of raster_names.
# We're going to use this to iterate through
spatialRaster <- vector("list", length(raster_names))

# create spatial raster entities
for(i in 1:length(raster_names)){
  spatialRaster[[i]] <- dvk_get_raster_metadata(raster_names[i],
                                                coord_name = "GCS_North_American_1983",  # "GCS_WGS_1984" <- from 2022-Webb, # retrieved from get_coord_list
                                                raster_combo[[i]]$attributeList) 
}


# -----------------------------------------------------------------------------

# Assign newly created spatialRaster AND spatialVector to the doc
doc$dataset$spatialRaster <- spatialRaster
doc$dataset$spatialVector <- spatialVector

# Null the otherEntities so that you don't have repeating IDs that would cause issues
doc$dataset$otherEntity <- NULL

# Moment of truth
eml_validate(doc)


## -- Look for Amps -- ##
# abstract
doc$dataset$abstract$para <- str_replace_all(doc$dataset$abstract$para,
                                             "&gt;", ">")

doc$dataset$abstract$para <- str_replace(doc$dataset$abstract$para,
                                             "GPS", "Global Positioning System (GPS)")

# Sampling Description
doc$dataset$methods$sampling$samplingDescription <- str_replace_all(doc$dataset$methods$sampling$samplingDescription,
                                                                    "lt;", "<")


## -- Assign Funding -- ##
# remove funding section
doc$dataset$project$funding <- NULL


eml_award <- eml$award()
eml_award$funderName <- "United States Fish & Wildlife Service"
eml_award$awardNumber <- "F20AC10873-00"
eml_award$title <- "North Pacific and Arctic Marine Vessel Traffic Dataset (2015-2020)"
eml_award$funderIdentifier <- NULL
eml_award$awardUrl <- NULL

doc$dataset$project$award <- NULL
doc$dataset$project$award <- eml_award
eml_validate(doc)



## -- Contact Info -- ##
doc$dataset$contact$positionName <- "PhD Candidate"


doc$dataset$contact$electronicMailAddress <- "kelly.kapsar@gmail.com"
doc$dataset$contact$onlineUrl <- "https://www.canr.msu.edu/people/kelly_kapsar"


## -- add in ERP -- ##
doc$dataset$methods$methodStep[[1]]$description$section <- "<title>Ethical Research Practices</title><para>These data were collected remotely via satellite by exactEartch Ltd (now Spire Global). In accordance with exactEarth's privacy policy, they have been de-identified and presented in aggregated form to preserve the anonymity of individual vessels. Data do not contain information pertaining to human or animal subjects.</para>"


eml_validate(doc)


## -- Update package -- ##
eml_path <- "~/Scratch/North_Pacific_and_Arctic_Marine_Vessel_Traffic.xml"
write_eml(doc, eml_path)

dp <- replaceMember(dp, xml, replacement = eml_path)

myAccessRules <- data.frame(subject="CN=arctic-data-admins,DC=dataone,DC=org", 
                            permission="changePermission")

newPackageId <- uploadDataPackage(d1c, dp, public = FALSE,
                                  accessRules = myAccessRules, quiet = FALSE)


## -- set rights & access -- ##
# Manually set ORCiD
# kelly Kapsar
subject <- 'http://orcid.org/0000-0002-2048-5020'

pids <- arcticdatautils::get_package(d1c@mn, packageId)

set_rights_and_access(d1c@mn,
                      pids = c(xml, pids$data, packageId),
                      subject = subject,
                      permissions = c('read', 'write', 'changePermission'))




# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
## -- add physicals -- ##
# all_pids <- get_package(d1c@mn, packageId, file_names = TRUE)
# all_pids <- reorder_pids(all_pids$data, doc)
#   # this doesn't actually look to be correct... 
# 
# 
# for(i in 1:20){
#   print(doc$dataset$spatialRaster[[i]]$entityName)
# }
# 
# doc$dataset$spatialRaster[[1]]$attributeList


# make a list of all the pids according to the document name, then assign them
# to the correct spatial raster

raster_names <- vector()

for(i in 1:length(doc$dataset$spatialRaster)){
 raster_names[[i]] <- (doc$dataset$spatialRaster[[i]]$entityName)
}


all_pids <- vector(length = length(raster_names))

for(i in 1:length(raster_names)){
  all_pids[[i]] <- selectMember(dp, name = "sysmeta@fileName", value = raster_names[[i]])
}

# test out the physical
test_phys <- vector("list")

for (i in 1:length(all_pids)){
  test_phys[[i]] <- pid_to_eml_physical(d1c@mn, all_pids[[i]])
}
test_phys[[1]]$objectName
test_phys[[1]]$dataFormat
test_phys[[1]]$id
test_phys[[1]]$authentication
test_phys[[1]]$distribution

test_phys[[420]]$objectName
raster_names[420]
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# Get all the raster entities
rasters <- which_in_eml(doc_old$dataset$otherEntity, "entityType", "image/tiff")
rasters <- doc$dataset$otherEntity[rasters]


# # re-arrange them to be in order
# names <- vector()
# for (i in 1:length(rasters)){
#   names[[i]] <- rasters[[i]]$entityName
# }
# rasters_ordered <- rasters[order(names)]




# raster_names <- vector()
# 
# for(i in 1:length(doc$dataset$spatialRaster)){
#   raster_names[[i]] <- (rasters[[i]]$entityName)
# }


# Create list of raster names in order of current version doc
# Why?? Because the names are already in ORDER you want them in
raster_names <- vector()

for(i in 1:length(doc_new$dataset$spatialRaster)){
  raster_names[[i]] <- (doc_new$dataset$spatialRaster[[i]]$entityName)
  raster_names[[i]] <- str_replace_all(raster_names[[i]], "-", "_")
}


# # create an "old" dp version with the resource map that matches the old_xml in doc_old
# dp_old <- getDataPackage(d1c, 
#                          identifier = "urn:uuid:28c37466-9e94-41e7-adaa-2ef10fa687cf", 
#                          lazyLoad=TRUE, quiet=FALSE)

all_pids <- vector(length = length(raster_names))

for(i in 1:length(raster_names)){
  all_pids[[i]] <- selectMember(dp, name = "sysmeta@fileName", value = raster_names[[i]])
}

# create test physicals
test_phys <- vector("list")

for (i in 1:length(all_pids)){
  test_phys[[i]] <- pid_to_eml_physical(d1c@mn, all_pids[[i]])
}


# assign physicals for real
for (i in 1:length(all_pids)){
  doc_new$dataset$spatialRaster[[i]]$physical <- pid_to_eml_physical(d1c@mn, all_pids[[i]])
}

eml_validate(doc_new)


## -- Update package -- ##
eml_path <- "~/Scratch/North_Pacific_and_Arctic_Marine_Vessel_Traffic.xml"
write_eml(doc_new, eml_path)

dp <- replaceMember(dp, xml, replacement = eml_path)

myAccessRules <- data.frame(subject="CN=arctic-data-admins,DC=dataone,DC=org", 
                            permission="changePermission")

newPackageId <- uploadDataPackage(d1c, dp, public = FALSE,
                                  accessRules = myAccessRules, quiet = FALSE)


## -- set rights & access -- ##
# Manually set ORCiD
# kelly Kapsar
subject <- 'http://orcid.org/0000-0002-2048-5020'

pids <- arcticdatautils::get_package(d1c@mn, packageId)

set_rights_and_access(d1c@mn,
                      pids = c(xml, pids$data, packageId),
                      subject = subject,
                      permissions = c('read', 'write', 'changePermission'))


#------------------------------------------------------------------------------
# Change all - to _ in raster entity names and re-upload. I think this will change the order on page

# remove ;gt again
## -- Look for Amps -- ##
# abstract
doc$dataset$abstract$para <- str_replace_all(doc$dataset$abstract$para,
                                             "&amp;amp;gt;", ">")

doc$dataset$abstract$para <- str_replace_all(doc$dataset$abstract$para,
                                             "&amp;gt;", ">")


## -- Change FormatIDs -- ## 
# Raster files
for(i in 1:length(doc$dataset$spatialRaster)){
  doc$dataset$spatialRaster[[i]]$physical$dataFormat$externallyDefinedFormat$formatName <- "image/geotiff"
}


for(i in 1:length(doc$dataset$spatialRaster)){
  doc$dataset$spatialRaster[[i]]$entityName <- 
    str_replace_all(doc$dataset$spatialRaster[[i]]$entityName, "-", "_")
}


## -- update format IDs -- ##
# get pids of all the files

raster_names <- vector()
for(i in 1:length(doc$dataset$spatialRaster)){
  raster_names[[i]] <- (doc$dataset$spatialRaster[[i]]$entityName)
}


# Only raster pids
all_pids <- vector(length = length(raster_names))
for(i in 1:length(raster_names)){
  all_pids[[i]] <- selectMember(dp, name = "sysmeta@fileName", value = raster_names[[i]])
}


for(i in 1:length(all_pids)){
  sysmeta <- getSystemMetadata(d1c@mn, all_pids[[i]])
  sysmeta@formatId <- "image/geotiff"
  updateSystemMetadata(d1c@mn, all_pids[[i]], sysmeta)
}



eml_validate(doc)


## -- Update package -- ##
eml_path <- "~/Scratch/North_Pacific_and_Arctic_Marine_Vessel_Traffic.xml"
write_eml(doc, eml_path)

dp <- replaceMember(dp, xml, replacement = eml_path)

myAccessRules <- data.frame(subject="CN=arctic-data-admins,DC=dataone,DC=org", 
                            permission="changePermission")

newPackageId <- uploadDataPackage(d1c, dp, public = FALSE,
                                  accessRules = myAccessRules, quiet = FALSE)


## -- set rights & access -- ##
# Manually set ORCiD
# kelly Kapsar
subject <- 'http://orcid.org/0000-0002-2048-5020'

pids <- arcticdatautils::get_package(d1c@mn, packageId)

set_rights_and_access(d1c@mn,
                      pids = c(xml, pids$data, packageId),
                      subject = subject,
                      permissions = c('read', 'write', 'changePermission'))
