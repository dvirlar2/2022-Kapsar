# Realized that the spatial Rasters are all messed up. 
# Physicals are wrong. Attributes are wrong. 
# Need to start FRESH and try again. 


## copied from Spatial Raster Test


# To Do's
# [X] Check for funding number
      # F20AC10873-00 ; USFWS
# [ ] Include funding number
# [X] Check for ethics statement
# [ ] Include ethics statement 
      # "These data were collected remotely via satellite by exactEartch Ltd (now Spire Global). In accordance with exactEarth's privacy policy, they have been de-identified and presented in aggregated form to preserve the anonymity of individual vessels. Data do not contain information pertaining to human or animal subjects." 
# [X] Format IDs
# [X] Unique Descriptions
# [X] FAIR publishing
# [ ] Carry over attributes
# [ ] Convert to SpatialVectors
# [ ] Convert to SpatialRasters
# [ ] Add dataset annotations



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

packageId <- "resource_map_urn:uuid:82a804cb-1ae8-43e5-94ff-8f5ec9865b87"
dp  <- getDataPackage(d1c, identifier = packageId, lazyLoad=TRUE, quiet=FALSE)


# Get the metadata id
xml <- selectMember(dp, name = "sysmeta@fileName", value = ".xml")


# get all versions of metadata
get_all_versions(d1c@mn, xml)


# Load in 16th version
doc <- read_eml(getObject(d1c@mn, "urn:uuid:c574b58f-7560-4712-bbd1-42b232cb87f4"))
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