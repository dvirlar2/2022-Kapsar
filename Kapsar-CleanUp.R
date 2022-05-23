# Realized that the spatial Rasters are all messed up. 
# Physicals are wrong. Attributes are wrong. 
# Need to start FRESH and try again. 

## copied from Spatial Raster Test
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


# load in the 20th version
doc <- read_eml(getObject(d1c@mn, "urn:uuid:0fd5d4e4-4c0e-4f32-8387-ef5176d9c04c"))
eml_validate(doc)



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
