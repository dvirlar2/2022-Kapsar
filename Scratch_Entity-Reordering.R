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


