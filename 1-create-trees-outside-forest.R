### PROCESS EACH TILE TO BUILD FOREST DATA ACCORDING TO FAO
### GENERATE ANOTHER DIRECTORY WITH PROCESSED TILES
### THE TILES THAT DO NOT HAVE ANY FOREST WILL NOT BE SAVED IN THE OUTPUT

# DIRECTORY WHERE MAPBIOMAS TIFF FILES FOR BRAZIL ARE STORED
mdir <- "C:/Users/pedro/Dropbox/pesquisa/2022/aline/"

# DIRECTORY WHERE TILES ARE STORED (BUILT BY THE PREVIOUS SCRIPT)
folder <- tools::file_path_sans_ext(list.files(mdir, pattern = "\\.tif$")[4])

# VALUE 3 MEANS FOREST IN MAPBIOMAS COLLECTION 6
myclass <- 3

# WHERE THE OUTPUT WILL BE STORED
dir.create(paste0(mdir, "result"), showWarnings = FALSE)

files <- list.files(paste0(mdir, folder), pattern = "\\.tif$", full.names = TRUE)

for(file in files){
  cat(paste0("Processing ", file, "\n"))
  
  mraster <- terra::rast(file)
  if(myclass %in% unique(values(mraster))){
    direction <- 4 # 4 PIXELS IN THE NEIGHBORHOOD
    outputFile <- paste0(mdir, "result/", basename(file))
    
    if(!file.exists(outputFile)){
      cat(paste0("Creating ", outputFile, "\n"))
      patched_raster <- landscapemetrics::get_patches(mraster, class = myclass, directions = direction)
      patched_raster <- patched_raster$layer_1[[paste0("class_", myclass)]]
      
      result <- terra::freq(patched_raster)
      
      if(!is.null(dim(result))){
        result <- as.data.frame(result)
        
        treesWithoutForest <- result %>%
          dplyr::mutate(area = units::set_units(count * 0.09, "ha")) %>% # area of each forest patch
          dplyr::filter(area < units::set_units(0.5, "ha")) # minimum area to be considered forest
        
        if(nrow(treesWithoutForest) > 0){ # there are trees without forest
          max_value <- max(result$value, na.rm = TRUE)
          replacements <- rep(0, max_value)
          replacements[treesWithoutForest$value] <- 1
          
          finalRaster <- terra::classify(patched_raster, data.frame(from = treesWithoutForest$value, to = 1), others = 0)
          terra::writeRaster(finalRaster, outputFile, overwrite = TRUE)
        }
        else{
          warning("The other situation occurs but it was not implemented")
        }
      }
    }
  }
}
