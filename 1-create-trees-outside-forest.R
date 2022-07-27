### PROCESS EACH TILE TO BUILD FOREST DATA ACCORDING TO FAO
### GENERATE ANOTHER DIRECTORY WITH PROCESSED TILES
### THE TILES THAT DO NOT HAVE ANY FOREST WILL NOT BE SAVED IN THE OUTPUT

# DIRECTORY WHERE MAPBIOMAS TIFF FILES FOR BRAZIL ARE STORED
mdir <- "C:/Users/pedro/Dropbox/pesquisa/2022/aline/"

# DIRECTORY WHERE TILES ARE STORED (BUILT BY THE PREVIOUS SCRIPT)
folder <- tools::file_path_sans_ext(list.files(mdir, pattern = "\\.tif$"))[2]

# VALUE 3 MEANS FOREST IN MAPBIOMAS COLLECTION 6
myclass <- 3

# WHERE THE OUTPUT WILL BE STORED
dir.create(paste0(mdir, "result"))

files <- list.files(paste0(mdir, folder))

for(file in files){
  cat(paste0("Processing ", file, "\n"))
  
  mraster <- raster::raster(paste0(mdir, biome, "/", file))
  if(myclass %in% raster::unique(mraster)){
    direction <- 4 # 4 PIXELS IN THE NEIGHBORHOOD
    outputFile <- paste0(mdir, "result/", file)
    
    if(!file.exists(outputFile)){
      cat(paste0("Creating ", outputFile, "\n"))
      patched_raster <- landscapemetrics::get_patches(mraster, class = myclass, directions = direction)
      patched_raster <- patched_raster$layer_1[[paste0("class_", myclass)]]
      #raster::writeRaster(patched_raster, paste0(mdir, "result/","patchedRaster2.tif"))
      result <- raster::freq(patched_raster, useNA = "no")
      
      if(!is.null(dim(result))){
        result <- result %>%
          as.data.frame()
        
        treesWithoutForest <- result %>%
          dplyr::mutate(area = units::set_units(count * 0.09, "ha")) %>% # area of each forest patch
          dplyr::filter(area < units::set_units(0.5, "ha")) # minimum area to be considered forest
        
        if(dim(treesWithoutForest)[1] > 0){ # there are trees without forest
          max_value <- max(result$value)
          replacements <- rep(0, max_value)
          replacements[treesWithoutForest$value] <- 1
          
          finalRaster <- raster::calc(patched_raster, fun = function(x) replacements[x])
          raster::writeRaster(finalRaster, outputFile)
        }
        else
          stop("The other situation occurs but it was not implemented")
          # IF THIS SITUATION OCCURS THIS MUST BE IMPLEMENTED
      }
    }
  }
}
