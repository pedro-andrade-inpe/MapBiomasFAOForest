### PROCESS EACH TILE

mdir <- "C:/Users/pedro/Dropbox/pesquisa/2022/mapbiomas/original_41/"
biomes <- tools::file_path_sans_ext(list.files(mdir, pattern = "\\.tif$"))
myclass <- 3

dir.create(paste0(mdir, "result"))
dir.create(paste0(mdir, "result/d4"))
dir.create(paste0(mdir, "result/d8"))

for(biome in biomes){
  cat(paste0("Processing ", biome, "\n"))
  
  files <- list.files(paste0(mdir, biome))
  
  for(file in files){
    cat(paste0("Processing ", file, "\n"))
    
    mraster <- raster::raster(paste0(mdir, biome, "/", file))
    if(myclass %in% raster::unique(mraster)){
      for(direction in c(4, 8)){
        patched_raster <- landscapemetrics::get_patches(mraster, class = myclass, directions = direction)
        patched_raster <- patched_raster$layer_1[[paste0("class_", myclass)]]
        result <- raster::freq(patched_raster, useNA = "no") %>%
          as.data.frame()
        
        treesOutsideForest <- result %>%
          dplyr::mutate(area = units::set_units(count * 0.09, "ha")) %>%
          dplyr::filter(area < units::set_units(0.5, "ha"))
        
        if(dim(treesOutsideForest)[1] > 0){ # there are trees without forest
          max_value <- max(result$value)
          replacements <- rep(NA, max_value)
          replacements[treesOutsideForest$value] <- 1
          
          finalRaster <- raster::calc(patched_raster, fun = function(x) replacements[x])
          raster::writeRaster(finalRaster, paste0(mdir, "result/d", direction, "/", file))
        }
      }
    }
  }
}
