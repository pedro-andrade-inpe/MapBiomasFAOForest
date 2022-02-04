
splitRaster <- function(inputFile, outputDir, n.side){
  r  <- raster::raster(inputFile)
  er <- raster::extent(r)
  
  dx <- (er[2] - er[1]) / n.side  # extent of one tile in x direction
  dy <- (er[4] - er[3]) / n.side  # extent of one tile in y direction
  xs <- seq(er[1], by = dx, length = n.side) #lower left x-coordinates
  ys <- seq(er[3], by = dy, length = n.side) #lower left y-coordinates
  cS <- expand.grid(x = xs, y = ys)
  
  ## loop over extents and crop
  for(i in 1:nrow(cS)) {
    cat(paste0("tile ", i, "/", nrow(cS), "\n"))
    ex1 <- c(cS[i, 1], cS[i, 1] + dx, cS[i, 2], cS[i, 2] + dy)  # create extents for cropping raster
    cl1 <- raster::crop(r, ex1, progress = "text")
    outputFile <- paste0(tools::file_path_sans_ext(basename(inputFile)), "-tile-", i, ".tif")
    raster::writeRaster(cl1, paste0(outputDir, "/", outputFile), progress = "text", overwrite = TRUE)
  }
}

### SPLIT TIF OF EACH BIOME

mdir <- "C:/Users/pedro/Dropbox/pesquisa/2022/mapbiomas/original_41/"
files <- list.files(mdir, pattern = "\\.tif$")

for(file in files){
  biome <- tools::file_path_sans_ext(file)
  file_with_path <- paste0(mdir, file)
  mraster <- raster::raster(file_with_path)
  break_dim <- ceiling(sqrt(raster::ncell(mraster) / 1e8)) # 1e8 x 1e8 pixels
  newdir <- paste0(mdir, biome)
  dir.create(newdir)
  
  splitRaster(file_with_path, newdir, break_dim)
}
