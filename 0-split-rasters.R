### SPLIT RASTER DATA INTO SEVERAL TILES TO ALLOW PROCESSING NEIGHBORHOODS
library(terra)

# DIRECTORY WHERE MAPBIOMAS TIFF FILES FOR BRAZIL ARE STORED
mdir <- "C:/Users/pedro/Dropbox/pesquisa/2022/aline/"

# SPLIT RASTER INTO SEVERAL TILES
splitRaster <- function(inputFile, outputDir, n.side){
  r  <- terra::rast(inputFile)
  er <- terra::ext(r)
  
  dx <- (er[2] - er[1]) / n.side  # extent of one tile in x direction
  dy <- (er[4] - er[3]) / n.side  # extent of one tile in y direction
  xs <- seq(er[1], by = dx, length = n.side) # lower left x-coordinates
  ys <- seq(er[3], by = dy, length = n.side) # lower left y-coordinates
  cS <- expand.grid(x = xs, y = ys)
  
  ## loop over extents and crop
  for(i in 1:nrow(cS)) {
    cat(paste0("tile ", i, "/", nrow(cS), "\n"))
    outputFile <- paste0(tools::file_path_sans_ext(basename(inputFile)), "-tile-", i, ".tif")
    output <- file.path(outputDir, outputFile)
    if(file.exists(output)) {
      cat("File already exists\n")
    } else {
      ex1 <- c(cS[i, 1], cS[i, 1] + dx, cS[i, 2], cS[i, 2] + dy)  # create extents for cropping raster
      cl1 <- terra::crop(r, terra::ext(ex1))
      terra::writeRaster(cl1, output, overwrite = TRUE)
    }
  }
}

# TIF FILES TO BE PROCESSED
files <- list.files(mdir, pattern = "\\.tif$")[4] # [4] AS 2000, 2012, 2016 DATA ARE ALSO IN THE DIRECTORY

for(file in files){
  fname <- tools::file_path_sans_ext(file)
  file_with_path <- file.path(mdir, file)
  mraster <- terra::rast(file_with_path)
  break_dim <- ceiling(sqrt(terra::ncell(mraster) / 5e7)) # TILES WITH 5E7 X 5E7 PIXELS 
  newdir <- file.path(mdir, fname)
  dir.create(newdir, showWarnings = FALSE)
  
  splitRaster(file_with_path, newdir, break_dim)
}

