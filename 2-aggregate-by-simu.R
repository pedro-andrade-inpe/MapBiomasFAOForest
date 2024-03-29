### AGGREGATE THE PIXELS INTO SIMU
### CREATES A CSV FILE WHERE EACH ROW CONTAINS THE NUMBER OF PIXELS OF A
### GIVEN TILE THAT OVERLAPS A GIVEN SIMU

require(magrittr)

# WHERE SIMU SHAPEFILE IS STORED
basedir <- "C:\\Users\\pedro\\Dropbox\\pesquisa\\2021\\r+\\simu-brazil\\"
v <- terra::vect(paste0(basedir, "simu-brazil.shp"))

# WHERE PROCESSED TILES ARE STORED (CREATED BY PREVIOUS SCRIPT)
mdir <- "C:/Users/pedro/Dropbox/pesquisa/2022/aline/result/"

files <- list.files(mdir, pattern = "\\.tif$")

mysum <- function(values) sum(values == 0, na.rm = TRUE)

for(file in files){
  inputfile <- paste0(mdir, file)
  inputRaster <- terra::rast(inputfile)
  cat(paste0("Processing ", file, "\n"))  
  
  for(i in 1:(dim(v)[1])){
    simu <- v[i,]
    pixels <- terra::extract(inputRaster, simu, fun = mysum)
    names(pixels) <- c("id", "quantity")
    pixels$id <- simu$ID
    pixels$file <- file
    pixels$line <- i
    if(pixels$quantity[1] > 0)
      write.table(pixels, "result-forest-simu-2020.csv", sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
  }
}
