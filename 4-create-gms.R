
require(magrittr)

basedir <- "C:\\Users\\pedro\\Dropbox\\pesquisa\\2021\\r+\\simu-brazil\\"
v <- terra::vect(paste0(basedir, "simu-brazil.shp"))
dim(v)

mdir <- "C:/Users/pedro/Dropbox/pesquisa/2022/mapbiomas/original_41/"

files <- list.files(mdir, pattern = "\\.tif$")

mysum <- function(values) sum(values == 3, na.rm = TRUE)

for(file in files){
  inputfile <- paste0(mdir, file)
  inputRaster <- terra::rast(inputfile)
  
  for(i in 1:(dim(v)[1])){
    cat(paste0("Processing ", i, " ", file, "\n"))  
    simu <- v[i,]
    pixels <- terra::extract(inputRaster, simu, fun = mysum)
    names(pixels) <- c("id", "quantity")
    pixels$id <- simu$ID
    pixels$file <- file
    pixels$line <- i
    write.table(pixels, "result-forest-simu.csv", sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
  }
}

#################################################################################

computePixelsPerSimU <- function(distance){
  outputFile <- paste0("result-outside-forest-simu-", distance, ".csv")
  mdir <- paste0(
    "C:\\Users\\pedro\\Dropbox\\pesquisa\\2022\\mapbiomas\\original_41\\result\\d",
    distance,
    "\\")
  
  files <- list.files(mdir)

  mysum <- function(values) sum(values, na.rm = TRUE)
  
  for(file in files){
    cat(paste0("Processing ", file, "\n"))  
    inputfile <- paste0(mdir, file)
    inputRaster <- terra::rast(inputfile)
    pixels <- terra::extract(inputRaster, v, fun = mysum)
    names(pixels) <- c("id", "quantity")
    pixels$file <- file
    write.table(pixels, outputFile, sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
  }
}

computePixelsPerSimU(4)
computePixelsPerSimU(8)


##############################################################################
## process the csv data to create qms files

require(dplyr)

setwd("C:/Users/pedro/Dropbox/github/GitHub/MapBiomasFAOForest")

mforest <- read.table("result-forest-simu.csv", sep=",", header = FALSE)

names(mforest) <- c("ID", "quantity", "biome", "i")

units::install_unit("Mha", "1e6ha")
units::install_unit("kha", "1e3ha")

pixelToHa <- function(value) units::set_units(value * 0.09, "ha")
pixelTokHa <- function(value) units::set_units(pixelToHa(value), "kha")

mforest <- mforest %>%
  dplyr::mutate(area = pixelTokHa(quantity)) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarize(farea = sum(area)) %>%
  dplyr::select(ID, farea)

mnames <- c("ID", "pixels", "biome")

rd4 <- read.csv("result-outside-forest-simu-4.csv", header = FALSE)
names(rd4) <- mnames

rd8 <- read.csv("result-outside-forest-simu-8.csv", header = FALSE)
names(rd8) <- mnames

basedir <- "C:\\Users\\pedro\\Dropbox\\pesquisa\\2021\\r+\\simu-brazil\\"
v <- terra::vect(paste0(basedir, "simu-brazil.shp"))

rd8$ID <- v$ID[rd8$ID]
rd4$ID <- v$ID[rd4$ID]

toSimU <- function(mdata)
  mdata %>%
  dplyr::group_by(ID) %>%
  dplyr::summarize(quantity = sum(pixels)) %>%
  dplyr::mutate(area = pixelTokHa(quantity)) %>% 
  dplyr::select(ID, area)

byBiomes <- function(mdata)
  mdata %>%
  dplyr::mutate(biome = substr(biome, 1, 4)) %>%
  dplyr::group_by(biome) %>%
  dplyr::summarize(quantity = sum(pixels)) %>%
  dplyr::mutate(area = pixelToHa(quantity)) %>%
  dplyr::select(biome, area)

r4simu <- toSimU(rd4)
names(r4simu) <- c("ID", "area4")
r4biomes <- byBiomes(rd4)

r8simu <- toSimU(rd8)
names(r8simu) <- c("ID", "area8")
r8biomes <- byBiomes(rd8)

basedir <- "C:\\Users\\pedro\\Dropbox\\pesquisa\\2021\\r+\\simu-brazil\\"
shp <- sf::read_sf(paste0(basedir, "simu-brazil.shp"))

result <- dplyr::left_join(shp, r4simu) %>%
  dplyr::mutate(area4 = tidyr::replace_na(area4, 0)) %>%
  dplyr::left_join(r8simu) %>%
  dplyr::mutate(area8 = tidyr::replace_na(area8, 0)) %>%
  dplyr::left_join(mforest) %>%
  dplyr::mutate(farea = tidyr::replace_na(farea, 0))

saveSimU <- function(df, filename){
  res1 <- paste0(df$ID, ".", "forestMapBiomas\t", df$farea) %>% data.frame()
  res2 <- paste0(df$ID, ".", "treesOutsideForest4\t", df$area4) %>% data.frame()
  res3 <- paste0(df$ID, ".", "treesOutsideForest8\t", df$area8) %>% data.frame()
  
  res <- rbind(res1, res2, res3)
  
  header <- "PARAMETER FOREST\n(SimUid,class)  sourcing in 1000 ha per SimU\n/"
  
  colnames(res) <- header
  write.table(res, filename, row.names = FALSE, quote = FALSE)
}

saveSimU(result, "simu-mapbiomas-forest-fao.gms")

sum(result$farea)
sum(result$area4)
sum(result$area8)
