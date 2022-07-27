
require(magrittr)

basedir <- "C:\\Users\\pedro\\Dropbox\\pesquisa\\2021\\r+\\simu-brazil\\"
v <- terra::vect(paste0(basedir, "simu-brazil.shp"))
dim(v)



mdir <- "C:/Users/pedro/Dropbox/pesquisa/2022/mapbiomas/original_41/"
mforest <- read.table("result-forest-simu-2022-simu.csv", sep=",", header = FALSE)
missingforest <- read.table("result-forest-simu-2022-missing2.csv", sep=",", header = FALSE)

names(mforest) <- c("ID", "quantity", "file", "i")
names(missingforest) <- c("ID", "quantity", "file", "i")

missingforest %>% dplyr::filter(ID == "142121")


sum(mforest$quantity)

mforest <- mforest %>% 
  dplyr::filter(!(ID %in% missingforest$ID)) %>%
  rbind(missingforest)



units::install_unit("Mha", "1e6ha")
units::install_unit("kha", "1e3ha")

pixelToHa <- function(value) units::set_units(value * 0.09, "ha")
pixelTokHa <- function(value) units::set_units(pixelToHa(value), "kha")

mforest <- mforest %>%
  dplyr::mutate(area = pixelTokHa(quantity)) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarize(farea = sum(area)) %>%
  dplyr::select(ID, farea)

saveSimU <- function(df, filename){
  res <- paste0(df$ID, ".", "forestFAOMapBiomas\t", df$farea) %>% data.frame()
  
  header <- "PARAMETER FOREST\n(SimUid,class)  sourcing in 1000 ha per SimU\n/"
  
  colnames(res) <- header
  write.table(res, filename, row.names = FALSE, quote = FALSE)
}

saveSimU(mforest, "simu-mapbiomas-forest-fao-2020-v2.gms")

