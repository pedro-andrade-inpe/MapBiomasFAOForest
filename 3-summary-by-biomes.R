

mdata <- read.csv("result_biomes.csv", header = FALSE)
names(mdata) <- c("size", "area", "biome", "directions", "file")

units::install_unit("Mha", "1e6ha")

mdata %>%
  dplyr::group_by(biome, directions) %>%
  dplyr::summarise(total_area = sum(area)) %>%
  dplyr::mutate(total_area = units::as_units(total_area, "ha")) %>%
  dplyr::mutate(total_area = units::set_units(total_area, "Mha"))

mdata %>%
  tidyr::spread(key = size, value = area) %>%
  dplyr::group_by(directions, biome) %>%
  dplyr::summarise(average_small_prop = mean(prop_small, na.rm = TRUE)) %>%
  tidyr::spread(key = directions, value = average_small_prop)

mdata %>%
  dplyr::filter(directions == 8 & biome == "AMAZONIA") %>%
  tidyr::spread(key = size, value = area) %>%
  dplyr::mutate(prop_small = small / (small + big) * 100) %>%
  dplyr::filter(!is.na(prop_small)) %>%
  `$`(prop_small) %>%
  hist(nclass = 20)

mdata %>%
  dplyr::group_by(biome, directions, size) %>%
  dplyr::summarise(total_area = sum(area)) %>%
  dplyr::mutate(total_area = units::as_units(total_area, "ha")) %>%
  dplyr::mutate(total_area = units::set_units(total_area, "Mha")) %>%
  tidyr::spread(key = size, value = total_area) %>%
  dplyr::rename(forestInBoth = big, forestOnlyInMapBiomas = small)


mdata %>%
  dplyr::filter(biome == "AMAZONIA") %>%
  tidyr::spread(key = size, value = area) %>%
  dplyr::filter(!is.na(small)) %>%
  dplyr::arrange(small) # AMAZONIA-tile-60.tif is the one with most small 21kha
