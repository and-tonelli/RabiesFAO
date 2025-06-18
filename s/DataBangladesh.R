require(tidyverse)
require(rnaturalearth)
require(sf)
require(terra)
require(readxl)
library(exactextractr)

BGD_adm2 <- read_sf("Data/gadm41_BGD_2.shp")
head(BGD_adm2)

# Rabies cases from Ghosh et al., 2024 (doi.org/10.1016/j.lansea.2024.100452)
rabies_data2019 <- read_excel("Data/mmc7.xlsx", sheet = 1)
rabies_data2020 <- read_excel("Data/mmc7.xlsx", sheet = 2)
rabies_data2021 <- read_excel("Data/mmc7.xlsx", sheet = 3)
rabies_data2022 <- read_excel("Data/mmc7.xlsx", sheet = 4)

# District name harmonisation
rabies_data2019$District[rabies_data2019$District == "Coxs-bazar"] <- "Cox'S Bazar"
rabies_data2019$District[rabies_data2019$District == "Panchagar"] <- "Panchagarh"
rabies_data2019$District[rabies_data2019$District == "Netrokona"] <- "Netrakona"
rabies_data2019$District[rabies_data2019$District == "Moulvibazar"] <- "Maulvibazar"
rabies_data2019$District[rabies_data2019$District == "Brahmanbaria"] <- "Brahamanbaria"
rabies_data2019$District[rabies_data2019$District == "Chapainababganj"] <- "Nawabganj"
rabies_data2019$District[rabies_data2019$District == "Narsinghdi"] <- "Narsingdi"
rabies_data2019$District[rabies_data2019$District == "Khagrachari"] <- "Khagrachhari"
rabies_data2019$District[rabies_data2019$District == "Coxs bazar"] <- "Cox'S Bazar"

rabies_data2020$District[rabies_data2020$District == "Coxsbazar"] <- "Cox'S Bazar"
rabies_data2020$District[rabies_data2020$District == "Cumilla"] <- "Comilla"
rabies_data2020$District[rabies_data2020$District == "Keraniganj"] <- "Dhaka" 
rabies_data2020$District[rabies_data2020$District == "Munsiganj"] <- "Munshiganj"
rabies_data2020$District[rabies_data2020$District == "Mymensigh"] <- "Mymensingh"

rabies_data2021$District[rabies_data2021$District == "Chattogram"] <- "Chittagong"
rabies_data2021$District[rabies_data2021$District == "chattogram"] <- "Chittagong"
rabies_data2021$District[rabies_data2021$District == "Joissore"] <- "Jessore"
rabies_data2021$District[rabies_data2021$District == "Kishorganj"] <- "Kishoreganj"
rabies_data2021$District[rabies_data2021$District == "Bagura"] <- "Bogra"
rabies_data2021$District[rabies_data2021$District == "barishal"] <- "Barisal"
rabies_data2021$District[rabies_data2021$District == "dhaka"] <- "Dhaka"

rabies_data2022$District <- str_to_sentence(rabies_data2022$District)
rabies_data2022$District[rabies_data2022$District == "Cumilla"] <- "Comilla"
rabies_data2022$District[rabies_data2022$District == "Sariyatpur"] <- "Shariatpur"
rabies_data2022$District[rabies_data2022$District == "Potuakhali"] <- "Patuakhali"
rabies_data2022$District[rabies_data2022$District == "Narsindi"] <- "Narsingdi"
rabies_data2022$District[rabies_data2022$District == "Chittagang"] <- "Chittagong"
rabies_data2022$District[rabies_data2022$District == "Laxmipur"] <- "Lakshmipur"
rabies_data2022$District[rabies_data2022$District == "Jhinaidaha"] <- "Jhenaidah"
rabies_data2022$District[rabies_data2022$District == "Dinazpur"] <- "Dinajpur"

setdiff(rabies_data2022$District, BGD_adm2$NAME_2)

# Aggregate cases by District
rbind(rabies_data2019 %>% group_by(District) %>% summarise(cases = n()),
      rabies_data2020 %>% group_by(District) %>% summarise(cases = n()),
      rabies_data2021 %>% group_by(District) %>% summarise(cases = n()),
      rabies_data2022 %>% group_by(District) %>% summarise(cases = n())) %>% 
  group_by(District) %>% 
  summarise(cases = sum(cases)) -> rabies_cases
  
dataset1 <- merge.data.frame(BGD_adm2, rabies_cases, by.x = 7, by.y = 1, all.x = T)

#### COVARIATES ####
# Land use and Land cover from Global Land Cover-SHARE (GLC-SHARE)
LULC_data <- rast("Data/glc_shv10_DOM.tif")
plot(LULC_data)

crs(LULC_data) <- sf::st_crs(4326)$proj4string

unique(values(LULC_data$glc_shv10_DOM))

results <- exact_extract(LULC_data, BGD_adm2, function(values, coverage_fraction) {
  valid <- !is.na(values)
  values <- values[valid]
  weights <- coverage_fraction[valid]
  
  props <- tapply(weights, values, sum)
  props <- props / sum(props)  # 100%
  
  return(props)
}, progress = TRUE)

# To dataframe
df_list <- lapply(results, function(x) {
  out <- as.data.frame(t(x))
  # Assicura che tutti i livelli siano presenti
  all_classes <- as.character(0:11)
  missing <- setdiff(all_classes, colnames(out))
  out[missing] <- 0
  out <- out[, sort(colnames(out))]
  return(out)
})

# Bind together
LULC_df <- bind_rows(df_list)

# Add district names
LULC_df$District <- BGD_adm2$NAME_2

LULC_df <- LULC_df %>%
  select(District, everything())

colnames(LULC_df)[-1] <- paste0("Class_", colnames(LULC_df)[-1])

dataset2 <- merge.data.frame(dataset1, LULC_df, by.x = 1, by.y = 1, all.x = T)

# population data from WorldPop
pop_data <- rast("Data/BGD_ppp_2020_adj_v2.tif")
pop_df <- exact_extract(pop_data, BGD_adm2, "sum")
pop_df <- tibble(District = BGD_adm2$NAME_2, population = pop_df)

dataset3 <- merge.data.frame(dataset2, pop_df, by.x = 1, by.y = 1, all.x = T)


# livestock data from Bangladesh Bureau of Statistics. Agriculture and Rural Statistics Survey (ARSS) Report-2018
livestock_data <- read.csv("Data/Bangladesh_Livestock.csv")
setdiff(livestock_data$Division_or_District, BGD_adm2$NAME_2)

# District name harmonisation
livestock_data$Division_or_District[livestock_data$Division_or_District == "Barishal"] <- "Barisal"
livestock_data$Division_or_District[livestock_data$Division_or_District == "Cumilla"] <- "Comilla"
livestock_data$Division_or_District[livestock_data$Division_or_District == "Brahmanbaria"] <- "Brahamanbaria"
livestock_data$Division_or_District[livestock_data$Division_or_District == "Chattogram"] <- "Chittagong"
livestock_data$Division_or_District[livestock_data$Division_or_District == "Cox's Bazar"] <- "Cox'S Bazar"
livestock_data$Division_or_District[livestock_data$Division_or_District == "Kishoregonj"] <- "Kishoreganj"
livestock_data$Division_or_District[livestock_data$Division_or_District == "Jashore"] <- "Jessore"
livestock_data$Division_or_District[livestock_data$Division_or_District == "Bogura"] <- "Bogra"
livestock_data$Division_or_District[livestock_data$Division_or_District == "Chapai Nawabganj"] <- "Nawabganj"

dataset4 <- merge.data.frame(dataset3, livestock_data, by.x = 1, by.y = 1, all.x = T)


# poverty data from the WorldBank
poverty_data <- read.csv("Data/bangladesh-zila-indicators.csv")
poverty_data$Zila.Name <- str_to_sentence(poverty_data$Zila.Name)

poverty_data$Zila.Name[poverty_data$Zila.Name == "Brahmanbaria"] <- "Brahamanbaria"
poverty_data$Zila.Name[poverty_data$Zila.Name == "Cox's bazar"] <- "Cox'S Bazar"
poverty_data$Zila.Name[poverty_data$Zila.Name == "Chapai nababganj"] <- "Nawabganj"

setdiff(poverty_data$Zila.Name, BGD_adm2$NAME_2)

dataset5 <- merge.data.frame(dataset4, poverty_data[c(3, 4, 5, 6, 8, 9, 16, 17, 109)], by.x = 1, by.y = 1, all.x = T)

# accessibility data from FAO-Data Hand-in-Hand initiative
access_data <- rast("Data/BGD_access_RC.tif")
plot(access_data)

acc_df <- exact_extract(access_data, BGD_adm2, "mean")
acc_df <- tibble(District = BGD_adm2$NAME_2, time_to_city = acc_df)

dataset6 <- merge.data.frame(dataset5, acc_df, by.x = 1, by.y = 1, all.x = T)

# dog density and vaccines from Ghosh et al., 2024 (doi.org/10.1016/j.lansea.2024.100452)
vaccination_data <- read_excel("Data/mmc2.xlsx")
vaccination_data$Districts <- str_replace(vaccination_data$Districts, " District", "")
vaccination_data$Districts <- str_to_sentence(vaccination_data$Districts)

# setdiff(vaccination_data$Districts, BGD_adm2$NAME_2)

# District name harmonisation
vaccination_data$Districts[vaccination_data$Districts == "Netrokona"] <- "Netrakona"
vaccination_data$Districts[vaccination_data$Districts == "Moulvibazar"] <- "Maulvibazar"
vaccination_data$Districts[vaccination_data$Districts == "Chapainawabganj"] <- "Nawabganj"
vaccination_data$Districts[vaccination_data$Districts == "Jhalokathi"] <- "Jhalokati"
vaccination_data$Districts[vaccination_data$Districts == "Brahmanbaria"] <- "Brahamanbaria"
vaccination_data$Districts[vaccination_data$Districts == "Cumilla"] <- "Comilla"
vaccination_data$Districts[vaccination_data$Districts == "Laksmipur"] <- "Lakshmipur"
vaccination_data$Districts[vaccination_data$Districts == "Coxs bazar"] <- "Cox'S Bazar"

vaccination_data %>% 
  group_by(Districts) %>% 
  mutate(percent_HtoD = mean(`% Humen to Dog`),
         estimated_dogs = mean(`Est: dogs`),
         vaccinated_dogs = sum(`Dogs  Vacc:`),
         mean_coverage = mean(`Coverage(%)`)) %>% 
  ungroup() %>% 
  dplyr::select(Districts, Round, percent_HtoD, estimated_dogs, vaccinated_dogs, mean_coverage) %>% 
  pivot_wider(
    names_from = Round,
    values_from = Round,
    names_prefix = "Round_"
  ) %>% 
  mutate(
    Round_1st  = if_else(!is.na(Round_1st), 1, 0),
    Round_2nd = if_else(!is.na(Round_2nd), 1, 0),
    Round_3rd  = if_else(!is.na(Round_3rd), 1, 0)
  ) -> vaccination_dataset

dataset7 <- merge.data.frame(dataset6, vaccination_dataset, by.x = 1, by.y = 1, all.x = T)

# wild reservoirs data from Worsley-Tonk et al., 2020 (doi.org/10.1371/journal.pntd.0008940)
rabies_classification_and_traits_carnivores_092020 <- read_csv("Data/rabies_classification_and_traits_carnivores_092020.csv") %>% filter(conservative == "Positive") %>% pull(Species)
rabies_classification_and_traits_bats_092020 <- read_csv("Data/rabies_classification_and_traits_bats_092020.csv") %>% filter(conservative == "Positive") %>% pull(Species)

# mammalian ranges from IUCN Red List (https://www.iucnredlist.org/resources/spatial-data-download)
mammals_ranges <- read_sf("MAMMALS.shp")

blank_wgs <- raster::raster(xmn = -180,
                            xmx = 180,
                            ymx = 90,
                            ymn = -90,
                            res = 0.089,
                            crs = "+proj=longlat +datum=WGS84 +no_defs")

require(fasterize)

mammals_ranges %>% 
  filter(binomial %in% c(rabies_classification_and_traits_bats_092020, rabies_classification_and_traits_carnivores_092020)) %>% 
  fasterize(.,
            blank_wgs,fun="sum") -> res_richness

reservoirs_df <- tibble(n_reservoirs = exact_extract(res_richness, BGD_adm2, "mean"))
reservoirs_df$District <- BGD_adm2$NAME_2

dataset8 <- merge.data.frame(dataset7, reservoirs_df, by.x = 1, by.y = 2, all.x = T)

dataset8 -> FinalDataset

# Check correlation -- need to reduce number of predictors
corrplot::corrplot(cor(FinalDataset %>% st_drop_geometry() %>% na.omit() %>% .[c(14:16, 18:22, 24, 26:28, 33:45, 48, 49)]), type = "lower", method = "square")

# Okay let's do some filtering and obtain the final dataset
FinalDataset %>% 
  select(c(1, 14, 16, 21, 22, 27, 28, 34, 37, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51)) %>% 
  mutate(dog_density = estimated_dogs/area_m2,
         pop_density = population/area_m2,
         vacc_dog_density = vaccinated_dogs/area_m2,
         Cow_density = Cow/area_m2) %>% 
  rename("Artificial" = Class_1,
         "Tree_cover" = Class_4,
         "Shrub_cover" = Class_5) %>% 
  select(-c(estimated_dogs, population, vaccinated_dogs, Cow)) -> FinDatasPred

units(FinalDataset$area_m2) <- make_units(km^2)

# Convert densities from 1/m² to 1/km²
units(FinalDataset$dog_density) <- make_units(1/km^2)
units(FinalDataset$pop_density) <- make_units(1/km^2)
units(FinalDataset$vacc_dog_density) <- make_units(1/km^2)

FinalDataset %<>% rename("area_km2" = area_m2)

corrplot::corrplot(cor(FinDatasPred %>% st_drop_geometry() %>% na.omit() %>% .[-c(1, 13, 16)]), type = "lower", method = "number")

# To drop: TreeCover (4); Primary employ agriculture (8); Population between 0 and 6 (9); Cow (22)   

FinDatasPred %<>% 
  select(c(1, 2, 5, 6, 7, 10, 12, 16, 17, 18, 19, 20, 21)) 
  
write_rds(FinalDataset, "Data/FinalDatasetRabiesBGD.rds")

