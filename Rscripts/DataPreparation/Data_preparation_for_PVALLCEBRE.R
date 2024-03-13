## Vallcebre Cal Sort data script
library(medfate)
library(medfateutils)
library(meteoland)
library(dplyr)
library(lubridate)
library(readxl)

data("SpParamsES")

# 0. LOAD DATA and METADATA -----------------------------------------------
env_data <- read.csv('SourceData/Tables/VallcebreSort/ESP_VAL_SOR_env_data.csv')
sapf_data <- read.csv('SourceData/Tables/VallcebreSort/ESP_VAL_SOR_sapf_data.csv')
sapf_flags <- read.csv('SourceData/Tables/VallcebreSort/ESP_VAL_SOR_sapf_flags.csv')
env_md <- read.csv('SourceData/Tables/VallcebreSort/ESP_VAL_SOR_env_md.csv')
site_md <- read.csv('SourceData/Tables/VallcebreSort/ESP_VAL_SOR_site_md.csv')
species_md <- read.csv('SourceData/Tables/VallcebreSort/ESP_VAL_SOR_species_md.csv')
stand_md <- read.csv('SourceData/Tables/VallcebreSort/ESP_VAL_SOR_stand_md.csv')
plant_md <- read.csv('SourceData/Tables/VallcebreSort/ESP_VAL_SOR_plant_md.csv')
meteo_vall <- read.table("SourceData/Tables/VallcebreSort/Vallcebre_Meteo.txt",
                         sep = "\t", dec = ",", header = TRUE)
  

# 1. SITE INFORMATION -----------------------------------------------------
siteData <- data.frame(
  Attribute = c('Plot name',
                'Country',
                'SAPFLUXNET code',
                'SAPFLUXNET contributor (affiliation)',
                'FLUXNET/ICOS code',
                'FLUXNET/ICOS contributor (affiliation)',
                'Latitude (º)',
                'Longitude (º)',
                'Elevation (m)',
                'Slope (º)',
                'Aspect (º)',
                'Parent material',
                'Soil texture',
                'MAT (ºC)',
                'MAP (mm)',
                'Stand description',
                'Stand LAI',
                'Species simulated',
                'Evaluation period',
                'Description DOI'),
  Value = c("Vallcebre (Cal Sort)",
            "Spain",
            site_md$si_code,
            "Rafael Poyatos (CREAF)",
            "",
            "",
            site_md$si_lat,
            site_md$si_long,
            site_md$si_elev,
            10,
            0,
            "Limestone",
            "Sandy clay loam",
            round(site_md$si_mat,1),
            round(site_md$si_map),
            "Pinus sylvestris forest in a terraced area",
            2.4,
            "Pinus sylvestris",
            "2003-2005",
            "10.5194/hess-9-493-2005")
)

# 2. TERRAIN DATA ---------------------------------------------------------
terrainData <- data.frame(
  latitude = site_md$si_lat,
  elevation = site_md$si_elev,
  aspect = 0, # North
  slope = 10 # Flat but in a N-facing slope
)

# 3. TREE DATA ----------------------------------------------------------
treeData <- data.frame(
  Species = "Pinus sylvestris",
  DBH =  16.2, #mean(plant_md[['pl_dbh']], na.rm = TRUE),
  Height = mean(plant_md[['pl_height']], na.rm = TRUE)*100,
  N =  stand_md$st_density, 
  Z50 = NA,  
  Z95 = NA,
  LAI = 2.4
)

# 4. SHRUB DATA -----------------------------------------------------------
shrubData <- data.frame(
  Species = "Buxus sempervirens",
  Cover = 5,
  Height = 100,
  Z50 = NA,
  Z95 = NA,
  LAI = NA
)
f = emptyforest()
f$treeData = treeData
f$shrubData = shrubData
summary(f, SpParamsES)


# 5. SEED DATA ------------------------------------------------------------
# there is no seed info

# 6. MISC DATA ------------------------------------------------------------
miscData <- data.frame(
  ID = 'PVALLCEBRE',
  SpParamsName = "SpParamsES",
  herbCover = 5, herbHeight = 20,
  Validation = 'global', Definitive = 'Yes'
)

# 7. SOIL DATA ------------------------------------------------------------
soilData <- data.frame(
  widths = c(300, 350, 350),
  clay = c(22, 18, 18),
  sand = c(59, 62, 62), 
  om = c(4, 1, 0),
  bd = c(1.18, 1.48, 1.5),
  rfc = c(19, 19, 50)
)
sum(soil_waterExtractable(soil(soilData), model="VG", minPsi = -4))

# 8. METEO DATA -----------------------------------------------------------
meteoData <- env_data %>%
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) %>%
  dplyr::group_by(dates) %>%
  dplyr::summarise(MinTemperature = min(ta, na.rm = TRUE),
                   MaxTemperature = max(ta, na.rm = TRUE),
                   MinRelativeHumidity = min(rh, na.rm = TRUE),
                   MaxRelativeHumidity = max(rh, na.rm = TRUE),
                   WindSpeed = mean(ws, na.rm = TRUE),
                   Radiation = (sum((sw_in * 900), na.rm = TRUE)/(24*3600)), # W/m2, a W/m2 en el día
                   Precipitation = sum(precip, na.rm = TRUE)) |>
  dplyr::mutate(Radiation = Radiation*3600*24/1000000) |> # w/m2 to MJ/m2/day
  dplyr::mutate(MinTemperature = dplyr::na_if(MinTemperature, Inf),
                MaxTemperature = dplyr::na_if(MaxTemperature, -Inf),
                MinRelativeHumidity = dplyr::na_if(MinRelativeHumidity, Inf),
                MaxRelativeHumidity = dplyr::na_if(MaxRelativeHumidity, -Inf),
                Radiation = dplyr::na_if(Radiation, 0))

# Intentem reemplaçar missings per valors interpolats
# coords_sf <- sf::st_sfc(sf::st_point(c(site_md$si_long,site_md$si_lat)), crs = 4326)
# plot_sf <- sf::st_sf(terrainData, coords_sf)
# climate_base = "emf/datasets/Climate/"
# for(year in 2003:2005) {
#   interpolator_file <- EMFdatautils::download_emfdata(climate_base,
#                                                       paste0("Products/InterpolationData/Catalunya/Historic/calibrated_2.0/interpolator_", year, "_calibrated.nc"))
#   interpolator <- meteoland::read_interpolator(interpolator_file)
#   interpolated_data <- meteoland::interpolate_data(plot_sf, interpolator)
#   meteoDataInt <- interpolated_data$interpolated_data[[1]]
#   write.csv(meteoDataInt, file = paste0("SourceData/Tables/VallcebreSort/meteoDataInterpolated_", year,".csv"), row.names = FALSE)
# }
meteoDataInt2003 <- read.csv(file = 'SourceData/Tables/VallcebreSort/meteoDataInterpolated_2003.csv')
meteoDataInt2004 <- read.csv(file = 'SourceData/Tables/VallcebreSort/meteoDataInterpolated_2004.csv')
meteoDataInt2005 <- read.csv(file = 'SourceData/Tables/VallcebreSort/meteoDataInterpolated_2005.csv')
meteoDataInt <- rbind(meteoDataInt2003, meteoDataInt2004, meteoDataInt2005)
meteoDataInt$dates <- as.Date(meteoDataInt$dates)
dates_mis <- meteoData$dates[is.na(meteoData$MinTemperature)]
meteoData[meteoData$dates %in% dates_mis, "MinTemperature"] <- meteoDataInt[meteoDataInt$dates %in% dates_mis, "MinTemperature"]
meteoData[meteoData$dates %in% dates_mis, "MaxTemperature"] <- meteoDataInt[meteoDataInt$dates %in% dates_mis, "MaxTemperature"]
dates_mis <- meteoData$dates[is.na(meteoData$MinRelativeHumidity)]
meteoData[meteoData$dates %in% dates_mis, "MinRelativeHumidity"] <- meteoDataInt[meteoDataInt$dates %in% dates_mis, "MinRelativeHumidity"]
meteoData[meteoData$dates %in% dates_mis, "MaxRelativeHumidity"] <- meteoDataInt[meteoDataInt$dates %in% dates_mis, "MaxRelativeHumidity"]
dates_mis <- meteoData$dates[is.na(meteoData$WindSpeed)]
meteoData[meteoData$dates %in% dates_mis, "WindSpeed"] <- meteoDataInt[meteoDataInt$dates %in% dates_mis, "WindSpeed"]
dates_mis <- meteoData$dates[is.na(meteoData$Radiation)]
meteoData[meteoData$dates %in% dates_mis, "Radiation"] <- meteoDataInt[meteoDataInt$dates %in% dates_mis, "Radiation"]
# Start with 2003
meteoDataInt2003 <- read.csv(file = 'SourceData/Tables/VallcebreSort/meteoDataInterpolated_2003.csv')
meteoDataInt2003$dates <- as.Date(meteoDataInt2003$dates)
meteoDataInt2003$WindSpeed <- pmin(meteoDataInt2003$WindSpeed, 5)
meteoData <- dplyr::full_join(meteoDataInt2003[!(meteoDataInt2003$dates %in% meteoData$dates),names(meteoData)], meteoData) |>
  dplyr::arrange(dates)

# 9. CUSTOM PARAMS --------------------------------------------------------
PS_index = SpParamsES$SpIndex[SpParamsES$Name=="Pinus sylvestris"]
BS_index = SpParamsES$SpIndex[SpParamsES$Name=="Buxus sempervirens"]
PS_cohname = paste0("T1_", PS_index)
BS_cohname = paste0("S1_", BS_index)
ps <- 1
bs <- 2
customParams <- data.frame(
  Species = c("Pinus sylvestris", "Buxus sempervirens"),
  SLA = NA,
  VCleaf_P12 = NA,
  VCleaf_P50 = NA,
  VCleaf_P88 = NA,
  VCleaf_slope = NA,
  VCstem_P12 = NA,
  VCstem_P50 = NA,
  VCstem_P88 = NA,
  VCstem_slope = NA,
  VCroot_P12 = NA,
  VCroot_P50 = NA,
  VCroot_P88 = NA,
  VCroot_slope = NA,
  VCleaf_kmax = NA,
  Kmax_stemxylem = NA,
  LeafEPS = NA,
  LeafPI0 = NA,
  LeafAF  = NA,
  StemEPS = NA,
  StemPI0 = NA,
  StemAF = NA,
  Gswmin = NA,
  Gswmax = NA,
  Gs_P50 = NA,
  Gs_slope = NA,
  Al2As = NA) 

As2Al <- plant_md[['pl_sapw_area']]/plant_md[['pl_leaf_area']] # cm2/m2
customParams$Al2As[ps] <- mean(10000/As2Al)

# 10. MEASURED DATA --------------------------------------------------------
# sapflow data, está en cm3/cm2/h, y el timestep es 15 minutos, así que tenemos que
# multiplicar por 0.25 para los cm3/cm2 en el timestep, por el As2Al de cada árbol y 
# dividir entre 1000 para tenerlo en L.
# Agrego los datos de sapflow por día, sumo todos los árboles y luego multiplico
# por LAI y divido por n = numero de arboles medidos
transp_data_temp <- sapf_data %>%
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('ESP_VAL_SOR')),
                   dplyr::funs(.*0.25)) %>%
  dplyr::mutate(ESP_VAL_SOR_Psy_Jt_1 = (ESP_VAL_SOR_Psy_Jt_1*As2Al[1]),
                ESP_VAL_SOR_Psy_Jt_10 = (ESP_VAL_SOR_Psy_Jt_10*As2Al[2]),
                ESP_VAL_SOR_Psy_Jt_11 = (ESP_VAL_SOR_Psy_Jt_11*As2Al[3]),
                ESP_VAL_SOR_Psy_Jt_12 = (ESP_VAL_SOR_Psy_Jt_12*As2Al[4]),
                ESP_VAL_SOR_Psy_Jt_13 = (ESP_VAL_SOR_Psy_Jt_13*As2Al[5]),
                ESP_VAL_SOR_Psy_Jt_2 = (ESP_VAL_SOR_Psy_Jt_2*As2Al[6]),
                ESP_VAL_SOR_Psy_Jt_3 = (ESP_VAL_SOR_Psy_Jt_3*As2Al[7]),
                ESP_VAL_SOR_Psy_Jt_4 = (ESP_VAL_SOR_Psy_Jt_4*As2Al[8]),
                ESP_VAL_SOR_Psy_Jt_5 = (ESP_VAL_SOR_Psy_Jt_5*As2Al[9]),
                ESP_VAL_SOR_Psy_Jt_6 = (ESP_VAL_SOR_Psy_Jt_6*As2Al[10]),
                ESP_VAL_SOR_Psy_Jt_7 = (ESP_VAL_SOR_Psy_Jt_7*As2Al[11]),
                ESP_VAL_SOR_Psy_Jt_8 = (ESP_VAL_SOR_Psy_Jt_8*As2Al[12]),
                ESP_VAL_SOR_Psy_Jt_9 = (ESP_VAL_SOR_Psy_Jt_9*As2Al[13])) %>%
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) %>%
  # sum days
  dplyr::group_by(dates) %>%
  dplyr::summarise_at(dplyr::vars(dplyr::starts_with('ESP_VAL_SOR')),
                      dplyr::funs(sum(., na.rm = TRUE))) %>%
  # remove the zeroes generated previously
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('ESP_VAL_SOR')),
                   dplyr::funs(replace(., . == 0, NA)))

transp_data_temp$E_PS <- rowSums(transp_data_temp[,-1], na.rm = TRUE) / rowSums(!is.na(transp_data_temp[,-1]), na.rm = TRUE)

measuredData <- data.frame(dates = transp_data_temp$dates,
                           E_PS = transp_data_temp$E_PS)
names(measuredData)[2] <- paste0("E_", PS_cohname)

smcData <- env_data |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  dplyr::group_by(dates) |>
  dplyr::summarise(SWC = mean(swc_shallow, na.rm = TRUE))

measuredData <- measuredData |>
  dplyr::left_join(smcData, by="dates")

# 11. EVALUATION PERIOD ---------------------------------------------------
# d = as.Date(meteoData$dates)
# meteoData <- meteoData[(d>="2004-01-01") & (d<"2005-12-31"),] #Select two years


# 12. REMARKS -------------------------------------------------------------
remarks <- data.frame(
  Title = c('Soil',
            'Vegetation',
            'Weather',
            'Sapflow'),
  Remark = c('Soil depth is 65 cm (30 + 35) but an additional layer of 35 cm is considered with 50% rocks',
             'Understory modelled using B. sempervirens',
             'Missing values have been complemented with interpolated data',
             'Sapflux density has been scaled to cohort level using measured plant Huber values')
)

# 13. SAVE DATA IN FOLDER -------------------------------------------------
folder_name <- file.path('Sites_data', 'PVALLCEBRE')
write.table(siteData, file = file.path(folder_name, 'PVALLCEBRE_siteData.txt'),
            row.names = FALSE, sep = '\t')
write.table(treeData, file = file.path(folder_name, 'PVALLCEBRE_treeData.txt'),
            row.names = FALSE, sep = '\t')
write.table(shrubData, file = file.path(folder_name, 'PVALLCEBRE_shrubData.txt'),
             row.names = FALSE, sep = '\t')
write.table(miscData, file = file.path(folder_name, 'PVALLCEBRE_miscData.txt'),
            row.names = FALSE, sep = '\t')
write.table(meteoData, file = file.path(folder_name, 'PVALLCEBRE_meteoData.txt'),
            row.names = FALSE, sep = '\t')
write.table(soilData, file = file.path(folder_name, 'PVALLCEBRE_soilData.txt'),
            row.names = FALSE, sep = '\t')
write.table(terrainData, file = file.path(folder_name, 'PVALLCEBRE_terrainData.txt'),
            row.names = FALSE, sep = '\t')
write.table(customParams, file = file.path(folder_name, 'PVALLCEBRE_customParams.txt'),
            row.names = FALSE, sep = '\t')
write.table(measuredData, file = file.path(folder_name, 'PVALLCEBRE_measuredData.txt'),
            row.names = FALSE, sep = '\t')
write.table(remarks, file = file.path(folder_name, 'PVALLCEBRE_remarks.txt'),
            row.names = FALSE, sep = '\t')


