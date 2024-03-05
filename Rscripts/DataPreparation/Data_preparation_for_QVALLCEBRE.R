## Vallcebre Cal Barrol data script
library(medfate)
library(meteoland)
library(dplyr)
library(lubridate)
library(readxl)

data("SpParamsMED")

# 0. LOAD DATA and METADATA -----------------------------------------------
env_data <- read.csv('SourceData/Tables/VallcebreBarrol/ESP_VAL_BAR_env_data.csv')
sapf_data <- read.csv('SourceData/Tables/VallcebreBarrol/ESP_VAL_BAR_sapf_data.csv')
sapf_flags <- read.csv('SourceData/Tables/VallcebreBarrol/ESP_VAL_BAR_sapf_flags.csv')
env_md <- read.csv('SourceData/Tables/VallcebreBarrol/ESP_VAL_BAR_env_md.csv')
site_md <- read.csv('SourceData/Tables/VallcebreBarrol/ESP_VAL_BAR_site_md.csv')
species_md <- read.csv('SourceData/Tables/VallcebreBarrol/ESP_VAL_BAR_species_md.csv')
stand_md <- read.csv('SourceData/Tables/VallcebreBarrol/ESP_VAL_BAR_stand_md.csv')
plant_md <- read.csv('SourceData/Tables/VallcebreBarrol/ESP_VAL_BAR_plant_md.csv')
meteo_vall <- read.table("SourceData/Tables/VallcebreBarrol/Vallcebre_Meteo.txt",
                         sep = "\t", dec = ",", header = TRUE)
  

# 1. SITE INFORMATION -----------------------------------------------------
siteData <- data.frame(
  Attribute = c('Plot name',
                'Country',
                'SAPFLUXNET code',
                'Contributor (affiliation)',
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
                'Description DOI'),
  Value = c("Vallcebre (Cal Barrol)",
            "Spain",
            site_md$si_code,
            "Rafael Poyatos (CREAF)",
            site_md$si_lat,
            site_md$si_long,
            site_md$si_elev,
            0,
            0,
            "Limestone",
            "Silty clay loam",
            site_md$si_mat,
            site_md$si_map,
            "Semi-deciduous sub-Mediterranean oak forest",
            2.1,
            "Quercus pubescens",
            "10.1093/treephys/27.4.537")
)

# 2. TERRAIN DATA ---------------------------------------------------------
terrainData <- data.frame(
  latitude = site_md$si_lat,
  elevation = site_md$si_elev,
  aspect = 0, # Flat
  slope = 0 # Flat
)

# 3. TREE DATA ----------------------------------------------------------
treeData <- data.frame(
  Species = "Quercus pubescens",
  DBH = mean(plant_md[['pl_dbh']], na.rm = TRUE),
  Height = mean(plant_md[['pl_height']], na.rm = TRUE)*100,
  N =  828, # stand_md$st_density, 
  Z50 = NA,  
  Z95 = NA,
  LAI = 2.1
)

# 4. SHRUB DATA -----------------------------------------------------------
shrubData <- data.frame(
  Species = "Buxus sempervirens",
  Cover = 20,
  Height = 100,
  Z50 = NA,
  Z95 = NA,
  LAI = NA
)
f = emptyforest()
f$treeData = treeData
f$shrubData = shrubData
summary(f, SpParamsMED)

# 5. SEED DATA ------------------------------------------------------------
# there is no seed info

# 6. MISC DATA ------------------------------------------------------------
miscData <- data.frame(
  ID = 'QVALLCEBRE',
  SpParamsName = "SpParamsMED",
  herbCover = 5, herbHeight = 20,
  Validation = 'global', Definitive = 'Yes'
)

# 7. SOIL DATA ------------------------------------------------------------
# extracted from the paper (Limousin 2009) y adaptado como Prades:
# "bulk density" de PRADES:
soilData <- data.frame(
  widths = c(300, 200, 1000, 2000),
  clay = c(31, 28.6, 28.6, 28.6),
  sand = c(9.7, 8.8, 8.8, 8.8), #14
  om = c(NA, NA, NA, NA),
  bd = c(1.23, 1.50, 1.50, 1.50),
  rfc = c(15, 20, 70, 90)
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
                   WindSpeed = mean(ws, na.rm = TRUE)) |>
  dplyr::mutate(MinTemperature = dplyr::na_if(MinTemperature, Inf),
                MaxTemperature = dplyr::na_if(MaxTemperature, -Inf),
                MinRelativeHumidity = dplyr::na_if(MinRelativeHumidity, Inf),
                MaxRelativeHumidity = dplyr::na_if(MaxRelativeHumidity, -Inf))

meteo_vall$dates <- as.Date(paste0(meteo_vall$any,"-", meteo_vall$mes, "-", meteo_vall$dia))
meteoData <- meteoData |>
  dplyr::left_join(meteo_vall[,c("dates", "Rg", "Rainfall")], by="dates") |>
  dplyr::rename(Radiation = "Rg",
                Precipitation = "Rainfall") 

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
#   write.csv(meteoDataInt, file = paste0("SourceData/Tables/VallcebreBarrol/meteoDataInterpolated_", year,".csv"), row.names = FALSE)
# }
meteoDataInt2004 <- read.csv(file = 'SourceData/Tables/VallcebreBarrol/meteoDataInterpolated_2004.csv')
meteoDataInt2005 <- read.csv(file = 'SourceData/Tables/VallcebreBarrol/meteoDataInterpolated_2005.csv')
meteoDataInt <- rbind(meteoDataInt2004, meteoDataInt2005)
meteoDataInt$dates <- as.Date(meteoDataInt$dates)
dates_mis <- meteoData$dates[is.na(meteoData$MinTemperature)]
meteoData[meteoData$dates %in% dates_mis, "MinTemperature"] <- meteoDataInt[meteoDataInt$dates %in% dates_mis, "MinTemperature"]
meteoData[meteoData$dates %in% dates_mis, "MaxTemperature"] <- meteoDataInt[meteoDataInt$dates %in% dates_mis, "MaxTemperature"]
meteoData[meteoData$dates %in% dates_mis, "MinRelativeHumidity"] <- meteoDataInt[meteoDataInt$dates %in% dates_mis, "MinRelativeHumidity"]
meteoData[meteoData$dates %in% dates_mis, "MaxRelativeHumidity"] <- meteoDataInt[meteoDataInt$dates %in% dates_mis, "MaxRelativeHumidity"]
dates_mis <- meteoData$dates[is.na(meteoData$WindSpeed)]
meteoData[meteoData$dates %in% dates_mis, "WindSpeed"] <- meteoDataInt[meteoDataInt$dates %in% dates_mis, "WindSpeed"]
# Start with 2003
meteoDataInt2003 <- read.csv(file = 'SourceData/Tables/VallcebreBarrol/meteoDataInterpolated_2003.csv')
meteoDataInt2003$dates <- as.Date(meteoDataInt2003$dates)
meteoData <- dplyr::full_join(meteoDataInt2003[!(meteoDataInt2003$dates %in% meteoData$dates),names(meteoData)], meteoData) |>
  dplyr::arrange(dates)

# 9. CUSTOM PARAMS --------------------------------------------------------
QP_index = SpParamsMED$SpIndex[SpParamsMED$Name=="Quercus pubescens"]
BS_index = SpParamsMED$SpIndex[SpParamsMED$Name=="Buxus sempervirens"]
QP_cohname = paste0("T1_", QP_index)
BS_cohname = paste0("S1_", BS_index)
qp <- 1
bs <- 2
customParams <- data.frame(
  Species = c("Quercus pubescens", "Buxus sempervirens"),
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
customParams$Al2As[qp] <- mean(10000/As2Al)

# 10. MEASURED DATA --------------------------------------------------------
# sapflow data, está en cm3/cm2 s, y el timestep es 15 minutos, así que tenemos que
# multiplicar por 15*60 segundos para los cm3/cm2 en el timestep, por el As2Al de cada árbol y 
# dividir entre 1000 para tenerlo en L.
# Agrego los datos de sapflow por día, sumo todos los árboles y luego multiplico
# por LAI y divido por n = numero de arboles medidos
transp_data_temp <- sapf_data %>%
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('ESP_VAL_BAR')),
                   dplyr::funs(.*60*15/1000)) %>%
  dplyr::mutate(ESP_VAL_BAR_Qpu_Jt_1 = (ESP_VAL_BAR_Qpu_Jt_1*As2Al[1]),
                ESP_VAL_BAR_Qpu_Jt_10 = (ESP_VAL_BAR_Qpu_Jt_10*As2Al[2]),
                ESP_VAL_BAR_Qpu_Jt_11 = (ESP_VAL_BAR_Qpu_Jt_11*As2Al[3]),
                ESP_VAL_BAR_Qpu_Jt_2 = (ESP_VAL_BAR_Qpu_Jt_2*As2Al[4]),
                ESP_VAL_BAR_Qpu_Jt_3 = (ESP_VAL_BAR_Qpu_Jt_3*As2Al[5]),
                ESP_VAL_BAR_Qpu_Jt_4 = (ESP_VAL_BAR_Qpu_Jt_4*As2Al[6]),
                ESP_VAL_BAR_Qpu_Jt_5 = (ESP_VAL_BAR_Qpu_Jt_5*As2Al[7]),
                ESP_VAL_BAR_Qpu_Jt_6 = (ESP_VAL_BAR_Qpu_Jt_6*As2Al[8]),
                ESP_VAL_BAR_Qpu_Jt_7 = (ESP_VAL_BAR_Qpu_Jt_7*As2Al[9]),
                ESP_VAL_BAR_Qpu_Jt_8 = (ESP_VAL_BAR_Qpu_Jt_8*As2Al[10]),
                ESP_VAL_BAR_Qpu_Jt_9 = (ESP_VAL_BAR_Qpu_Jt_9*As2Al[11])) %>%
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) %>%
  # sum days
  dplyr::group_by(dates) %>%
  dplyr::summarise_at(dplyr::vars(dplyr::starts_with('ESP_VAL_BAR')),
                      dplyr::funs(sum(., na.rm = TRUE))) %>%
  # remove the zeroes generated previously
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('ESP_VAL_BAR')),
                   dplyr::funs(replace(., . == 0, NA)))

transp_data_temp$E_QP <- rowSums(transp_data_temp[,-1], na.rm = TRUE) / rowSums(!is.na(transp_data_temp[,-1]), na.rm = TRUE)

measuredData <- data.frame(dates = transp_data_temp$dates,
                           E_QP = transp_data_temp$E_QP)
names(measuredData)[2] <- paste0("E_", QP_cohname)

smcData <- env_data |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  dplyr::group_by(dates) |>
  dplyr::summarise(SWC = mean(swc_shallow, na.rm = TRUE))

measuredData <- measuredData |>
  dplyr::left_join(smcData, by="dates")

# 11. EVALUATION PERIOD ---------------------------------------------------
d = as.Date(meteoData$dates)
meteoData <- meteoData[(d>="2004-01-01") & (d<"2005-12-31"),] #Select two years


# 12. REMARKS -------------------------------------------------------------
remarks <- data.frame(
  Title = c('Soil',
            'Vegetation',
            'Weather',
            'Sapflow'),
  Remark = c('Soil depth is 50 cm but additional rocky layers were added',
             'Understory modelled using B. sempervirens',
             'Missing values have been complemented with interpolated data',
             'Sapflux density has been scaled to cohort level using measured plant Huber values')
)

# 13. SAVE DATA IN FOLDER -------------------------------------------------
folder_name <- file.path('Sites_data', 'QVALLCEBRE')
write.table(siteData, file = file.path(folder_name, 'QVALLCEBRE_siteData.txt'),
            row.names = FALSE, sep = '\t')
write.table(treeData, file = file.path(folder_name, 'QVALLCEBRE_treeData.txt'),
            row.names = FALSE, sep = '\t')
write.table(shrubData, file = file.path(folder_name, 'QVALLCEBRE_shrubData.txt'),
             row.names = FALSE, sep = '\t')
write.table(miscData, file = file.path(folder_name, 'QVALLCEBRE_miscData.txt'),
            row.names = FALSE, sep = '\t')
write.table(meteoData, file = file.path(folder_name, 'QVALLCEBRE_meteoData.txt'),
            row.names = FALSE, sep = '\t')
write.table(soilData, file = file.path(folder_name, 'QVALLCEBRE_soilData.txt'),
            row.names = FALSE, sep = '\t')
write.table(terrainData, file = file.path(folder_name, 'QVALLCEBRE_terrainData.txt'),
            row.names = FALSE, sep = '\t')
write.table(customParams, file = file.path(folder_name, 'QVALLCEBRE_customParams.txt'),
            row.names = FALSE, sep = '\t')
write.table(measuredData, file = file.path(folder_name, 'QVALLCEBRE_measuredData.txt'),
            row.names = FALSE, sep = '\t')

write.table(remarks, file = file.path(folder_name, 'QVALLCEBRE_remarks.txt'),
            row.names = FALSE, sep = '\t')


