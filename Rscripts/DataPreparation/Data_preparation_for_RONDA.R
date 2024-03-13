## RONDA data script
library(medfate)
library(medfateutils)
library(meteoland)
library(dplyr)
library(lubridate)
library(readxl)

data("SpParamsES")

# 0. LOAD DATA and METADATA -----------------------------------------------
env_data <- read.csv('SourceData/Tables/Ronda/ESP_RON_PIL_env_data.csv')
sapf_data <- read.csv('SourceData/Tables/Ronda/ESP_RON_PIL_sapf_data.csv')
env_md <- read.csv('SourceData/Tables/Ronda/ESP_RON_PIL_env_md.csv')
site_md <- read.csv('SourceData/Tables/Ronda/ESP_RON_PIL_site_md.csv')
stand_md <- read.csv('SourceData/Tables/Ronda/ESP_RON_PIL_stand_md.csv')
plant_md <- read.csv('SourceData/Tables/Ronda/ESP_RON_PIL_plant_md.csv')
species_md <- read.csv('SourceData/Tables/Ronda/ESP_RON_PIL_species_md.csv')

AP_index = SpParamsES$SpIndex[SpParamsES$Name=="Abies pinsapo"]
TB_index = SpParamsES$SpIndex[SpParamsES$Name=="Taxus baccata"]

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
                'Period simulated',
                'Description DOI'),
  Value = c("Ronda (Pilones)",
            "Spain",
            "ESP_RON_PIL",
            "Víctor Lechuga (UJaén)",
            "",
            "",
            site_md$si_lat,
            site_md$si_long,
            site_md$si_elev,
            15,
            315, #NW 
            "",
            "Silty loam",
            round(site_md$si_mat,1),
            round(site_md$si_map),
            "Mixed gimnosperm forest dominated by Abies pinsapo",
            NA,
            "Abies pinsapo, Taxus baccata",
            "2011-2013",
            "10.3390/f10121132")
)


# 2. TERRAIN DATA ---------------------------------------------------------
terrainData <- data.frame(
  latitude = 36.692881,
  elevation = 1734,
  aspect = 315, # North-West
  slope = 15 # 
)

# 3. TREE DATA ----------------------------------------------------------
plant_md$sp <- c("ap", "ap", "tb", "tb", "ap", "ap", "ap", "ap", "ap", "ap", "ap", "ap")
#Basal area reported to be 7.24 m2/ha (SAPFLUXNET) or 9.15 m2/ha (Paper)
# 97% basal area corresponds to Abies pinsapo
# 3% basal area corresponds to Taxus baccata
treeData <- data.frame(
  Species = c("Abies pinsapo", "Taxus baccata"),
  DBH = c(15.3, 15), # from paper
  Height = 100*c(mean(plant_md$pl_height[plant_md$sp=="ap"],na.rm=TRUE),
                 mean(plant_md$pl_height[plant_md$sp=="tb"],na.rm=TRUE)), # from plant data
  N = c(486,15),# from paper
  Z50 = NA,
  Z95 = NA
)
f <-emptyforest()
f$treeData <- treeData
summary(f, SpParamsES)

# 4. SHRUB DATA -----------------------------------------------------------
# The understorey is scarce, mainly constituted by scattered shrubs (Genista falcate and Pteridium aquilinum).
shrubData <- data.frame(
  Species = numeric(0), 
  Cover = numeric(0),
  Height = numeric(0),
  Z50 = numeric(0),
  Z95 = numeric(0)
)

# 5. SEED DATA ------------------------------------------------------------
# there is no seed info


# 6. MISC DATA ------------------------------------------------------------
miscData <- data.frame(
  ID = 'RONDA',
  SpParamsName = "SpParamsES",
  herbCover = 10, herbHeight = 20,
  Validation = 'global', Definitive = 'No'
)

# 7. SOIL DATA ------------------------------------------------------------
# coords_sf <- sf::st_sfc(sf::st_point(c(site_md$si_long,site_md$si_lat)), crs = 4326)
# soilData <- medfateutils::soilgridsParams(coords_sf,  c(300, 700, 1000, 2500))
soilData <- data.frame(
  widths = c(300, 700, 1000, 2500),
  clay = c(18.96667, 19.65000, 20.10000, 20.10000),
  sand = c(44.46667, 45.20000, 45.70000,45.70000),
  om = c(3.97, 1.18, 0.65, 0),
  bd = c(1.276667, 1.420000,1.480000,1.480000),
  rfc = c(19.46667,40,80,90),
  VG_theta_sat = rep(0.55, 4),
  VG_theta_res = rep(0.1, 4)
)
s = soil(soilData, VG_PTF = "Toth")
sum(soil_waterExtractable(s, model="VG", minPsi = -4))

# 8. METEO DATA -----------------------------------------------------------
# coords_sf <- sf::st_sfc(sf::st_point(c(site_md$si_long,site_md$si_lat)), crs = 4326)
# plot_sf <- sf::st_sf(terrainData, coords_sf)
# climate_base = "emf/datasets/Climate/"
# for(year in 2011:2013) {
#    interpolator_file <- EMFdatautils::download_emfdata(climate_base,
#                                                        paste0("Products/InterpolationData/Spain/Historic/calibrated/AEMET_29_daily_interpolator_", year,".nc"))
#   interpolator <- meteoland::read_interpolator(interpolator_file)
#   interpolated_data <- meteoland::interpolate_data(plot_sf, interpolator)
#   write.csv(interpolated_data$interpolated_data[[1]], file = paste0("SourceData/Tables/Ronda/meteoDataInterpolated_", year, ".csv"), row.names = FALSE)
# }
meteoDataInt2011 <- read.csv(file = 'SourceData/Tables/Ronda/meteoDataInterpolated_2011.csv')
meteoDataInt2012 <- read.csv(file = 'SourceData/Tables/Ronda/meteoDataInterpolated_2012.csv')
meteoDataInt2013 <- read.csv(file = 'SourceData/Tables/Ronda/meteoDataInterpolated_2013.csv')
meteoDataInt <- rbind(meteoDataInt2011, meteoDataInt2012, meteoDataInt2013)
meteoDataInt$dates <- as.Date(meteoDataInt$dates)
rm(meteoDataInt2011, meteoDataInt2012, meteoDataInt2013)
meteoDataSite <- env_data |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  dplyr::group_by(dates) |>
  dplyr::summarise(MinTemperature = min(ta, na.rm = TRUE),
                   MaxTemperature = max(ta, na.rm = TRUE),
                   MinRelativeHumidity = min(rh, na.rm = TRUE),
                   MaxRelativeHumidity = max(rh, na.rm = TRUE)) |>
  dplyr::mutate_at(dplyr::vars(2:5),
                   dplyr::funs(replace(., is.infinite(.), NA))) |>
  dplyr::mutate_at(dplyr::vars(2:5),
                   dplyr::funs(replace(., is.nan(.), NA))) 
# # CHeck
# ggplot()+
#   geom_line(aes(x = dates, y=MaxTemperature), col="black", data = meteoDataInt)+
#   geom_line(aes(x = dates, y=MaxTemperature), col="red", data = meteoDataSite)
# ggplot()+
#   geom_line(aes(x = dates, y=MinTemperature), col="black", data = meteoDataInt)+
#   geom_line(aes(x = dates, y=MinTemperature), col="red",  data = meteoDataSite)
# ggplot()+
#   geom_line(aes(x = dates, y=MinRelativeHumidity), col="black", data = meteoDataInt)+
#   geom_line(aes(x = dates, y=MinRelativeHumidity), col="red", data = meteoDataSite)
# ggplot()+
#   geom_line(aes(x = dates, y=MaxRelativeHumidity), col="black", data = meteoDataInt)+
#   geom_line(aes(x = dates, y=MaxRelativeHumidity), col="red", data = meteoDataSite)
# Replace observed data
meteoData <- meteoDataInt
meteoData[meteoData$dates %in% meteoDataSite$dates[!is.na(meteoDataSite$MinTemperature)], "MinTemperature"] <- meteoDataSite$MinTemperature[!is.na(meteoDataSite$MinTemperature)]
meteoData[meteoData$dates %in% meteoDataSite$dates[!is.na(meteoDataSite$MaxTemperature)], "MaxTemperature"] <- meteoDataSite$MaxTemperature[!is.na(meteoDataSite$MaxTemperature)]
meteoData[meteoData$dates %in% meteoDataSite$dates[!is.na(meteoDataSite$MinRelativeHumidity)], "MinRelativeHumidity"] <- meteoDataSite$MinRelativeHumidity[!is.na(meteoDataSite$MinRelativeHumidity)]
meteoData[meteoData$dates %in% meteoDataSite$dates[!is.na(meteoDataSite$MaxRelativeHumidity)], "MaxRelativeHumidity"] <- meteoDataSite$MaxRelativeHumidity[!is.na(meteoDataSite$MaxRelativeHumidity)]



# 9. CUSTOM PARAMS --------------------------------------------------------
AP_cohname = paste0("T1_", AP_index)
TB_cohname = paste0("T2_", TB_index)
ap<- 1
tb<-2

customParams <- data.frame(
  Species = c("Abies pinsapo", "Taxus baccata"),
  Vmax298 = NA,
  Jmax298 = NA,
  Kmax_stemxylem = NA,
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


customParams$Al2As[ap] <- 0.14/(pi*(8.3*0.001/2)^2)
customParams$Al2As[tb] <- 6790.546

# 10. MEASURED DATA --------------------------------------------------------
# sapflow data, está en cm3 cm-2 sapwood h-1 , y el timestep es 30 minutos, 
# per passar a dm3 per m-2 fulla en 30 m cal dividir per 2 (per passar a 30 min), multiplicar per 0.001 (per passar a de cm3 a dm3)
# i dividir per Al2As (en m2 per cm2 sapwood)
# Sumamos todo el día y luego multiplicamos por le numero total de arboles y dividimos por los arboles medidos
# y el area de la parcela
transp_data_temp <- sapf_data |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  dplyr::mutate(ESP_RON_PIL_Api_Js_1 = 0.001*0.5*ESP_RON_PIL_Api_Js_1/(customParams$Al2As[ap]/10000),
                ESP_RON_PIL_Api_Js_10 = 0.001*0.5*ESP_RON_PIL_Api_Js_10/(customParams$Al2As[ap]/10000),
                ESP_RON_PIL_Tba_Js_11 = 0.001*0.5*ESP_RON_PIL_Tba_Js_11/(customParams$Al2As[tb]/10000),
                ESP_RON_PIL_Tba_Js_12 = 0.001*0.5*ESP_RON_PIL_Tba_Js_12/(customParams$Al2As[tb]/10000),
                ESP_RON_PIL_Api_Js_2 = 0.001*0.5*ESP_RON_PIL_Api_Js_2/(customParams$Al2As[ap]/10000),
                ESP_RON_PIL_Api_Js_3 = 0.001*0.5*ESP_RON_PIL_Api_Js_3/(customParams$Al2As[ap]/10000),
                ESP_RON_PIL_Api_Js_4 = 0.001*0.5*ESP_RON_PIL_Api_Js_4/(customParams$Al2As[ap]/10000),
                ESP_RON_PIL_Api_Js_5 = 0.001*0.5*ESP_RON_PIL_Api_Js_5/(customParams$Al2As[ap]/10000),
                ESP_RON_PIL_Api_Js_6 = 0.001*0.5*ESP_RON_PIL_Api_Js_6/(customParams$Al2As[ap]/10000),
                ESP_RON_PIL_Api_Js_7 = 0.001*0.5*ESP_RON_PIL_Api_Js_7/(customParams$Al2As[ap]/10000),
                ESP_RON_PIL_Api_Js_8 = 0.001*0.5*ESP_RON_PIL_Api_Js_8/(customParams$Al2As[ap]/10000),
                ESP_RON_PIL_Api_Js_9 = 0.001*0.5*ESP_RON_PIL_Api_Js_9/(customParams$Al2As[ap]/10000))|>
  dplyr::group_by(dates)  |>
  dplyr::summarise_at(dplyr::vars(dplyr::starts_with('ESP_RON_PIL')),
                      dplyr::funs(sum(., na.rm = TRUE)))  |>
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('ESP_RON_PIL')),
                   dplyr::funs(replace(., . == 0, NA)))


transp_data_temp2<-data.frame(dates = transp_data_temp$dates,
                              E_Api = rowMeans(transp_data_temp[,2:11], na.rm=TRUE),
                              E_Tba = rowMeans(transp_data_temp[,12:13], na.rm=TRUE))

measuredData <- env_data %>%
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  group_by(dates) |>
  summarise(SWC = mean(swc_shallow)) |>
  dplyr::mutate(SWC_err = NA)|>
  dplyr::full_join(transp_data_temp2, by = 'dates')|>
  dplyr::arrange(dates) 
names(measuredData)[4:5] = c(paste0("E_", AP_cohname),
                             paste0("E_", TB_cohname))

# 11. EVALUATION PERIOD ---------------------------------------------------
# Select evaluation dates


# 12. REMARKS -------------------------------------------------------------
remarks <- data.frame(
  Title = c('Soil',
            'Vegetation',
            'Weather'),
  Remark = c('Taken from SoilGrids with theta_sat and theta_res modified',
             'Understory not considered',
             'Complemented with interpolated weather')
)



# 13. SAVE DATA IN FOLDER -------------------------------------------------
folder_name <- file.path('Sites_data', 'RONDA')

write.table(siteData, file = file.path(folder_name, 'RONDA_siteData.txt'),
            row.names = FALSE, sep = '\t')

write.table(treeData, file = file.path(folder_name, 'RONDA_treeData.txt'),
            row.names = FALSE, sep = '\t')

write.table(shrubData, file = file.path(folder_name, 'RONDA_shrubData.txt'),
            row.names = FALSE, sep = '\t')

write.table(miscData, file = file.path(folder_name, 'RONDA_miscData.txt'),
            row.names = FALSE, sep = '\t')

write.table(meteoData, file = file.path(folder_name, 'RONDA_meteoData.txt'),
            row.names = FALSE, sep = '\t')

write.table(soilData, file = file.path(folder_name, 'RONDA_soilData.txt'),
            row.names = FALSE, sep = '\t')

write.table(terrainData, file = file.path(folder_name, 'RONDA_terrainData.txt'),
            row.names = FALSE, sep = '\t')

write.table(customParams, file = file.path(folder_name, 'RONDA_customParams.txt'),
            row.names = FALSE, sep = '\t')

write.table(measuredData, file = file.path(folder_name, 'RONDA_measuredData.txt'),
            row.names = FALSE, sep = '\t')

write.table(remarks, file = file.path(folder_name, 'RONDA_remarks.txt'),
            row.names = FALSE, sep = '\t')

