## Rinconada Q pyrenaica data script
library(medfate)
library(meteoland)
library(dplyr)
library(lubridate)
library(readxl)
data("SpParamsMED")

# 0. LOAD DATA and METADATA -----------------------------------------------
env_data <- read.csv('SourceData/Tables/Rinconada/ESP_RIN_env_data.csv')
sapf_data <- read.csv('SourceData/Tables/Rinconada/ESP_RIN_sapf_data.csv')
env_md <- read.csv('SourceData/Tables/Rinconada/ESP_RIN_env_md.csv')
site_md <- read.csv('SourceData/Tables/Rinconada/ESP_RIN_site_md.csv')
stand_md <- read.csv('SourceData/Tables/Rinconada/ESP_RIN_stand_md.csv')
plant_md <- read.csv('SourceData/Tables/Rinconada/ESP_RIN_plant_md.csv')
species_md <- read.csv('SourceData/Tables/Rinconada/ESP_RIN_species_md.csv')
swc_esprin <- readxl::read_xlsx('SourceData/Tables/Rinconada/ESP_RIN_SWC.xlsx')

QP_index = SpParamsMED$SpIndex[SpParamsMED$Name=="Quercus pyrenaica"]


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
  Value = c("Rinconada",
            "Spain",
            "ESP_RIN",
            "Virginia Hernandez-Santana (IRNAS-CSIC)",
            "",
            "",
            40.60028,
            -6.016667,
            1200,
            10,
            0, # North Orientation?
            "",
            "Silty loam",
            10.0,
            1000,
            "Young, homogeneous, Quercus pyrenaica regrowth forest",
            3.4,
            "Quercus pyrenaica",
            "2006-2007",
            "10.1016/j.foreco.2008.03.004")
)

# 2. TERRAIN DATA ---------------------------------------------------------
# sacado de los metadatos de sapfluxnet
terrainData <- data.frame(
  latitude = 40.60028,
  elevation = 1200,
  aspect = 0, # North
  slope = 10 # 
)

# 3. TREE DATA ----------------------------------------------------------
treeData <- data.frame(
  Species = "Quercus pyrenaica",
  DBH = 11.7, # from paper
  Height = 740, # from paper
  N = 1975,# from paper
  Z50 = 300,  # datos que luego se modificaran por la optimización
  Z95 = 1500,  # datos que luego se modificaran por la optimización
  LAI = 3.4
)

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
  ID = 'ESPRIN',
  SpParamsName = "SpParamsMED",
  herbCover = 5, herbHeight = 10,
  Validation = 'global', Definitive = 'Yes'
)

# 7. SOIL DATA ------------------------------------------------------------
# coords_sf <- sf::st_sfc(sf::st_point(c(-6.016667,40.60028)), crs = 4326)
# soilData <- medfateutils::soilgridsParams(coords_sf,  c(300, 700, 1000, 2500))
soilData <- data.frame(
  widths = c(250, 250, 500, 1000, 2500),
  clay = c(19.10, 23.95, 23.95, 24.50, 24.50),
  sand = c(45.33333, 41.60000, 41.60000, 42.30000,42.30000),
  om = c(4.0, 2, 1.315000, 0.820000, 0),
  bd = c(1.48, 1.48, 1.48,1.51,1.560000),
  rfc = c(10, 21.00,24.85,60,85),
  VG_theta_sat = rep(0.35, 5),
  VG_theta_res = rep(0.03, 5)
)
s = soil(soilData, VG_PTF = "Toth")
sum(soil_waterExtractable(s, model="VG", minPsi = -4))


# 8. METEO DATA -----------------------------------------------------------
meteoDataSite <- env_data |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  dplyr::group_by(dates) |>
  dplyr::summarise(MinTemperature = min(ta, na.rm = TRUE),
                   MaxTemperature = max(ta, na.rm = TRUE),
                   MinRelativeHumidity = min(rh, na.rm = TRUE),
                   MaxRelativeHumidity = max(rh, na.rm = TRUE),
                   WindSpeed = mean(ws, na.rm = TRUE),
                   Radiation = (sum((sw_in * 3600), na.rm = TRUE)/(24*3600)), # W/m2, a W/m2 en el día
                   Precipitation = sum(precip, na.rm = TRUE)) |>
  dplyr::mutate(Radiation = Radiation*3600*24/1000000) |> # J/m2 to MJ/m2/day
  dplyr::mutate_at(dplyr::vars(2:8),
                   dplyr::funs(replace(., is.infinite(.), NA))) %>%
  dplyr::mutate_at(dplyr::vars(2:8),
                   dplyr::funs(replace(., is.nan(.), NA))) %>%
  dplyr::mutate(Radiation = na_if(Radiation, 0))

# climate_base = "emf/datasets/Climate/"
# interpolator_file <- EMFdatautils::download_emfdata(climate_base,
#                                                     "Products/InterpolationData/Spain/Historic/calibrated/AEMET_37_daily_interpolator_2007.nc")
# interpolator <- meteoland::read_interpolator(interpolator_file)
# coords_sf <- sf::st_sfc(sf::st_point(c(-6.016667,40.60028)), crs = 4326)
# plot_sf <- sf::st_sf(terrainData, coords_sf)
# interpolated_data <- meteoland::interpolate_data(plot_sf, interpolator)
# meteoDataInt <- interpolated_data$interpolated_data[[1]]
# write.csv(meteoDataInt, file = 'SourceData/Tables/Rinconada/meteoDataInterpolated_2006.csv', row.names = FALSE)
# interpolator_file <- EMFdatautils::download_emfdata(climate_base,
#                                                     "Products/InterpolationData/Spain/Historic/calibrated/AEMET_37_daily_interpolator_2007.nc")
# interpolator <- meteoland::read_interpolator(interpolator_file)
# coords_sf <- sf::st_sfc(sf::st_point(c(-6.016667,40.60028)), crs = 4326)
# plot_sf <- sf::st_sf(terrainData, coords_sf)
# interpolated_data <- meteoland::interpolate_data(plot_sf, interpolator)
# meteoDataInt <- interpolated_data$interpolated_data[[1]]
# write.csv(meteoDataInt, file = 'SourceData/Tables/Rinconada/meteoDataInterpolated_2007.csv', row.names = FALSE)
meteoDataInt2006 <- read.csv(file = 'SourceData/Tables/Rinconada/meteoDataInterpolated_2006.csv')
meteoDataInt2007 <- read.csv(file = 'SourceData/Tables/Rinconada/meteoDataInterpolated_2007.csv')
meteoDataInt <- rbind(meteoDataInt2006, meteoDataInt2007)
meteoDataInt$dates <- as.Date(meteoDataInt$dates)
# CHeck
# ggplot()+
#   geom_line(aes(x = dates, y=MaxTemperature), col="black", data = meteoDataInt)+
#   geom_line(aes(x = dates, y=MaxTemperature), col="red", data = meteoDataSite)
# ggplot()+
#   geom_line(aes(x = dates, y=MinTemperature), col="black", data = meteoDataInt)+
#   geom_line(aes(x = dates, y=MinTemperature), col="red",  data = meteoDataSite)
# ggplot()+
#   geom_line(aes(x = dates, y=Precipitation), col="black", data = meteoDataInt)+
#   geom_line(aes(x = dates, y=Precipitation), col="red", data = meteoDataSite)
# ggplot()+
#   geom_line(aes(x = dates, y=Radiation), col="black", data = meteoDataInt)+
#   geom_line(aes(x = dates, y=Radiation), col="red", data = meteoDataSite)
# ggplot()+
#   geom_line(aes(x = dates, y=MinRelativeHumidity), col="black", data = meteoDataInt)+
#   geom_line(aes(x = dates, y=MinRelativeHumidity), col="red", data = meteoDataSite)
# ggplot()+
#   geom_line(aes(x = dates, y=MaxRelativeHumidity), col="black", data = meteoDataInt)+
#   geom_line(aes(x = dates, y=MaxRelativeHumidity), col="red", data = meteoDataSite)

meteoData <- rbind(meteoDataInt[!(meteoDataInt$dates %in% meteoDataSite$dates), names(meteoDataSite)], meteoDataSite) |>
  dplyr::arrange(dates)


# 9. CUSTOM PARAMS --------------------------------------------------------
QP_cohname = paste0("T1_", QP_index)
qp<- 1
customParams <- data.frame(
  Species = c("Quercus pyrenaica"),
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

Al2As_sp <- 4189.325
customParams$Al2As <- Al2As_sp
customParams$Kmax_stemxylem <- 1.0
customParams$VCleaf_kmax <- 4
# customParams$Vmax298 <- 100
customParams$Gswmax <- 0.300
customParams$Gswmin <- 0.003
# customParams$Gsw_AC_slope <- 8

# 10. MEASURED DATA --------------------------------------------------------
# sapflow data, está en dm3/h, y el timestep es 60 minutos, ya tenemos los L por hora.
# Sumamos todo el día y luego multiplicamos por le numero total de arboles y dividimos por los arboles medidos
# y el area de la parcela
transp_data_temp <- sapf_data |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  dplyr::group_by(dates)  |>
  dplyr::summarise_at(dplyr::vars(dplyr::starts_with('ESP_RIN')),
                      dplyr::funs(sum(., na.rm = TRUE)))  |>
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('ESP_RIN')),
                   dplyr::funs(replace(., . == 0, NA))) |>
  dplyr::mutate(ESP_RIN_Qpy_Jt_1 = ESP_RIN_Qpy_Jt_1/(plant_md$pl_sapw_area[1]/10000)/Al2As_sp,
                ESP_RIN_Qpy_Jt_2 = ESP_RIN_Qpy_Jt_2/(plant_md$pl_sapw_area[2]/10000)/Al2As_sp,
                ESP_RIN_Qpy_Jt_3 = ESP_RIN_Qpy_Jt_3/(plant_md$pl_sapw_area[3]/10000)/Al2As_sp,
                ESP_RIN_Qpy_Jt_4 = ESP_RIN_Qpy_Jt_4/(plant_md$pl_sapw_area[4]/10000)/Al2As_sp,
                ESP_RIN_Qpy_Jt_5 = ESP_RIN_Qpy_Jt_5/(plant_md$pl_sapw_area[5]/10000)/Al2As_sp,
                ESP_RIN_Qpy_Jt_6 = ESP_RIN_Qpy_Jt_6/(plant_md$pl_sapw_area[6]/10000)/Al2As_sp,
                ESP_RIN_Qpy_Jt_7 = ESP_RIN_Qpy_Jt_7/(plant_md$pl_sapw_area[7]/10000)/Al2As_sp,
                ESP_RIN_Qpy_Jt_8 = ESP_RIN_Qpy_Jt_8/(plant_md$pl_sapw_area[8]/10000)/Al2As_sp)

transp_data_temp2<-data.frame(dates = transp_data_temp$dates,
                         E = rowMeans(transp_data_temp[,-1], na.rm = TRUE))

measuredData <- swc_esprin %>%
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  dplyr::select(dates, swc_shallow, swc_deep) |>
  dplyr::rename(SWC = swc_shallow, SWC_2 = swc_deep) |>
  dplyr::full_join(transp_data_temp2, by = 'dates') |>
  dplyr::mutate(SWC_err = NA) |>
  dplyr::select(dates, SWC, SWC_err, SWC_2, E) |>
  dplyr::arrange(dates)

wpData <- read_xlsx("SourceData/Tables/Rinconada/ESP_RIN_WP.xlsx")
wpData$dates = as.Date(wpData$dates)
measuredData <- full_join(measuredData, wpData, by="dates")|>
  dplyr::arrange(dates)
names(measuredData)[5:9] = c(paste0("E_", QP_cohname),
                             paste0("PD_", QP_cohname),
                             paste0("PD_", QP_cohname, "_err"),
                             paste0("MD_", QP_cohname),
                             paste0("MD_", QP_cohname, "_err"))

# 11. EVALUATION PERIOD ---------------------------------------------------
# Select evaluation dates


# 12. REMARKS -------------------------------------------------------------
remarks <- data.frame(
  Title = c('Soil',
            'Vegetation',
            'Weather'),
  Remark = c('Taken from SoilGrids, with modification of theta_sat and theta_res',
             'Understory not considered',
             'Available weather complemented with interpolation')
)



# 13. SAVE DATA IN FOLDER -------------------------------------------------
folder_name <- file.path('Sites_data', 'ESPRIN')

write.table(siteData, file = file.path(folder_name, 'ESPRIN_siteData.txt'),
            row.names = FALSE, sep = '\t')

write.table(treeData, file = file.path(folder_name, 'ESPRIN_treeData.txt'),
            row.names = FALSE, sep = '\t')

write.table(shrubData, file = file.path(folder_name, 'ESPRIN_shrubData.txt'),
            row.names = FALSE, sep = '\t')

write.table(miscData, file = file.path(folder_name, 'ESPRIN_miscData.txt'),
            row.names = FALSE, sep = '\t')

write.table(meteoData, file = file.path(folder_name, 'ESPRIN_meteoData.txt'),
            row.names = FALSE, sep = '\t')

write.table(soilData, file = file.path(folder_name, 'ESPRIN_soilData.txt'),
            row.names = FALSE, sep = '\t')

write.table(terrainData, file = file.path(folder_name, 'ESPRIN_terrainData.txt'),
            row.names = FALSE, sep = '\t')

write.table(customParams, file = file.path(folder_name, 'ESPRIN_customParams.txt'),
            row.names = FALSE, sep = '\t')

write.table(measuredData, file = file.path(folder_name, 'ESPRIN_measuredData.txt'),
            row.names = FALSE, sep = '\t')

write.table(remarks, file = file.path(folder_name, 'ESPRIN_remarks.txt'),
            row.names = FALSE, sep = '\t')

