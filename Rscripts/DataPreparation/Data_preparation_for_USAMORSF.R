## US. Morgan Monroe data script
library(medfate)
library(meteoland)
library(dplyr)
library(lubridate)
library(readxl)
data("SpParamsUS")

# 0. LOAD DATA and METADATA -----------------------------------------------
env_data <- read.csv('SourceData/Tables/MorganMonroe/USA_MOR_SF_env_data.csv')
sapf_data <- read.csv('SourceData/Tables/MorganMonroe/USA_MOR_SF_sapf_data.csv')
env_md <- read.csv('SourceData/Tables/MorganMonroe/USA_MOR_SF_env_md.csv')
site_md <- read.csv('SourceData/Tables/MorganMonroe/USA_MOR_SF_site_md.csv')
stand_md <- read.csv('SourceData/Tables/MorganMonroe/USA_MOR_SF_stand_md.csv')
plant_md <- read.csv('SourceData/Tables/MorganMonroe/USA_MOR_SF_plant_md.csv')
species_md <- read.csv('SourceData/Tables/MorganMonroe/USA_MOR_SF_species_md.csv')


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
  Value = c("Morgan Mornoe",
            "USA",
            site_md$si_code,
            "Koong Yi (Indiana University Bloomington)",
            site_md$si_lat,
            site_md$si_long,
            site_md$si_elev,
            0,
            0, #NW 
            "",
            "Silty clay loam",
            site_md$si_mat,
            site_md$si_map,
            "Mixed temperate forest",
            5,
            "Acer saccharum, Liriodendron tulipifera, Quercus rubra, Quercus alba",
            "")
)


# 2. TERRAIN DATA ---------------------------------------------------------
terrainData <- data.frame(
  latitude = site_md$si_lat,
  elevation = site_md$si_elev,
  aspect = 0, # flat
  slope = 0 # 
)

# 3. TREE DATA ----------------------------------------------------------
treeData <- data.frame(
  Species = c("Acer saccharum", "Liriodendron tulipifera", "Quercus rubra", "Quercus alba"),
  DBH = c(mean(plant_md$pl_dbh[plant_md$pl_species=="Acer saccharum"],na.rm=TRUE),
          mean(plant_md$pl_dbh[plant_md$pl_species=="Liriodendron tulipifera"],na.rm=TRUE),
          mean(plant_md$pl_dbh[plant_md$pl_species=="Quercus rubra"],na.rm=TRUE),
          mean(plant_md$pl_dbh[plant_md$pl_species=="Quercus alba"],na.rm=TRUE)), # from paper
  Height = 100*c(mean(plant_md$pl_height[plant_md$pl_species=="Acer saccharum"],na.rm=TRUE),
                 mean(plant_md$pl_height[plant_md$pl_species=="Liriodendron tulipifera"],na.rm=TRUE),
                 mean(plant_md$pl_height[plant_md$pl_species=="Quercus rubra"],na.rm=TRUE),
                 mean(plant_md$pl_height[plant_md$pl_species=="Quercus alba"],na.rm=TRUE)),
  N = NA,
  Z50 = NA,
  Z95 = NA,
  LAI = rep(5/4, 4) # No information to split LAI
)
f <-emptyforest()
f$treeData <- treeData
summary(f, SpParamsUS)

# 4. SHRUB DATA -----------------------------------------------------------
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
  ID = 'USAMORSF',
  SpParamsName = "SpParamsUS",
  herbCover = 10, herbHeight = 20,
  Validation = 'global', Definitive = 'No'
)

# 7. METEO DATA -----------------------------------------------------------
meteoData <- env_data |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  dplyr::group_by(dates) |>
  dplyr::summarise(MinTemperature = min(ta, na.rm = TRUE),
                   MaxTemperature = max(ta, na.rm = TRUE),
                   MinRelativeHumidity = min(rh, na.rm = TRUE),
                   MaxRelativeHumidity = max(rh, na.rm = TRUE),
                   WindSpeed = mean(ws, na.rm = TRUE),
                   Radiation = (sum((ext_rad * 3600), na.rm = TRUE)/(24*3600)), # W/m2, a W/m2 en el día
                   Precipitation = sum(precip, na.rm = TRUE)) |>
  dplyr::mutate(Radiation = Radiation*3600*24/1000000) |>
  dplyr::mutate_at(dplyr::vars(2:5),
                   dplyr::funs(replace(., is.infinite(.), NA))) |>
  dplyr::mutate_at(dplyr::vars(2:5),
                   dplyr::funs(replace(., is.nan(.), NA))) 

# 8. SOIL DATA ------------------------------------------------------------
# coords_sf <- sf::st_sfc(sf::st_point(c(site_md$si_long,site_md$si_lat)), crs = 4326)
# soilData <- medfateutils::soilgridsParams(coords_sf,  c(300, 700, 1000, 2500))
soilData <- data.frame(
  widths = c(300, 700, 1000, 2500),
  clay = c(24.70, 27.95, 35.30, 35.30),
  sand = c(8.166667, 11.000000, 20.100000,20.100000),
  om = c(1.940, 0.265, 0.180, 0),
  bd = c(1.430, 1.645,1.740,1.740),
  rfc = c(3,4.3,60,90)
)
s = soil(soilData, VG_PTF = "Toth")
sum(soil_waterExtractable(s, model="VG", minPsi = -4))


# 9. CUSTOM PARAMS --------------------------------------------------------
AS_index = SpParamsUS$SpIndex[SpParamsUS$Name=="Acer saccharum"]
LT_index = SpParamsUS$SpIndex[SpParamsUS$Name=="Liriodendron tulipifera"]
QR_index = SpParamsUS$SpIndex[SpParamsUS$Name=="Quercus rubra"]
QA_index = SpParamsUS$SpIndex[SpParamsUS$Name=="Quercus alba"]
AS_cohname = paste0("T1_", AS_index)
LT_cohname = paste0("T2_", LT_index)
QR_cohname = paste0("T3_", QR_index)
QA_cohname = paste0("T4_", QA_index)
as<- 1
lt<-2
qr<-3
qa<-4

customParams <- data.frame(
  Species = treeData$Species,
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

Al2As_sp = c(SpParamsUS$Al2As[SpParamsUS$Name == "Acer saccharum"],
             SpParamsUS$Al2As[SpParamsUS$Name == "Liriodendron tulipifera"],
             SpParamsUS$Al2As[SpParamsUS$Name == "Quercus rubra"],
             SpParamsUS$Al2As[SpParamsUS$Name == "Quercus alba"])
Al2As_sp[2] <- mean(Al2As_sp, na.rm=TRUE)
customParams$Al2As <- Al2As_sp

# 10. MEASURED DATA --------------------------------------------------------
# sapflow data, está en cm3 h-1 , y el timestep es 60 minutos, 
# cal dividir per sapwood area, multiplicar per 0.001 (per passar a de cm3 a dm3)
# i dividir per Al2As (en m2 per cm2 sapwood)
# Sumamos todo el día 
transp_data_temp <- sapf_data |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  dplyr::mutate(USA_MOR_SF_Asa_Js_1 = (10000/1000)*USA_MOR_SF_Asa_Js_1/(Al2As_sp[1]*plant_md$pl_sapw_area[1]),
                USA_MOR_SF_Asa_Js_2 = (10000/1000)*USA_MOR_SF_Asa_Js_2/(Al2As_sp[1]*plant_md$pl_sapw_area[2]),
                USA_MOR_SF_Ltu_Js_3 = (10000/1000)*USA_MOR_SF_Ltu_Js_3/(Al2As_sp[2]*plant_md$pl_sapw_area[3]),
                USA_MOR_SF_Ltu_Js_4 = (10000/1000)*USA_MOR_SF_Ltu_Js_4/(Al2As_sp[2]*plant_md$pl_sapw_area[4]),
                USA_MOR_SF_Qru_Js_5 = (10000/1000)*USA_MOR_SF_Qru_Js_5/(Al2As_sp[3]*plant_md$pl_sapw_area[5]),
                USA_MOR_SF_Qal_Js_6 = (10000/1000)*USA_MOR_SF_Qal_Js_6/(Al2As_sp[4]*plant_md$pl_sapw_area[6]))|>
  dplyr::group_by(dates)  |>
  dplyr::summarise_at(dplyr::vars(dplyr::starts_with('USA_MOR_SF')),
                      dplyr::funs(sum(., na.rm = TRUE)))  |>
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('USA_MOR_SF')),
                   dplyr::funs(replace(., . == 0, NA)))

transp_data_temp2<-data.frame(dates = transp_data_temp$dates,
                              E_Asa = rowMeans(transp_data_temp[,2:3], na.rm=TRUE),
                              E_Ltu = rowMeans(transp_data_temp[,4:5], na.rm=TRUE),
                              E_Qru = rowMeans(transp_data_temp[,6], na.rm=TRUE),
                              E_Qal = rowMeans(transp_data_temp[,7], na.rm=TRUE))

measuredData <- env_data %>%
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  group_by(dates) |>
  summarise(SWC = mean(swc_shallow)) |>
  dplyr::mutate(SWC_err = NA)|>
  dplyr::full_join(transp_data_temp2, by = 'dates')|>
  dplyr::arrange(dates) 
names(measuredData)[4:7] = c(paste0("E_", AS_cohname),
                             paste0("E_", LT_cohname),
                             paste0("E_", QR_cohname),
                             paste0("E_", QA_cohname))

# 11. EVALUATION PERIOD ---------------------------------------------------
# Select evaluation dates


# 12. REMARKS -------------------------------------------------------------
remarks <- data.frame(
  Title = c('Soil',
            'Vegetation',
            'Weather'),
  Remark = c('Taken from SoilGrids with theta_sat and theta_res modified',
             'Understory not considered',
             'Using extraterrestrial radiation')
)



# 13. SAVE DATA IN FOLDER -------------------------------------------------
folder_name <- file.path('Sites_data', 'USAMORSF')

write.table(siteData, file = file.path(folder_name, 'USAMORSF_siteData.txt'),
            row.names = FALSE, sep = '\t')

write.table(treeData, file = file.path(folder_name, 'USAMORSF_treeData.txt'),
            row.names = FALSE, sep = '\t')

write.table(shrubData, file = file.path(folder_name, 'USAMORSF_shrubData.txt'),
            row.names = FALSE, sep = '\t')

write.table(miscData, file = file.path(folder_name, 'USAMORSF_miscData.txt'),
            row.names = FALSE, sep = '\t')

write.table(meteoData, file = file.path(folder_name, 'USAMORSF_meteoData.txt'),
            row.names = FALSE, sep = '\t')

write.table(soilData, file = file.path(folder_name, 'USAMORSF_soilData.txt'),
            row.names = FALSE, sep = '\t')

write.table(terrainData, file = file.path(folder_name, 'USAMORSF_terrainData.txt'),
            row.names = FALSE, sep = '\t')

write.table(customParams, file = file.path(folder_name, 'USAMORSF_customParams.txt'),
            row.names = FALSE, sep = '\t')

write.table(measuredData, file = file.path(folder_name, 'USAMORSF_measuredData.txt'),
            row.names = FALSE, sep = '\t')

write.table(remarks, file = file.path(folder_name, 'USAMORSF_remarks.txt'),
            row.names = FALSE, sep = '\t')

