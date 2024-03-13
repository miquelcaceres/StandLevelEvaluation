## GB Griffin Forest data script
library(medfate)
library(medfateutils)
library(meteoland)
library(dplyr)
library(lubridate)
library(readxl)

data("SpParamsFR")

# 0. LOAD DATA and METADATA -----------------------------------------------
env_data <- read.csv('SourceData/Tables/GriffinForest/GBR_ABE_PLO_env_data.csv')
sapf_data <- read.csv('SourceData/Tables/GriffinForest/GBR_ABE_PLO_sapf_data.csv')
env_md <- read.csv('SourceData/Tables/GriffinForest/GBR_ABE_PLO_env_md.csv')
site_md <- read.csv('SourceData/Tables/GriffinForest/GBR_ABE_PLO_site_md.csv')
stand_md <- read.csv('SourceData/Tables/GriffinForest/GBR_ABE_PLO_stand_md.csv')
plant_md <- read.csv('SourceData/Tables/GriffinForest/GBR_ABE_PLO_plant_md.csv')
species_md <- read.csv('SourceData/Tables/GriffinForest/GBR_ABE_PLO_species_md.csv')


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
  Value = c("GriffinForest",
            "Great Britain",
            site_md$si_code,
            "Lisa Wingate (INRAE ISPA)",
            site_md$si_lat,
            site_md$si_long,
            site_md$si_elev,
            4.3, # = 5-10%
            0, #N 
            "",
            "Sandy loam",
            site_md$si_mat,
            site_md$si_map,
            "Sitka spruce plantation",
            stand_md$st_lai,
            "Picea sitchensis",
            "10.1111/j.1365-3040.2007.01647.x")
)


# 2. TERRAIN DATA ---------------------------------------------------------
terrainData <- data.frame(
  latitude = site_md$si_lat,
  elevation = site_md$si_elev,
  aspect = 0, # N
  slope = 4.3 # = 5-10%
)

# 3. TREE DATA ----------------------------------------------------------
# stand basal area = 32 
# Density 
treeData <- data.frame(
  Species = c("Picea sitchensis"),
  DBH = 200*sqrt(32/(pi*2200)), # From basal area 
  Height = 100*10, # 10 m
  N = 2200,
  Z50 = NA,
  Z95 = NA,
  LAI = 6
)
f <-emptyforest()
f$treeData <- treeData
summary(f, SpParamsFR)

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
  ID = 'GRIFFIN',
  SpParamsName = "SpParamsFR",
  herbCover = 5, herbHeight = 10,
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
                   Radiation = (sum((sw_in * 900), na.rm = TRUE)/(24*3600)), # W/m2, a W/m2 en el día
                   Precipitation = sum(precip, na.rm = TRUE)) |>
  dplyr::mutate(Radiation = Radiation*3600*24/1000000,
                WindSpeed = NA) |>
  dplyr::mutate_at(dplyr::vars(2:5),
                   dplyr::funs(replace(., is.infinite(.), NA))) |>
  dplyr::mutate_at(dplyr::vars(2:5),
                   dplyr::funs(replace(., is.nan(.), NA))) 

# 8. SOIL DATA ------------------------------------------------------------
# coords_sf <- sf::st_sfc(sf::st_point(c(site_md$si_long,site_md$si_lat)), crs = 4326)
# soilData <- medfateutils::soilgridsParams(coords_sf,  c(300, 700, 1000, 2500))
soilData <- data.frame(
  widths = c(300, 700, 1000, 2500),
  clay = c(12.76667, 11.10000, 11.10000, 11.10000),
  sand = c(67.36667, 71.05000, 72.40000,72.40000),
  om = c(15.24667, 4.00000, 3.83000, 0),
  bd = c(0.840, 1.045,1.170,1.170),
  rfc = c(19.86667,25.60000,60,90),
  VG_theta_res = rep(0.09,4),
  VG_theta_sat = rep(0.8, 4)
)
s = soil(soilData, VG_PTF = "Toth")
sum(soil_waterExtractable(s, model="VG", minPsi = -4))


# 9. CUSTOM PARAMS --------------------------------------------------------
PS_index = SpParamsFR$SpIndex[SpParamsFR$Name=="Picea sitchensis"]
PS_cohname = paste0("T1_", PS_index)
ps<- 1
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

customParams$LeafAngle <- 30
  
As2Al = plant_md[['pl_sapw_area']]/plant_md[['pl_leaf_area']] # cm2/m2
Al2As_sp = 10000/mean(As2Al)

customParams$Al2As <- Al2As_sp

# 10. MEASURED DATA --------------------------------------------------------
# sapflow data, está en cm3 cm-2 h-1, y el timestep es 30 minutos, 
# cal dividir per 2 (per tenir flow en els 30 min), multiplicar per As2Al (cm2/m2), 
# multiplicar per 0.001 (per passar a de cm3 a dm3)
# Sumamos todo el día 
sapflow_factor <- 0.5*3600/1000
transp_data_temp <- sapf_data |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  dplyr::mutate(GBR_ABE_PLO_Psi_Jt_1 = sapflow_factor*GBR_ABE_PLO_Psi_Jt_1*As2Al[1],
                GBR_ABE_PLO_Psi_Jt_10 = sapflow_factor*GBR_ABE_PLO_Psi_Jt_10*As2Al[2],
                GBR_ABE_PLO_Psi_Jt_11 = sapflow_factor*GBR_ABE_PLO_Psi_Jt_11*As2Al[3],
                GBR_ABE_PLO_Psi_Jt_12 = sapflow_factor*GBR_ABE_PLO_Psi_Jt_12*As2Al[4],
                GBR_ABE_PLO_Psi_Jt_13 = sapflow_factor*GBR_ABE_PLO_Psi_Jt_13*As2Al[5],
                GBR_ABE_PLO_Psi_Jt_14 = sapflow_factor*GBR_ABE_PLO_Psi_Jt_14*As2Al[6],
                GBR_ABE_PLO_Psi_Jt_15 = sapflow_factor*GBR_ABE_PLO_Psi_Jt_15*As2Al[7],
                GBR_ABE_PLO_Psi_Jt_2 = sapflow_factor*GBR_ABE_PLO_Psi_Jt_2*As2Al[8],
                GBR_ABE_PLO_Psi_Jt_3 = sapflow_factor*GBR_ABE_PLO_Psi_Jt_3*As2Al[9],
                GBR_ABE_PLO_Psi_Jt_4 = sapflow_factor*GBR_ABE_PLO_Psi_Jt_4*As2Al[10],
                GBR_ABE_PLO_Psi_Jt_5 = sapflow_factor*GBR_ABE_PLO_Psi_Jt_5*As2Al[11],
                GBR_ABE_PLO_Psi_Jt_6 = sapflow_factor*GBR_ABE_PLO_Psi_Jt_6*As2Al[12],
                GBR_ABE_PLO_Psi_Jt_7 = sapflow_factor*GBR_ABE_PLO_Psi_Jt_7*As2Al[13],
                GBR_ABE_PLO_Psi_Jt_8 = sapflow_factor*GBR_ABE_PLO_Psi_Jt_8*As2Al[14],
                GBR_ABE_PLO_Psi_Jt_9 = sapflow_factor*GBR_ABE_PLO_Psi_Jt_9*As2Al[15])|>
  dplyr::group_by(dates)  |>
  dplyr::summarise_at(dplyr::vars(dplyr::starts_with('GBR_ABE_PLO')),
                      dplyr::funs(sum(., na.rm = TRUE)))  |>
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('GBR_ABE_PLO')),
                   dplyr::funs(replace(., . == 0, NA)))

transp_data_temp2<-data.frame(dates = transp_data_temp$dates,
                              E_Ps = rowMeans(transp_data_temp[,-1], na.rm=TRUE))

measuredData <- env_data %>%
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  group_by(dates) |>
  summarise(SWC = mean(swc_shallow)) |>
  dplyr::mutate(SWC_err = NA)|>
  dplyr::full_join(transp_data_temp2, by = 'dates')|>
  dplyr::arrange(dates) 
names(measuredData)[4] = paste0("E_", PS_cohname)

# 11. EVALUATION PERIOD ---------------------------------------------------
# Select evaluation dates

# 12. REMARKS -------------------------------------------------------------
remarks <- data.frame(
  Title = c('Soil',
            'Vegetation',
            'Weather'),
  Remark = c('Taken from SoilGrids with theta_sat and theta_res modified',
             'Plantation',
             'Windspeed is missing')
)



# 13. SAVE DATA IN FOLDER -------------------------------------------------
folder_name <- file.path('Sites_data', 'GRIFFIN')
if(!dir.exists(folder_name)) dir.create(folder_name)

write.table(siteData, file = file.path(folder_name, 'GRIFFIN_siteData.txt'),
            row.names = FALSE, sep = '\t')

write.table(treeData, file = file.path(folder_name, 'GRIFFIN_treeData.txt'),
            row.names = FALSE, sep = '\t')

write.table(shrubData, file = file.path(folder_name, 'GRIFFIN_shrubData.txt'),
            row.names = FALSE, sep = '\t')

write.table(miscData, file = file.path(folder_name, 'GRIFFIN_miscData.txt'),
            row.names = FALSE, sep = '\t')

write.table(meteoData, file = file.path(folder_name, 'GRIFFIN_meteoData.txt'),
            row.names = FALSE, sep = '\t')

write.table(soilData, file = file.path(folder_name, 'GRIFFIN_soilData.txt'),
            row.names = FALSE, sep = '\t')

write.table(terrainData, file = file.path(folder_name, 'GRIFFIN_terrainData.txt'),
            row.names = FALSE, sep = '\t')

write.table(customParams, file = file.path(folder_name, 'GRIFFIN_customParams.txt'),
            row.names = FALSE, sep = '\t')

write.table(measuredData, file = file.path(folder_name, 'GRIFFIN_measuredData.txt'),
            row.names = FALSE, sep = '\t')

write.table(remarks, file = file.path(folder_name, 'GRIFFIN_remarks.txt'),
            row.names = FALSE, sep = '\t')

