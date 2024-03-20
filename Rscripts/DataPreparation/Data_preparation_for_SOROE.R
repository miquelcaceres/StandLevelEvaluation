## Soroe data script
library(medfate)
library(medfateutils)
library(meteoland)
library(dplyr)
library(lubridate)
library(readxl)

data("SpParamsFR")

# 0. LOAD DATA and METADATA -----------------------------------------------
Soroe_stand <- read.table('SourceData/Tables/Soroe/Soroe_stand.csv',
                               sep = ',', header = TRUE)
fluxnet_data <- read.csv('SourceData/Tables/Soroe/FLX_DK-Sor_FLUXNET2015_SUBSET_DD_1996-2014_2-4.csv')
# fluxnet_data_hourly <- read.csv('SourceData/Tables/Soroe/FLX_DK-Sor_FLUXNET2015_SUBSET_HH_1996-2014_2-4.csv')


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
                'Forest stand',
                'Stand LAI',
                'Stand description DOI',
                'Species simulated',
                'Species parameter table',
                'Simulation period',
                'Evaluation period'),
  Value = c("Soroe",
            "Denmark",
            "",
            "",
            "DK-Sor",
            "Andreas Ibrom (Technical University of Denmark)",
            11.6446,
            55.4859,
            40,
            0, 
            NA,  
            "", #PArent material
            "", #Soil texture type
            8.2, 
            660, 
            "European beech forest",
            4.5,
            "10.1016/j.agrformet.2011.02.013",
            "Fagus sylvatica",
            "SpParamsFR",
            "2003-2006",
            "2003-2006")
)


# 2. TERRAIN DATA ---------------------------------------------------------
terrainData <- data.frame(
  latitude = 55.4859,
  elevation = 40,
  aspect = NA, # N
  slope = 0 # = 5-10%
)

# 3. TREE DATA ----------------------------------------------------------
#Fill the list
mapping = c("Species.name" = "Species", "DBH" = "Diameter.cm")
sampled_area <- pi*56.42^2 #Radius of 56.42 to obtain the hectare
treeData <- medfateutils::forest_mapTreeTable(subset(Soroe_stand, Plot.Code=="SOROE"), 
                                              mapping_x = mapping, SpParams = SpParamsFR,
                                              plot_size_x = sampled_area)
#Add height
treeData$Height <- 100 * 1.806*treeData$DBH^0.518
#Force Height based on data available
treeData$Height <- 2099 #cm

Soroe_forest <- emptyforest()
Soroe_forest$treeData <- treeData
Soroe_forest <- forest_mergeTrees(Soroe_forest)
treeData <- Soroe_forest$treeData
treeData$LAI <- 4.5
Soroe_forest$treeData <- treeData
summary(Soroe_forest, SpParamsFR)

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
  ID = 'SOROE',
  SpParamsName = "SpParamsFR",
  herbCover = 0, herbHeight = 0,
  Validation = 'global', Definitive = 'No'
)

# 7. SOIL DATA ------------------------------------------------------------
# sf_pt <- sf::st_sfc(sf::st_point(c(11.6446, 55.4859)), crs = 4326)
# soilData <- medfateutils::soilgridsParams(sf_pt, widths = c(400, 200, 3000))
soilData <- data.frame(
  widths = c(400, 200, 3000),
  clay = c(15.27, 19.20, 19.60),
  sand = c(58.73, 55.70, 55.45),
  om = c(5.42, 2.18, 1.78),
  bd = c(1.28, 1.58,1.65),
  rfc = c(7.48,6.10,7.55)
)
#Usually the rock fragments are underestimated from soilgrids so we force the value
#Soroe_soil_props$widths <- 1800
soilData$clay[1] <- 25.10564112 #From PROFOUND data
soilData$sand[1] <- 50.35045536 #From PROFOUND data
soilData$clay[2] <- 39.17793701 #From PROFOUND data
soilData$sand[2] <- 34.30827219 #From PROFOUND data

s = soil(soilData, VG_PTF = "Toth")
sum(soil_waterExtractable(s, model="VG", minPsi = -4))

# 8. METEO DATA -----------------------------------------------------------
# meteoData <- fluxnet_data_hourly |>
#   dplyr::mutate(RH = replace(RH, RH==-9999, NA)) |>
#   dplyr::mutate(dates = as.Date(substr(as.character(TIMESTAMP_START),1,8), format = "%Y%m%d")) |>
#   dplyr::group_by(dates) |>
#   dplyr::summarise(MinTemperature = min(TA_F, na.rm = TRUE),
#                    MaxTemperature = max(TA_F, na.rm = TRUE),
#                    MinRelativeHumidity = min(RH, na.rm = TRUE),
#                    MaxRelativeHumidity = max(RH, na.rm = TRUE),
#                    Radiation = (sum((SW_IN_F * 1800), na.rm = TRUE)/(24*3600)), # W/m2, a W/m2 en el día
#                    Precipitation = sum(P_F, na.rm = TRUE),
#                    WindSpeed = mean(WS_F, na.rm = TRUE)) |>
#   dplyr::mutate(Radiation = Radiation*3600*24/1000000) |>
#   dplyr::mutate_at(dplyr::vars(2:5),
#                    dplyr::funs(replace(., is.infinite(.), NA))) |>
#   dplyr::mutate_at(dplyr::vars(2:5),
#                    dplyr::funs(replace(., is.nan(.), NA)))

meteoData <- read.table('SourceData/Tables/Soroe/Soroe_meteo.csv',
                               sep = ',', header = TRUE)
meteoData$dates <- as.Date(meteoData$dates, format = '%d/%m/%y')

# 9. CUSTOM PARAMS --------------------------------------------------------
FS_index = SpParamsFR$SpIndex[SpParamsFR$Name=="Fagus sylvatica"]
FS_cohname = paste0("T1_", FS_index)
fs<- 1
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

customParams$Vmax298 = 94.5
customParams$Jmax298 = 159.9

# 10. MEASURED DATA --------------------------------------------------------
measuredData <- fluxnet_data |>
  dplyr::mutate(SWC_F_MDS_1 = replace(SWC_F_MDS_1, SWC_F_MDS_1==-9999, NA),
                H_F_MDS = replace(H_F_MDS, H_F_MDS==-9999, NA),
                LE_F_MDS = replace(LE_F_MDS, LE_F_MDS==-9999, NA),
                GPP_NT_VUT_REF = replace(GPP_NT_VUT_REF, GPP_NT_VUT_REF==-9999, NA))|>
  dplyr::mutate(dates = as.Date(as.character(TIMESTAMP), format = "%Y%m%d")) |>
  dplyr::select(dates, SWC_F_MDS_1, H_F_MDS, LE_F_MDS, GPP_NT_VUT_REF) |>
  dplyr::mutate(SWC = SWC_F_MDS_1/100,
                H = (3600*24/1e6)*H_F_MDS,# From Wm2 to MJ/m2
                LE = (3600*24/1e6)*LE_F_MDS,# From Wm2 to MJ/m2
                GPP = GPP_NT_VUT_REF) |>
  dplyr::select(-SWC_F_MDS_1, -H_F_MDS, -LE_F_MDS, -GPP_NT_VUT_REF)


# 11. SIMULATION/EVALUATION PERIOD ---------------------------------------------------
# Select evaluation dates
simulation_period <- seq(as.Date("2003-01-01"),as.Date("2006-12-31"), by="day")
evaluation_period <- seq(as.Date("2003-01-01"),as.Date("2006-12-31"), by="day")
meteoData <- meteoData |> filter(dates %in% simulation_period)
measuredData <- measuredData |> filter(dates %in% evaluation_period)

# 12. REMARKS -------------------------------------------------------------
remarks <- data.frame(
  Title = c('Soil',
            'Vegetation',
            'Weather',
            'Sapflow',
            'Soil moisture',
            'Eddy covariance'),
  Remark = c('Taken from SoilGrids',
             'No understory',
             'From V. saponaro',
             'Not available',
             'Taken from FLUXNET',
             'Variables H_F_MDS and LE_F_MDS for sensible and latent heat')
)



# 13. SAVE DATA IN FOLDER -------------------------------------------------
folder_name <- file.path('Sites_data', 'SOROE')
if(!dir.exists(folder_name)) dir.create(folder_name)

write.table(siteData, file = file.path(folder_name, 'SOROE_siteData.txt'),
            row.names = FALSE, sep = '\t')

write.table(treeData, file = file.path(folder_name, 'SOROE_treeData.txt'),
            row.names = FALSE, sep = '\t')

write.table(shrubData, file = file.path(folder_name, 'SOROE_shrubData.txt'),
            row.names = FALSE, sep = '\t')

write.table(miscData, file = file.path(folder_name, 'SOROE_miscData.txt'),
            row.names = FALSE, sep = '\t')

write.table(meteoData, file = file.path(folder_name, 'SOROE_meteoData.txt'),
            row.names = FALSE, sep = '\t')

write.table(soilData, file = file.path(folder_name, 'SOROE_soilData.txt'),
            row.names = FALSE, sep = '\t')

write.table(terrainData, file = file.path(folder_name, 'SOROE_terrainData.txt'),
            row.names = FALSE, sep = '\t')

write.table(customParams, file = file.path(folder_name, 'SOROE_customParams.txt'),
            row.names = FALSE, sep = '\t')

write.table(measuredData, file = file.path(folder_name, 'SOROE_measuredData.txt'),
            row.names = FALSE, sep = '\t')

write.table(remarks, file = file.path(folder_name, 'SOROE_remarks.txt'),
            row.names = FALSE, sep = '\t')

