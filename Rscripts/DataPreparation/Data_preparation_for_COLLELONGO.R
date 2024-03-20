## Collelongo data script
library(medfate)
library(medfateutils)
library(meteoland)
library(dplyr)
library(lubridate)
library(readxl)

data("SpParamsFR")

# 0. LOAD DATA and METADATA -----------------------------------------------
Collelongo_stand <- read.table('SourceData/Tables/Collelongo/Collelongo_stand.csv',
                               sep = ',', header = TRUE)
fluxnet_data <- read.csv('SourceData/Tables/Collelongo/FLX_IT-Col_FLUXNET2015_FULLSET_DD_1996-2014_1-4.csv')
# fluxnet_data_hourly <- read.csv('SourceData/Tables/Collelongo/FLX_IT-Col_FLUXNET2015_FULLSET_HH_1996-2014_1-4.csv')


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
  Value = c("Collelongo",
            "Italy",
            "",
            "",
            "IT-Col",
            "Giorgio Matteucci (IEIF CNR)",
            13.5881,
            41.8494,
            1560,
            19.29, 
            252,  
            "Calcareous", #PArent material
            "Silt loam", #Soil texture type
            6.3, 
            1180, 
            "European beech forest",
            5.5,
            "10.1111/j.1365-2486.1996.tb00072.x",
            "Fagus sylvatica",
            "SpParamsFR",
            "2011-2013",
            "2011-2013")
)


# 2. TERRAIN DATA ---------------------------------------------------------
terrainData <- data.frame(
  latitude = 41.8494,
  elevation = 1560,
  aspect = 252, # N
  slope = 19.29 # = 5-10%
)

# 3. TREE DATA ----------------------------------------------------------
#Fill the list
mapping = c("Species.name" = "Species", "DBH" = "Diameter.cm")
sampled_area <- pi*56.42^2 #Radius of 56.42 to obtain the hectare
treeData <- medfateutils::forest_mapTreeTable(subset(Collelongo_stand, Plot.Code=="COLLELONGO"), 
                                              mapping_x = mapping, SpParams = SpParamsMED,
                                              plot_size_x = sampled_area)
#Add height
treeData$Height <- 100 * 1.806*treeData$DBH^0.518
#Force Height based on data available
treeData$Height <- 1898.9 #cm

Collelongo_forest <- emptyforest()
Collelongo_forest$treeData <- treeData
Collelongo_forest <- forest_mergeTrees(Collelongo_forest)
treeData <- Collelongo_forest$treeData
treeData$LAI <- 5.5
treeData$CR <- 0.5
Collelongo_forest$treeData <- treeData
summary(Collelongo_forest, SpParamsFR)

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
  ID = 'COLLELONGO',
  SpParamsName = "SpParamsFR",
  herbCover = 0, herbHeight = 0,
  Validation = 'global', Definitive = 'No'
)

# 7. SOIL DATA ------------------------------------------------------------
# sf_pt <- sf::st_sfc(sf::st_point(c(13.5881, 41.8494)), crs = 4326)
# soilData <- medfateutils::soilgridsParams(sf_pt, widths = c(300, 700, 2000))
soilData <- data.frame(
  widths = c(300, 700, 2000),
  clay = c(27.23333, 30.90000, 31.20000),
  sand = c(32.50, 32.15, 33.70),
  om = c(5.916667, 1.955000, 1.430000),
  bd = c(0.97, 1.37,1.44),
  rfc = c(17.4,20.9,22.8)
)
#Usually the rock fragments are underestimated from soilgrids so we force the value
soilData$clay[1] <- 15 #Mean value from field data (A1-A2 horizons 0-400mm)
soilData$sand[1] <- 40.45 #Mean value from field data (A1-A2 horizons 0-400mm)
soilData$clay[2] <- 19.2 #Mean value from field data (B1-B2t horizons 400-1100mm)
soilData$sand[2] <- 30 #Mean value from field data (B1-B2t horizons 400-1100mm)

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

meteoData <- read.table('SourceData/Tables/Collelongo/Collelongo_meteo.csv',
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
simulation_period <- seq(as.Date("2011-01-01"),as.Date("2013-12-31"), by="day")
evaluation_period <- seq(as.Date("2011-01-01"),as.Date("2013-12-31"), by="day")
meteoData <- meteoData |> filter(dates %in% simulation_period)
measuredData <- measuredData |> filter(dates %in% evaluation_period)

# 12. REMARKS -------------------------------------------------------------
remarks <- data.frame(
  Title = c('Soil',
            'Vegetation',
            'Weather',
            'Sapflow',
            'Eddy covariance'),
  Remark = c('Taken from SoilGrids',
             'No understory',
             'From V. saponaro',
             'Not available',
             'Variables H_F_MDS and LE_F_MDS for sensible and latent heat')
)



# 13. SAVE DATA IN FOLDER -------------------------------------------------
folder_name <- file.path('Sites_data', 'COLLELONGO')
if(!dir.exists(folder_name)) dir.create(folder_name)

write.table(siteData, file = file.path(folder_name, 'COLLELONGO_siteData.txt'),
            row.names = FALSE, sep = '\t')

write.table(treeData, file = file.path(folder_name, 'COLLELONGO_treeData.txt'),
            row.names = FALSE, sep = '\t')

write.table(shrubData, file = file.path(folder_name, 'COLLELONGO_shrubData.txt'),
            row.names = FALSE, sep = '\t')

write.table(miscData, file = file.path(folder_name, 'COLLELONGO_miscData.txt'),
            row.names = FALSE, sep = '\t')

write.table(meteoData, file = file.path(folder_name, 'COLLELONGO_meteoData.txt'),
            row.names = FALSE, sep = '\t')

write.table(soilData, file = file.path(folder_name, 'COLLELONGO_soilData.txt'),
            row.names = FALSE, sep = '\t')

write.table(terrainData, file = file.path(folder_name, 'COLLELONGO_terrainData.txt'),
            row.names = FALSE, sep = '\t')

write.table(customParams, file = file.path(folder_name, 'COLLELONGO_customParams.txt'),
            row.names = FALSE, sep = '\t')

write.table(measuredData, file = file.path(folder_name, 'COLLELONGO_measuredData.txt'),
            row.names = FALSE, sep = '\t')

write.table(remarks, file = file.path(folder_name, 'COLLELONGO_remarks.txt'),
            row.names = FALSE, sep = '\t')

