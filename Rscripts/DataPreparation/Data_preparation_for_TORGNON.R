## Torgnon data script
library(medfate)
library(medfateutils)
library(meteoland)
library(dplyr)
library(lubridate)
library(readxl)

data("SpParamsFR")

# 0. LOAD DATA and METADATA -----------------------------------------------
env_data <- read.csv('SourceData/Tables/Torgnon/ITA_TOR_env_data.csv')
sapf_data <- read.csv('SourceData/Tables/Torgnon/ITA_TOR_sapf_data.csv')
env_md <- read.csv('SourceData/Tables/Torgnon/ITA_TOR_env_md.csv')
site_md <- read.csv('SourceData/Tables/Torgnon/ITA_TOR_site_md.csv')
stand_md <- read.csv('SourceData/Tables/Torgnon/ITA_TOR_stand_md.csv')
plant_md <- read.csv('SourceData/Tables/Torgnon/ITA_TOR_plant_md.csv')
species_md <- read.csv('SourceData/Tables/Torgnon/ITA_TOR_species_md.csv')
fluxnet_data <- read.csv('SourceData/Tables/Torgnon/FLX_IT-Tor_FLUXNET2015_SUBSET_DD_2008-2014_2-4.csv')
fluxnet_data_hourly <- read.csv('SourceData/Tables/Torgnon/FLX_IT-Tor_FLUXNET2015_SUBSET_HH_2008-2014_2-4.csv')

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
  Value = c("Torgnon",
            "Italy",
            site_md$si_code,
            "Marta Galvagno (ARPA VdA))",
            "IT-Tor",
            "Edoardo cremonese (ARPA VdA))",
            round(site_md$si_lat,6),
            round(site_md$si_long,6),
            site_md$si_elev,
            2, # < 2%
            110, #S 
            "",
            "Loamy sand",
            round(site_md$si_mat,1),
            round(site_md$si_map),
            "European larch forest",
            stand_md$st_lai,
            "110.1007/s00484-012-0614-y",
            "Larix decidua",
            "SpParamsFR",
            "2013-2016",
            "2013-2016")
)


# 2. TERRAIN DATA ---------------------------------------------------------
terrainData <- data.frame(
  latitude = site_md$si_lat,
  elevation = site_md$si_elev,
  aspect = 110, # S
  slope = 2 # = 5-10%
)

# 3. TREE DATA ----------------------------------------------------------
# stand basal area = 15 (79% Quercus petraea, 15% Carpinus betulus)
# Density 
treeData <- data.frame(
  Species = c("Larix decidua subsp. decidua"),
  DBH = 200*sqrt(stand_md$st_basal_area/(pi*stand_md$st_density)), # From basal area 
  Height = 100*stand_md$st_height, # 28 m Carpinus in the understory
  N = stand_md$st_density,
  Z50 = NA,
  Z95 = NA,
  LAI = stand_md$st_lai
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
  ID = 'TORGNON',
  SpParamsName = "SpParamsFR",
  herbCover = 0, herbHeight = 0,
  Validation = 'global', Definitive = 'No'
)

# 7. METEO DATA -----------------------------------------------------------
meteoData1 <- fluxnet_data_hourly |>
  dplyr::mutate(RH = replace(RH, RH==-9999, NA)) |>
  dplyr::mutate(dates = as.Date(substr(as.character(TIMESTAMP_START),1,8), format = "%Y%m%d")) |>
  dplyr::group_by(dates) |>
  dplyr::summarise(MinTemperature = min(TA_F, na.rm = TRUE),
                   MaxTemperature = max(TA_F, na.rm = TRUE),
                   MinRelativeHumidity = min(RH, na.rm = TRUE),
                   MaxRelativeHumidity = max(RH, na.rm = TRUE),
                   Radiation = (sum((SW_IN_F * 1800), na.rm = TRUE)/(24*3600)), # W/m2, a W/m2 en el día
                   Precipitation = sum(P_F, na.rm = TRUE),
                   WindSpeed = mean(WS_F, na.rm = TRUE)) |>
  dplyr::mutate(Radiation = Radiation*3600*24/1000000) |>
  dplyr::mutate_at(dplyr::vars(2:5),
                   dplyr::funs(replace(., is.infinite(.), NA))) |>
  dplyr::mutate_at(dplyr::vars(2:5),
                   dplyr::funs(replace(., is.nan(.), NA))) 

meteoData2 <- env_data |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  dplyr::group_by(dates) |>
  dplyr::summarise(MinTemperature = min(ta, na.rm = TRUE),
                   MaxTemperature = max(ta, na.rm = TRUE),
                   MinRelativeHumidity = min(rh, na.rm = TRUE),
                   MaxRelativeHumidity = max(rh, na.rm = TRUE),
                   Radiation = (sum((sw_in * 1800), na.rm = TRUE)/(24*3600)), # W/m2, a W/m2 en el día
                   Precipitation = sum(precip, na.rm = TRUE),
                   WindSpeed = mean(ws, na.rm = TRUE)) |>
  dplyr::mutate(Radiation = Radiation*3600*24/1000000) |>
  dplyr::mutate_at(dplyr::vars(2:5),
                   dplyr::funs(replace(., is.infinite(.), NA))) |>
  dplyr::mutate_at(dplyr::vars(2:5),
                   dplyr::funs(replace(., is.nan(.), NA))) 


#Imputation of missing vaues
dates_mis <- which(is.na(meteoData2$MinTemperature))
meteoData2[318, 2:8] <- meteoData2[317, 2:8]
meteoData2[319, 2:8] <- meteoData2[317, 2:8]
meteoData2[481, 2:8] <- meteoData2[480, 2:8]
meteoData2[482, 2:8] <- meteoData2[480, 2:8]
meteoData2[483, 2:8] <- meteoData2[480, 2:8]

meteoData <- dplyr::bind_rows(meteoData1, meteoData2)

# 8. SOIL DATA ------------------------------------------------------------
# coords_sf <- sf::st_sfc(sf::st_point(c(site_md$si_long,site_md$si_lat)), crs = 4326)
# soilData <- medfateutils::soilgridsParams(coords_sf,  c(300, 300, 1400, 2000))
soilData <- data.frame(
  widths = c(300, 300, 1400, 2000),
  clay = c(21.5, 24.2, 23.2, 22.8),
  sand = c(41.7, 40.7, 40.4,41.1),
  om = c(9.100, 3.590, 2.295, 2.200),
  bd = c(1.056667, 1.330000,1.460000,1.470000),
  rfc = c(16.43333,22.30000,80,90)
)
s = soil(soilData, VG_PTF = "Toth")
sum(soil_waterExtractable(s, model="VG", minPsi = -4))


# 9. CUSTOM PARAMS --------------------------------------------------------
LD_index = SpParamsFR$SpIndex[SpParamsFR$Name=="Larix decidua subsp. decidua"]
LD_cohname = paste0("T1_", LD_index)
ld<- 1
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

Al2As_sp <- SpParamsFR$Al2As[SpParamsFR$Name=="Larix"] # m2/m2
customParams$Al2As <- Al2As_sp
customParams$LeafAngle <- 30

# 10. MEASURED DATA --------------------------------------------------------
# sapflow data, está en cm3 cm-2 h-1, y el timestep es 30 minutos, 
# cal dividir per 2 (per tenir flow en els 30 min), dividir per sapwood area,
# dividir per Al2As (m2/cm2), 
# multiplicar per 0.001 (per passar a de cm3 a dm3)
# Sumamos todo el día 
sapflow_factor <- 0.5/1000
transp_data_temp <- sapf_data |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  dplyr::mutate(ITA_TOR_Lde_Js_1 = sapflow_factor*ITA_TOR_Lde_Js_1/(Al2As_sp[1]/10000),
                ITA_TOR_Lde_Js_2 = sapflow_factor*ITA_TOR_Lde_Js_2/(Al2As_sp[1]/10000),
                ITA_TOR_Lde_Js_3 = sapflow_factor*ITA_TOR_Lde_Js_3/(Al2As_sp[1]/10000),
                ITA_TOR_Lde_Js_4 = sapflow_factor*ITA_TOR_Lde_Js_4/(Al2As_sp[1]/10000))|>
  dplyr::group_by(dates)  |>
  dplyr::summarise_at(dplyr::vars(dplyr::starts_with('ITA_TOR')),
                      dplyr::funs(sum(., na.rm = TRUE)))  |>
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('ITA_TOR')),
                   dplyr::funs(replace(., . == 0, NA)))

transp_data_temp2<-data.frame(dates = transp_data_temp$dates,
                              E_Ld = rowMeans(transp_data_temp[,2:5], na.rm=TRUE))

measuredData <- env_data %>%
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  group_by(dates) |>
  summarise(SWC = mean(swc_shallow),
            SWC_2 = mean(swc_deep)) |>
  dplyr::mutate(SWC_err = NA)|>
  dplyr::full_join(transp_data_temp2, by = 'dates')|>
  dplyr::arrange(dates) 
names(measuredData)[5] = c(paste0("E_", LD_cohname))

fluxData <- fluxnet_data |>
  dplyr::mutate(LE_F_MDS = replace(LE_F_MDS, LE_F_MDS==-9999, NA),
                LE_F_MDS = replace(LE_F_MDS, LE_F_MDS==-9999, NA),
                GPP_NT_VUT_REF = replace(GPP_NT_VUT_REF, GPP_NT_VUT_REF==-9999, NA),
                SWC_F_MDS_1 = replace(SWC_F_MDS_1, SWC_F_MDS_1==-9999, NA)) |>
  dplyr::mutate(dates = as.Date(as.character(TIMESTAMP), format = "%Y%m%d")) |>
  dplyr::select(dates, GPP_NT_VUT_REF, H_F_MDS, LE_F_MDS, SWC_F_MDS_1) |>
  dplyr::mutate(SWC = SWC_F_MDS_1/100,
                H = (3600*24/1e6)*H_F_MDS,
                LE = (3600*24/1e6)*LE_F_MDS,
                GPP = GPP_NT_VUT_REF) |>
  dplyr::select(-GPP_NT_VUT_REF, -H_F_MDS,-LE_F_MDS, -SWC_F_MDS_1)

measuredData <- measuredData|>
  dplyr::full_join(fluxData, by=c("dates", "SWC"))|>
  dplyr::arrange(dates) 

# 11. SIMULATION/EVALUATION PERIOD ---------------------------------------------------
simulation_period <- seq(as.Date("2013-01-01"),as.Date("2015-12-31"), by="day")
evaluation_period <- seq(as.Date("2013-01-01"),as.Date("2015-12-31"), by="day")
meteoData <- meteoData |> filter(dates %in% simulation_period)
measuredData <- measuredData |> filter(dates %in% evaluation_period)

# 12. REMARKS -------------------------------------------------------------
remarks <- data.frame(
  Title = c('Soil',
            'Vegetation',
            'Weather',
            'Sapflow',
            'Eddy covariance'),
  Remark = c('Taken from SoilGrids with theta_sat and theta_res modified',
             'Plantation',
             'Imputation for 4 days',
             'Huber value estimated at the species level for scaling',
             'Variables H_F_MDS and LE_F_MDS')
)



# 13. SAVE DATA IN FOLDER -------------------------------------------------
folder_name <- file.path('Sites_data', 'TORGNON')
if(!dir.exists(folder_name)) dir.create(folder_name)

write.table(siteData, file = file.path(folder_name, 'TORGNON_siteData.txt'),
            row.names = FALSE, sep = '\t')

write.table(treeData, file = file.path(folder_name, 'TORGNON_treeData.txt'),
            row.names = FALSE, sep = '\t')

write.table(shrubData, file = file.path(folder_name, 'TORGNON_shrubData.txt'),
            row.names = FALSE, sep = '\t')

write.table(miscData, file = file.path(folder_name, 'TORGNON_miscData.txt'),
            row.names = FALSE, sep = '\t')

write.table(meteoData, file = file.path(folder_name, 'TORGNON_meteoData.txt'),
            row.names = FALSE, sep = '\t')

write.table(soilData, file = file.path(folder_name, 'TORGNON_soilData.txt'),
            row.names = FALSE, sep = '\t')

write.table(terrainData, file = file.path(folder_name, 'TORGNON_terrainData.txt'),
            row.names = FALSE, sep = '\t')

write.table(customParams, file = file.path(folder_name, 'TORGNON_customParams.txt'),
            row.names = FALSE, sep = '\t')

write.table(measuredData, file = file.path(folder_name, 'TORGNON_measuredData.txt'),
            row.names = FALSE, sep = '\t')

write.table(remarks, file = file.path(folder_name, 'TORGNON_remarks.txt'),
            row.names = FALSE, sep = '\t')

