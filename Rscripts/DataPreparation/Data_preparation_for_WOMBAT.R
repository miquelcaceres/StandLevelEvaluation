## WOMBAT data script
library(medfate)
library(medfateutils)
library(meteoland)
library(dplyr)
library(lubridate)
library(readxl)

data("SpParamsAU")

# 0. LOAD DATA and METADATA -----------------------------------------------
env_data <- read.csv('SourceData/Tables/Wombat/AUS_WOM_env_data.csv')
sapf_data <- read.csv('SourceData/Tables/Wombat/AUS_WOM_sapf_data.csv')
env_md <- read.csv('SourceData/Tables/Wombat/AUS_WOM_env_md.csv')
site_md <- read.csv('SourceData/Tables/Wombat/AUS_WOM_site_md.csv')
stand_md <- read.csv('SourceData/Tables/Wombat/AUS_WOM_stand_md.csv')
plant_md <- read.csv('SourceData/Tables/Wombat/AUS_WOM_plant_md.csv')
species_md <- read.csv('SourceData/Tables/Wombat/AUS_WOM_species_md.csv')
fluxnet_data <- read.csv('SourceData/Tables/Wombat/FLX_AU-Wom_FLUXNET2015_SUBSET_DD_2010-2014_1-4.csv')


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
  Value = c("Wombat",
            "Australia",
            site_md$si_code,
            "Anne Griebel (U. of Melbourne)",
            "AU-Wom",
            "Stefan Arndt (U. of Melbourne)",
            round(site_md$si_lat,5),
            round(site_md$si_long,5),
            site_md$si_elev,
            0, # < 2%
            0, #N 
            "",
            "Loam",
            round(site_md$si_mat,1),
            round(site_md$si_map),
            "Mixed eucalyptus forest",
            stand_md$st_lai,
            "10.1016/j.foreco.2016.12.017",
            "Eucalyptus obliqua, E. radiata, E. rubida",
            "SpParamsAU",
            "2013-2015",
            "2013-2015")
)


# 2. TERRAIN DATA ---------------------------------------------------------
terrainData <- data.frame(
  latitude = site_md$si_lat,
  elevation = site_md$si_elev,
  aspect = 0, # Flat
  slope = 0 # Flat
)

# 3. TREE DATA ----------------------------------------------------------
# stand basal area 42
# Eucalyptus obliqua = 70%, E. rubida = 21%, E. radiata = 9%
# Density 
treeData <- data.frame(
  Species = c("Eucalyptus obliqua", "Eucalyptus rubida", "Eucalyptus radiata"),
  DBH = 23, #?
  Height = rep(100*stand_md$st_height,3),
  N = stand_md$st_density*c(0.7, 0.21, 0.09),
  Z50 = rep(300,3),
  Z95 = rep(1000,3),
  LAI = stand_md$st_lai*c(0.7, 0.21, 0.09)
)
f <-emptyforest()
f$treeData <- treeData
summary(f, SpParamsAU)

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
  ID = 'WOMBAT',
  SpParamsName = "SpParamsAU",
  herbCover = 0, herbHeight = 0,
  Validation = 'global', Definitive = 'No'
)

# 7. SOIL DATA ------------------------------------------------------------
# coords_sf <- sf::st_sfc(sf::st_point(c(site_md$si_long,site_md$si_lat)), crs = 4326)
# soilData <- medfateutils::soilgridsParams(coords_sf,  c(300, 700, 1000, 2000))
soilData <- data.frame(
  widths =  c(300, 700, 1000, 2000),
  clay = c(24.30, 35.25, 33.10, 33.10),
  sand = c(55.13333, 44.60000, 45.90000,45.90000),
  om = c(8.340, 4.465, 4.160, 0),
  bd = c(0.9866667, 1.2100000,1.2800000,1.350000),
  rfc = c(13.60,14.35,50,90)
)
s = soil(soilData, VG_PTF = "Toth")
sum(soil_waterExtractable(s, model="VG", minPsi = -4))

# 8. METEO DATA -----------------------------------------------------------
meteoData <- env_data |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP))) |>
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

# 9. CUSTOM PARAMS --------------------------------------------------------
EOb_index = SpParamsAU$SpIndex[SpParamsAU$Name=="Eucalyptus obliqua"]
ERu_index = SpParamsAU$SpIndex[SpParamsAU$Name=="Eucalyptus rubida"]
ERa_index = SpParamsAU$SpIndex[SpParamsAU$Name=="Eucalyptus radiata"]
EOb_cohname = paste0("T1_", EOb_index)
ERu_cohname = paste0("T2_", ERu_index)
ERa_cohname = paste0("T3_", ERa_index)
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

Al2As_sp <- mean(SpParamsAU$Al2As[SpParamsAU$Genus=="Eucalyptus"], na.rm=T) # m2/m2
customParams$Al2As <- Al2As_sp

# 10. MEASURED DATA --------------------------------------------------------
# sapflow data, está en cm3 cm-2 h-1 y el timestep es 30 minutos, 
# cal dividir per 2 (per tenir flow en els 30 min),
# dividir per Al2As (m2/cm2), 
# multiplicar per 0.001 (per passar a de cm3 a dm3)
# Sumamos todo el día 
sapflow_factor <- 0.5/1000
transp_data_temp <- sapf_data |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  dplyr::mutate(AUS_WOM_Eru_Js_1 = sapflow_factor*AUS_WOM_Eru_Js_1/(Al2As_sp[1]/10000),
                AUS_WOM_Eru_Js_10 = sapflow_factor*AUS_WOM_Eru_Js_10/(Al2As_sp[1]/10000),
                AUS_WOM_Eob_Js_11 = sapflow_factor*AUS_WOM_Eob_Js_11/(Al2As_sp[1]/10000),
                AUS_WOM_Eru_Js_2 = sapflow_factor*AUS_WOM_Eru_Js_2/(Al2As_sp[1]/10000),
                AUS_WOM_Eob_Js_3 = sapflow_factor*AUS_WOM_Eob_Js_3/(Al2As_sp[1]/10000),
                AUS_WOM_Eru_Js_4 = sapflow_factor*AUS_WOM_Eru_Js_4/(Al2As_sp[1]/10000),
                AUS_WOM_Eob_Js_5 = sapflow_factor*AUS_WOM_Eob_Js_5/(Al2As_sp[1]/10000),
                AUS_WOM_Eob_Js_6 = sapflow_factor*AUS_WOM_Eob_Js_6/(Al2As_sp[1]/10000),
                AUS_WOM_Eob_Js_7 = sapflow_factor*AUS_WOM_Eob_Js_7/(Al2As_sp[1]/10000),
                AUS_WOM_Eru_Js_8 = sapflow_factor*AUS_WOM_Eru_Js_8/(Al2As_sp[1]/10000),
                AUS_WOM_Eob_Js_9 = sapflow_factor*AUS_WOM_Eob_Js_9/(Al2As_sp[1]/10000))|>
  dplyr::group_by(dates)  |>
  dplyr::summarise_at(dplyr::vars(dplyr::starts_with('AUS_WOM')),
                      dplyr::funs(sum(., na.rm = TRUE)))  |>
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('AUS_WOM')),
                   dplyr::funs(replace(., . == 0, NA)))

transp_data_temp2<-data.frame(dates = transp_data_temp$dates,
                              E_Eob = rowMeans(transp_data_temp[,c(4,6,7,8,10,12)], na.rm=TRUE),
                              E_Eru = rowMeans(transp_data_temp[,c(2,3,5,9,11)], na.rm=TRUE))
names(transp_data_temp2)[2] = paste0("E_", EOb_cohname)
names(transp_data_temp2)[3] = paste0("E_", ERu_cohname)


fluxData <- fluxnet_data |>
  dplyr::mutate(H_CORR = replace(H_CORR, H_CORR==-9999, NA),
                LE_CORR = replace(LE_CORR, LE_CORR==-9999, NA),
                GPP_NT_VUT_REF = replace(GPP_NT_VUT_REF, GPP_NT_VUT_REF==-9999, NA))|>
  dplyr::mutate(dates = as.Date(as.character(TIMESTAMP), format = "%Y%m%d")) |>
  dplyr::select(dates,H_CORR, LE_CORR, GPP_NT_VUT_REF) |>
  dplyr::mutate(H = (3600*24/1e6)*H_CORR,# From Wm2 to MJ/m2
                LE = (3600*24/1e6)*LE_CORR,# From Wm2 to MJ/m2
                GPP = GPP_NT_VUT_REF) |>
  dplyr::select(-H_CORR, -LE_CORR, -GPP_NT_VUT_REF)

measuredData <- env_data |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid')))  |>
  dplyr::select(dates, swc_shallow)  |>
  dplyr::group_by(dates)  |>
  dplyr::summarise(SWC = mean(swc_shallow, na.rm = TRUE))  |>
  dplyr::left_join(fluxData, by = 'dates') |>
  dplyr::left_join(transp_data_temp2, by = 'dates')

# measuredData <- env_data |>
#   dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid')))  |>
#   dplyr::select(dates, swc_shallow)  |>
#   dplyr::group_by(dates)  |>
#   dplyr::summarise(SWC = mean(swc_shallow, na.rm = TRUE))  |>
#   dplyr::left_join(fluxData, by = 'dates') |>
#   dplyr::left_join(transp_data_temp2, by = 'dates') 

# 11. SIMULATION/EVALUATION PERIOD ---------------------------------------------------
simulation_period <- seq(as.Date("2013-01-01"),as.Date("2015-11-01"), by="day")
evaluation_period <- seq(as.Date("2013-01-01"),as.Date("2015-11-01"), by="day")
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
             'No understory or secondary species considered.',
             '',
             'Species-level Huber value used for scaling',
             'Variables taken: LE_CORR and GPP_NT_VUT_REF')
)



# 13. SAVE DATA IN FOLDER -------------------------------------------------
folder_name <- file.path('Sites_data', 'WOMBAT')
if(!dir.exists(folder_name)) dir.create(folder_name)

write.table(siteData, file = file.path(folder_name, 'WOMBAT_siteData.txt'),
            row.names = FALSE, sep = '\t')

write.table(treeData, file = file.path(folder_name, 'WOMBAT_treeData.txt'),
            row.names = FALSE, sep = '\t')

write.table(shrubData, file = file.path(folder_name, 'WOMBAT_shrubData.txt'),
            row.names = FALSE, sep = '\t')

write.table(miscData, file = file.path(folder_name, 'WOMBAT_miscData.txt'),
            row.names = FALSE, sep = '\t')

write.table(meteoData, file = file.path(folder_name, 'WOMBAT_meteoData.txt'),
            row.names = FALSE, sep = '\t')

write.table(soilData, file = file.path(folder_name, 'WOMBAT_soilData.txt'),
            row.names = FALSE, sep = '\t')

write.table(terrainData, file = file.path(folder_name, 'WOMBAT_terrainData.txt'),
            row.names = FALSE, sep = '\t')

write.table(customParams, file = file.path(folder_name, 'WOMBAT_customParams.txt'),
            row.names = FALSE, sep = '\t')

write.table(measuredData, file = file.path(folder_name, 'WOMBAT_measuredData.txt'),
            row.names = FALSE, sep = '\t')

write.table(remarks, file = file.path(folder_name, 'WOMBAT_remarks.txt'),
            row.names = FALSE, sep = '\t')

