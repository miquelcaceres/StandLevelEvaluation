## Pinheiro da cruz script
library(medfate)
library(medfateutils)
library(meteoland)
library(dplyr)
library(lubridate)
library(readxl)

data("SpParamsES")

# 0. LOAD DATA and METADATA -----------------------------------------------
env_data <- read.csv('SourceData/Tables/Pinheiro/PRT_PIN_env_data.csv')
sapf_data <- read.csv('SourceData/Tables/Pinheiro/PRT_PIN_sapf_data.csv')
env_md <- read.csv('SourceData/Tables/Pinheiro/PRT_PIN_env_md.csv')
site_md <- read.csv('SourceData/Tables/Pinheiro/PRT_PIN_site_md.csv')
stand_md <- read.csv('SourceData/Tables/Pinheiro/PRT_PIN_stand_md.csv')
plant_md <- read.csv('SourceData/Tables/Pinheiro/PRT_PIN_plant_md.csv')
species_md <- read.csv('SourceData/Tables/Pinheiro/PRT_PIN_species_md.csv')


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
  Value = c("Pinheiro da Cruz",
            "Portugal",
            "PRT_PIN",
            "Katherine Rascher",
            "",
            "",
            round(site_md$si_lat,5),
            round(site_md$si_long,5),
            site_md$si_elev,
            0,
            0,
            "",
            "Sandy",
            round(site_md$si_mat,1),
            round(site_md$si_map),
            "Maritime pine stand invaded by Acacia longifolia in the understory",
            "",
            "Pinus pinaster, Acacia longifolia",
            "2001-2003",
            "10.1007/s10021-011-9453-7")
)

# 2. TERRAIN DATA ---------------------------------------------------------
terrainData <- data.frame(
  latitude = site_md$si_lat,
  elevation = site_md$si_elev,
  aspect = 0, # Flat
  slope = 0 # Flat
)

# 3. TREE DATA ----------------------------------------------------------
# Basal area 26.08 + 6.86
treeData <- data.frame(
  Species = c("Pinus pinaster","Acacia spp."),
  DBH = c(23.5, 3.1), # from paper
  Height = c(1140, 320),
  N =  10000*c(26.08/(pi*(23.5/2)^2), 6.86/(pi*(3.1/2)^2)), 
  Z50 = NA,  
  Z95 = NA,
  LAI = c(1.2,1)
)
f <-emptyforest()
f$treeData <- treeData
summary(f, SpParamsES)

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
  ID = 'PINHEIRO',
  SpParamsName = "SpParamsES",
  herbCover = 0, herbHeight = 0,
  Validation = 'global', Definitive = 'Yes'
)

# 7. SOIL DATA ------------------------------------------------------------
# coords_sf <- sf::st_sfc(sf::st_point(c(site_md$si_long,site_md$si_lat)), crs = 4326)
# soilData <- medfateutils::soilgridsParams(coords_sf,  c(300, 700, 1000, 2000))
soilData <- data.frame(
  widths =  c(300, 700, 1000, 2000),
  clay = c(16.56667, 18.30000, 18.00000, 18.00000),
  sand = c(58.43333, 56.85000, 58.10000,58.10000),
  om = c(4.126667, 2.350000, 2.090000, 0),
  bd = c(1.38, 1.51,1.52,1.550000),
  rfc = c(17.26667,18.25000,80,90)
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
                   Radiation = (sum((sw_in * 900), na.rm = TRUE)/(24*3600))) |>
  dplyr::mutate(Radiation = Radiation*3600*24/1000000) |>
  dplyr::mutate_at(dplyr::vars(2:5),
                   dplyr::funs(replace(., is.infinite(.), NA))) |>
  dplyr::mutate_at(dplyr::vars(2:5),
                   dplyr::funs(replace(., is.nan(.), NA))) 

meteoData$Precipitation <- 0

# 9. CUSTOM PARAMS --------------------------------------------------------
PI_index = SpParamsES$SpIndex[SpParamsES$Name=="Pinus pinaster"]
PI_cohname = paste0("T1_", PI_index)
AL_index = SpParamsES$SpIndex[SpParamsES$Name=="Acacia spp."]
AL_cohname = paste0("T2_", AL_index)
customParams <- data.frame(
  Species = c("Pinus pinaster", "Acacia spp."),
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

Al2As_sp = c(SpParamsES$Al2As[SpParamsES$Name=="Pinus pinaster"],
             SpParamsES$Al2As[SpParamsES$Name=="Acacia spp."])
customParams$Al2As = Al2As_sp

# 10. MEASURED DATA --------------------------------------------------------
# sapflow data, está en cm3 cm-2 h-1 y el timestep es 15 minutos, 
# dividir per Al2As (m2/cm2), 
# multiplicar per 0.001 (per passar a de cm3 a dm3)
# Sumamos todo el día 
sapflow_factor <- 0.25/1000
transp_data_temp <- sapf_data |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  dplyr::mutate(PRT_PIN_Alo_Js_1 = sapflow_factor*PRT_PIN_Alo_Js_1/(Al2As_sp[2]/10000),
                PRT_PIN_Alo_Js_2 = sapflow_factor*PRT_PIN_Alo_Js_2/(Al2As_sp[2]/10000),
                PRT_PIN_Alo_Js_3 = sapflow_factor*PRT_PIN_Alo_Js_3/(Al2As_sp[2]/10000),
                PRT_PIN_Alo_Js_4 = sapflow_factor*PRT_PIN_Alo_Js_4/(Al2As_sp[2]/10000),
                PRT_PIN_Alo_Js_5 = sapflow_factor*PRT_PIN_Alo_Js_5/(Al2As_sp[2]/10000),
                PRT_PIN_Alo_Js_6 = sapflow_factor*PRT_PIN_Alo_Js_6/(Al2As_sp[2]/10000),
                PRT_PIN_Alo_Js_7 = sapflow_factor*PRT_PIN_Alo_Js_7/(Al2As_sp[2]/10000),
                PRT_PIN_Alo_Js_8 = sapflow_factor*PRT_PIN_Alo_Js_8/(Al2As_sp[2]/10000),
                PRT_PIN_Alo_Js_9 = sapflow_factor*PRT_PIN_Alo_Js_9/(Al2As_sp[2]/10000),
                PRT_PIN_Alo_Js_10 = sapflow_factor*PRT_PIN_Alo_Js_10/(Al2As_sp[2]/10000),
                PRT_PIN_Ppi_Js_11 = sapflow_factor*PRT_PIN_Ppi_Js_11/(Al2As_sp[1]/10000),
                PRT_PIN_Ppi_Js_12 = sapflow_factor*PRT_PIN_Ppi_Js_12/(Al2As_sp[1]/10000),
                PRT_PIN_Ppi_Js_13 = sapflow_factor*PRT_PIN_Ppi_Js_13/(Al2As_sp[1]/10000),
                PRT_PIN_Ppi_Js_14 = sapflow_factor*PRT_PIN_Ppi_Js_14/(Al2As_sp[1]/10000),
                PRT_PIN_Ppi_Js_15 = sapflow_factor*PRT_PIN_Ppi_Js_15/(Al2As_sp[1]/10000),
                PRT_PIN_Ppi_Js_16 = sapflow_factor*PRT_PIN_Ppi_Js_16/(Al2As_sp[1]/10000),
                PRT_PIN_Ppi_Js_17 = sapflow_factor*PRT_PIN_Ppi_Js_17/(Al2As_sp[1]/10000),
                PRT_PIN_Ppi_Js_18 = sapflow_factor*PRT_PIN_Ppi_Js_18/(Al2As_sp[1]/10000),
                PRT_PIN_Ppi_Js_19 = sapflow_factor*PRT_PIN_Ppi_Js_19/(Al2As_sp[1]/10000),
                PRT_PIN_Ppi_Js_20 = sapflow_factor*PRT_PIN_Ppi_Js_20/(Al2As_sp[1]/10000))|>
  dplyr::group_by(dates)  |>
  dplyr::summarise_at(dplyr::vars(dplyr::starts_with('PRT_PIN')),
                      dplyr::funs(sum(., na.rm = TRUE)))  |>
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('PRT_PIN')),
                   dplyr::funs(replace(., . == 0, NA)))

transp_data_temp2<-data.frame(dates = transp_data_temp$dates,
                              E_Ppi = rowMeans(transp_data_temp[,12:21], na.rm=TRUE),
                              E_Alo = rowMeans(transp_data_temp[,2:11], na.rm=TRUE))
names(transp_data_temp2)[2] = paste0("E_", PI_cohname)
names(transp_data_temp2)[3] = paste0("E_", AL_cohname)

# fluxData <- fluxnet_data |>
#   dplyr::mutate(LE_CORR = replace(LE_CORR, LE_CORR==-9999, NA),
#                 GPP_NT_VUT_REF = replace(GPP_NT_VUT_REF, GPP_NT_VUT_REF==-9999, NA))|>
#   dplyr::mutate(dates = as.Date(as.character(TIMESTAMP), format = "%Y%m%d")) |>
#   dplyr::select(dates, LE_CORR, GPP_NT_VUT_REF) |>
#   dplyr::mutate(LE = (3600*24/1e6)*LE_CORR,# From Wm2 to MJ/m2
#                 GPP = GPP_NT_VUT_REF) |>
#   dplyr::select(-LE_CORR, -GPP_NT_VUT_REF)

measuredData <- env_data |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid')))  |>
  dplyr::select(dates, swc_shallow)  |>
  dplyr::group_by(dates)  |>
  dplyr::summarise(SWC = mean(swc_shallow, na.rm = TRUE))  |>
  dplyr::left_join(transp_data_temp2, by = 'dates')

# 11. EVALUATION PERIOD ---------------------------------------------------
evaluation_period <- seq(as.Date("2007-09-30"),as.Date("2008-07-31"), by="day")
measuredData <- measuredData |> filter(dates %in% evaluation_period)
meteoData <- meteoData |> filter(dates %in% evaluation_period)
row.names(meteoData) <- NULL
row.names(measuredData) <- NULL

# 12. REMARKS -------------------------------------------------------------
remarks <- data.frame(
  Title = c('Soil',
            'Vegetation',
            'Weather'),
  Remark = c('Taken from SoilGrids',
             'No understory with 0% herbaceous cover',
             'Precipitation is missing')
)

# 13. SAVE DATA IN FOLDER -------------------------------------------------
folder_name <- file.path('Sites_data', 'PINHEIRO')
if(!dir.exists(folder_name)) dir.create(folder_name)

write.table(siteData, file = file.path(folder_name, 'PINHEIRO_siteData.txt'),
            row.names = FALSE, sep = '\t')
write.table(treeData, file = file.path(folder_name, 'PINHEIRO_treeData.txt'),
            row.names = FALSE, sep = '\t')
write.table(shrubData, file = file.path(folder_name, 'PINHEIRO_shrubData.txt'),
             row.names = FALSE, sep = '\t')
write.table(miscData, file = file.path(folder_name, 'PINHEIRO_miscData.txt'),
            row.names = FALSE, sep = '\t')
write.table(meteoData, file = file.path(folder_name, 'PINHEIRO_meteoData.txt'),
            row.names = FALSE, sep = '\t')
write.table(soilData, file = file.path(folder_name, 'PINHEIRO_soilData.txt'),
            row.names = FALSE, sep = '\t')

write.table(terrainData, file = file.path(folder_name, 'PINHEIRO_terrainData.txt'),
            row.names = FALSE, sep = '\t')

write.table(customParams, file = file.path(folder_name, 'PINHEIRO_customParams.txt'),
            row.names = FALSE, sep = '\t')

write.table(measuredData, file = file.path(folder_name, 'PINHEIRO_measuredData.txt'),
            row.names = FALSE, sep = '\t')

write.table(remarks, file = file.path(folder_name, 'PINHEIRO_remarks.txt'),
            row.names = FALSE, sep = '\t')


