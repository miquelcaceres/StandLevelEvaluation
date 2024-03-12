## Mitra Q. ilex data script
library(medfate)
library(meteoland)
library(dplyr)
library(lubridate)
library(readxl)

data("SpParamsMED")

# 0. LOAD DATA and METADATA -----------------------------------------------
env_data <- read.csv('SourceData/Tables/Mitra/PRT_MIT_env_data.csv')
sapf_data <- read.csv('SourceData/Tables/Mitra/PRT_MIT_sapf_data.csv')
env_md <- read.csv('SourceData/Tables/Mitra/PRT_MIT_env_md.csv')
site_md <- read.csv('SourceData/Tables/Mitra/PRT_MIT_site_md.csv')
stand_md <- read.csv('SourceData/Tables/Mitra/PRT_MIT_stand_md.csv')
plant_md <- read.csv('SourceData/Tables/Mitra/PRT_MIT_plant_md.csv')


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
  Value = c("Mitra II",
            "Portugal",
            "PRT_MIT",
            "Teresa David (INIAV IP)",
            "PT-Mi1",
            "Joao Santos Pereira",
            round(site_md$si_lat,5),
            round(site_md$si_long,5),
            site_md$si_elev,
            0,
            0,
            "Granite",
            "Sand",
            round(site_md$si_mat,1),
            round(site_md$si_map),
            "Evergreen forest dominated by Quercus ilex subsp. rotundifolia",
            stand_md$st_lai,
            "Quercus ilex",
            "2001-2003",
            "10.1093/treephys/27.6.793")
)

# 2. TERRAIN DATA ---------------------------------------------------------
terrainData <- data.frame(
  latitude = site_md$si_lat,
  elevation = site_md$si_elev,
  aspect = 0, # Flat
  slope = 0 # Flat
)

# 3. TREE DATA ----------------------------------------------------------
# Basal area 3.6
treeData <- data.frame(
  Species = "Quercus ilex",
  DBH = mean(plant_md[['pl_dbh']]),
  Height = stand_md$st_height*100,
  N =  stand_md$st_density, 
  Z50 = 529,  
  Z95 = 2287,
  LAI = stand_md$st_lai
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
  ID = 'MITRA',
  SpParamsName = "SpParamsES",
  herbCover = 100, herbHeight = 15,
  Validation = 'global', Definitive = 'Yes'
)

# 7. SOIL DATA ------------------------------------------------------------
# coords_sf <- sf::st_sfc(sf::st_point(c(site_md$si_long,site_md$si_lat)), crs = 4326)
# soilData <- medfateutils::soilgridsParams(coords_sf,  c(300, 700, 1000, 2000))
soilData <- data.frame(
  widths =  c(300, 700, 1000, 2000),
  clay = c(14.86667, 15.05000, 14.70000, 14.70000),
  sand = c(58.4, 63.8, 64.1,64.1),
  om = c(2.92, 1.10, 0.87, 0),
  bd = c(1.463333, 1.535000,1.550000,1.550000),
  rfc = c(20.46667,21.65000,80,90)
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
                   Radiation = (sum((sw_in * 3600), na.rm = TRUE)/(24*3600)), # W/m2, a W/m2 en el día
                   Precipitation = sum(precip, na.rm = TRUE),
                   WindSpeed = mean(ws, na.rm = TRUE)) |>
  dplyr::mutate(Radiation = Radiation*3600*24/1000000) |>
  dplyr::mutate_at(dplyr::vars(2:5),
                   dplyr::funs(replace(., is.infinite(.), NA))) |>
  dplyr::mutate_at(dplyr::vars(2:5),
                   dplyr::funs(replace(., is.nan(.), NA))) 

# 9. CUSTOM PARAMS --------------------------------------------------------
QI_index = SpParamsES$SpIndex[SpParamsES$Name=="Quercus ilex"]
QI_cohname = paste0("T1_", QI_index)
qi <- 1
customParams <- data.frame(
  Species = c("Quercus ilex"),
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
customParams$VCleaf_kmax[qi] <- 2.63
customParams$SLA[qi] <- 4.55
customParams$Kmax_stemxylem[qi] <- 0.20

# VC stem - Q. ilex
slope <- 30
P50 <- -6.4
P88 <- P50 + log((100.0/88.0)-1.0)*(25.0/slope)
P12 <- P50 + log((100.0/12.0)-1.0)*(25.0/slope)
customParams$VCstem_P12[qi] <- P12
customParams$VCstem_P50[qi] <- P50
customParams$VCstem_P88[qi] <- P88
customParams$VCstem_slope[qi] <- slope
# VC leaf - Q. ilex
slope <- 40
P50 <- -5.25
P88 <- P50 + log((100.0/88.0)-1.0)*(25.0/slope)
P12 <- P50 + log((100.0/12.0)-1.0)*(25.0/slope)
customParams$VCleaf_P12[qi] <- P12
customParams$VCleaf_P50[qi] <- P50
customParams$VCleaf_P88[qi] <- P88
customParams$VCleaf_slope[qi] <- slope
# VC root - Q. ilex
slope <- 40
P50 <- -3.25
P88 <- P50 + log((100.0/88.0)-1.0)*(25.0/slope)
P12 <- P50 + log((100.0/12.0)-1.0)*(25.0/slope)
customParams$VCroot_P12[qi] <- P12
customParams$VCroot_P50[qi] <- P50
customParams$VCroot_P88[qi] <- P88
customParams$VCroot_slope[qi] <- slope

customParams$LeafEPS[qi] <- 15 
customParams$LeafPI0[qi] <- -2.5
customParams$LeafAF[qi] <- 0.4

customParams$StemEPS[qi] <- 15 
customParams$StemPI0[qi] <- -2.5
customParams$StemAF[qi] <- 0.4

customParams$Gswmax[qi] <- 0.200 
customParams$Gswmin[qi] <- 0.002 

# P12_gs	-1
# P88_gs	-2.7
customParams$Gs_slope[qi] <- (88.0 - 12.0)/(2.7 - 1);
customParams$Gs_P50[qi] <- -1.0 + log(0.12/0.88)/(customParams$Gs_slope[qi]/25)

Al2As_sp = 1540.671
customParams$Al2As[qi] = Al2As_sp

# 10. MEASURED DATA --------------------------------------------------------
# sapflow data, está en cm3 cm-2 h-1 y el timestep es 60 minutos, 
# dividir per Al2As (m2/cm2), 
# multiplicar per 0.001 (per passar a de cm3 a dm3)
# Sumamos todo el día 
sapflow_factor <- 1/1000
EQI_name = paste0("E_",QI_cohname)
transp_data_temp <- sapf_data |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  dplyr::mutate(PRT_MIT_Qro_Js_1 = sapflow_factor*PRT_MIT_Qro_Js_1/(Al2As_sp[1]/10000),
                PRT_MIT_Qro_Js_2 = sapflow_factor*PRT_MIT_Qro_Js_2/(Al2As_sp[1]/10000),
                PRT_MIT_Qro_Js_3 = sapflow_factor*PRT_MIT_Qro_Js_3/(Al2As_sp[1]/10000),
                PRT_MIT_Qro_Js_4 = sapflow_factor*PRT_MIT_Qro_Js_4/(Al2As_sp[1]/10000))|>
  dplyr::group_by(dates)  |>
  dplyr::summarise_at(dplyr::vars(dplyr::starts_with('PRT_MIT')),
                      dplyr::funs(sum(., na.rm = TRUE)))  |>
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('PRT_MIT')),
                   dplyr::funs(replace(., . == 0, NA)))


transp_data_temp2<-data.frame(dates = transp_data_temp$dates,
                              E_QI = rowMeans(transp_data_temp[,-1], na.rm=TRUE))
names(transp_data_temp2)[2] = paste0("E_", QI_cohname)

# fluxData <- fluxnet_data |>
#   dplyr::mutate(LE_CORR = replace(LE_CORR, LE_CORR==-9999, NA),
#                 GPP_NT_VUT_REF = replace(GPP_NT_VUT_REF, GPP_NT_VUT_REF==-9999, NA))|>
#   dplyr::mutate(dates = as.Date(as.character(TIMESTAMP), format = "%Y%m%d")) |>
#   dplyr::select(dates, LE_CORR, GPP_NT_VUT_REF) |>
#   dplyr::mutate(LE = (3600*24/1e6)*LE_CORR,# From Wm2 to MJ/m2
#                 GPP = GPP_NT_VUT_REF) |>
#   dplyr::select(-LE_CORR, -GPP_NT_VUT_REF)

measuredData <- transp_data_temp2


# 11. EVALUATION PERIOD ---------------------------------------------------
evaluation_period <- seq(as.Date("2001-01-01"),as.Date("2003-12-31"), by="day")
measuredData <- measuredData |> filter(dates %in% evaluation_period)
meteoData <- meteoData |> filter(dates %in% evaluation_period)
row.names(meteoData) <- NULL
row.names(measuredData) <- NULL

# 12. REMARKS -------------------------------------------------------------
remarks <- data.frame(
  Title = c('Soil',
            'Vegetation'),
  Remark = c('Adjusted theta_res and theta_sat',
             'No understory but 100% herbaceous cover')
)

# 13. SAVE DATA IN FOLDER -------------------------------------------------
folder_name <- file.path('Sites_data', 'MITRA')
if(!dir.exists(folder_name)) dir.create(folder_name)

write.table(siteData, file = file.path(folder_name, 'MITRA_siteData.txt'),
            row.names = FALSE, sep = '\t')
write.table(treeData, file = file.path(folder_name, 'MITRA_treeData.txt'),
            row.names = FALSE, sep = '\t')
write.table(shrubData, file = file.path(folder_name, 'MITRA_shrubData.txt'),
             row.names = FALSE, sep = '\t')
write.table(miscData, file = file.path(folder_name, 'MITRA_miscData.txt'),
            row.names = FALSE, sep = '\t')
write.table(meteoData, file = file.path(folder_name, 'MITRA_meteoData.txt'),
            row.names = FALSE, sep = '\t')
write.table(soilData, file = file.path(folder_name, 'MITRA_soilData.txt'),
            row.names = FALSE, sep = '\t')

write.table(terrainData, file = file.path(folder_name, 'MITRA_terrainData.txt'),
            row.names = FALSE, sep = '\t')

write.table(customParams, file = file.path(folder_name, 'MITRA_customParams.txt'),
            row.names = FALSE, sep = '\t')

write.table(measuredData, file = file.path(folder_name, 'MITRA_measuredData.txt'),
            row.names = FALSE, sep = '\t')

write.table(remarks, file = file.path(folder_name, 'MITRA_remarks.txt'),
            row.names = FALSE, sep = '\t')


