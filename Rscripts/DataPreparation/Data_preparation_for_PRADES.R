## Prades data script
library(medfate)
library(medfateutils)
library(dplyr)
library(lubridate)
library(meteoland)
library(readxl)

data("SpParamsES")

# 0. LOAD DATA and METADATA -----------------------------------------------
env_data <- read.csv('SourceData/Tables/Prades/ESP_TIL_MIX_env_data.csv')
sapf_data <- read.csv('SourceData/Tables/Prades/ESP_TIL_MIX_sapf_data.csv')
env_md <- read.csv('SourceData/Tables/Prades/ESP_TIL_MIX_env_md.csv')
site_md <- read.csv('SourceData/Tables/Prades/ESP_TIL_MIX_site_md.csv')
stand_md <- read.csv('SourceData/Tables/Prades/ESP_TIL_MIX_stand_md.csv')
plant_md <- read.csv('SourceData/Tables/Prades/ESP_TIL_MIX_plant_md.csv')
prades_wp <- read.table("SourceData/Tables/Prades/TitllarWaterPotentials.csv",
                        sep = ",", header = TRUE)


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
  Value = c("Prades (Tillar valley)",
            "Spain",
            "ESP_TIL_MIX",
            "Rafael Poyatos (CREAF)",
            "",
            "",
            round(site_md$si_lat,6),
            round(site_md$si_long,6),
            site_md$si_elev,
            35,
            8.53,
            "Fractured schist",
            "Clay loam",
            round(site_md$si_mat,1),
            site_md$si_map,
            "Mixed forest with P. sylvestris (overstory) Q. ilex (midstory)",
            stand_md$st_lai,
            "10.1111/nph.12278",
            "Quercus ilex, Pinus sylvestris",
            "SpParamsES",
            "2010-2013",
            "2010-2013")
)

# 2. TERRAIN DATA ---------------------------------------------------------
terrainData <- data.frame(
  latitude = site_md$si_lat,
  elevation = site_md$si_elev,
  aspect = 8.53,
  slope = 35
)

# 3. TREE DATA ------------------------------------------------------------
treeData <- data.frame(
  Species = c("Pinus sylvestris", "Quercus ilex"),
  DBH = c(27.70, 8.40),
  Height = c(1424, 500),
  N = c(257, 2913),
  Z50 = c(300,529),
  Z95 = c(1200,2287),
  LAI = c(0.58, 2.69)
)

f = emptyforest()
f$treeData = treeData
vprofile_leafAreaDensity(f, SpParamsES, draw=T)
vprofile_rootDistribution(f, SpParams = SpParamsES)
summary(f, SpParamsES)

# 4. SHRUB DATA -----------------------------------------------------------
# there is no shrub info
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
  ID = 'PRADES',
  SpParamsName = "SpParamsES",
  herbCover = 10, herbHeight = 20,
  Validation = 'global_transp', Definitive = 'Yes'
)


# 7. SOIL DATA ------------------------------------------------------------
# aumento la roca en la ultima capa 
soilData <- data.frame(
  widths = c(300, 700, 1000, 2500),
  clay = c(21, 19, 19, 19),
  sand = c(47, 48, 48, 48),
  om = rep(4, 3,1,1), #as in CB
  bd = rep(1.5, 4),
  rfc = c(45, 70, 85, 90)
)
sum(soil_waterExtractable(soil(soilData, VG_PTF = "Toth"), model="VG", minPsi = -4))


# 8. METEO DATA -----------------------------------------------------------
meteoData <- env_data |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  dplyr::group_by(dates) |>
  dplyr::summarise(MinTemperature = min(ta, na.rm = TRUE),
                   MaxTemperature = max(ta, na.rm = TRUE),
                   MinRelativeHumidity = min(rh, na.rm = TRUE),
                   MaxRelativeHumidity = max(rh, na.rm = TRUE),
                   WindSpeed = mean(ws, na.rm = TRUE),
                   Radiation = (sum((sw_in * 900), na.rm = TRUE)/(24*3600)), # W/m2, a W/m2 en el día
                   Precipitation = sum(precip, na.rm = TRUE)) |>
  dplyr::mutate(Radiation = Radiation*3600*24/1000000)|> # w/m2 to MJ/m2/day
  dplyr::mutate_at(dplyr::vars(dplyr::ends_with('Humidity')),
                   dplyr::funs(replace(., . > 100, 100))) |>
  dplyr::mutate_at(dplyr::vars(dplyr::ends_with('Speed')),
                   dplyr::funs(replace(., is.nan(.), NA))) 


# 9. CUSTOM PARAMS --------------------------------------------------------
QI_index = SpParamsES$SpIndex[SpParamsES$Name=="Quercus ilex"]
PS_index = SpParamsES$SpIndex[SpParamsES$Name=="Pinus sylvestris"]
PS_cohname = paste0("T1_", PS_index)
QI_cohname = paste0("T2_", QI_index)
ps <- 1
qi <- 2
customParams <- data.frame(
  Species = c("Pinus sylvestris","Quercus ilex"),
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

customParams$VCleaf_kmax[ps] <- 4.0
customParams$VCleaf_kmax[qi] <- 2.63

# VC root - P. sylvestris
slope <- 35.51402
P50 <- -1.65
P88 <- P50 + log((100.0/88.0)-1.0)*(25.0/slope)
P12 <- P50 + log((100.0/12.0)-1.0)*(25.0/slope)
customParams$VCroot_P12[ps] <- P12
customParams$VCroot_P50[ps] <- P50
customParams$VCroot_P88[ps] <- P88
customParams$VCroot_slope[ps] <- slope

# VC stem - P. sylvestris
slope <- 22.57
P50 <- -3.2
P88 <- P50 + log((100.0/88.0)-1.0)*(25.0/slope)
P12 <- P50 + log((100.0/12.0)-1.0)*(25.0/slope)
customParams$VCstem_P12[ps] <- P12
customParams$VCstem_P50[ps] <- P50
customParams$VCstem_P88[ps] <- P88
customParams$VCstem_slope[ps] <- slope

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
# VC root  - Q. ilex
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
customParams$LeafEPS[ps] <- 5.31 
customParams$LeafPI0[ps] <- -1.5
customParams$LeafAF[ps] <- 0.6

customParams$StemEPS[qi] <- 15 
customParams$StemPI0[qi] <- -2.5
customParams$StemAF[qi] <- 0.4
customParams$StemEPS[ps] <- 5
customParams$StemPI0[ps] <- -1.65
customParams$StemAF[ps] <- 0.4

customParams$Gswmax[qi] <- 0.200 
customParams$Gswmin[qi] <- 0.002 
customParams$Gswmax[ps] <- 0.180 
customParams$Gswmin[ps] <- 0.001 


# Qilex/P.halepensis
customParams$Gs_slope[qi] <- (88.0 - 12.0)/(2.7 - 1);
customParams$Gs_P50[qi] <- -1.0 + log(0.12/0.88)/(customParams$Gs_slope[qi]/25)
customParams$Gs_slope[ps] <- (88.0 - 12.0)/(2.14 - 1.36);
customParams$Gs_P50[ps] <- -1.36 + log(0.12/0.88)/(customParams$Gs_slope[ps]/25)

As2Al = plant_md[['pl_sapw_area']]/plant_md[['pl_leaf_area']] # cm2/m2
Al2As_sp = c(mean(10000/As2Al[plant_md$pl_species=="Pinus sylvestris"]), mean(10000/As2Al[plant_md$pl_species=="Quercus ilex"]))
customParams$Al2As = Al2As_sp

# 10. MEASURED DATA --------------------------------------------------------
# sapflow data, está en cm3 cm-2 h-1 of leaf area, y el timestep es 15 minutos, así que tenemos que
# multiplicar por 15*60 segundos para los l/m-2 en el timestep.
# Agrego los datos de sapflow por día, promedio todos los árboles 
transp_data_temp <- sapf_data |>
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('ESP_TIL_MIX')),
                   dplyr::funs(.*0.25*1e4/1e3))|>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  dplyr::group_by(dates) |>
  dplyr::summarise_at(dplyr::vars(dplyr::starts_with('ESP_TIL_MIX')),
                      dplyr::funs(sum(., na.rm = TRUE))) |>
  # remove the zeroes generated previously
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('ESP_TIL_MIX')),
                   dplyr::funs(replace(., . == 0, NA)))
transp_data_temp$E_Ps <- rowMeans(transp_data_temp[,2:23], na.rm=TRUE)
transp_data_temp$E_Qi <- rowMeans(transp_data_temp[,24:33], na.rm=TRUE)
transp_data_temp2 <- transp_data_temp |>
  dplyr::select(dates, E_Ps, E_Qi)
names(transp_data_temp2)[2] <- paste0("E_",PS_cohname)
names(transp_data_temp2)[3] <- paste0("E_",QI_cohname)


wpData_PS <- prades_wp |>
  dplyr::filter(Species=="Pinus sylvestris" & DefoliationClass=="ND") |>
  dplyr::mutate(dates = as.Date(Timestamp2)) |>
  dplyr::select(dates, WaterPotential.MD, WaterPotential.PD) |>
  group_by(dates) |>
  summarize(PD_PS = mean(WaterPotential.PD, na.rm=T), PD_PS_err = sd(WaterPotential.PD, na.rm=T),
            MD_PS = mean(WaterPotential.MD, na.rm=T), MD_PS_err = sd(WaterPotential.MD, na.rm=T))
names(wpData_PS)[2:5] = c(paste0("PD_", PS_cohname),
                          paste0("PD_", PS_cohname, "_err"),
                          paste0("MD_", PS_cohname),
                          paste0("MD_", PS_cohname, "_err"))

wpData_QI <- prades_wp |>
  dplyr::filter(Species=="Quercus ilex") |>
  dplyr::mutate(dates = as.Date(Timestamp2)) |>
  dplyr::select(dates, WaterPotential.MD, WaterPotential.PD) |>
  group_by(dates) |>
  summarize(PD_PS = mean(WaterPotential.PD, na.rm=T), PD_PS_err = sd(WaterPotential.PD, na.rm=T),
            MD_PS = mean(WaterPotential.MD, na.rm=T), MD_PS_err = sd(WaterPotential.MD, na.rm=T))
names(wpData_QI)[2:5] = c(paste0("PD_", QI_cohname),
                          paste0("PD_", QI_cohname, "_err"),
                          paste0("MD_", QI_cohname),
                          paste0("MD_", QI_cohname, "_err"))

names(measuredData)[5:14] = c(paste0("E_",c(PS_cohname, QI_cohname)),
                             paste0("PD_", PS_cohname),
                             paste0("PD_", PS_cohname, "_err"),
                             paste0("MD_", PS_cohname),
                             paste0("MD_", PS_cohname, "_err"),
                             paste0("PD_", QI_cohname),
                             paste0("PD_", QI_cohname, "_err"),
                             paste0("MD_", QI_cohname),
                             paste0("MD_", QI_cohname, "_err"))


measuredData <- env_data |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  dplyr::select(dates, swc_shallow) |>
  dplyr::group_by(dates) |>
  dplyr::summarise(SWC = mean(swc_shallow, na.rm = TRUE)) |>
  dplyr::left_join(transp_data_temp2, by = 'dates') |>
  dplyr::left_join(wpData_PS, by = 'dates') |>
  dplyr::left_join(wpData_QI, by = 'dates')


# 11. SIMULATION/EVALUATION PERIOD ---------------------------------------------------
simulation_period <- seq(as.Date("2010-04-29"),as.Date("2013-11-01"), by="day")
evaluation_period <- seq(as.Date("2010-04-29"),as.Date("2013-11-01"), by="day")
meteoData <- meteoData |> filter(dates %in% simulation_period)
measuredData <- measuredData |> filter(dates %in% evaluation_period)

# 12. REMARKS -------------------------------------------------------------
remarks <- data.frame(
  Title = c('Soil',
            'Vegetation',
            'Sapflow'),
  Remark = c('Additional rocky layer considered',
             'Understory not considered',
             'No scaling required (already per leaf area)')
)

# 13. SAVE DATA IN FOLDER -------------------------------------------------
folder_name <- file.path('Sites_data', 'PRADES')
write.table(siteData, file = file.path(folder_name, 'PRADES_siteData.txt'),
            row.names = FALSE, sep = '\t')
write.table(treeData, file = file.path(folder_name, 'PRADES_treeData.txt'),
            row.names = FALSE, sep = '\t')
write.table(miscData, file = file.path(folder_name, 'PRADES_miscData.txt'),
            row.names = FALSE, sep = '\t')
write.table(meteoData, file = file.path(folder_name, 'PRADES_meteoData.txt'),
            row.names = FALSE, sep = '\t')
write.table(soilData, file = file.path(folder_name, 'PRADES_soilData.txt'),
            row.names = FALSE, sep = '\t')
write.table(terrainData, file = file.path(folder_name, 'PRADES_terrainData.txt'),
            row.names = FALSE, sep = '\t')
write.table(customParams, file = file.path(folder_name, 'PRADES_customParams.txt'),
            row.names = FALSE, sep = '\t')
write.table(measuredData, file = file.path(folder_name, 'PRADES_measuredData.txt'),
            row.names = FALSE, sep = '\t')
write.table(remarks, file = file.path(folder_name, 'PRADES_remarks.txt'),
            row.names = FALSE, sep = '\t')

