## Prades data script
library(medfate)
library(dplyr)
library(lubridate)
library(meteoland)
library(readxl)

data("SpParamsMED")

# 0. LOAD DATA and METADATA -----------------------------------------------
env_data <- read.csv('SourceData/Tables/Prades/ESP_TIL_MIX_env_data.csv')
sapf_data <- read.csv('SourceData/Tables/Prades/ESP_TIL_MIX_sapf_data.csv')
plant_md <- read.csv('SourceData/Tables/Prades/ESP_TIL_MIX_plant_md.csv')


# 0. SITE INFORMATION -----------------------------------------------------
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
  Value = c("Prades (Tillar valley)",
            "Spain",
            "ESP_TIL_MIX",
            "Rafael Poyatos (CREAF)",
            41.33,
            1.02,
            1018,
            35,
            8.53,
            "Fractured schist",
            "Clay loam",
            11.3,
            664,
            "Mixed forest with P. sylvestris (overstory) Q. ilex (midstory)",
            3.27,
            "Quercus ilex, Pinus sylvestris",
            "10.1111/nph.12278")
)

# 1. TREE DATA ------------------------------------------------------------
QI_index = SpParamsMED$SpIndex[SpParamsMED$Name=="Quercus ilex"]
PS_index = SpParamsMED$SpIndex[SpParamsMED$Name=="Pinus sylvestris"]

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
vprofile_leafAreaDensity(f, SpParamsMED, draw=T)
vprofile_rootDistribution(f, SpParams = SpParamsMED)
summary(f, SpParamsMED)

# 2. SHRUB DATA -----------------------------------------------------------
# there is no shrub info


# 3. SEED DATA ------------------------------------------------------------
# there is no seed info


# 4. MISC DATA ------------------------------------------------------------
miscData <- data.frame(
  ID = 'PRADES',
  herbCover = 10, herbHeight = 20,
  Validation = 'global_transp', Definitive = 'Yes'
)


# 5. SOIL DATA ------------------------------------------------------------
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

# 6. TERRAIN DATA ---------------------------------------------------------
terrainData <- data.frame(
  latitude = 41.33,
  elevation = 1018,
  aspect = 8.53,
  slope = 35
)

# 7. METEO DATA -----------------------------------------------------------

meteoData <- env_data %>%
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) %>%
  dplyr::group_by(dates) %>%
  dplyr::summarise(MinTemperature = min(ta, na.rm = TRUE),
                   MaxTemperature = max(ta, na.rm = TRUE),
                   MinRelativeHumidity = min(rh, na.rm = TRUE),
                   MaxRelativeHumidity = max(rh, na.rm = TRUE),
                   WindSpeed = mean(ws, na.rm = TRUE),
                   Radiation = (sum((sw_in * 900), na.rm = TRUE)/(24*3600)), # W/m2, a W/m2 en el día
                   Precipitation = sum(precip, na.rm = TRUE)) %>%
  dplyr::mutate(Radiation = Radiation*3600*24/1000000) %>% # w/m2 to MJ/m2/day
  dplyr::mutate_at(dplyr::vars(dplyr::ends_with('Humidity')),
                   dplyr::funs(replace(., . > 100, 100))) %>%
  dplyr::mutate_at(dplyr::vars(dplyr::ends_with('Speed')),
                   dplyr::funs(replace(., is.nan(.), NA))) %>%
  as.data.frame()


# 8. CUSTOM PARAMS --------------------------------------------------------
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


# 9. MEASURED DATA --------------------------------------------------------
# sapflow data, está en mm3/mm2 s, y el timestep es 15 minutos, así que tenemos que
# multiplicar por 15*60 segundos para los mm3/mm2 en el timestep, por el As2Al de cada árbol *100 (mm2 to cm2 sapwood area) y 
# dividir entre 1000000 para tenerlo en L.
# Agrego los datos de sapflow por día, sumo todos los árboles y luego multiplico
# por LAI y divido por n = numero de arboles medidos
As2Al = plant_md[['pl_sapw_area']]/plant_md[['pl_leaf_area']] # cm2/m2

Al2As_sp = c(mean(10000/As2Al[c(1:22)]), mean(10000/As2Al[c(23:28)]))
customParams$Al2As = Al2As_sp

transp_data_temp <- sapf_data %>%
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('ESP_TIL_MIX')),
                   dplyr::funs(.*60*15*100/1000000)) %>%
  dplyr::mutate(ESP_TIL_MIX_Psy_Js_1 = (ESP_TIL_MIX_Psy_Js_1*As2Al[1]),
                ESP_TIL_MIX_Psy_Js_2 = (ESP_TIL_MIX_Psy_Js_2*As2Al[2]),
                ESP_TIL_MIX_Psy_Js_3 = (ESP_TIL_MIX_Psy_Js_3*As2Al[3]),
                ESP_TIL_MIX_Psy_Js_4 = (ESP_TIL_MIX_Psy_Js_4*As2Al[4]),
                ESP_TIL_MIX_Psy_Js_5 = (ESP_TIL_MIX_Psy_Js_5*As2Al[5]),
                ESP_TIL_MIX_Psy_Js_6 = (ESP_TIL_MIX_Psy_Js_6*As2Al[6]),
                ESP_TIL_MIX_Psy_Js_7 = (ESP_TIL_MIX_Psy_Js_7*As2Al[7]),
                ESP_TIL_MIX_Psy_Js_8 = (ESP_TIL_MIX_Psy_Js_8*As2Al[8]),
                ESP_TIL_MIX_Psy_Js_9 = (ESP_TIL_MIX_Psy_Js_9*As2Al[9]),
                ESP_TIL_MIX_Psy_Js_10 = (ESP_TIL_MIX_Psy_Js_10*As2Al[10]),
                ESP_TIL_MIX_Psy_Js_11 = (ESP_TIL_MIX_Psy_Js_11*As2Al[11]),
                ESP_TIL_MIX_Psy_Js_12 = (ESP_TIL_MIX_Psy_Js_12*As2Al[12]),
                ESP_TIL_MIX_Psy_Js_13 = (ESP_TIL_MIX_Psy_Js_13*As2Al[13]),
                ESP_TIL_MIX_Psy_Js_14 = (ESP_TIL_MIX_Psy_Js_14*As2Al[14]),
                ESP_TIL_MIX_Psy_Js_15 = (ESP_TIL_MIX_Psy_Js_15*As2Al[15]),
                ESP_TIL_MIX_Psy_Js_16 = (ESP_TIL_MIX_Psy_Js_16*As2Al[16]),
                ESP_TIL_MIX_Psy_Js_17 = (ESP_TIL_MIX_Psy_Js_17*As2Al[17]),
                ESP_TIL_MIX_Psy_Js_18 = (ESP_TIL_MIX_Psy_Js_18*As2Al[18]),
                ESP_TIL_MIX_Psy_Js_19 = (ESP_TIL_MIX_Psy_Js_19*As2Al[19]),
                ESP_TIL_MIX_Psy_Js_20 = (ESP_TIL_MIX_Psy_Js_20*As2Al[20]),
                ESP_TIL_MIX_Psy_Js_21 = (ESP_TIL_MIX_Psy_Js_21*As2Al[21]),
                ESP_TIL_MIX_Psy_Js_22 = (ESP_TIL_MIX_Psy_Js_22*As2Al[22]),
                ESP_TIL_MIX_Qil_Js_23 = (ESP_TIL_MIX_Qil_Js_23*As2Al[23]),
                ESP_TIL_MIX_Qil_Js_24 = (ESP_TIL_MIX_Qil_Js_24*As2Al[24]),
                ESP_TIL_MIX_Qil_Js_25 = (ESP_TIL_MIX_Qil_Js_25*As2Al[25]),
                ESP_TIL_MIX_Qil_Js_26 = (ESP_TIL_MIX_Qil_Js_26*As2Al[26]),
                ESP_TIL_MIX_Qil_Js_27 = (ESP_TIL_MIX_Qil_Js_27*As2Al[27]),
                ESP_TIL_MIX_Qil_Js_28 = (ESP_TIL_MIX_Qil_Js_28*As2Al[28]),
                ESP_TIL_MIX_Qil_Js_29 = (ESP_TIL_MIX_Qil_Js_29*As2Al[29]),
                ESP_TIL_MIX_Qil_Js_30 = (ESP_TIL_MIX_Qil_Js_30*As2Al[30]),
                ESP_TIL_MIX_Qil_Js_31 = (ESP_TIL_MIX_Qil_Js_31*As2Al[31]),
                ESP_TIL_MIX_Qil_Js_32 = (ESP_TIL_MIX_Qil_Js_32*As2Al[32])) %>%
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) %>%
  # sum days
  dplyr::group_by(dates) %>%
  dplyr::summarise_at(dplyr::vars(dplyr::starts_with('ESP_TIL_MIX')),
                      dplyr::funs(sum(., na.rm = TRUE))) %>%
  # remove the zeroes generated previously
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('ESP_TIL_MIX')),
                   dplyr::funs(replace(., . == 0, NA)))%>%
  # cohorts transpiration per leaf area
  dplyr::mutate(
    E_PS = (rowSums(.[names(.)[c(2:23)]], na.rm = TRUE))/(rowSums(!is.na(.[names(.)[c(2:23)]]))),
    E_QI = (rowSums(.[names(.)[c(24:29)]], na.rm = TRUE))/(rowSums(!is.na(.[names(.)[c(24:29)]]))),
    Eplanttot = E_PS + E_QI
  ) %>%
  dplyr::select(dates, Eplanttot, E_PS, E_QI)

measuredData <- env_data %>%
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) %>%
  dplyr::select(dates, swc_shallow) %>%
  dplyr::group_by(dates) %>%
  dplyr::summarise(SWC = mean(swc_shallow, na.rm = TRUE)) %>%
  dplyr::left_join(transp_data_temp, by = 'dates') %>%
  # dplyr::filter(Date > as.Date('2010-06-01') & Date <= as.Date('2013-12-31')) %>%
  dplyr::mutate(Eplanttot = NA, SWC_err = NA) %>%
  dplyr::select(dates, SWC, SWC_err, Eplanttot, E_PS, E_QI)

measuredData<-as.data.frame(measuredData)
row.names(measuredData) <- measuredData$dates

prades_wp <- read.table("SourceData/Tables/Prades/TitllarWaterPotentials.csv",
                        sep = ",", header = TRUE)

wpData_PS <- prades_wp %>%
  filter(Species=="Pinus sylvestris" & DefoliationClass=="ND") %>%
  group_by(Timestamp2, Species) %>%
  summarize(PD_PS = mean(WaterPotential.PD, na.rm=T), PD_PS_err = sd(WaterPotential.PD, na.rm=T),
            MD_PS = mean(WaterPotential.MD, na.rm=T), MD_PS_err = sd(WaterPotential.MD, na.rm=T))

measuredData[as.character(wpData_PS$Timestamp2), paste0("PD_", PS_cohname)] = wpData_PS$PD_PS
measuredData[as.character(wpData_PS$Timestamp2), paste0("PD_", PS_cohname,"_err")] = wpData_PS$PD_PS_err
measuredData[as.character(wpData_PS$Timestamp2), paste0("MD_", PS_cohname)] = wpData_PS$MD_PS
measuredData[as.character(wpData_PS$Timestamp2), paste0("MD_", PS_cohname,"_err")] = wpData_PS$MD_PS_err

wpData_QI <- prades_wp %>%
  filter(Species=="Quercus ilex") %>%
  group_by(Timestamp2, Species) %>%
  summarize(PD_QI = mean(WaterPotential.PD, na.rm=T), PD_QI_err = sd(WaterPotential.PD, na.rm=T),
            MD_QI = mean(WaterPotential.MD, na.rm=T), MD_QI_err = sd(WaterPotential.MD, na.rm=T))
measuredData[as.character(wpData_QI$Timestamp2), paste0("PD_", QI_cohname)] = wpData_QI$PD_QI
measuredData[as.character(wpData_QI$Timestamp2), paste0("PD_", QI_cohname,"_err")] = wpData_QI$PD_QI_err
measuredData[as.character(wpData_QI$Timestamp2), paste0("MD_", QI_cohname)] = wpData_QI$MD_QI
measuredData[as.character(wpData_QI$Timestamp2), paste0("MD_", QI_cohname,"_err")] = wpData_QI$MD_QI_err

names(measuredData)[5:14] = c(paste0("E_",c(PS_cohname, QI_cohname)),
                             paste0("PD_", PS_cohname),
                             paste0("PD_", PS_cohname, "_err"),
                             paste0("MD_", PS_cohname),
                             paste0("MD_", PS_cohname, "_err"),
                             paste0("PD_", QI_cohname),
                             paste0("PD_", QI_cohname, "_err"),
                             paste0("MD_", QI_cohname),
                             paste0("MD_", QI_cohname, "_err"))
row.names(measuredData) <- NULL


# 10. EVALUATION PERIOD ---------------------------------------------------
d = as.Date(meteoData$dates)
meteoData <- meteoData[(d>="2011-01-01") & (d<"2011-12-31"),] #Select three years

# 11. REMARKS -------------------------------------------------------------
remarks <- data.frame(
  Title = c('Soil',
            'Fine Roots Proportion',
            'FC',
            'W',
            'LAI'),
  Remark = c('Provided by authors with modified rfc',
             'Optimization mode 2',
             'Not modified',
             'Initial value adjusted to measured value for first layer',
             'Calculated by model')
)

# 12. SAVE DATA IN FOLDER -------------------------------------------------
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

