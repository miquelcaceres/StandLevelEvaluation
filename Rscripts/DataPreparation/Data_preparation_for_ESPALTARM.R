## Alto Tajo (ARMALLONES) mixed forest
library(medfate)
library(meteoland)
library(dplyr)
library(lubridate)
library(readxl)
data("SpParamsMED")

# 0. LOAD DATA and METADATA -----------------------------------------------
env_data <- read.csv('SourceData/Tables/AltoTajo/ESP_ALT_ARM_env_data.csv')
sapf_data <- read.csv('SourceData/Tables/AltoTajo/ESP_ALT_ARM_sapf_data.csv')
env_md <- read.csv('SourceData/Tables/AltoTajo/ESP_ALT_ARM_env_md.csv')
site_md <- read.csv('SourceData/Tables/AltoTajo/ESP_ALT_ARM_site_md.csv')
stand_md <- read.csv('SourceData/Tables/AltoTajo/ESP_ALT_ARM_stand_md.csv')
plant_md <- read.csv('SourceData/Tables/AltoTajo/ESP_ALT_ARM_plant_md.csv')
species_md <- read.csv('SourceData/Tables/AltoTajo/ESP_ALT_ARM_species_md.csv')


PN_index = SpParamsMED$SpIndex[SpParamsMED$Name=="Pinus nigra"]
QF_index = SpParamsMED$SpIndex[SpParamsMED$Name=="Quercus faginea"]
QI_index = SpParamsMED$SpIndex[SpParamsMED$Name=="Quercus ilex"]

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
                'Period simulated',
                'Description DOI'),
  Value = c("Alto Tajo (Armallones)",
            "Spain",
            "ESP_ALT_ARM",
            "Alicia Forner (MNCN-CSIC)",
            "",
            "",
            40.7769,
            -2.3283,
            1079,
            25.64,
            270,
            "Cretaceous and Jurassic limestone",
            "Clay",
            10.1,
            495,
            "Sparse mixed forest dominated by three species",
            1.09,
            "Pinus nigra, Quercus faginea, Quercus ilex",
            "2012-2013",
            "10.1007/S11258-014-0351-x")
)

# 2. TERRAIN DATA ---------------------------------------------------------
terrainData <- data.frame(
  latitude = 40.7769,
  elevation = 1079,
  aspect = 270, # West
  slope = 25.64
)

# 3. TREE DATA ----------------------------------------------------------
# dbh por especies
dbh_sp <- plant_md |>
  group_by(pl_species) |>
  summarise(dbh = mean(pl_dbh, na.rm = TRUE)) |>
  pull('dbh')

treeData <- data.frame(
  Species = c("Pinus nigra", "Quercus faginea", "Quercus ilex"), 
  DBH = dbh_sp,
  Height = 100*c(1.385*(dbh_sp[1]^0.669), 
                 1.602*(dbh_sp[2]^0.549), 
                 1.806*(dbh_sp[3]^0.518)), # sin datos, alometries de IEFC
  N = ((species_md$sp_basal_area_perc/100)*stand_md$st_basal_area)/(pi*(((dbh_sp/100)/2)^2)),
  Z50 = c(300, 529, 529),
  Z95 = c(1200,2287,2287)
)
f = emptyforest()
f$treeData = treeData
treeData$LAI <- species_LAI(f, SpParamsMED)
treeData$LAI <- treeData$LAI*(1.09/sum(treeData$LAI)) ## CORRECT LAI = 1.09


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
  ID = 'ESPALTARM',
  SpParamsName = "SpParamsMED",
  herbCover = 10, herbHeight = 20,
  Validation = 'global_transp', Definitive = 'Yes'
)

# 7. SOIL DATA ------------------------------------------------------------
# No hay descripción de suelo, ni porcentajes ni bulk ni nada. SoilGrids 
# coords_sf <- sf::st_sfc(sf::st_point(c(-2.3283,40.7769)), crs = 4326)
# soilData <- medfateutils::soilgridsParams(coords_sf,  c(300, 700, 1000, 2500))
# soilData$rfc =c(45,65,80,95)
soilData <- data.frame(
  widths = c(300, 700, 1000, 2500),
  clay = c(21.8667, 23.80000, 24.10000, 24.10000),
  sand = c(41.4, 42.2, 41.7,41.7),
  om = c(4.26, 0.87, 0.55, 0.55),
  bd = c(1.243333, 1.510000,1.560000,1.560000),
  rfc = c(45,65,90,95)
)
s = soil(soilData, VG_PTF = "Toth")
sum(soil_waterExtractable(s, model="VG", minPsi = -4))

# 8. METEO DATA -----------------------------------------------------------
meteoData <- env_data |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  dplyr::group_by(dates) |>
  dplyr::summarise(MinTemperature = min(ta, na.rm = TRUE),
                   MaxTemperature = max(ta, na.rm = TRUE),
                   MinRelativeHumidity = min(rh, na.rm = TRUE),
                   MaxRelativeHumidity = max(rh, na.rm = TRUE),
                   WindSpeed = mean(ws, na.rm = TRUE),
                   # la radiacion es par, para convertirla a sw se multiplica
                   # por 0.473 (sapfluxnet unit conversion)
                   Radiation = (sum((ppfd_in * 1800), na.rm = TRUE)/(24*3600))*0.473, # de par en umol/m2/sec a sw_in en W/m2
                   Precipitation = sum(precip, na.rm = TRUE)) |>
  dplyr::mutate(Radiation = Radiation*3600*24/1000000) 
meteoData<- meteoData[!is.infinite(meteoData$MinTemperature),]



# 9. CUSTOM PARAMS --------------------------------------------------------
PN_cohname = paste0("T1_", PN_index)
QF_cohname = paste0("T2_", QF_index)
QI_cohname = paste0("T3_", QI_index)
pn <- 1
qf <- 2
qi<- 3
customParams <- data.frame(
  Species = c("Pinus nigra", "Quercus faginea", "Quercus ilex"),
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
  Al2As = c(1272,1488,1541)) 

customParams$VCleaf_kmax[qi] <- 2.63


# VC root - P. nigra = P. sylvestris
slope <- 35.51402
P50 <- -1.65
P88 <- P50 + log((100.0/88.0)-1.0)*(25.0/slope)
P12 <- P50 + log((100.0/12.0)-1.0)*(25.0/slope)
customParams$VCroot_P12[pn] <- P12
customParams$VCroot_P50[pn] <- P50
customParams$VCroot_P88[pn] <- P88
customParams$VCroot_slope[pn] <- slope

# VC stem - P. nigra = P. sylvestris
slope <- 22.57
P50 <- -3.2
P88 <- P50 + log((100.0/88.0)-1.0)*(25.0/slope)
P12 <- P50 + log((100.0/12.0)-1.0)*(25.0/slope)
customParams$VCstem_P12[pn] <- P12
customParams$VCstem_P50[pn] <- P50
customParams$VCstem_P88[pn] <- P88
customParams$VCstem_slope[pn] <- slope


# VC root  - Q. ilex
slope <- 40
P50 <- -3.25
P88 <- P50 + log((100.0/88.0)-1.0)*(25.0/slope)
P12 <- P50 + log((100.0/12.0)-1.0)*(25.0/slope)
customParams$VCroot_P12[qi] <- P12
customParams$VCroot_P50[qi] <- P50
customParams$VCroot_P88[qi] <- P88
customParams$VCroot_slope[qi] <- slope
customParams$VCroot_P12[qf] <- P12 # = Q. ilex
customParams$VCroot_P50[qf] <- P50
customParams$VCroot_P88[qf] <- P88
customParams$VCroot_slope[qf] <- slope

# VC stem  - Q. ilex
slope <- 30
P50 <- -6.4
P88 <- P50 + log((100.0/88.0)-1.0)*(25.0/slope)
P12 <- P50 + log((100.0/12.0)-1.0)*(25.0/slope)
customParams$VCstem_P12[qi] <- P12
customParams$VCstem_P50[qi] <- P50
customParams$VCstem_P88[qi] <- P88
customParams$VCstem_slope[qi] <- slope
customParams$VCstem_P12[qf] <- P12 # = Q. ilex
customParams$VCstem_P50[qf] <- P50
customParams$VCstem_P88[qf] <- P88
customParams$VCstem_slope[qf] <- slope

# VC leaf - Q. ilex
slope <- 40
P50 <- -5.25
P88 <- P50 + log((100.0/88.0)-1.0)*(25.0/slope)
P12 <- P50 + log((100.0/12.0)-1.0)*(25.0/slope)
customParams$VCleaf_P12[qi] <- P12
customParams$VCleaf_P50[qi] <- P50
customParams$VCleaf_P88[qi] <- P88
customParams$VCleaf_slope[qi] <- slope
customParams$VCleaf_P12[qf] <- P12 # = Q. ilex
customParams$VCleaf_P50[qf] <- P50
customParams$VCleaf_P88[qf] <- P88
customParams$VCleaf_slope[qf] <- slope


customParams$LeafEPS[qi] <- 15 
customParams$LeafPI0[qi] <- -2.5
customParams$LeafAF[qi] <- 0.4
customParams$LeafEPS[qf] <- 15 # = Q. ilex
customParams$LeafPI0[qf] <- -2.5
customParams$LeafAF[qf] <- 0.4
customParams$LeafEPS[pn] <- 5.31 # = P. sylvestris
customParams$LeafPI0[pn] <- -1.5
customParams$LeafAF[pn] <- 0.6

customParams$StemEPS[qi] <- 15 
customParams$StemPI0[qi] <- -2.5
customParams$StemAF[qi] <- 0.4
customParams$StemEPS[qf] <- 15 # = Q. ilex
customParams$StemPI0[qf] <- -2.5
customParams$StemAF[qf] <- 0.4
customParams$StemEPS[pn] <- 5 # = P. sylvestris
customParams$StemPI0[pn] <- -1.65
customParams$StemAF[pn] <- 0.4

customParams$Gswmax[qi] <- 0.220 
customParams$Gswmin[qi] <- 0.002 
customParams$Gswmax[qf] <- 0.220 # = Q. ilex
customParams$Gswmin[qf] <- 0.002 
customParams$Gswmax[pn] <- 0.180 # = P. sylvestris
customParams$Gswmin[pn] <- 0.001 


# P. angustifolia	P.latifolia	Qilex	Phalepensis	Phalepensis_OLD
# P12_gs	-2.2	 -1.65	-1	-1.36	-1.36
# P88_gs	-3.8	 -2.5	-2.7	 -2.14	-2.33
customParams$Gs_slope[qi] <- (88.0 - 12.0)/(2.7 - 1);
customParams$Gs_P50[qi] <- -1.0 + log(0.12/0.88)/(customParams$Gs_slope[qi]/25)
customParams$Gs_slope[qf] <- (88.0 - 12.0)/(2.7 - 1);# = Q. ilex
customParams$Gs_P50[qf] <- -1.0 + log(0.12/0.88)/(customParams$Gs_slope[qf]/25)
customParams$Gs_slope[pn] <- (88.0 - 12.0)/(2.14 - 1.36);
customParams$Gs_P50[pn] <- -1.36 + log(0.12/0.88)/(customParams$Gs_slope[pn]/25)
# # par_QI = hydraulics_psi2Weibull(-6.5,-8.5)
# customParams <- data.frame(
#   SpIndex = c(PN_index, QF_index, QI_index), # P. nigra, Q. faginea, Q. ilex
#   Cohort = c(PN_cohname, QF_cohname, QI_cohname),
#   # Species specific
#   g = c(1.0,0.8, 0.8),
#   Kmax_stemxylem = c(0.41, 0.7, 0.4), # cambio solo el de faginea para ponerle un valor "empirico"
#   VCleaf_kmax = c(4,6,2.63),
#   VCleaf_c = c(NA,NA,5.41),
#   VCleaf_d = c(NA,NA,-4.18),
#   
#   Gwmax = c(0.22, 0.6, 0.21),
#   # VCstem_c = c(NA,NA,par_QI[["c"]]),
#   # VCstem_d = c(NA,NA,par_QI[["d"]]),
#   LeafPI0 = c(NA,NA,-2.66), 
#   LeafEPS = c(NA,NA,10.57),
#   LeafAF = c(NA,NA,0.43),
#   
#   # Plot specific
#   Al2As = c(1272,1488,1541),
#   LAI_live = LAI,
#   LAI_expanded = LAI
# )

# 10. MEASURED DATA --------------------------------------------------------
# sapflow data, está en Kg/h, y el timestep es 30 minutos, así que si dividimos
# entre 2 ya tenemos los L en esos 30 min. 
transp_data_temp <- sapf_data |>
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('ESP_ALT_ARM')),
                   dplyr::funs(./2)) |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  # sum days
  dplyr::group_by(dates) |>
  dplyr::summarise_at(dplyr::vars(dplyr::starts_with('ESP_ALT_ARM')),
                      dplyr::funs(sum(., na.rm = TRUE))) |>
  # remove the zeroes generated previously
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('ESP_ALT_ARM')),
                   dplyr::funs(replace(., . == 0, NA)))  |>
  # cohorts transpiration
  dplyr::mutate(
    E_PN = (ESP_ALT_ARM_Pni_Jt_1+ESP_ALT_ARM_Pni_Jt_2 +ESP_ALT_ARM_Pni_Jt_3 +ESP_ALT_ARM_Pni_Jt_4 + ESP_ALT_ARM_Pni_Jt_13)/5, 
    E_QF = (ESP_ALT_ARM_Qfa_Jt_5 + ESP_ALT_ARM_Qfa_Jt_6 + ESP_ALT_ARM_Qfa_Jt_7 + ESP_ALT_ARM_Qfa_Jt_8 + ESP_ALT_ARM_Qfa_Jt_9 + ESP_ALT_ARM_Qfa_Jt_14)/6,
    E_QI = (ESP_ALT_ARM_Qil_Jt_10 + ESP_ALT_ARM_Qil_Jt_11 + ESP_ALT_ARM_Qil_Jt_12 + ESP_ALT_ARM_Qil_Jt_15)/4,
    Eplanttot = NA
  ) |> #Transform from kg/day/tree to l/day/m2 of leaves 
  dplyr::mutate(
    E_PN = E_PN * treeData$N[1] * (1/treeData$LAI[1])*(1/10000),
    E_QF = E_QF * treeData$N[2] * (1/treeData$LAI[2])*(1/10000),
    E_QI = E_QI * treeData$N[3] * (1/treeData$LAI[3])*(1/10000)
  ) |>
  dplyr::mutate( # Manually scaling
    E_PN = E_PN * (1/3600),
    E_QF = E_QF * (1/3600),
    E_QI = E_QI * (1/3600)
  ) |>
  dplyr::select(dates, Eplanttot, E_PN, E_QF, E_QI)

measuredData <- env_data %>%
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) %>%
  dplyr::select(dates, swc_shallow) %>%
  dplyr::group_by(dates) %>%
  dplyr::summarise(SWC = mean(swc_shallow, na.rm = TRUE)) %>%
  dplyr::left_join(transp_data_temp, by = 'dates') %>%
  dplyr::mutate(SWC_err = NA) %>%
  dplyr::select(dates, SWC, SWC_err, Eplanttot, E_PN, E_QF, E_QI)

wpData <- read_xlsx("SourceData/Tables/AltoTajo/Potencial Armallones.xlsx")
wpData$dates = as.Date(wpData$Date)

wpDataSumQI = wpData |>
  filter(Species == "qi") |>
  select(-Species) |>
  dplyr::group_by(dates) |>
  dplyr::summarise(PD_QI = mean(PD, na.rm=T), 
                   PD_QI_err = sd(PD, na.rm=T),
                   MD_QI = mean(MD, na.rm=T), 
                   MD_QI_err = sd(MD, na.rm=T)) 
measuredData <- left_join(measuredData, wpDataSumQI, by="dates")
wpDataSumPN = wpData|>
  filter(Species == "pi") |>
  select(-Species) |>
  dplyr::group_by(dates) |>
  dplyr::summarise(PD_PN = mean(PD, na.rm=T), 
                   PD_PN_err = sd(PD, na.rm=T),
                   MD_PN = mean(MD, na.rm=T), 
                   MD_PN_err = sd(MD, na.rm=T)) 
measuredData <- left_join(measuredData, wpDataSumPN, by="dates")
wpDataSumQF = wpData |> 
  filter(Species == "qf") |> 
  select(-Species) |>
  dplyr::group_by(dates) |>
  dplyr::summarise(PD_QF = mean(PD, na.rm=T), 
                   PD_QF_err = sd(PD, na.rm=T),
                   MD_QF = mean(MD, na.rm=T), 
                   MD_QF_err = sd(MD, na.rm=T)) 
measuredData <- left_join(measuredData, wpDataSumQF, by="dates")

names(measuredData)[5:19] = c(paste0("E_",c(PN_cohname, QF_cohname, QI_cohname)),
                              paste0("PD_", QI_cohname),
                              paste0("PD_", QI_cohname, "_err"),
                              paste0("MD_", QI_cohname),
                              paste0("MD_", QI_cohname, "_err"),
                              paste0("PD_", PN_cohname),
                              paste0("PD_", PN_cohname, "_err"),
                              paste0("MD_", PN_cohname),
                              paste0("MD_", PN_cohname, "_err"),
                              paste0("PD_", QF_cohname),
                              paste0("PD_", QF_cohname, "_err"),
                              paste0("MD_", QF_cohname),
                              paste0("MD_", QF_cohname, "_err"))


# 11. EVALUATION PERIOD ---------------------------------------------------
# Select evaluation dates
d = as.Date(meteoData$dates)
meteoData <- meteoData[(d>="2012-01-17") & (d<="2013-12-31"),] #Select years
d = as.Date(measuredData$dates)
measuredData <- measuredData[(d>="2012-01-17") & (d<="2013-12-31"),] #Select years


# 12. REMARKS -------------------------------------------------------------
remarks <- data.frame(
  Title = c('Soil',
            'Vegetation'),
  Remark = c('Taken from Soilgrids',
             'Understory not considered')
)

# 13. SAVE DATA IN FOLDER -------------------------------------------------
folder_name <- file.path('Sites_data', 'ESPALTARM')

write.table(siteData, file = file.path(folder_name, 'ESPALTARM_siteData.txt'),
            row.names = FALSE, sep = '\t')

write.table(treeData, file = file.path(folder_name, 'ESPALTARM_treeData.txt'),
            row.names = FALSE, sep = '\t')

write.table(shrubData, file = file.path(folder_name, 'ESPALTARM_shrubData.txt'),
            row.names = FALSE, sep = '\t')

write.table(miscData, file = file.path(folder_name, 'ESPALTARM_miscData.txt'),
            row.names = FALSE, sep = '\t')

write.table(meteoData, file = file.path(folder_name, 'ESPALTARM_meteoData.txt'),
            row.names = FALSE, sep = '\t')

write.table(soilData, file = file.path(folder_name, 'ESPALTARM_soilData.txt'),
            row.names = FALSE, sep = '\t')

write.table(terrainData, file = file.path(folder_name, 'ESPALTARM_terrainData.txt'),
            row.names = FALSE, sep = '\t')

write.table(customParams, file = file.path(folder_name, 'ESPALTARM_customParams.txt'),
            row.names = FALSE, sep = '\t')

write.table(measuredData, file = file.path(folder_name, 'ESPALTARM_measuredData.txt'),
            row.names = FALSE, sep = '\t')

write.table(remarks, file = file.path(folder_name, 'ESPALTARM_remarks.txt'),
            row.names = FALSE, sep = '\t')

