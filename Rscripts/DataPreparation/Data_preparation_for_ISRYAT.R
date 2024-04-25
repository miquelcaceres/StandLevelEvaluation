## Israel P. halepensis data script
library(medfate)
library(medfateutils)
library(dplyr)
library(lubridate)
library(meteoland)
library(readxl)

data("SpParamsES")

# 0. LOAD DATA and METADATA -----------------------------------------------
env_data <- read.csv('SourceData/Tables/Yatir/ISR_YAT_YAT_env_data.csv')
sapf_data <- read.csv('SourceData/Tables/Yatir/ISR_YAT_YAT_sapf_data.csv')
env_md <- read.csv('SourceData/Tables/Yatir/ISR_YAT_YAT_env_md.csv')
site_md <- read.csv('SourceData/Tables/Yatir/ISR_YAT_YAT_site_md.csv')
stand_md <- read.csv('SourceData/Tables/Yatir/ISR_YAT_YAT_stand_md.csv')
plant_md <- read.csv('SourceData/Tables/Yatir/ISR_YAT_YAT_plant_md.csv')
yat_meteo <- read.table("SourceData/Tables/Yatir/Yatir_Climate_ForSurEau_2010_2022.csv", 
           sep = ";", header = TRUE, dec=",")
fluxnet_data <- read.csv('SourceData/Tables/Yatir/FLX_IL-Yat_FLUXNET2015_FULLSET_DD_2000-2020_beta-3.csv')

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
  Value = c("Yatir",
            "Israel",
            "ISR_YAT_YAT",
            "Fyodor Tatarinov (Weizmann Institute of Science)",
            "IL-Yat",
            "Dan Yakir (Weizmann Institute of Science)",
            site_md$si_lat,
            site_md$si_long,
            site_md$si_elev,
            0,
            0,
            "Chalk and limestone",
            "Clay loam",
            round(site_md$si_mat,1),
            round(site_md$si_map),
            "Pinus halepensis managed plantation",
            1.5,
            "10.1111/nph.13597",
            "Pinus halepensis",
            "SpParamsES",
            "2014-2015",
            "2014-2015")
)

# 2. TERRAIN DATA ---------------------------------------------------------
# sacado de los metadatos de sapfluxnet
terrainData <- data.frame(
  latitude = 31.345,
  elevation = 650,
  aspect = 0, # Flat
  slope = 0 # Hilltop
)

# 3. TREE DATA ------------------------------------------------------------
PH_index = SpParamsES$SpIndex[SpParamsES$Name=="Pinus halepensis"]
treeData <- data.frame(
  Species = "Pinus halepensis",
  DBH = 19.8, # from paper
  Height = 1020, # from paper
  N = 300,
  Z50 = 150,  
  Z95 = 2000,
  LAI = 1.7
)
f = emptyforest()
f$treeData = treeData
vprofile_leafAreaDensity(f, SpParamsES, draw=T)
vprofile_rootDistribution(f, SpParams = SpParamsES)
summary(f, SpParamsES)

# 4. SHRUB DATA -----------------------------------------------------------
# there is no shrub info at the moment
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
  ID = 'ISRYAT',
  SpParamsName = "SpParamsES",
  herbCover = 0, herbHeight = 0,
  Validation = 'global_transp', Definitive = 'No'
)

# 7. SOIL DATA ------------------------------------------------------------
soilData <- data.frame(
  widths = c(20, 30, 100, 100, 100, 150, 500, 3000),
  sand = rep(31, 8),
  clay = c(10, 30, 30, 40, 42, 42, 42, 42),
  om = c(40,3,2,2,1,1,1,0),
  bd = c(0.3, 1.65, 1.57, 1.61, 1.54, 1.54, 1.54, 1.54),
  rfc = c(0,0,5,10,20,20,30, 90),
  VG_theta_sat = c(0.7, 0.3, 0.3, 0.3, 0.3, 0.33, 0.33, 0.33),
  VG_theta_res = c(0.05, 0.05, 0.05, 0.08, 0.09, 0.11, 0.11, 0.11)
)
s<-soil(soilData, VG_PTF = "Toth")
sum(soil_waterExtractable(s, model="VG", minPsi = -4))


# 8. METEO DATA -----------------------------------------------------------
# FROM SAPFLUXNET
# meteoData <- env_data |>
#   dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
#   dplyr::filter(!is.na(dates)) |>
#   dplyr::group_by(dates) |>
#   dplyr::summarise(MinTemperature = min(ta, na.rm = TRUE),
#                    MaxTemperature = max(ta, na.rm = TRUE),
#                    MinRelativeHumidity = min(rh, na.rm = TRUE),
#                    MaxRelativeHumidity = max(rh, na.rm = TRUE),
#                    WindSpeed = mean(ws, na.rm = TRUE),
#                    Radiation = (sum((sw_in * 1800), na.rm = TRUE)/(24*3600)), # W/m2, a W/m2 en el día
#                    Precipitation = sum(precip, na.rm = TRUE)) |>
#   dplyr::mutate(Radiation = Radiation*3600*24/1000000) # J/m2 to MJ/m2/day
meteoData <- yat_meteo |>
  rename(dates = DATE,
         MinTemperature = Tair_min,
         MaxTemperature = Tair_max,
         Radiation = RG_sum,
         Precipitation = PPT_sum,
         MinRelativeHumidity = RHair_min,
         MaxRelativeHumidity = RHair_max,
         WindSpeed = WS_mean) |>
  mutate(dates = as.Date(dates, format = "%d/%m/%Y")) |>
  select(dates, MinTemperature, MaxTemperature, Radiation, Precipitation, 
         MinRelativeHumidity, MaxRelativeHumidity, WindSpeed)

# meteoData1 <- fluxnet_data_hourly |>
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


# 9. CUSTOM PARAMS --------------------------------------------------------
PH_cohname = paste0("T1_", PH_index)
ph <- 1
customParams <- data.frame(
  Species = c("Pinus halepensis"),
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

customParams$VCleaf_kmax[ph] <- 4.0



slope <- 46
P50 <- -4.79
P88 <- P50 + log((100.0/88.0)-1.0)*(25.0/slope)
P12 <- P50 + log((100.0/12.0)-1.0)*(25.0/slope)
customParams$VCstem_P12[ph] <- P12
customParams$VCstem_P50[ph] <- P50
customParams$VCstem_P88[ph] <- P88
customParams$VCstem_slope[ph] <- slope

# RESISTANT LEAF AND ROOT (non-segmented)
customParams$VCleaf_P12[ph] <- P12
customParams$VCleaf_P50[ph] <- P50
customParams$VCleaf_P88[ph] <- P88
customParams$VCleaf_slope[ph] <- slope


customParams$VCroot_P12[ph] <- -1.0
customParams$VCroot_P50[ph] <- -1.741565
customParams$VCroot_P88[ph] <- -2.301482

customParams$LeafEPS[ph] <- 5.31 
customParams$LeafPI0[ph] <- -1.5
customParams$LeafAF[ph] <- 0.6

customParams$StemEPS[ph] <- 5
customParams$StemPI0[ph] <- -1.65
customParams$StemAF[ph] <- 0.4

customParams$Gswmax[ph] <- 0.2175 
customParams$Gswmin[ph] <- 0.001 

# Phalepensis	Phalepensis_OLD
# P12_gs	-1.36	-1.36
# P88_gs	-2.14	-2.33
customParams$Gs_slope[ph] <- (88.0 - 12.0)/(2.14 - 1.36);
customParams$Gs_P50[ph] <- -1.36 + log(0.12/0.88)/(customParams$Gs_slope[ph]/25)


# 10. MEASURED DATA --------------------------------------------------------
# sapflow data, está en dm3/h, y el timestep es 30 minutos, así que si dividimos
# entre dos ya tenemos los L en esa media hora. sumamos todo el día y 
# promediamos entre arboles. a continuación multiplicamos por la densidad de la parcela
# y dividimos por el LAI para obtener el valor por superficie de hoja
transp_data_temp <- sapf_data |>
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('ISR_YAT_YAT')),
                   dplyr::funs(./2)) |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  # sum days
  dplyr::group_by(dates) |>
  dplyr::summarise_at(dplyr::vars(dplyr::starts_with('ISR_YAT_YAT')),
                      dplyr::funs(sum(., na.rm = TRUE))) |>
  # remove the zeroes generated previously
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('ISR_YAT_YAT')),
                   dplyr::funs(replace(., . == 0, NA)))
transp_data_temp$E_Ph <- rowMeans(transp_data_temp[,2:25], na.rm=TRUE)
transp_data_temp2 <- transp_data_temp |>
  dplyr::select(dates, E_Ph) |>
  dplyr::mutate(E_Ph = E_Ph*(stand_md$st_density/(stand_md$st_lai*10000)))
names(transp_data_temp2)[2] <- paste0("E_",PH_cohname)

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
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  dplyr::select(dates, swc_shallow, swc_deep) |>
  dplyr::group_by(dates) |>
  dplyr::summarise(SWC.3 = mean(swc_shallow, na.rm = TRUE)/100, # soil water content in env_data as %
                   SWC.6 = mean(swc_deep, na.rm = TRUE)/100) |>
  dplyr::select(dates, SWC.3, SWC.6) |>
  dplyr::left_join(transp_data_temp2, by = 'dates') |>
  dplyr::left_join(fluxData, by = 'dates') |>
  dplyr::filter(!is.na(dates)) |>
  dplyr::arrange(dates)

# 11. SIMULATION/EVALUATION PERIOD ---------------------------------------------------
simulation_period <- seq(as.Date("2014-01-01"),as.Date("2014-12-31"), by="day")
evaluation_period <- seq(as.Date("2014-01-01"),as.Date("2014-12-31"), by="day")
measuredData <- measuredData |> filter(dates %in% simulation_period)
meteoData <- meteoData |> filter(dates %in% evaluation_period)


# 12. REMARKS -------------------------------------------------------------
remarks <- data.frame(
  Title = c('Soil',
            'Vegetation',
            'Sapflow'),
  Remark = c('50-cm soil with rocky layers. Modification of theta_res and theta_sat',
             'No understory considered',
             'Scaling done using stand density and stand LAI')
)

# 13. SAVE DATA IN FOLDER -------------------------------------------------
folder_name <- file.path('Sites_data', 'ISRYAT')
write.table(siteData, file = file.path(folder_name, 'ISRYAT_siteData.txt'),
            row.names = FALSE, sep = '\t')

write.table(treeData, file = file.path(folder_name, 'ISRYAT_treeData.txt'),
            row.names = FALSE, sep = '\t')

write.table(miscData, file = file.path(folder_name, 'ISRYAT_miscData.txt'),
            row.names = FALSE, sep = '\t')

write.table(meteoData, file = file.path(folder_name, 'ISRYAT_meteoData.txt'),
            row.names = FALSE, sep = '\t')

write.table(soilData, file = file.path(folder_name, 'ISRYAT_soilData.txt'),
            row.names = FALSE, sep = '\t')

write.table(terrainData, file = file.path(folder_name, 'ISRYAT_terrainData.txt'),
            row.names = FALSE, sep = '\t')

write.table(customParams, file = file.path(folder_name, 'ISRYAT_customParams.txt'),
            row.names = FALSE, sep = '\t')

write.table(measuredData, file = file.path(folder_name, 'ISRYAT_measuredData.txt'),
            row.names = FALSE, sep = '\t')

write.table(remarks, file = file.path(folder_name, 'ISRYAT_remarks.txt'),
            row.names = FALSE, sep = '\t')

