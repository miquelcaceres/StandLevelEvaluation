## Israel P. halepensis data script
library(medfate)
library(dplyr)
library(lubridate)
library(meteoland)
library(readxl)

data("SpParamsMED")

# 0. LOAD DATA and METADATA -----------------------------------------------
env_data <- read.csv('SourceData/Tables/Yatir/ISR_YAT_YAT_env_data.csv')
sapf_data <- read.csv('SourceData/Tables/Yatir/ISR_YAT_YAT_sapf_data.csv')
env_md <- read.csv('SourceData/Tables/Yatir/ISR_YAT_YAT_env_md.csv')
site_md <- read.csv('SourceData/Tables/Yatir/ISR_YAT_YAT_site_md.csv')
stand_md <- read.csv('SourceData/Tables/Yatir/ISR_YAT_YAT_stand_md.csv')
plant_md <- read.csv('SourceData/Tables/Yatir/ISR_YAT_YAT_plant_md.csv')
yat_meteo <- read.table("SourceData/Tables/Yatir/Yatir_Climate_ForSurEau_2010_2022.csv", 
           sep = ";", header = TRUE, dec=",")

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
  Value = c("Yatir",
            "Israel",
            "ISR_YAT_YAT",
            "Fyodor Tatarinov (WIS)",
            "",
            "",
            site_md$si_lat,
            site_md$si_long,
            site_md$si_elev,
            0,
            0,
            "",
            "Clay loam",
            round(site_md$si_mat,1),
            round(site_md$si_map),
            "Pinus halepensis managed plantation",
            1.5,
            "Pinus halepensis",
            "2014-2015",
            "10.1111/nph.13597")
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
PH_index = SpParamsMED$SpIndex[SpParamsMED$Name=="Pinus halepensis"]
treeData <- data.frame(
  Species = "Pinus halepensis",
  DBH = 19.8, # from paper
  Height = 1020, # from paper
  N = 300,
  Z50 = 400,  
  Z95 = 2500,
  LAI = 1.7
)
f = emptyforest()
f$treeData = treeData
vprofile_leafAreaDensity(f, SpParamsMED, draw=T)
vprofile_rootDistribution(f, SpParams = SpParamsMED)
summary(f, SpParamsMED)

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
  SpParamsName = "SpParamsMED",
  herbCover = 0, herbHeight = 0,
  Validation = 'global_transp', Definitive = 'No'
)

# 7. SOIL DATA ------------------------------------------------------------
soilData <- data.frame(
  widths = c(200, 300, 3000),
  sand = rep(31, 3),
  clay = rep(28, 3),
  om = c(6,3,1),
  bd = rep(1.45, 3),
  rfc = c(75, 75, 85),
  VG_theta_sat = rep(0.45, 3),
  VG_theta_res = rep(0.003, 3)
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
         MeanTemperature = Tair_mean,
         Radiation = RG_sum,
         Precipitation = PPT_sum,
         MinRelativeHumidity = RHair_min,
         MaxRelativeHumidity = RHair_max,
         MeanRelativeHumidity = RHair_mean,
         WindSpeed = WS_mean) |>
  mutate(dates = as.Date(dates, format = "%d/%m/%Y"))

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
# No tenemos area del plot, navegando entre referencias del artículo
# (Biogeochemical factors contributing to enhanced carbon storage following
# afforestation of a semi-arid shrubland && Water limitation to soil CO2 efflux
# in a pine forest at the semiarid “timberline”)
# vemos que son cinco plots de 30x30m, lo que hace un área de 30x30x5 = 4500m2
# N
N_real <- (treeData[['N']]*4500)/10000

# sapflow data, está en dm3/h, y el timestep es 30 minutos, así que si dividimos
# entre dos ya tenemos los L en esa media hora. sumamos todo el día y luego
# multiplicamos por le numero total de arboles y dividimos por los arboles medidos
# y el area de la parcela
transp_data_temp <- sapf_data %>%
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('ISR_YAT_YAT')),
                   dplyr::funs(./2)) %>%
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) %>%
  # sum days
  dplyr::group_by(dates) %>%
  dplyr::summarise_at(dplyr::vars(dplyr::starts_with('ISR_YAT_YAT')),
                      dplyr::funs(sum(., na.rm = TRUE))) %>%
  # remove the zeroes generated previously
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('ISR_YAT_YAT')),
                   dplyr::funs(replace(., . == 0, NA))) %>%
  dplyr::mutate(Eplanttot = (rowSums(.[names(.)[-1]], na.rm = TRUE) * N_real)/(4500*rowSums(!is.na(.[names(.)[-1]]))),
                E_T1_148 = Eplanttot) %>%
  dplyr::select(dates, Eplanttot, E_T1_148)

measuredData <- env_data %>%
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) %>%
  dplyr::select(dates, swc_shallow, swc_deep) %>%
  dplyr::group_by(dates) %>%
  dplyr::summarise(SWC = mean(swc_shallow, na.rm = TRUE)/100, # soil water content in env_data as %
                   SWC_2 = mean(swc_deep, na.rm = TRUE)/100) %>%
  dplyr::left_join(transp_data_temp, by = 'dates') %>%
  dplyr::mutate(SWC_err = NA) %>%
  dplyr::select(dates, SWC, SWC_err, SWC_2, Eplanttot, E_T1_148) |>
  dplyr::filter(!is.na(dates))

# 11. EVALUATION PERIOD ---------------------------------------------------
evaluation_period <- seq(as.Date("2014-01-01"),as.Date("2015-02-04"), by="day")
measuredData <- measuredData |> filter(dates %in% evaluation_period)
meteoData <- meteoData |> filter(dates %in% evaluation_period)
row.names(meteoData) <- NULL
row.names(measuredData) <- NULL
summary(measuredData)
summary(meteoData)

# 12. REMARKS -------------------------------------------------------------
remarks <- data.frame(
  Title = c('Soil',
            'Vegetation'),
  Remark = c('50-cm soil with rocky layers. Modification of theta_res',
             'No understory considered')
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

