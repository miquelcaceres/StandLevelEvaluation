## Puechabon Q. ilex data script
library(medfate)
library(medfateutils)
library(meteoland)
library(dplyr)
library(lubridate)
library(readxl)

data("SpParamsFR")

# 0. LOAD DATA and METADATA -----------------------------------------------
env_data <- read.csv('SourceData/Tables/Puechabon/FRA_PUE_env_data.csv')
sapf_data <- read.csv('SourceData/Tables/Puechabon/FRA_PUE_sapf_data.csv')
env_md <- read.csv('SourceData/Tables/Puechabon/FRA_PUE_env_md.csv')
site_md <- read.csv('SourceData/Tables/Puechabon/FRA_PUE_site_md.csv')
stand_md <- read.csv('SourceData/Tables/Puechabon/FRA_PUE_stand_md.csv')
plant_md <- read.csv('SourceData/Tables/Puechabon/FRA_PUE_plant_md.csv')
fluxnet_data <- read.csv('SourceData/Tables/Puechabon/FLX_FR-Pue_FLUXNET2015_SUBSET_DD_2000-2014_2-4.csv')
pue_meteo <- readr::read_delim("SourceData/Tables/Puechabon/Climat_Puechabon_site.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

QI_index = SpParamsFR$SpIndex[SpParamsFR$Name=="Quercus ilex"]
BS_index = SpParamsFR$SpIndex[SpParamsFR$Name=="Buxus sempervirens"]


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
  Value = c("Puéchabon",
            "France",
            "FRA_PUE",
            "Jean-Marc Limousin (CEFE-CNRS)",
            "FR-Pue",
            "Jean-Marc Limousin (CEFE-CNRS)",
            43.74,
            3.60,
            270,
            0,
            0,
            "Limestone",
            "Silty clay loam",
            round(site_md$si_mat,1),
            round(site_md$si_map),
            "Dense evergreen forest dominated by Q. ilex",
            2.0,
            "10.1111/j.1365-2486.2009.01852.x",
            "Quercus ilex, Buxus sempervirens",
            "SpParamsFR",
            "2004-2006",
            "2004-2006")
)

# 2. TERRAIN DATA ---------------------------------------------------------
terrainData <- data.frame(
  latitude = 43.74139,
  elevation = 270,
  aspect = 0, # Flat
  slope = 0 # Flat
)

# 3. TREE DATA ----------------------------------------------------------
treeData <- data.frame(
  Species = "Quercus ilex",
  DBH = mean(plant_md[['pl_dbh']]),
  Height = mean(plant_md[['pl_height']], na.rm = TRUE)*100,
  N =  1750, 
  Z50 = 529,  
  Z95 = 2287,
  LAI = 2.0
)

# 4. SHRUB DATA -----------------------------------------------------------
shrubData <- data.frame(
  Species = "Buxus sempervirens",
  Cover = 13,
  Height = 200,
  Z50 = 390,
  Z95 = 1470,
  LAI = 0.2
)

# 5. SEED DATA ------------------------------------------------------------
# there is no seed info

# 6. MISC DATA ------------------------------------------------------------
miscData <- data.frame(
  ID = 'FRAPUE',
  SpParamsName = "SpParamsFR",
  herbCover = 10, herbHeight = 20,
  Validation = 'global', Definitive = 'Yes'
)

# 7. SOIL DATA ------------------------------------------------------------
# extracted from the paper (Limousin 2009) y adaptado como Prades:
# "bulk density" de PRADES:
soilData <- data.frame(
  widths = c(300, 200, 1500, 2500),
  clay = c(39, 39, 39, 39),
  sand = c(26, 26, 26, 26), #14
  om = c(6, 3, 1, 1),
  bd = c(1.45, 1.45, 1.45, 1.45),
  rfc = c(75, 75, 80, 90),
  VG_theta_sat = rep(0.33, 4),
  VG_theta_res = rep(0.008, 4)
)
sum(soil_waterExtractable(soil(soilData), model="VG", minPsi = -4))


# 8. METEO DATA -----------------------------------------------------------
meteoData <- pue_meteo |>
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

# meteoData <- env_data |>
#   dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid')))|>
#   dplyr::filter(!is.na(dates)) |>
#   dplyr::group_by(dates) |>
#   dplyr::summarise(MinTemperature = min(ta, na.rm = TRUE),
#                    MaxTemperature = max(ta, na.rm = TRUE),
#                    MinRelativeHumidity = min(rh, na.rm = TRUE),
#                    MaxRelativeHumidity = max(rh, na.rm = TRUE),
#                    WindSpeed = mean(ws, na.rm = TRUE),
#                    Radiation = (sum((sw_in * 1800), na.rm = TRUE)/(24*3600)), # W/m2, a W/m2 en el día
#                    Precipitation = sum(precip, na.rm = TRUE)) %>%
#   dplyr::mutate(Radiation = Radiation*3600*24/1000000) %>% # w/m2 to MJ/m2/day
#   dplyr::mutate_at(dplyr::vars(dplyr::ends_with('Humidity')),
#                    dplyr::funs(replace(., . > 100, 100))) %>%
#   dplyr::mutate_at(dplyr::vars(dplyr::ends_with('Speed')),
#                    dplyr::funs(replace(., is.nan(.), NA)))



# 9. CUSTOM PARAMS --------------------------------------------------------
QI_cohname = paste0("T1_", QI_index)
BS_cohname = paste0("S1_", BS_index)
qi <- 1
bs <- 2
customParams <- data.frame(
  Species = c("Quercus ilex", "Buxus sempervirens"),
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
customParams$VCleaf_kmax[bs] <- 2.0
customParams$SLA[qi] <- 4.55
customParams$SLA[bs] <- 5.19
customParams$Kmax_stemxylem[qi] <- 0.20
customParams$Kmax_stemxylem[bs] <- 0.15

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

# VC leaf - B. sempervirens
slope <- 40
P50 <- -6.25
P88 <- P50 + log((100.0/88.0)-1.0)*(25.0/slope)
P12 <- P50 + log((100.0/12.0)-1.0)*(25.0/slope)
customParams$VCleaf_P12[bs] <- P12
customParams$VCleaf_P50[bs] <- P50
customParams$VCleaf_P88[bs] <- P88
customParams$VCleaf_slope[bs] <- slope


customParams$LeafEPS[qi] <- 15 
customParams$LeafPI0[qi] <- -2.5
customParams$LeafAF[qi] <- 0.4

customParams$StemEPS[qi] <- 15 
customParams$StemPI0[qi] <- -2.5
customParams$StemAF[qi] <- 0.4

customParams$Gswmax[qi] <- 0.200 
customParams$Gswmin[qi] <- 0.002 
customParams$Gswmax[bs] <- 0.180 
customParams$Gswmin[bs] <- 0.002 

# P12_gs	-1
# P88_gs	-2.7
customParams$Gs_slope[qi] <- (88.0 - 12.0)/(2.7 - 1);
customParams$Gs_P50[qi] <- -1.0 + log(0.12/0.88)/(customParams$Gs_slope[qi]/25)

# 10. MEASURED DATA --------------------------------------------------------
# Agrego los datos de sapflow por día, sumo todos los árboles y luego multiplico
# por LAI y divido por n = numero de arboles medidos
# Para agregar por día el sapflow, al estar medido en dm3/dm2h y en timesteps
# de 30 minutos, se divide entre 2 (media hora) y se multiplica por el As2Al
# del árbol divido por 100 (de cm2 a dm2)
# LAI = 2.2
As2Al = plant_md[['pl_sapw_area']]/plant_md[['pl_leaf_area']] # cm2/m2

Al2As_sp = mean(10000/As2Al)
customParams$Al2As[qi] = Al2As_sp

EQI_name = paste0("E_",QI_cohname)
EBS_name = paste0("E_",BS_cohname)
transp_data_temp <- sapf_data %>%
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('FRA_PUE')),
                   dplyr::funs(. *(1/200))) %>%
  dplyr::mutate(FRA_PUE_Qil_Js_1 = (FRA_PUE_Qil_Js_1*As2Al[1]),
                FRA_PUE_Qil_Js_2 = (FRA_PUE_Qil_Js_2*As2Al[2]),
                FRA_PUE_Qil_Js_3 = (FRA_PUE_Qil_Js_3*As2Al[3]),
                FRA_PUE_Qil_Js_4 = (FRA_PUE_Qil_Js_4*As2Al[4]),
                FRA_PUE_Qil_Js_5 = (FRA_PUE_Qil_Js_5*As2Al[5]),
                FRA_PUE_Qil_Js_6 = (FRA_PUE_Qil_Js_6*As2Al[6]),
                FRA_PUE_Qil_Js_7 = (FRA_PUE_Qil_Js_7*As2Al[7]),
                FRA_PUE_Qil_Js_8 = (FRA_PUE_Qil_Js_8*As2Al[8]),
                FRA_PUE_Qil_Js_9 = (FRA_PUE_Qil_Js_9*As2Al[9]),
                FRA_PUE_Qil_Js_10 = (FRA_PUE_Qil_Js_10*As2Al[10]),
                FRA_PUE_Qil_Js_11 = (FRA_PUE_Qil_Js_11*As2Al[11]),
                FRA_PUE_Qil_Js_12 = (FRA_PUE_Qil_Js_12*As2Al[12]),
                FRA_PUE_Qil_Js_13 = (FRA_PUE_Qil_Js_13*As2Al[13]),
                FRA_PUE_Qil_Js_14 = (FRA_PUE_Qil_Js_14*As2Al[14]),
                FRA_PUE_Qil_Js_15 = (FRA_PUE_Qil_Js_15*As2Al[15]),
                FRA_PUE_Qil_Js_16 = (FRA_PUE_Qil_Js_16*As2Al[16]),
                FRA_PUE_Qil_Js_17 = (FRA_PUE_Qil_Js_17*As2Al[17]),
                FRA_PUE_Qil_Js_18 = (FRA_PUE_Qil_Js_18*As2Al[18]),
                FRA_PUE_Qil_Js_19 = (FRA_PUE_Qil_Js_19*As2Al[19]),
                FRA_PUE_Qil_Js_20 = (FRA_PUE_Qil_Js_20*As2Al[20]),
                FRA_PUE_Qil_Js_21 = (FRA_PUE_Qil_Js_21*As2Al[21]),
                FRA_PUE_Qil_Js_22 = (FRA_PUE_Qil_Js_22*As2Al[22]),
                FRA_PUE_Qil_Js_23 = (FRA_PUE_Qil_Js_23*As2Al[23]),
                FRA_PUE_Qil_Js_24 = (FRA_PUE_Qil_Js_24*As2Al[24]),
                FRA_PUE_Qil_Js_25 = (FRA_PUE_Qil_Js_25*As2Al[25])) %>%
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) %>%
  # sum days
  dplyr::group_by(dates) %>%
  dplyr::summarise_at(dplyr::vars(dplyr::starts_with('FRA_PUE')),
                      dplyr::funs(sum(., na.rm = TRUE))) %>%
  # remove the zeroes generated previously
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('FRA_PUE')),
                   dplyr::funs(replace(., . == 0, NA)))%>%
  dplyr::mutate(E_QI = (rowSums(.[names(.)[-1]], na.rm = TRUE))/(rowSums(!is.na(.[names(.)[-1]]))),
                E_BS = NA) %>%
  dplyr::select(dates, E_QI, E_BS)

fluxData <- fluxnet_data |>
  dplyr::mutate(H_CORR = replace(H_CORR, H_CORR==-9999, NA),
                LE_CORR = replace(LE_CORR, LE_CORR==-9999, NA),
                GPP_NT_VUT_REF = replace(GPP_NT_VUT_REF, GPP_NT_VUT_REF==-9999, NA))|>
  dplyr::mutate(dates = as.Date(as.character(TIMESTAMP), format = "%Y%m%d")) |>
  dplyr::select(dates, H_CORR, LE_CORR, GPP_NT_VUT_REF) |>
  dplyr::mutate(H = (3600*24/1e6)*H_CORR,# From Wm2 to MJ/m2
                LE = (3600*24/1e6)*LE_CORR,# From Wm2 to MJ/m2
                GPP = GPP_NT_VUT_REF) |>
  dplyr::select(-H_CORR, -LE_CORR, -GPP_NT_VUT_REF)

wpData <- read_xlsx("SourceData/Tables/Puechabon/FRA_PUE_WaterPotentials.xlsx") |>
  dplyr::mutate(Date = as.Date(Date)) |>
  dplyr::rename(dates = Date)

measuredData <- env_data |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid')))  |>
  dplyr::select(dates, swc_shallow)  |>
  dplyr::group_by(dates)  |>
  dplyr::summarise(SWC = mean(swc_shallow, na.rm = TRUE))  |>
  dplyr::left_join(transp_data_temp, by = 'dates') %>%
  dplyr::filter(dates > as.Date('2002-12-31') & dates < as.Date('2016-01-01')) %>%
  dplyr::mutate(SWC_err = NA)  |>
  dplyr::select(dates, SWC, SWC_err, E_QI, E_BS) |>
  dplyr::left_join(fluxData, by="dates")|>
  dplyr::left_join(wpData, by="dates")

names(measuredData)[4:5] <- paste0("E_",c(QI_cohname, BS_cohname))
names(measuredData)[9] <- paste0("PD_", QI_cohname)
names(measuredData)[10] <- paste0("PD_", QI_cohname, "_err")
names(measuredData)[11] <- paste0("MD_", QI_cohname)
names(measuredData)[12] <- paste0("MD_", QI_cohname, "_err")

# 11. SIMULATION/EVALUATION PERIOD ---------------------------------------------------
simulation_period <- seq(as.Date("2004-01-01"),as.Date("2006-12-31"), by="day")
evaluation_period <- seq(as.Date("2004-01-01"),as.Date("2006-12-31"), by="day")
meteoData <- meteoData |> filter(dates %in% simulation_period)
measuredData <- measuredData |> filter(dates %in% evaluation_period)

# 12. REMARKS -------------------------------------------------------------
remarks <- data.frame(
  Title = c('Soil',
            'Vegetation'),
  Remark = c('Adjusted theta_res and theta_sat',
             'Using B. sempervirens as understory')
)

# 13. SAVE DATA IN FOLDER -------------------------------------------------
folder_name <- file.path('Sites_data', 'FRAPUE')
write.table(siteData, file = file.path(folder_name, 'FRAPUE_siteData.txt'),
            row.names = FALSE, sep = '\t')
write.table(treeData, file = file.path(folder_name, 'FRAPUE_treeData.txt'),
            row.names = FALSE, sep = '\t')
write.table(shrubData, file = file.path(folder_name, 'FRAPUE_shrubData.txt'),
             row.names = FALSE, sep = '\t')
write.table(miscData, file = file.path(folder_name, 'FRAPUE_miscData.txt'),
            row.names = FALSE, sep = '\t')
write.table(meteoData, file = file.path(folder_name, 'FRAPUE_meteoData.txt'),
            row.names = FALSE, sep = '\t')
write.table(soilData, file = file.path(folder_name, 'FRAPUE_soilData.txt'),
            row.names = FALSE, sep = '\t')

write.table(terrainData, file = file.path(folder_name, 'FRAPUE_terrainData.txt'),
            row.names = FALSE, sep = '\t')

write.table(customParams, file = file.path(folder_name, 'FRAPUE_customParams.txt'),
            row.names = FALSE, sep = '\t')

write.table(measuredData, file = file.path(folder_name, 'FRAPUE_measuredData.txt'),
            row.names = FALSE, sep = '\t')

write.table(remarks, file = file.path(folder_name, 'FRAPUE_remarks.txt'),
            row.names = FALSE, sep = '\t')


