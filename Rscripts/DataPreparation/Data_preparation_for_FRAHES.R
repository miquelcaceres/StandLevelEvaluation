## FRAHES data script
library(medfate)
library(medfateutils)
library(dplyr)
library(lubridate)
library(meteoland)
library(readxl)

data("SpParamsFR")

# 0. LOAD DATA and METADATA -----------------------------------------------
env_data <- read.csv('SourceData/Tables/Hesse/FRA_HES_HE2_NON_env_data.csv')
sapf_data <- read.csv('SourceData/Tables/Hesse/FRA_HES_HE2_NON_sapf_data.csv')
env_md <- read.csv('SourceData/Tables/Hesse/FRA_HES_HE2_NON_env_md.csv')
site_md <- read.csv('SourceData/Tables/Hesse/FRA_HES_HE2_NON_site_md.csv')
stand_md <- read.csv('SourceData/Tables/Hesse/FRA_HES_HE2_NON_stand_md.csv')
plant_md <- read.csv('SourceData/Tables/Hesse/FRA_HES_HE2_NON_plant_md.csv')
species_md <- read.csv('SourceData/Tables/Hesse/FRA_HES_HE2_NON_species_md.csv')
swc_FRAHES <- readxl::read_xls('SourceData/Tables/Hesse/soilwater_Hesse.xls')

flux_data <- dplyr::bind_rows(
  read.csv('SourceData/Tables/Hesse/EFDC_L2_Flx_FRHes_2001_v04_30m.txt'),
  read.csv('SourceData/Tables/Hesse/EFDC_L2_Flx_FRHes_2002_v04_30m.txt'),
  read.csv('SourceData/Tables/Hesse/EFDC_L2_Flx_FRHes_2003_v02_30m.txt'),
  read.csv('SourceData/Tables/Hesse/EFDC_L2_Flx_FRHes_2004_v05_30m.txt'),
  read.csv('SourceData/Tables/Hesse/EFDC_L2_Flx_FRHes_2005_v06_30m.txt')
  )

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
  Value = c("Hesse",
            "France",
            "FRA_HES_HE2_NON",
            "André Granier (INRAE)",
            "FR-Hes",
            "Matthias Cuntz (INRAE)",
            round(site_md$si_lat,6),
            round(site_md$si_long,6),
            site_md$si_elev,
            0,
            0,
            "",
            "Silt loam",
            round(site_md$si_mat,1),
            round(site_md$si_map),
            "Naturally regenerated, managed beech forest",
            7,
            "10.1051/forest:2008052",
            "Fagus sylvatica",
            "SpParamsFR",
            "2001-2004",
            "2001-2004")
)

# 2. TERRAIN DATA ---------------------------------------------------------
terrainData <- data.frame(
  latitude = site_md$si_lat,
  elevation = site_md$si_elev,
  aspect = 0, # Flat
  slope = 0 # < 2% slope
)

# 3. TREE DATA ------------------------------------------------------------
FS_index = SpParamsFR$SpIndex[SpParamsFR$Name=="Fagus sylvatica"]
treeData <- data.frame(
  Species = "Fagus sylvatica", 
  DBH = mean(plant_md[['pl_dbh']]),
  Height = 1300, # from sapfluxnet metadata
  N = 3203, # paper para el año 2001.
  Z50 = 300,  # datos que luego se modificaran por la optimización
  Z95 = 1200,  # datos que luego se modificaran por la optimización
  LAI = 7 # From Granier's paper for 2001-2004
)
f = emptyforest()
f$treeData = treeData
vprofile_leafAreaDensity(f, SpParamsFR, draw=T)
vprofile_rootDistribution(f, SpParams = SpParamsFR)
summary(f, SpParamsFR)

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
  ID = 'FRAHES',
  SpParamsName = "SpParamsFR",
  herbCover = 5, herbHeight = 20,
  Validation = 'global', Definitive = 'No'
)

# 7. SOIL DATA ------------------------------------------------------------
soilData <- data.frame(
  widths = c(200, 300, 700, 2100),
  sand = c(8, 8,8,8),
  clay = c(25,35,45,45),
  om = c(6,3,1,0),
  rfc = c(9, 13, 15, 90),
  bd = c(1.16, 1.37, 1.58, 1.58),
  VG_theta_sat = c(0.5528318,0.5044341,0.4560364,0.4560364)
)
s <- soil(soilData, VG_PTF = "Toth")
sum(soil_waterExtractable(s, model="VG", minPsi = -4))



# 8. METEO DATA -----------------------------------------------------------
meteoData <- env_data |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid')),
                rh = (vpd*100)/(0.61078*exp((17.269*ta)/(237.3+ta)))) |> # no hay rh, asi que transformo la vpd
  dplyr::group_by(dates) |>
  dplyr::summarise(MeanTemperature = mean(ta, na.rm = TRUE),
                   MinTemperature = min(ta, na.rm = TRUE),
                   MaxTemperature = max(ta, na.rm = TRUE),
                   MeanRelativeHumidity = mean(rh, na.rm = TRUE),
                   MinRelativeHumidity = min(rh, na.rm = TRUE),
                   MaxRelativeHumidity = max(rh, na.rm = TRUE),
                   WindSpeed = mean(ws, na.rm = TRUE),
                   Radiation = (sum((sw_in * 1800), na.rm = TRUE)/(24*3600)), # W/m2, a W/m2 en el día
                   Precipitation = sum(precip, na.rm = TRUE)) |>
  dplyr::mutate(Radiation = Radiation*3600*24/1000000) %>% # J/m2 to MJ/m2/day
  dplyr::mutate_at(dplyr::vars(2:10),
                   dplyr::funs(replace(., is.infinite(.), NA))) |>
  dplyr::mutate_at(dplyr::vars(2:10),
                   dplyr::funs(replace(., is.nan(.), NA))) |>
  dplyr::mutate(Radiation = na_if(Radiation, 0)) 


# 9. CUSTOM PARAMS --------------------------------------------------------
FS_cohname = paste0("T1_", FS_index)
fs <- 1
customParams <- data.frame(
  Species = c("Fagus sylvatica"),
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

Al2As_sp <- 2076.12
customParams$Al2As <- Al2As_sp

# 10. MEASURED DATA --------------------------------------------------------
# sapflow data, está en cm3 cm-2 h-1, y el timestep es 30 minutos, 
# cal dividir per 2 (per tenir flow en els 30 min), multiplicar per As2Al (cm2/m2), 
# multiplicar per 0.001 (per passar a de cm3 a dm3)
# Sumamos todo el día 
sapflow_factor <- 0.5/1000
transp_data_temp <- sapf_data |>
  dplyr::mutate(FRA_HES_HE2_NON_Fsy_Js_1 = sapflow_factor*FRA_HES_HE2_NON_Fsy_Js_1/(Al2As_sp[1]/10000),
                FRA_HES_HE2_NON_Fsy_Js_2 = sapflow_factor*FRA_HES_HE2_NON_Fsy_Js_2/(Al2As_sp[1]/10000),
                FRA_HES_HE2_NON_Fsy_Js_3 = sapflow_factor*FRA_HES_HE2_NON_Fsy_Js_3/(Al2As_sp[1]/10000),
                FRA_HES_HE2_NON_Fsy_Js_4 = sapflow_factor*FRA_HES_HE2_NON_Fsy_Js_4/(Al2As_sp[1]/10000),
                FRA_HES_HE2_NON_Fsy_Js_5 = sapflow_factor*FRA_HES_HE2_NON_Fsy_Js_5/(Al2As_sp[1]/10000),
                FRA_HES_HE2_NON_Fsy_Js_6 = sapflow_factor*FRA_HES_HE2_NON_Fsy_Js_6/(Al2As_sp[1]/10000),
                FRA_HES_HE2_NON_Fsy_Js_7 = sapflow_factor*FRA_HES_HE2_NON_Fsy_Js_7/(Al2As_sp[1]/10000),
                FRA_HES_HE2_NON_Fsy_Js_8 = sapflow_factor*FRA_HES_HE2_NON_Fsy_Js_8/(Al2As_sp[1]/10000),
                FRA_HES_HE2_NON_Fsy_Js_9 = sapflow_factor*FRA_HES_HE2_NON_Fsy_Js_9/(Al2As_sp[1]/10000),
                FRA_HES_HE2_NON_Fsy_Js_10 = sapflow_factor*FRA_HES_HE2_NON_Fsy_Js_10/(Al2As_sp[1]/10000)) |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>  
  dplyr::filter(!is.na(dates)) |>
  dplyr::group_by(dates) |># sum days
  dplyr::summarise_at(dplyr::vars(dplyr::starts_with('FRA_HES_HE2_NON')),
                      dplyr::funs(sum(., na.rm = TRUE)))  |>
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('FRA_HES_HE2_NON')),
                   dplyr::funs(replace(., . == 0, NA)))
transp_data_temp$E_Fs <- rowMeans(transp_data_temp[,2:11], na.rm=TRUE)
transp_data_temp2 <- transp_data_temp |>
  dplyr::select(dates, E_Fs)
names(transp_data_temp2)[2] <- paste0("E_",FS_cohname)

ydoy.ymd <- function(year, DOY){
  # define the origine as the first day of each year
  origin <- as.Date(paste(year, "01-01", sep = "-"))
  # convert doy into date
  ymd <- as.Date(DOY-1, origin = origin)
  return(ymd)
}
measuredData <- swc_FRAHES |>
  dplyr::mutate(dates = as.Date(ydoy.ymd(year, DOY))) |>
  dplyr::select(dates, 3:15) |>
  dplyr::mutate(
    SWC = (`H-10` + `H-20` + `H-30`)/3) |>
  dplyr::select(dates, SWC) |>
  dplyr::full_join(transp_data_temp2, by = 'dates') |>
  dplyr::arrange(dates)

fluxData <- flux_data |>
  dplyr::mutate(SW_IN = replace(SW_IN, SW_IN==-9999, NA),
                WS = replace(WS, WS==-9999, NA),
                TA = replace(TA, TA==-9999, NA),
                P = replace(P, P==-9999, NA),
                LE = replace(LE, LE==-9999, NA),
                RH = replace(RH, RH==-9999, NA),
                PA = replace(PA, PA==-9999, NA),
                SWC = replace(SWC, SWC==-9999, NA)) |>
  dplyr::mutate(dates = as.Date(substr(as.character(TIMESTAMP_START),1,8), format = "%Y%m%d")) |>
  dplyr::group_by(dates) |>
  dplyr::summarise(MinTemperature = min(TA, na.rm = TRUE),
                   MaxTemperature = max(TA, na.rm = TRUE),
                   MinRelativeHumidity = min(RH, na.rm = TRUE),
                   MaxRelativeHumidity = max(RH, na.rm = TRUE),
                   Radiation = (sum((SW_IN * 1800), na.rm = TRUE)/(24*3600)), # W/m2, a W/m2 en el día
                   Precipitation = sum(P, na.rm = TRUE),
                   WindSpeed = mean(WS, na.rm = TRUE),
                   LE = (3600*24/1e6)*mean(LE, na.rm = TRUE),
                   SWC = mean(SWC, na.rm = TRUE))

measuredData <- measuredData |>
  dplyr::full_join(fluxData[,c("dates", "LE")], by = 'dates') |>
  dplyr::arrange(dates)


# 11. SIMULATION/EVALUATION PERIOD ---------------------------------------------------
simulation_period <- seq(as.Date("2001-01-01"),as.Date("2004-12-31"), by="day")
evaluation_period <- seq(as.Date("2001-01-01"),as.Date("2004-12-31"), by="day")
meteoData <- meteoData |> filter(dates %in% simulation_period)
measuredData <- measuredData |> filter(dates %in% evaluation_period)



# 12. REMARKS -------------------------------------------------------------
remarks <- data.frame(
  Title = c('Soil',
            'Vegetation',
            'Sapflow'),
  Remark = c('VG_theta_sat modified',
             'No woody understory but 5% herbaceous layer',
             'Scaling using species-level Huber value')
)

# 13. SAVE DATA IN FOLDER -------------------------------------------------
folder_name <- file.path('Sites_data', 'FRAHES')

write.table(siteData, file = file.path(folder_name, 'FRAHES_siteData.txt'),
            row.names = FALSE, sep = '\t')

write.table(treeData, file = file.path(folder_name, 'FRAHES_treeData.txt'),
            row.names = FALSE, sep = '\t')

write.table(miscData, file = file.path(folder_name, 'FRAHES_miscData.txt'),
            row.names = FALSE, sep = '\t')

write.table(meteoData, file = file.path(folder_name, 'FRAHES_meteoData.txt'),
            row.names = FALSE, sep = '\t')

write.table(soilData, file = file.path(folder_name, 'FRAHES_soilData.txt'),
            row.names = FALSE, sep = '\t')

write.table(terrainData, file = file.path(folder_name, 'FRAHES_terrainData.txt'),
            row.names = FALSE, sep = '\t')

write.table(customParams, file = file.path(folder_name, 'FRAHES_customParams.txt'),
            row.names = FALSE, sep = '\t')

write.table(measuredData, file = file.path(folder_name, 'FRAHES_measuredData.txt'),
            row.names = FALSE, sep = '\t')

write.table(remarks, file = file.path(folder_name, 'FRAHES_remarks.txt'),
            row.names = FALSE, sep = '\t')
