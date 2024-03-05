## FRAHES data script
library(medfate)
library(dplyr)
library(lubridate)
library(meteoland)
library(readxl)

data("SpParamsMED")

# 0. LOAD DATA and METADATA -----------------------------------------------
env_data <- read.csv('SourceData/Tables/Hesse/FRA_HES_HE2_NON_env_data.csv')
sapf_data <- read.csv('SourceData/Tables/Hesse/FRA_HES_HE2_NON_sapf_data.csv')
env_md <- read.csv('SourceData/Tables/Hesse/FRA_HES_HE2_NON_env_md.csv')
site_md <- read.csv('SourceData/Tables/Hesse/FRA_HES_HE2_NON_site_md.csv')
stand_md <- read.csv('SourceData/Tables/Hesse/FRA_HES_HE2_NON_stand_md.csv')
plant_md <- read.csv('SourceData/Tables/Hesse/FRA_HES_HE2_NON_plant_md.csv')
species_md <- read.csv('SourceData/Tables/Hesse/FRA_HES_HE2_NON_species_md.csv')
swc_FRAHES <- readxl::read_xls('SourceData/Tables/Hesse/soilwater_Hesse.xls')

# 1. SITE INFORMATION -----------------------------------------------------
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
  Value = c("Hesse",
            "France",
            "FRA_HES_HE2_NON",
            "André Granier (INRAE)",
            48.6742,
            7.0647,
            300,
            0,
            0,
            "",
            "Silt loam",
            9.3,
            729,
            "Naturally regenerated, managed beech forest",
            5,
            "Fagus sylvatica",
            "10.1051/forest:2008052")
)

# 2. TERRAIN DATA ---------------------------------------------------------
terrainData <- data.frame(
  latitude = 48.6742,
  elevation = 300,
  aspect = 0, # Flat
  slope = 0 # < 2% slope
)

# 3. TREE DATA ------------------------------------------------------------
FS_index = SpParamsMED$SpIndex[SpParamsMED$Name=="Fagus sylvatica"]
treeData <- data.frame(
  Species = "Fagus sylvatica", 
  DBH = mean(plant_md[['pl_dbh']]),
  Height = 1300, # from sapfluxnet metadata
  N = 3203, # paper para el año 2001.
  Z50 = 300,  # datos que luego se modificaran por la optimización
  Z95 = 1200,  # datos que luego se modificaran por la optimización
  LAI = 5
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
  ID = 'FRAHES',
  SpParamsName = "SpParamsMED",
  herbCover = 5, herbHeight = 20,
  Validation = 'global', Definitive = 'No'
)

# 7. SOIL DATA ------------------------------------------------------------
soilData <- data.frame(
  widths = c(200, 300, 700, 1000),
  sand = c(8, 8,8,8),
  clay = c(25,35,45,45),
  om = c(6,3,1,0),
  rfc = c(9, 13, 15, 95),
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



# 10. MEASURED DATA --------------------------------------------------------
# N
N_real <- (treeData[['N']]*6000)/10000

# sapflow data, está en dm3/dm2 h, y el timestep es 30 minutos, así que si dividimos
# entre dos ya tenemos los L en esa media hora. Además, es sapflow relativo a sapwood area por
# tanto, multiplicamos por el sapwood area pasado de cm2 a dm2
# Sumamos todo el día y luego multiplicamos por le numero total de arboles y dividimos por los arboles medidos
# y el area de la parcela
transp_data_temp <- sapf_data |>
  dplyr::mutate(FRA_HES_HE2_NON_Fsy_Js_1 = (FRA_HES_HE2_NON_Fsy_Js_1*plant_md[['pl_sapw_area']][1])/200,
                FRA_HES_HE2_NON_Fsy_Js_2 = (FRA_HES_HE2_NON_Fsy_Js_2*plant_md[['pl_sapw_area']][2])/200,
                FRA_HES_HE2_NON_Fsy_Js_3 = (FRA_HES_HE2_NON_Fsy_Js_3*plant_md[['pl_sapw_area']][3])/200,
                FRA_HES_HE2_NON_Fsy_Js_4 = (FRA_HES_HE2_NON_Fsy_Js_4*plant_md[['pl_sapw_area']][4])/200,
                FRA_HES_HE2_NON_Fsy_Js_5 = (FRA_HES_HE2_NON_Fsy_Js_5*plant_md[['pl_sapw_area']][5])/200,
                FRA_HES_HE2_NON_Fsy_Js_6 = (FRA_HES_HE2_NON_Fsy_Js_6*plant_md[['pl_sapw_area']][6])/200,
                FRA_HES_HE2_NON_Fsy_Js_7 = (FRA_HES_HE2_NON_Fsy_Js_7*plant_md[['pl_sapw_area']][7])/200,
                FRA_HES_HE2_NON_Fsy_Js_8 = (FRA_HES_HE2_NON_Fsy_Js_8*plant_md[['pl_sapw_area']][8])/200,
                FRA_HES_HE2_NON_Fsy_Js_9 = (FRA_HES_HE2_NON_Fsy_Js_9*plant_md[['pl_sapw_area']][9])/200,
                FRA_HES_HE2_NON_Fsy_Js_10 = (FRA_HES_HE2_NON_Fsy_Js_10*plant_md[['pl_sapw_area']][10])/200) |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>  
  dplyr::filter(!is.na(dates)) |>
  dplyr::group_by(dates) |># sum days
  dplyr::summarise_at(dplyr::vars(dplyr::starts_with('FRA_HES_HE2_NON')),
                      list(. = sum, sum = ~ sum(.x, na.rm = TRUE)))
transp_data_temp$Eplanttot <- rowSums(transp_data_temp[,-1], na.rm=TRUE)/6000*rowSums(!is.na(transp_data_temp[,-1]))
transp_data_temp <- transp_data_temp |>
  dplyr::mutate(E_T1_97 = Eplanttot) |>
  dplyr::select(dates, Eplanttot, E_T1_97)

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
  dplyr::full_join(transp_data_temp, by = 'dates') |>
  dplyr::mutate(SWC_err = NA) |>
  dplyr::select(dates, SWC, SWC_err, Eplanttot, E_T1_97) |>
  dplyr::arrange(dates)


# 11. EVALUATION PERIOD ---------------------------------------------------
evaluation_period <- seq(as.Date("2001-01-01"),as.Date("2005-12-31"), by="day")
measuredData <- measuredData |> filter(dates %in% evaluation_period)
meteoData <- meteoData |> filter(dates %in% evaluation_period)
row.names(meteoData) <- NULL
row.names(measuredData) <- NULL



# 12. REMARKS -------------------------------------------------------------
remarks <- data.frame(
  Title = c('Soil',
            'Vegetation'),
  Remark = c('VG_theta_sat modified',
             'No understory')
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
