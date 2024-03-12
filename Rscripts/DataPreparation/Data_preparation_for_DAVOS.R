## Davos data script
library(medfate)
library(meteoland)
library(dplyr)
library(lubridate)
library(readxl)
data("SpParamsFR")

# 0. LOAD DATA and METADATA -----------------------------------------------
env_data <- read.csv('SourceData/Tables/Davos/CHE_DAV_SEE_env_data.csv')
sapf_data <- read.csv('SourceData/Tables/Davos/CHE_DAV_SEE_sapf_data.csv')
env_md <- read.csv('SourceData/Tables/Davos/CHE_DAV_SEE_env_md.csv')
site_md <- read.csv('SourceData/Tables/Davos/CHE_DAV_SEE_site_md.csv')
stand_md <- read.csv('SourceData/Tables/Davos/CHE_DAV_SEE_stand_md.csv')
plant_md <- read.csv('SourceData/Tables/Davos/CHE_DAV_SEE_plant_md.csv')
species_md <- read.csv('SourceData/Tables/Davos/CHE_DAV_SEE_species_md.csv')
fluxnet_data <- read.csv('SourceData/Tables/Davos/FLX_CH-Dav_FLUXNET2015_SUBSET_DD_1997-2014_1-4.csv')


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
  Value = c("Davos Seehornwald",
            "Switzerland",
            site_md$si_code,
            "Roman Zweifel (WSL)",
            "CH-Dav",
            "Nina Buchmann (ETH)",
            site_md$si_lat,
            site_md$si_long,
            site_md$si_elev,
            0, # < 2%
            0, #N 
            "",
            "Loamy sand",
            round(site_md$si_mat,1),
            round(site_md$si_map),
            "Subalpine coniferous (spruce) forest",
            stand_md$st_lai,
            "Picea abies",
            "2010",
            "10.1007/s10021-011-9481-3")
)


# 2. TERRAIN DATA ---------------------------------------------------------
terrainData <- data.frame(
  latitude = site_md$si_lat,
  elevation = site_md$si_elev,
  aspect = 0, # N
  slope = 0 # = 5-10%
)

# 3. TREE DATA ----------------------------------------------------------
# stand basal area ?
# Density 
treeData <- data.frame(
  Species = c("Picea abies"),
  DBH = 20, #?
  Height = 100*stand_md$st_height,
  N = stand_md$st_density,
  Z50 = NA,
  Z95 = NA,
  LAI = stand_md$st_lai
)
f <-emptyforest()
f$treeData <- treeData
summary(f, SpParamsFR)

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
  ID = 'DAVOS',
  SpParamsName = "SpParamsFR",
  herbCover = 0, herbHeight = 0,
  Validation = 'global', Definitive = 'No'
)

# 7. SOIL DATA ------------------------------------------------------------
# coords_sf <- sf::st_sfc(sf::st_point(c(site_md$si_long+0.01,site_md$si_lat)), crs = 4326)
# soilData <- medfateutils::soilgridsParams(coords_sf,  c(300, 600, 1100, 2500))
soilData <- data.frame(
  widths = c(300, 600, 1100, 2500),
  clay = c(13.53333, 14.67143, 15.22000, 15.22000),
  sand = c(50.96667, 51.02857, 51.68000,52.10000),
  om = c(9.176667, 3.307143, 3.546000, 3.550000),
  bd = c(1.016667, 1.338571,1.406000,1.410000),
  rfc = c(12.73333,22.98571,80,90)
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
                   Radiation = (sum((sw_in * 900), na.rm = TRUE)/(24*3600)), # W/m2, a W/m2 en el día
                   Precipitation = sum(precip, na.rm = TRUE),
                   WindSpeed = mean(ws, na.rm = TRUE)) |>
  dplyr::mutate(Radiation = Radiation*3600*24/1000000) |>
  dplyr::mutate_at(dplyr::vars(2:5),
                   dplyr::funs(replace(., is.infinite(.), NA))) |>
  dplyr::mutate_at(dplyr::vars(2:5),
                   dplyr::funs(replace(., is.nan(.), NA))) 



# 9. CUSTOM PARAMS --------------------------------------------------------
PA_index = SpParamsFR$SpIndex[SpParamsFR$Name=="Picea abies"]
PA_cohname = paste0("T1_", PA_index)
pa<- 1
customParams <- data.frame(
  Species = treeData$Species,
  Vmax298 = NA,
  Jmax298 = NA,
  Kmax_stemxylem = NA,
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

Al2As_sp <- SpParamsFR$Al2As[SpParamsFR$Name=="Picea abies"] #9487.313) # m2/m2
customParams$Al2As <- Al2As_sp

# 10. MEASURED DATA --------------------------------------------------------
# sapflow data, está en cm3 cm-2 h-1 y el timestep es 30 minutos, 
# cal dividir per 2 (per tenir flow en els 30 min),
# dividir per Al2As (m2/cm2), 
# multiplicar per 0.001 (per passar a de cm3 a dm3)
# Sumamos todo el día 
sapflow_factor <- 0.5/1000
transp_data_temp <- sapf_data |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  dplyr::mutate(CHE_DAV_SEE_Pab_Jt_1 = sapflow_factor*CHE_DAV_SEE_Pab_Jt_1/(Al2As_sp[1]/10000),
                CHE_DAV_SEE_Pab_Jt_2 = sapflow_factor*CHE_DAV_SEE_Pab_Jt_2/(Al2As_sp[1]/10000),
                CHE_DAV_SEE_Pab_Jt_3 = sapflow_factor*CHE_DAV_SEE_Pab_Jt_3/(Al2As_sp[1]/10000),
                CHE_DAV_SEE_Pab_Jt_4 = sapflow_factor*CHE_DAV_SEE_Pab_Jt_4/(Al2As_sp[1]/10000),
                CHE_DAV_SEE_Pab_Jt_5 = sapflow_factor*CHE_DAV_SEE_Pab_Jt_5/(Al2As_sp[1]/10000))|>
  dplyr::group_by(dates)  |>
  dplyr::summarise_at(dplyr::vars(dplyr::starts_with('CHE_DAV_SEE')),
                      dplyr::funs(sum(., na.rm = TRUE)))  |>
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('CHE_DAV_SEE')),
                   dplyr::funs(replace(., . == 0, NA)))

transp_data_temp2<-data.frame(dates = transp_data_temp$dates,
                              E_Pa = rowMeans(transp_data_temp[,2:6], na.rm=TRUE))
names(transp_data_temp2)[2] = paste0("E_", PA_cohname)


fluxData <- fluxnet_data |>
  dplyr::mutate(LE_CORR = replace(LE_CORR, LE_CORR==-9999, NA),
                GPP_NT_VUT_REF = replace(GPP_NT_VUT_REF, GPP_NT_VUT_REF==-9999, NA))|>
  dplyr::mutate(dates = as.Date(as.character(TIMESTAMP), format = "%Y%m%d")) |>
  dplyr::select(dates, LE_CORR, GPP_NT_VUT_REF) |>
  dplyr::mutate(LE = (3600*24/1e6)*LE_CORR,# From Wm2 to MJ/m2
                GPP = GPP_NT_VUT_REF) |>
  dplyr::select(-LE_CORR, -GPP_NT_VUT_REF)

measuredData <- env_data |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid')))  |>
  dplyr::select(dates, swc_shallow)  |>
  dplyr::group_by(dates)  |>
  dplyr::summarise(SWC = mean(swc_shallow, na.rm = TRUE))  |>
  dplyr::left_join(transp_data_temp2, by = 'dates') |>
  dplyr::left_join(fluxData, by = 'dates')


# measuredData <- measuredData  |>
#   dplyr::left_join(fluxData, by="dates")

# 11. EVALUATION PERIOD ---------------------------------------------------
# Select evaluation dates
evaluation_period <- seq(as.Date("2010-01-01"),as.Date("2010-12-01"), by="day")
measuredData <- measuredData |> filter(dates %in% evaluation_period)
meteoData <- meteoData |> filter(dates %in% evaluation_period)
row.names(meteoData) <- NULL
row.names(measuredData) <- NULL

# 12. REMARKS -------------------------------------------------------------
remarks <- data.frame(
  Title = c('Soil',
            'Vegetation',
            'Sapflow'),
  Remark = c('Taken from SoilGrids',
             'No understory or secondary species considered',
             'Species-wise Huber value used for scaling')
)



# 13. SAVE DATA IN FOLDER -------------------------------------------------
folder_name <- file.path('Sites_data', 'DAVOS')
if(!dir.exists(folder_name)) dir.create(folder_name)

write.table(siteData, file = file.path(folder_name, 'DAVOS_siteData.txt'),
            row.names = FALSE, sep = '\t')

write.table(treeData, file = file.path(folder_name, 'DAVOS_treeData.txt'),
            row.names = FALSE, sep = '\t')

write.table(shrubData, file = file.path(folder_name, 'DAVOS_shrubData.txt'),
            row.names = FALSE, sep = '\t')

write.table(miscData, file = file.path(folder_name, 'DAVOS_miscData.txt'),
            row.names = FALSE, sep = '\t')

write.table(meteoData, file = file.path(folder_name, 'DAVOS_meteoData.txt'),
            row.names = FALSE, sep = '\t')

write.table(soilData, file = file.path(folder_name, 'DAVOS_soilData.txt'),
            row.names = FALSE, sep = '\t')

write.table(terrainData, file = file.path(folder_name, 'DAVOS_terrainData.txt'),
            row.names = FALSE, sep = '\t')

write.table(customParams, file = file.path(folder_name, 'DAVOS_customParams.txt'),
            row.names = FALSE, sep = '\t')

write.table(measuredData, file = file.path(folder_name, 'DAVOS_measuredData.txt'),
            row.names = FALSE, sep = '\t')

write.table(remarks, file = file.path(folder_name, 'DAVOS_remarks.txt'),
            row.names = FALSE, sep = '\t')

