## US. Morgan Monroe data script
library(medfate)
library(meteoland)
library(dplyr)
library(lubridate)
library(readxl)
data("SpParamsUS")

# 0. LOAD DATA and METADATA -----------------------------------------------
env_data <- read.csv('SourceData/Tables/MorganMonroe/USA_MOR_SF_env_data.csv')
sapf_data <- read.csv('SourceData/Tables/MorganMonroe/USA_MOR_SF_sapf_data.csv')
env_md <- read.csv('SourceData/Tables/MorganMonroe/USA_MOR_SF_env_md.csv')
site_md <- read.csv('SourceData/Tables/MorganMonroe/USA_MOR_SF_site_md.csv')
stand_md <- read.csv('SourceData/Tables/MorganMonroe/USA_MOR_SF_stand_md.csv')
plant_md <- read.csv('SourceData/Tables/MorganMonroe/USA_MOR_SF_plant_md.csv')
species_md <- read.csv('SourceData/Tables/MorganMonroe/USA_MOR_SF_species_md.csv')
fluxnet_data <- read.csv('SourceData/Tables/MorganMonroe/FLX_US-MMS_FLUXNET2015_SUBSET_DD_1999-2014_1-4.csv')
fluxnet_data_hourly <- read.csv('SourceData/Tables/MorganMonroe/FLX_US-MMS_FLUXNET2015_SUBSET_HR_1999-2014_1-4.csv')


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
  Value = c("Morgan-Mornoe",
            "USA",
            site_md$si_code,
            "Koong Yi (Indiana University Bloomington)",
            "US-MMS",
            "Kim Novick (Indiana University)",
            round(site_md$si_lat,6),
            round(site_md$si_long,6),
            site_md$si_elev,
            0,
            0, #NW 
            "",
            "Silty clay loam",
            round(site_md$si_mat,1),
            round(site_md$si_map),
            "Mixed temperate forest",
            5,
            "Acer saccharum, Liriodendron tulipifera, Quercus rubra, Quercus alba",
            "2011-2013",
            "")
)


# 2. TERRAIN DATA ---------------------------------------------------------
terrainData <- data.frame(
  latitude = site_md$si_lat,
  elevation = site_md$si_elev,
  aspect = 0, # flat
  slope = 0 # 
)

# 3. TREE DATA ----------------------------------------------------------
treeData <- data.frame(
  Species = c("Acer saccharum", "Liriodendron tulipifera", "Quercus rubra", "Quercus alba"),
  DBH = c(mean(plant_md$pl_dbh[plant_md$pl_species=="Acer saccharum"],na.rm=TRUE),
          mean(plant_md$pl_dbh[plant_md$pl_species=="Liriodendron tulipifera"],na.rm=TRUE),
          mean(plant_md$pl_dbh[plant_md$pl_species=="Quercus rubra"],na.rm=TRUE),
          mean(plant_md$pl_dbh[plant_md$pl_species=="Quercus alba"],na.rm=TRUE)), # from paper
  Height = rep(100*stand_md$st_height,4),
  N = NA,
  Z50 = NA,
  Z95 = NA,
  LAI = rep(5/4, 4) # No information to split LAI
)
f <-emptyforest()
f$treeData <- treeData
summary(f, SpParamsUS)

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
  ID = 'USAMORSF',
  SpParamsName = "SpParamsUS",
  herbCover = 10, herbHeight = 20,
  Validation = 'global', Definitive = 'No'
)

# 7. METEO DATA -----------------------------------------------------------
meteoData <- fluxnet_data_hourly |>
  dplyr::mutate(RH = replace(RH, RH==-9999, NA)) |>
  dplyr::mutate(dates = as.Date(substr(as.character(TIMESTAMP_START),1,8), format = "%Y%m%d")) |>
  dplyr::group_by(dates) |>
  dplyr::summarise(MinTemperature = min(TA_F, na.rm = TRUE),
                   MaxTemperature = max(TA_F, na.rm = TRUE),
                   MinRelativeHumidity = min(RH, na.rm = TRUE),
                   MaxRelativeHumidity = max(RH, na.rm = TRUE),
                   Radiation = (sum((SW_IN_F * 1800), na.rm = TRUE)/(24*3600)), # W/m2, a W/m2 en el día
                   Precipitation = sum(P_F, na.rm = TRUE),
                   WindSpeed = mean(WS_F, na.rm = TRUE)) |>
  dplyr::mutate(Radiation = Radiation*3600*24/1000000) |>
  dplyr::mutate_at(dplyr::vars(2:5),
                   dplyr::funs(replace(., is.infinite(.), NA))) |>
  dplyr::mutate_at(dplyr::vars(2:5),
                   dplyr::funs(replace(., is.nan(.), NA))) 

# 
# meteoData <- env_data |>
#   dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
#   dplyr::group_by(dates) |>
#   dplyr::summarise(MinTemperature = min(ta, na.rm = TRUE),
#                    MaxTemperature = max(ta, na.rm = TRUE),
#                    MinRelativeHumidity = min(rh, na.rm = TRUE),
#                    MaxRelativeHumidity = max(rh, na.rm = TRUE),
#                    WindSpeed = mean(ws, na.rm = TRUE),
#                    Radiation = (sum((ext_rad * 3600), na.rm = TRUE)/(24*3600)), # W/m2, a W/m2 en el día
#                    Precipitation = sum(precip, na.rm = TRUE)) |>
#   dplyr::mutate(Radiation = Radiation*3600*24/1000000) |>
#   dplyr::mutate_at(dplyr::vars(2:5),
#                    dplyr::funs(replace(., is.infinite(.), NA))) |>
#   dplyr::mutate_at(dplyr::vars(2:5),
#                    dplyr::funs(replace(., is.nan(.), NA))) 

# 8. SOIL DATA ------------------------------------------------------------
# coords_sf <- sf::st_sfc(sf::st_point(c(site_md$si_long,site_md$si_lat)), crs = 4326)
# soilData <- medfateutils::soilgridsParams(coords_sf,  c(300, 700, 1000, 2500))
soilData <- data.frame(
  widths = c(300, 700, 1000, 2500),
  clay = c(24.70, 27.95, 35.30, 35.30),
  sand = c(8.166667, 11.000000, 20.100000,20.100000),
  om = c(1.940, 0.265, 0.180, 0),
  bd = c(1.430, 1.645,1.740,1.740),
  rfc = c(3,4.3,75,95)
)
s = soil(soilData, VG_PTF = "Toth")
sum(soil_waterExtractable(s, model="VG", minPsi = -4))


# 9. CUSTOM PARAMS --------------------------------------------------------
AS_index = SpParamsUS$SpIndex[SpParamsUS$Name=="Acer saccharum"]
LT_index = SpParamsUS$SpIndex[SpParamsUS$Name=="Liriodendron tulipifera"]
QR_index = SpParamsUS$SpIndex[SpParamsUS$Name=="Quercus rubra"]
QA_index = SpParamsUS$SpIndex[SpParamsUS$Name=="Quercus alba"]
AS_cohname = paste0("T1_", AS_index)
LT_cohname = paste0("T2_", LT_index)
QR_cohname = paste0("T3_", QR_index)
QA_cohname = paste0("T4_", QA_index)
as<- 1
lt<-2
qr<-3
qa<-4

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

Al2As_sp = c(SpParamsUS$Al2As[SpParamsUS$Name == "Acer saccharum"],
             SpParamsUS$Al2As[SpParamsUS$Name == "Liriodendron tulipifera"],
             SpParamsUS$Al2As[SpParamsUS$Name == "Quercus rubra"],
             SpParamsUS$Al2As[SpParamsUS$Name == "Quercus alba"])
Al2As_sp[2] <- mean(Al2As_sp, na.rm=TRUE)
customParams$Al2As <- Al2As_sp

# 10. MEASURED DATA --------------------------------------------------------
# sapflow data, está en cm3 h-1 , y el timestep es 60 minutos, 
# cal dividir per sapwood area, multiplicar per 0.001 (per passar a de cm3 a dm3)
# i dividir per Al2As (en m2 per cm2 sapwood)
# Sumamos todo el día 
sapflow_factor <- 1/1000
transp_data_temp <- sapf_data |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  dplyr::mutate(USA_MOR_SF_Asa_Js_1 = sapflow_factor*USA_MOR_SF_Asa_Js_1/(Al2As_sp[1]/10000*plant_md$pl_sapw_area[1]),
                USA_MOR_SF_Asa_Js_2 = sapflow_factor*USA_MOR_SF_Asa_Js_2/(Al2As_sp[1]/10000*plant_md$pl_sapw_area[2]),
                USA_MOR_SF_Ltu_Js_3 = sapflow_factor*USA_MOR_SF_Ltu_Js_3/(Al2As_sp[2]/10000*plant_md$pl_sapw_area[3]),
                USA_MOR_SF_Ltu_Js_4 = sapflow_factor*USA_MOR_SF_Ltu_Js_4/(Al2As_sp[2]/10000*plant_md$pl_sapw_area[4]),
                USA_MOR_SF_Qru_Js_5 = sapflow_factor*USA_MOR_SF_Qru_Js_5/(Al2As_sp[3]/10000*plant_md$pl_sapw_area[5]),
                USA_MOR_SF_Qal_Js_6 = sapflow_factor*USA_MOR_SF_Qal_Js_6/(Al2As_sp[4]/10000*plant_md$pl_sapw_area[6]))|>
  dplyr::group_by(dates)  |>
  dplyr::summarise_at(dplyr::vars(dplyr::starts_with('USA_MOR_SF')),
                      dplyr::funs(sum(., na.rm = TRUE)))  |>
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('USA_MOR_SF')),
                   dplyr::funs(replace(., . == 0, NA)))

transp_data_temp2<-data.frame(dates = transp_data_temp$dates,
                              E_Asa = rowMeans(transp_data_temp[,2:3], na.rm=TRUE),
                              E_Ltu = rowMeans(transp_data_temp[,4:5], na.rm=TRUE),
                              E_Qru = rowMeans(transp_data_temp[,6], na.rm=TRUE),
                              E_Qal = rowMeans(transp_data_temp[,7], na.rm=TRUE))

fluxData <- fluxnet_data |>
  dplyr::mutate(SWC_F_MDS_1 = replace(SWC_F_MDS_1, SWC_F_MDS_1==-9999, NA),
                LE_CORR = replace(LE_CORR, LE_CORR==-9999, NA),
                GPP_NT_VUT_REF = replace(GPP_NT_VUT_REF, GPP_NT_VUT_REF==-9999, NA))|>
  dplyr::mutate(dates = as.Date(as.character(TIMESTAMP), format = "%Y%m%d")) |>
  dplyr::select(dates, SWC_F_MDS_1, LE_CORR, GPP_NT_VUT_REF) |>
  dplyr::mutate(SWC = SWC_F_MDS_1/100,
                LE = (3600*24/1e6)*LE_CORR,# From Wm2 to MJ/m2
                GPP = GPP_NT_VUT_REF) |>
  dplyr::select(-LE_CORR, -GPP_NT_VUT_REF, -SWC_F_MDS_1)

# measuredData <- env_data %>%
#   dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
#   group_by(dates) |>
#   summarise(SWC = mean(swc_shallow)) |>
#   dplyr::mutate(SWC_err = NA)|>
#   dplyr::full_join(fluxData, by = 'dates')|>
measuredData <- fluxData |>
  dplyr::full_join(transp_data_temp2, by = 'dates')|>
  dplyr::arrange(dates) 
names(measuredData)[5:8] = c(paste0("E_", AS_cohname),
                             paste0("E_", LT_cohname),
                             paste0("E_", QR_cohname),
                             paste0("E_", QA_cohname))

# 11. EVALUATION PERIOD ---------------------------------------------------
# Select evaluation dates
evaluation_period <- seq(as.Date("2011-01-01"),as.Date("2013-12-31"), by="day")
measuredData <- measuredData |> filter(dates %in% evaluation_period)
meteoData <- meteoData |> filter(dates %in% evaluation_period)
row.names(meteoData) <- NULL
row.names(measuredData) <- NULL
# which(is.na(meteoData$MaxRelativeHumidity))
meteoData[156, c("MinRelativeHumidity", "MaxRelativeHumidity")] <- meteoData[155, c("MinRelativeHumidity", "MaxRelativeHumidity")]
meteoData[162, c("MinRelativeHumidity", "MaxRelativeHumidity")] <- meteoData[161, c("MinRelativeHumidity", "MaxRelativeHumidity")]
meteoData[391, c("MinRelativeHumidity", "MaxRelativeHumidity")] <- meteoData[390, c("MinRelativeHumidity", "MaxRelativeHumidity")]
meteoData[392, c("MinRelativeHumidity", "MaxRelativeHumidity")] <- meteoData[390, c("MinRelativeHumidity", "MaxRelativeHumidity")]
summary(meteoData)
# 12. REMARKS -------------------------------------------------------------
remarks <- data.frame(
  Title = c('Soil',
            'Vegetation',
            'Weather',
            'Soil moisture'),
  Remark = c('Taken from SoilGrids',
             'Understory not considered',
             'Using FLUXNET weather',
             'Taken from FLUXNET data')
)



# 13. SAVE DATA IN FOLDER -------------------------------------------------
folder_name <- file.path('Sites_data', 'USAMORSF')

write.table(siteData, file = file.path(folder_name, 'USAMORSF_siteData.txt'),
            row.names = FALSE, sep = '\t')

write.table(treeData, file = file.path(folder_name, 'USAMORSF_treeData.txt'),
            row.names = FALSE, sep = '\t')

write.table(shrubData, file = file.path(folder_name, 'USAMORSF_shrubData.txt'),
            row.names = FALSE, sep = '\t')

write.table(miscData, file = file.path(folder_name, 'USAMORSF_miscData.txt'),
            row.names = FALSE, sep = '\t')

write.table(meteoData, file = file.path(folder_name, 'USAMORSF_meteoData.txt'),
            row.names = FALSE, sep = '\t')

write.table(soilData, file = file.path(folder_name, 'USAMORSF_soilData.txt'),
            row.names = FALSE, sep = '\t')

write.table(terrainData, file = file.path(folder_name, 'USAMORSF_terrainData.txt'),
            row.names = FALSE, sep = '\t')

write.table(customParams, file = file.path(folder_name, 'USAMORSF_customParams.txt'),
            row.names = FALSE, sep = '\t')

write.table(measuredData, file = file.path(folder_name, 'USAMORSF_measuredData.txt'),
            row.names = FALSE, sep = '\t')

write.table(remarks, file = file.path(folder_name, 'USAMORSF_remarks.txt'),
            row.names = FALSE, sep = '\t')

