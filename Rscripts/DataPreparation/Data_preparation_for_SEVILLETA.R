## US. Sevilleta data script
library(medfate)
library(medfateutils)
library(meteoland)
library(dplyr)
library(lubridate)
library(readxl)

data("SpParamsUS")

# 0. LOAD DATA and METADATA -----------------------------------------------
env_data <- read.csv('SourceData/Tables/Sevilleta/USA_PJS_P04_AMB_env_data.csv')
sapf_data <- read.csv('SourceData/Tables/Sevilleta/USA_PJS_P04_AMB_sapf_data.csv')
env_md <- read.csv('SourceData/Tables/Sevilleta/USA_PJS_P04_AMB_env_md.csv')
site_md <- read.csv('SourceData/Tables/Sevilleta/USA_PJS_P04_AMB_site_md.csv')
stand_md <- read.csv('SourceData/Tables/Sevilleta/USA_PJS_P04_AMB_stand_md.csv')
plant_md <- read.csv('SourceData/Tables/Sevilleta/USA_PJS_P04_AMB_plant_md.csv')
species_md <- read.csv('SourceData/Tables/Sevilleta/USA_PJS_P04_AMB_species_md.csv')


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
  Value = c("Sevilleta",
            "USA",
            site_md$si_code,
            "William Pockman (University of New Mexico, USA)",
            "",
            "",
            round(site_md$si_lat,6),
            round(site_md$si_long,6),
            site_md$si_elev,
            1,
            0, #NW 
            "",
            "Sandy loam",
            round(site_md$si_mat,1),
            round(site_md$si_map),
            "Mixed pine-juniper forest",
            0.71,
            "10.1890/ES11-00369.1",
            "Pinus edulis, Juniperus monosperma",
            "SpParamsUS",
            "2010-2016",
            "2010-2016")
)


# 2. TERRAIN DATA ---------------------------------------------------------
terrainData <- data.frame(
  latitude = site_md$si_lat,
  elevation = site_md$si_elev,
  aspect = 0, # flat
  slope = 1 # 
)

# 3. TREE DATA ----------------------------------------------------------
# stand basal area = 19.33
# Piñon 2.3
# Juniper 17
pi_fr <- 2.3/19.33
ju_fr <- 17/19.33
treeData <- data.frame(
  Species = c("Pinus edulis", "Juniperus monosperma"),
  DBH = c(mean(plant_md$pl_dbh[plant_md$pl_species=="Pinus edulis"],na.rm=TRUE),
          mean(plant_md$pl_dbh[plant_md$pl_species=="Juniperus monosperma"],na.rm=TRUE)), # from paper
  Height = 100*c(mean(plant_md$pl_height[plant_md$pl_species=="Pinus edulis"],na.rm=TRUE),
                 mean(plant_md$pl_height[plant_md$pl_species=="Juniperus monosperma"],na.rm=TRUE)),
  N = c(77, 273),
  Z50 = c(200,200),
  Z95 = c(3400, 3400),
  LAI = stand_md$st_lai*c(pi_fr, ju_fr) # No information to split LAI
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
  ID = 'SEVILLETA',
  SpParamsName = "SpParamsUS",
  herbCover = 5, herbHeight = 20,
  Validation = 'global', Definitive = 'No'
)

# 7. METEO DATA -----------------------------------------------------------
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
  dplyr::mutate(Radiation = Radiation*3600*24/1000000) |>
  dplyr::mutate_at(dplyr::vars(2:5),
                   dplyr::funs(replace(., is.infinite(.), NA))) |>
  dplyr::mutate_at(dplyr::vars(2:5),
                   dplyr::funs(replace(., is.nan(.), NA))) 

# 8. SOIL DATA ------------------------------------------------------------
# coords_sf <- sf::st_sfc(sf::st_point(c(site_md$si_long,site_md$si_lat)), crs = 4326)
# soilData <- medfateutils::soilgridsParams(coords_sf,  c(300, 700, 1000, 2500))
soilData <- data.frame(
  widths = c(300, 700, 1000, 2500),
  clay = c(21.16667, 22.75000, 21.00000, 21.00000),
  sand = c(8.166667, 11.000000, 20.100000,20.100000),
  om = c(1.183333, 0.395000, 0.160000, 0),
  bd = c(1.48, 1.55,1.54,1.54),
  rfc = c(50,60,85,95),
  VG_theta_sat = rep(0.2,4),
  VG_theta_res = rep(0.001,4)
)
s = soil(soilData, VG_PTF = "Toth")
sum(soil_waterExtractable(s, model="VG", minPsi = -4))


# 9. CUSTOM PARAMS --------------------------------------------------------
PE_index = SpParamsUS$SpIndex[SpParamsUS$Name=="Pinus edulis"]
JM_index = SpParamsUS$SpIndex[SpParamsUS$Name=="Juniperus monosperma"]
PE_cohname = paste0("T1_", PE_index)
JM_cohname = paste0("T2_", JM_index)
pe<- 1
jm<-2

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

As2Al = plant_md[['pl_sapw_area']]/plant_md[['pl_leaf_area']] # cm2/m2
Al2As_sp = 10000/c(mean(As2Al[plant_md$pl_species=="Pinus edulis"]),
                   mean(As2Al[plant_md$pl_species=="Juniperus monosperma"]))

customParams$Al2As <- Al2As_sp

# 10. MEASURED DATA --------------------------------------------------------
# sapflow data, está en cm3 cm-2 h-1 of leaf area, y el timestep es 15 minutos, así que tenemos que
# multiplicar por 15*60 segundos para los l/m-2 en el timestep.
# Agrego los datos de sapflow por día, promedio todos los árboles 
transp_data_temp <- sapf_data |>
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('USA_PJS_P04')),
                   dplyr::funs(.*0.25*1e4/1e3))|>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  dplyr::group_by(dates) |>
  dplyr::summarise_at(dplyr::vars(dplyr::starts_with('USA_PJS_P04')),
                      dplyr::funs(sum(., na.rm = TRUE))) |>
  # remove the zeroes generated previously
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('USA_PJS_P04')),
                   dplyr::funs(replace(., . == 0, NA)))

transp_data_temp2<-data.frame(dates = transp_data_temp$dates,
                              E_Ped = rowMeans(transp_data_temp[,c(2,3,4,5,6)], na.rm=TRUE),
                              E_JMo = rowMeans(transp_data_temp[, c(7,8,9,10,11)], na.rm=TRUE))

measuredData <- env_data %>%
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  group_by(dates) |>
  summarise(SWC = mean(swc_shallow),
            SWC_2 = mean(swc_deep)) |>
  dplyr::full_join(transp_data_temp2, by = 'dates')|>
  dplyr::arrange(dates) 
names(measuredData)[4:5] = c(paste0("E_", PE_cohname),
                             paste0("E_", JM_cohname))

# 11. SIMULATION/EVALUATION PERIOD ---------------------------------------------------
simulation_period <- seq(as.Date("2010-01-01"),as.Date("2016-12-31"), by="day")
evaluation_period <- seq(as.Date("2010-01-01"),as.Date("2016-12-31"), by="day")
meteoData <- meteoData |> filter(dates %in% simulation_period)
measuredData <- measuredData |> filter(dates %in% evaluation_period)

# 12. REMARKS -------------------------------------------------------------
remarks <- data.frame(
  Title = c('Soil',
            'Vegetation'),
  Remark = c('Taken from SoilGrids with theta_sat and theta_res modified',
             'Understory not considered')
)



# 13. SAVE DATA IN FOLDER -------------------------------------------------
folder_name <- file.path('Sites_data', 'SEVILLETA')

write.table(siteData, file = file.path(folder_name, 'SEVILLETA_siteData.txt'),
            row.names = FALSE, sep = '\t')

write.table(treeData, file = file.path(folder_name, 'SEVILLETA_treeData.txt'),
            row.names = FALSE, sep = '\t')

write.table(shrubData, file = file.path(folder_name, 'SEVILLETA_shrubData.txt'),
            row.names = FALSE, sep = '\t')

write.table(miscData, file = file.path(folder_name, 'SEVILLETA_miscData.txt'),
            row.names = FALSE, sep = '\t')

write.table(meteoData, file = file.path(folder_name, 'SEVILLETA_meteoData.txt'),
            row.names = FALSE, sep = '\t')

write.table(soilData, file = file.path(folder_name, 'SEVILLETA_soilData.txt'),
            row.names = FALSE, sep = '\t')

write.table(terrainData, file = file.path(folder_name, 'SEVILLETA_terrainData.txt'),
            row.names = FALSE, sep = '\t')

write.table(customParams, file = file.path(folder_name, 'SEVILLETA_customParams.txt'),
            row.names = FALSE, sep = '\t')

write.table(measuredData, file = file.path(folder_name, 'SEVILLETA_measuredData.txt'),
            row.names = FALSE, sep = '\t')

write.table(remarks, file = file.path(folder_name, 'SEVILLETA_remarks.txt'),
            row.names = FALSE, sep = '\t')

