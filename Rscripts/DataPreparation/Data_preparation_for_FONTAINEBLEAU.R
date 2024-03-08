## Fontainebleau data script
library(medfate)
library(meteoland)
library(dplyr)
library(lubridate)
library(readxl)
data("SpParamsFR")

# 0. LOAD DATA and METADATA -----------------------------------------------
env_data <- read.csv('SourceData/Tables/Fontainebleau/FRA_FON_env_data.csv')
sapf_data <- read.csv('SourceData/Tables/Fontainebleau/FRA_FON_sapf_data.csv')
env_md <- read.csv('SourceData/Tables/Fontainebleau/FRA_FON_env_md.csv')
site_md <- read.csv('SourceData/Tables/Fontainebleau/FRA_FON_site_md.csv')
stand_md <- read.csv('SourceData/Tables/Fontainebleau/FRA_FON_stand_md.csv')
plant_md <- read.csv('SourceData/Tables/Fontainebleau/FRA_FON_plant_md.csv')
species_md <- read.csv('SourceData/Tables/Fontainebleau/FRA_FON_species_md.csv')


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
  Value = c("Fontainebleau-Barbeau",
            "France",
            site_md$si_code,
            "Nicolas Delpierre (Univ. Paris-Sud)",
            site_md$si_lat,
            site_md$si_long,
            site_md$si_elev,
            0, # < 2%
            0, #N 
            "Millstone",
            "Loam",
            site_md$si_mat,
            site_md$si_map,
            "Mixed deciduous forest",
            stand_md$st_lai,
            "Quercus petraea, Carpinus betulus",
            "10.1111/nph.13771")
)


# 2. TERRAIN DATA ---------------------------------------------------------
terrainData <- data.frame(
  latitude = site_md$si_lat,
  elevation = site_md$si_elev,
  aspect = 0, # N
  slope = 0 # = 5-10%
)

# 3. TREE DATA ----------------------------------------------------------
# stand basal area = 25 (79% Quercus petraea, 15% Carpinus betulus)
# Density 
treeData <- data.frame(
  Species = c("Quercus petraea", "Carpinus betulus"),
  DBH = c(33, 10), #c(mean(plant_md$pl_dbh[plant_md$pl_species=="Quercus petraea"],na.rm=TRUE),
        #  mean(plant_md$pl_dbh[plant_md$pl_species=="Carpinus betulus"],na.rm=TRUE)), # From basal area 
  Height = 100*c(28, 5), # 28 m Carpinus in the understory
  N = 1104*c(0.2, 0.8),
  Z50 = NA,
  Z95 = NA,
  LAI = 6*c(0.79, 0.21)
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
  ID = 'FONTAINEBLEAU',
  SpParamsName = "SpParamsFR",
  herbCover = 0, herbHeight = 0,
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
                   Radiation = (sum((sw_in * 900), na.rm = TRUE)/(24*3600)), # W/m2, a W/m2 en el día
                   Precipitation = sum(precip, na.rm = TRUE),
                   WindSpeed = mean(ws, na.rm = TRUE)) |>
  dplyr::mutate(Radiation = Radiation*3600*24/1000000) |>
  dplyr::mutate_at(dplyr::vars(2:5),
                   dplyr::funs(replace(., is.infinite(.), NA))) |>
  dplyr::mutate_at(dplyr::vars(2:5),
                   dplyr::funs(replace(., is.nan(.), NA))) 

# 8. SOIL DATA ------------------------------------------------------------
coords_sf <- sf::st_sfc(sf::st_point(c(site_md$si_long,site_md$si_lat)), crs = 4326)
soilData <- medfateutils::soilgridsParams(coords_sf,  c(300, 600, 1100, 2500))
soilData <- data.frame(
  widths = c(300, 600, 1100, 2500),
  clay = c(24.13333, 30.48571, 30.20000, 29.90000),
  sand = c(26.56667, 25.22857, 25.64000,25.80000),
  om = c(3.673333, 0.6228571, 0.4720000, 0.4500000),
  bd = c(1.286667, 1.504286,1.518000,1.520000),
  rfc = c(14.43333,15.54286,80,90)
)
s = soil(soilData, VG_PTF = "Toth")
sum(soil_waterExtractable(s, model="VG", minPsi = -4))


# 9. CUSTOM PARAMS --------------------------------------------------------
QP_index = SpParamsFR$SpIndex[SpParamsFR$Name=="Quercus petraea"]
CB_index = SpParamsFR$SpIndex[SpParamsFR$Name=="Carpinus betulus"]
QP_cohname = paste0("T1_", QP_index)
CB_cohname = paste0("T2_", CB_index)
qp<- 1
cb<- 2
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

Al2As_sp <- rep(SpParamsFR$Al2As[SpParamsFR$Name=="Quercus petraea"],2) #9487.313) # m2/m2
customParams$Al2As <- Al2As_sp

# 10. MEASURED DATA --------------------------------------------------------
# sapflow data, está en cm3 h-1, y el timestep es 30 minutos, 
# cal dividir per 2 (per tenir flow en els 30 min), dividir per sapwood area,
# dividir per Al2As (m2/cm2), 
# multiplicar per 0.001 (per passar a de cm3 a dm3)
# Sumamos todo el día 
sapwood_area <- plant_md$pl_sapw_area #cm2
# model between sapwood area and dbh
trunk_area <- pi*(plant_md$pl_dbh/2)^2
sapwood_area2 <- 1/100 * (-2.02419 + 0.76517*(trunk_area*100))
sapwood_area[is.na(sapwood_area)] <- sapwood_area2[is.na(sapwood_area)]
sapflow_factor <- 0.25/1000
transp_data_temp <- sapf_data |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  dplyr::mutate(FRA_FON_Cbe_Jt_1 = sapflow_factor*FRA_FON_Cbe_Jt_1/(sapwood_area[1]*Al2As_sp[2]/10000),
                FRA_FON_Cbe_Jt_2 = sapflow_factor*FRA_FON_Cbe_Jt_2/(sapwood_area[2]*Al2As_sp[2]/10000),
                FRA_FON_Cbe_Jt_3 = sapflow_factor*FRA_FON_Cbe_Jt_3/(sapwood_area[3]*Al2As_sp[2]/10000),
                FRA_FON_Qpe_Jt_4 = sapflow_factor*FRA_FON_Qpe_Jt_4/(sapwood_area[4]*Al2As_sp[1]/10000),
                FRA_FON_Qpe_Jt_5 = sapflow_factor*FRA_FON_Qpe_Jt_5/(sapwood_area[5]*Al2As_sp[1]/10000),
                FRA_FON_Qpe_Jt_6 = sapflow_factor*FRA_FON_Qpe_Jt_6/(sapwood_area[6]*Al2As_sp[1]/10000),
                FRA_FON_Qpe_Jt_7 = sapflow_factor*FRA_FON_Qpe_Jt_7/(sapwood_area[7]*Al2As_sp[1]/10000),
                FRA_FON_Qpe_Jt_8 = sapflow_factor*FRA_FON_Qpe_Jt_8/(sapwood_area[8]*Al2As_sp[1]/10000))|>
  dplyr::group_by(dates)  |>
  dplyr::summarise_at(dplyr::vars(dplyr::starts_with('FRA_FON')),
                      dplyr::funs(sum(., na.rm = TRUE)))  |>
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('FRA_FON')),
                   dplyr::funs(replace(., . == 0, NA)))

measuredData<-data.frame(dates = transp_data_temp$dates,
                              E_Cb = rowMeans(transp_data_temp[,2:4], na.rm=TRUE),
                              E_Qp = rowMeans(transp_data_temp[,5:9], na.rm=TRUE))
names(measuredData)[2] = paste0("E_", CB_cohname)
names(measuredData)[3] = paste0("E_", QP_cohname)

# 11. EVALUATION PERIOD ---------------------------------------------------
# Select evaluation dates
evaluation_period <- seq(as.Date("2006-01-01"),as.Date("2006-12-31"), by="day")
measuredData <- measuredData |> filter(dates %in% evaluation_period)
meteoData <- meteoData |> filter(dates %in% evaluation_period)

# 12. REMARKS -------------------------------------------------------------
remarks <- data.frame(
  Title = c('Soil',
            'Vegetation',
            'Weather',
            'Sapflow'),
  Remark = c('Taken from SoilGrids with theta_sat and theta_res modified',
             'Plantation',
             'Windspeed is missing',
             'Sapwood area estimated from dbh for trees within missing data')
)



# 13. SAVE DATA IN FOLDER -------------------------------------------------
folder_name <- file.path('Sites_data', 'FONTAINEBLEAU')
if(!dir.exists(folder_name)) dir.create(folder_name)

write.table(siteData, file = file.path(folder_name, 'FONTAINEBLEAU_siteData.txt'),
            row.names = FALSE, sep = '\t')

write.table(treeData, file = file.path(folder_name, 'FONTAINEBLEAU_treeData.txt'),
            row.names = FALSE, sep = '\t')

write.table(shrubData, file = file.path(folder_name, 'FONTAINEBLEAU_shrubData.txt'),
            row.names = FALSE, sep = '\t')

write.table(miscData, file = file.path(folder_name, 'FONTAINEBLEAU_miscData.txt'),
            row.names = FALSE, sep = '\t')

write.table(meteoData, file = file.path(folder_name, 'FONTAINEBLEAU_meteoData.txt'),
            row.names = FALSE, sep = '\t')

write.table(soilData, file = file.path(folder_name, 'FONTAINEBLEAU_soilData.txt'),
            row.names = FALSE, sep = '\t')

write.table(terrainData, file = file.path(folder_name, 'FONTAINEBLEAU_terrainData.txt'),
            row.names = FALSE, sep = '\t')

write.table(customParams, file = file.path(folder_name, 'FONTAINEBLEAU_customParams.txt'),
            row.names = FALSE, sep = '\t')

write.table(measuredData, file = file.path(folder_name, 'FONTAINEBLEAU_measuredData.txt'),
            row.names = FALSE, sep = '\t')

write.table(remarks, file = file.path(folder_name, 'FONTAINEBLEAU_remarks.txt'),
            row.names = FALSE, sep = '\t')

