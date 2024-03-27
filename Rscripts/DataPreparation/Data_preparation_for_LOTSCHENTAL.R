## Switzerland Lotschental data script
library(medfate)
library(medfateutils)
library(meteoland)
library(dplyr)
library(lubridate)
library(readxl)
data("SpParamsFR")

# 0. LOAD DATA and METADATA -----------------------------------------------
env_data <- read.csv('SourceData/Tables/Lotschental/CHE_LOT_NOR_env_data.csv')
sapf_data <- read.csv('SourceData/Tables/Lotschental/CHE_LOT_NOR_sapf_data.csv')
env_md <- read.csv('SourceData/Tables/Lotschental/CHE_LOT_NOR_env_md.csv')
site_md <- read.csv('SourceData/Tables/Lotschental/CHE_LOT_NOR_site_md.csv')
stand_md <- read.csv('SourceData/Tables/Lotschental/CHE_LOT_NOR_stand_md.csv')
plant_md <- read.csv('SourceData/Tables/Lotschental/CHE_LOT_NOR_plant_md.csv')
species_md <- read.csv('SourceData/Tables/Lotschental/CHE_LOT_NOR_species_md.csv')
meteo_blatten <- read.csv('SourceData/Tables/Lotschental/blatten_data.csv', sep = ";", blank.lines.skip = TRUE,
                          na.strings = "-")
env_antoine <- read.csv('SourceData/Tables/Lotschental/Environmental data for miquel - 2024-03-19.csv') |>
  tidyr::pivot_wider(names_from = "Variable", values_from = "Value")

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
  Value = c("Lötschental",
            "Switzerland",
            site_md$si_code,
            "Patrick Fonti (WSL)",
            "",
            "",
            round(site_md$si_lat,4),
            round(site_md$si_long,4),
            site_md$si_elev,
            36.87, # 60%
            0, #N 
            "Calcareous",
            "Loam",
            round(site_md$si_mat,1),
            round(site_md$si_map),
            "Mixed evergreen Norway spruce and deciduous European larch forest",
            3,
            "10.1016/j.agrformet.2012.08.002",
            "Picea abies, Larix decidua subsp. decidua",
            "SpParamsFR",
            "2013-2016",
            "2013-2016")
)


# 2. TERRAIN DATA ---------------------------------------------------------
terrainData <- data.frame(
  latitude = site_md$si_lat,
  elevation = site_md$si_elev,
  aspect = 0, # N
  slope = 36.87 # 60% 
)

# 3. TREE DATA ----------------------------------------------------------
# stand basal area = 55.87 (26.4 Larix)
# Larix decidua subsp. decidua 26.4, Picea abies 29.43
# Density 
treeData <- data.frame(
  Species = c("Larix decidua subsp. decidua", "Picea abies"),
  DBH = c(mean(plant_md$pl_dbh[plant_md$pl_species=="Larix decidua"],na.rm=TRUE),
          mean(plant_md$pl_dbh[plant_md$pl_species=="Picea abies"],na.rm=TRUE)), # from paper
  Height = 100*c(mean(plant_md$pl_height[plant_md$pl_species=="Larix decidua"],na.rm=TRUE),
                 mean(plant_md$pl_height[plant_md$pl_species=="Picea abies"],na.rm=TRUE)),
  N = 732*c(26.4, 29.43)/55.87,
  Z50 = NA,
  Z95 = NA,
  LAI = 3*c(26.4,29.43)/55.87  # Divide according to basal area proportions
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
  ID = 'LOTSCHENTAL',
  SpParamsName = "SpParamsFR",
  herbCover = 0, herbHeight = 0,
  Validation = 'global', Definitive = 'No'
)

# 7. METEO DATA -----------------------------------------------------------
meteoData <- env_data |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid')),
                rh = (vpd*100)/(0.61078*exp((17.269*ta)/(237.3+ta)))) |> # no hay rh, asi que transformo la vpd
  dplyr::group_by(dates) |>
  dplyr::summarise(MinTemperature = min(ta, na.rm = TRUE),
                   MaxTemperature = max(ta, na.rm = TRUE),
                   MinRelativeHumidity = min(rh, na.rm = TRUE),
                   MaxRelativeHumidity = max(rh, na.rm = TRUE)) |>
  dplyr::mutate_at(dplyr::vars(2:5),
                   dplyr::funs(replace(., is.infinite(.), NA))) |>
  dplyr::mutate_at(dplyr::vars(2:5),
                   dplyr::funs(replace(., is.nan(.), NA))) 

meteoData_blatten <- meteo_blatten |>
  dplyr::mutate(dates = as.Date(substr(time,1,8), format="%Y%m%d")) |>
  dplyr::group_by(dates) |>
  dplyr::summarise(MinTemperature = min(tre200h0, na.rm = TRUE),
                   MaxTemperature = max(tre200h0, na.rm = TRUE),
                   MinRelativeHumidity = min(ure200h0, na.rm = TRUE),
                   MaxRelativeHumidity = max(ure200h0, na.rm = TRUE),
                   WindSpeed = mean(fkl010h0, na.rm = TRUE),
                   Radiation = mean(gre000h0, na.rm = TRUE), 
                   Precipitation = sum(rre150h0, na.rm = TRUE)) |>
  dplyr::mutate(Radiation = Radiation*3600*24/1000000) |>
  dplyr::mutate_at(dplyr::vars(2:5),
                   dplyr::funs(replace(., is.infinite(.), NA))) |>
  dplyr::mutate_at(dplyr::vars(2:5),
                   dplyr::funs(replace(., is.nan(.), NA))) 

meteoData <- meteoData |>
  dplyr::left_join(meteoData_blatten[,c("dates", "Radiation", "Precipitation", "WindSpeed")], by="dates")

meteoData <- dplyr::bind_rows(meteoData,
                              meteoData_blatten[!(meteoData_blatten$dates %in% meteoData$dates),])

# 8. SOIL DATA ------------------------------------------------------------
# coords_sf <- sf::st_sfc(sf::st_point(c(site_md$si_long,site_md$si_lat)), crs = 4326)
# soilData <- medfateutils::soilgridsParams(coords_sf,  c(300, 700, 1000, 2500))
soilData <- data.frame(
  widths = c(300, 700, 1000, 2000),
  clay = c(23.60, 22.05, 22.80, 22.80),
  sand = c(42.43333, 46.25000, 46.10000,46.10000),
  om = c(6.896667, 2.560000, 2.350000, 0),
  bd = c(1.140, 1.415,1.490,1.490),
  rfc = c(45,80,90,99),
  VG_theta_sat = rep(0.28, 4),
  VG_theta_res = rep(0.07, 4)
)
s = soil(soilData, VG_PTF = "Toth")
sum(soil_waterExtractable(s, model="VG", minPsi = -4))


# 9. CUSTOM PARAMS --------------------------------------------------------
LD_index = SpParamsFR$SpIndex[SpParamsFR$Name=="Larix decidua subsp. decidua"]
PA_index = SpParamsFR$SpIndex[SpParamsFR$Name=="Picea abies"]
LD_cohname = paste0("T1_", LD_index)
PA_cohname = paste0("T2_", PA_index)
ld<- 1
pa<-2

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

Al2As_sp = c(SpParamsFR$Al2As[SpParamsFR$Name=="Larix"],
             SpParamsFR$Al2As[SpParamsFR$Name=="Picea abies"])
customParams$Al2As <- Al2As_sp
customParams$LeafAngle <- c(30,42)
customParams$LeafAngleSD <- c(21,2)
customParams$Kmax_stemxylem[ld] <- 20  #Larix sibirica
  
# 10. MEASURED DATA --------------------------------------------------------
# sapflow data, está en cm3 cm-2 h-1, y el timestep es 15 minutos, 
# cal dividir per 4 (per tenir flow en els 15 min), dividir por Al2As, multiplicar per 0.001 (per passar a de cm3 a dm3)
# Sumamos todo el día 
sapflow_factor <- 0.25/1000
transp_data_temp <- sapf_data |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  dplyr::mutate(CHE_LOT_NOR_Lde_Js_1 = sapflow_factor*CHE_LOT_NOR_Lde_Js_1/(Al2As_sp[1]/10000),
                CHE_LOT_NOR_Lde_Js_2 = sapflow_factor*CHE_LOT_NOR_Lde_Js_2/(Al2As_sp[1]/10000),
                CHE_LOT_NOR_Lde_Js_3 = sapflow_factor*CHE_LOT_NOR_Lde_Js_3/(Al2As_sp[1]/10000),
                CHE_LOT_NOR_Pab_Js_4 = sapflow_factor*CHE_LOT_NOR_Pab_Js_4/(Al2As_sp[2]/10000),
                CHE_LOT_NOR_Pab_Js_5 = sapflow_factor*CHE_LOT_NOR_Pab_Js_5/(Al2As_sp[2]/10000),
                CHE_LOT_NOR_Pab_Js_6 = sapflow_factor*CHE_LOT_NOR_Pab_Js_6/(Al2As_sp[2]/10000))|>
  dplyr::group_by(dates)  |>
  dplyr::summarise_at(dplyr::vars(dplyr::starts_with('CHE_LOT_NOR')),
                      dplyr::funs(sum(., na.rm = TRUE)))  |>
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('CHE_LOT_NOR')),
                   dplyr::funs(replace(., . == 0, NA)))

transp_data_temp2<-data.frame(dates = transp_data_temp$dates,
                              E_Lde = rowMeans(transp_data_temp[,c(2,3,4)], na.rm=TRUE),
                              E_Pab = rowMeans(transp_data_temp[, c(5,6,7)], na.rm=TRUE))

# measuredData <- env_data %>%
#   dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
#   group_by(dates) |>
#   summarise(SWC = mean(swc_shallow),
#             SWC_2 = mean(swc_deep)) |>
#   dplyr::mutate(SWC_err = NA)|>
#   dplyr::full_join(transp_data_temp2, by = 'dates')|>
#   dplyr::arrange(dates) 
# names(measuredData)[5:6] = c(paste0("E_", LD_cohname),
#                              paste0("E_", PA_cohname))

measuredData <- env_antoine |>
  dplyr::mutate(dates = as.Date(substr(Date_time,1,10)),
                SWC = rowMeans(env_antoine[,names(env_antoine) %in% c("SM0 10cm [unitless]", "SM2 10cm [unitless]","SM3 10cm [unitless]","SM4 10cm [unitless]")], na.rm=TRUE)/100) |>
  dplyr::select(dates, SWC) |>
  dplyr::group_by(dates) |>
  dplyr::summarise(SWC = mean(SWC, na.rm=TRUE), .groups = "drop") |>
  dplyr::full_join(transp_data_temp2, by = 'dates')|>
  dplyr::arrange(dates) 
names(measuredData)[3:4] = c(paste0("E_", LD_cohname),
                             paste0("E_", PA_cohname))

 
# 11. EVALUATION PERIOD ---------------------------------------------------
simulation_period <- seq(as.Date("2013-05-30"),as.Date("2016-12-31"), by="day")
evaluation_period <- seq(as.Date("2013-05-30"),as.Date("2016-12-31"), by="day")
meteoData <- meteoData |> filter(dates %in% simulation_period)
meteoData_blatten <- meteoData_blatten |> filter(dates %in% simulation_period)
measuredData <- measuredData |> filter(dates %in% evaluation_period)

#fill gaps with meteodata blatten
meteoData$MinTemperature[is.na(meteoData$MinTemperature)] <- meteoData_blatten$MinTemperature[is.na(meteoData$MinTemperature)]
meteoData$MaxTemperature[is.na(meteoData$MaxTemperature)] <- meteoData_blatten$MaxTemperature[is.na(meteoData$MaxTemperature)]
meteoData$MinRelativeHumidity[is.na(meteoData$MinRelativeHumidity)] <- meteoData_blatten$MinRelativeHumidity[is.na(meteoData$MinRelativeHumidity)]
meteoData$MaxRelativeHumidity[is.na(meteoData$MaxRelativeHumidity)] <- meteoData_blatten$MaxRelativeHumidity[is.na(meteoData$MaxRelativeHumidity)]
meteoData$Radiation[is.na(meteoData$Radiation)] <- meteoData$Radiation[which(is.na(meteoData$Radiation))+1]

# 12. REMARKS -------------------------------------------------------------
remarks <- data.frame(
  Title = c('Soil',
            'Vegetation',
            'Weather',
            'Soil moisture'),
  Remark = c('Taken from SoilGrids with theta_sat and theta_res modified',
             'Understory not considered. LAI not available',
             'Complemented with weather station in the same valley for Radiation, Precipitation and WindSpeed (2013-2016); and all variables for 2016',
             'Provided by A. Cabon')
)



# 13. SAVE DATA IN FOLDER -------------------------------------------------
folder_name <- file.path('Sites_data', 'LOTSCHENTAL')

write.table(siteData, file = file.path(folder_name, 'LOTSCHENTAL_siteData.txt'),
            row.names = FALSE, sep = '\t')

write.table(treeData, file = file.path(folder_name, 'LOTSCHENTAL_treeData.txt'),
            row.names = FALSE, sep = '\t')

write.table(shrubData, file = file.path(folder_name, 'LOTSCHENTAL_shrubData.txt'),
            row.names = FALSE, sep = '\t')

write.table(miscData, file = file.path(folder_name, 'LOTSCHENTAL_miscData.txt'),
            row.names = FALSE, sep = '\t')

write.table(meteoData, file = file.path(folder_name, 'LOTSCHENTAL_meteoData.txt'),
            row.names = FALSE, sep = '\t')

write.table(soilData, file = file.path(folder_name, 'LOTSCHENTAL_soilData.txt'),
            row.names = FALSE, sep = '\t')

write.table(terrainData, file = file.path(folder_name, 'LOTSCHENTAL_terrainData.txt'),
            row.names = FALSE, sep = '\t')

write.table(customParams, file = file.path(folder_name, 'LOTSCHENTAL_customParams.txt'),
            row.names = FALSE, sep = '\t')

write.table(measuredData, file = file.path(folder_name, 'LOTSCHENTAL_measuredData.txt'),
            row.names = FALSE, sep = '\t')

write.table(remarks, file = file.path(folder_name, 'LOTSCHENTAL_remarks.txt'),
            row.names = FALSE, sep = '\t')

