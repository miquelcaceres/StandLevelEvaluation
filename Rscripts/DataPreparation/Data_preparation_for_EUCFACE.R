## EucFACE data script
library(medfate)
library(medfateutils)
library(meteoland)
library(dplyr)
library(lubridate)
library(readxl)

data("SpParamsAU")

# 0. LOAD DATA and METADATA -----------------------------------------------
env_data <- read.csv('SourceData/Tables/EucFACE/AUS_RIC_EUC_ELE_env_data.csv')
sapf_data <- read.csv('SourceData/Tables/EucFACE/AUS_RIC_EUC_ELE_sapf_data.csv')
env_md <- read.csv('SourceData/Tables/EucFACE/AUS_RIC_EUC_ELE_env_md.csv')
site_md <- read.csv('SourceData/Tables/EucFACE/AUS_RIC_EUC_ELE_site_md.csv')
stand_md <- read.csv('SourceData/Tables/EucFACE/AUS_RIC_EUC_ELE_stand_md.csv')
plant_md <- read.csv('SourceData/Tables/EucFACE/AUS_RIC_EUC_ELE_plant_md.csv')
species_md <- read.csv('SourceData/Tables/EucFACE/AUS_RIC_EUC_ELE_species_md.csv')
psi_data <- read_xlsx('SourceData/Tables/EucFACE/AUS_RICH_EUC_PSI_sapfluxnet.xlsx', sheet = "Data")
fluxnet_data <- read.csv('SourceData/Tables/EucFACE/FLX_AU-Cum_FLUXNET2015_SUBSET_DD_2012-2014_2-4.csv')


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
  Value = c("EucFACE",
            "Australia",
            site_md$si_code,
            "Teresa Gimeno (CREAF)",
            "AU-Cum",
            "Elise Pendall (U. Western Sidney)",
            round(site_md$si_lat,5),
            round(site_md$si_long,5),
            site_md$si_elev,
            0, # < 2%
            0, #N 
            "",
            "Sandy loam",
            round(site_md$si_mat,1),
            round(site_md$si_map),
            "Eucalyptus trees in ambient (control) plots of a CO2 enrichment experiment",
            stand_md$st_lai,
            "10.1111/1365-2435.12532",
            "Eucalyptus tereticornis",
            "SpParamsAU",
            "2012-2014",
            "2012-2014")
)


# 2. TERRAIN DATA ---------------------------------------------------------
terrainData <- data.frame(
  latitude = site_md$si_lat,
  elevation = site_md$si_elev,
  aspect = 0, # Flat
  slope = 0 # Flat
)

# 3. TREE DATA ----------------------------------------------------------
# stand basal area ?
# Density 
treeData <- data.frame(
  Species = c("Eucalyptus tereticornis"),
  DBH = 21, #?
  Height = 100*stand_md$st_height,
  N = stand_md$st_density,
  Z50 = 200,
  Z95 = 3000,
  LAI = stand_md$st_lai
)
f <-emptyforest()
f$treeData <- treeData
summary(f, SpParamsAU)

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
  ID = 'EUCFACE',
  SpParamsName = "SpParamsAU",
  herbCover = 10, herbHeight = 10,
  Validation = 'global', Definitive = 'No'
)

# 7. SOIL DATA ------------------------------------------------------------
# The upper soil (up to 30–50 cm) is a loamy sand (>75% sand), slightly acidic (pH = 4.5) and with low organic C (<1%) and overall low phosphorus (Ellsworth et al., 2017). 
# At 30–70 cm depth, there is a layer of higher clay content (15%–35% clay), below which the soil is a sandy loam or sandy clay loam. 
# Between 300–350 and 450 cm depth, the soil is clay (>40% clay).
# soil depth = 1300
# coords_sf <- sf::st_sfc(sf::st_point(c(site_md$si_long+0.01,site_md$si_lat)), crs = 4326)
# soilData <- medfateutils::soilgridsParams(coords_sf,  c(300, 500, 500, 2500))
soilData <- data.frame(
  widths =  c(300, 500, 500, 2700),
  clay = c(18.16667, 31.00000, 31.10000, 30.60000),
  sand = c(61.86667, 52.46667, 52.73750,53.30000),
  om = c(1.8700000, 0.6566667, 0.6912500, 0.5600000),
  bd = c(1.246667, 1.313333,1.331250,1.350000),
  rfc = c(6.566667,20,20,90),
  VG_theta_res = rep(0.03, 4),
  VG_theta_sat = rep(0.4, 4)
)
s = soil(soilData, VG_PTF = "Toth")
sum(soil_waterExtractable(s, model="VG", minPsi = -4))

# 8. METEO DATA -----------------------------------------------------------
meteoData <- env_data |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP))) |>
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
meteoData <- meteoData[-nrow(meteoData),]
meteoData$CO2 <- 390

# 9. CUSTOM PARAMS --------------------------------------------------------
ET_index = SpParamsAU$SpIndex[SpParamsAU$Name=="Eucalyptus tereticornis"]
ET_cohname = paste0("T1_", ET_index)
et<- 1
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

# Al2As_sp <- mean(SpParamsAU$Al2As[SpParamsAU$Genus=="Eucalyptus"], na.rm=TRUE) # m2/m2
Al2As_sp <- 6896.552 # Atwell et al 2007
# Al2As_sp <- SpParamsAU$Al2As[SpParamsAU$Name=="Eucalyptus tereticornis"] # m2/m2
customParams$Al2As <- Al2As_sp
customParams$Vmax298 <- 91
customParams$Jmax298 <- 159
# 10. MEASURED DATA --------------------------------------------------------
# sapflow data, está en cm3 cm-2 h-1 y el timestep es 30 minutos, 
# cal dividir per 2 (per tenir flow en els 30 min),
# dividir per Al2As (m2/cm2), 
# multiplicar per 0.001 (per passar a de cm3 a dm3)
# Sumamos todo el día 
sapflow_factor <- 0.5/1000
transp_data_temp <- sapf_data |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  dplyr::mutate(AUS_RIC_EUC_ELE_Ete_Js_1 = sapflow_factor*AUS_RIC_EUC_ELE_Ete_Js_1/(Al2As_sp[1]/10000),
                AUS_RIC_EUC_ELE_Ete_Js_10 = sapflow_factor*AUS_RIC_EUC_ELE_Ete_Js_10/(Al2As_sp[1]/10000),
                AUS_RIC_EUC_ELE_Ete_Js_2 = sapflow_factor*AUS_RIC_EUC_ELE_Ete_Js_2/(Al2As_sp[1]/10000),
                AUS_RIC_EUC_ELE_Ete_Js_3 = sapflow_factor*AUS_RIC_EUC_ELE_Ete_Js_3/(Al2As_sp[1]/10000),
                AUS_RIC_EUC_ELE_Ete_Js_4 = sapflow_factor*AUS_RIC_EUC_ELE_Ete_Js_4/(Al2As_sp[1]/10000),
                AUS_RIC_EUC_ELE_Ete_Js_5 = sapflow_factor*AUS_RIC_EUC_ELE_Ete_Js_5/(Al2As_sp[1]/10000),
                AUS_RIC_EUC_ELE_Ete_Js_6 = sapflow_factor*AUS_RIC_EUC_ELE_Ete_Js_6/(Al2As_sp[1]/10000),
                AUS_RIC_EUC_ELE_Ete_Js_7 = sapflow_factor*AUS_RIC_EUC_ELE_Ete_Js_7/(Al2As_sp[1]/10000),
                AUS_RIC_EUC_ELE_Ete_Js_8 = sapflow_factor*AUS_RIC_EUC_ELE_Ete_Js_8/(Al2As_sp[1]/10000),
                AUS_RIC_EUC_ELE_Ete_Js_9 = sapflow_factor*AUS_RIC_EUC_ELE_Ete_Js_9/(Al2As_sp[1]/10000))|>
  dplyr::group_by(dates)  |>
  dplyr::summarise_at(dplyr::vars(dplyr::starts_with('AUS_RIC_EUC_ELE')),
                      dplyr::funs(sum(., na.rm = TRUE)))  |>
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('AUS_RIC_EUC_ELE')),
                   dplyr::funs(replace(., . == 0, NA)))

transp_data_temp2<-data.frame(dates = transp_data_temp$dates,
                              E_Et = rowMeans(transp_data_temp[,2:11], na.rm=TRUE))
names(transp_data_temp2)[2] = paste0("E_", ET_cohname)

pl_names <- plant_md$pl_name
psiData <- psi_data |>
  dplyr::filter(pl_name %in% pl_names,
                time_psi %in% c("midday", "pre-dawn")) |>
  dplyr::select("timestamp", "pl_name", "Ψ", "time_psi") |>
  dplyr::rename("Psi" = "Ψ") |>
  dplyr::mutate(time_psi = replace(time_psi, time_psi=="pre-dawn", "predawn")) |>
  dplyr::mutate(dates = date(as_datetime(timestamp, tz = 'Europe/Madrid'))) |>
  dplyr::select(-timestamp) |>
  tidyr::pivot_wider(names_from = time_psi, values_from = Psi) |>
  dplyr::group_by(dates) |>
  dplyr::summarise(WP_PD = mean(predawn, na.rm = TRUE),
                   WP_PD_ERR = sd(predawn, na.rm = TRUE),
                   WP_MD = mean(midday, na.rm = TRUE),
                   WP_MD_ERR = sd(midday, na.rm = TRUE))

names(psiData)[2] <- paste0("PD_", ET_cohname)
names(psiData)[3] <- paste0("PD_", ET_cohname, "_err")
names(psiData)[4] <- paste0("MD_", ET_cohname)
names(psiData)[5] <- paste0("MD_", ET_cohname, "_err")


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
  dplyr::left_join(fluxData, by = 'dates') |>
  dplyr::left_join(transp_data_temp2, by = 'dates') |>
  dplyr::left_join(psiData, by = 'dates')

# 11. SIMULATION/EVALUATION PERIOD ---------------------------------------------------

# 12. REMARKS -------------------------------------------------------------
remarks <- data.frame(
  Title = c('Soil',
            'Vegetation',
            'Weather',
            'Sapflow',
            'Eddy covariance'),
  Remark = c('Taken from SoilGrids',
             'No understory or secondary species considered. 10% Herbaceous cover',
             'CO2 set to 390 ppm',
             'Species-level Huber value used for scaling',
             'Variables taken: LE_CORR and GPP_NT_VUT_REF')
)



# 13. SAVE DATA IN FOLDER -------------------------------------------------
folder_name <- file.path('Sites_data', 'EUCFACE')
if(!dir.exists(folder_name)) dir.create(folder_name)

write.table(siteData, file = file.path(folder_name, 'EUCFACE_siteData.txt'),
            row.names = FALSE, sep = '\t')

write.table(treeData, file = file.path(folder_name, 'EUCFACE_treeData.txt'),
            row.names = FALSE, sep = '\t')

write.table(shrubData, file = file.path(folder_name, 'EUCFACE_shrubData.txt'),
            row.names = FALSE, sep = '\t')

write.table(miscData, file = file.path(folder_name, 'EUCFACE_miscData.txt'),
            row.names = FALSE, sep = '\t')

write.table(meteoData, file = file.path(folder_name, 'EUCFACE_meteoData.txt'),
            row.names = FALSE, sep = '\t')

write.table(soilData, file = file.path(folder_name, 'EUCFACE_soilData.txt'),
            row.names = FALSE, sep = '\t')

write.table(terrainData, file = file.path(folder_name, 'EUCFACE_terrainData.txt'),
            row.names = FALSE, sep = '\t')

write.table(customParams, file = file.path(folder_name, 'EUCFACE_customParams.txt'),
            row.names = FALSE, sep = '\t')

write.table(measuredData, file = file.path(folder_name, 'EUCFACE_measuredData.txt'),
            row.names = FALSE, sep = '\t')

write.table(remarks, file = file.path(folder_name, 'EUCFACE_remarks.txt'),
            row.names = FALSE, sep = '\t')

