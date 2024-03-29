## canbalasc data script
library(medfate)
library(medfateutils)
library(meteoland)
library(dplyr)
library(lubridate)
library(readxl)

data("SpParamsES")

# 0. LOAD DATA and METADATA -----------------------------------------------
env_data <- read.csv('SourceData/Tables/CanBalasc/ESP_CAN_env_data.csv')
sapf_data <- read.csv('SourceData/Tables/CanBalasc/ESP_CAN_sapf_data.csv')
env_md <- read.csv('SourceData/Tables/CanBalasc/ESP_CAN_env_md.csv')
site_md <- read.csv('SourceData/Tables/CanBalasc/ESP_CAN_site_md.csv')
stand_md <- read.csv('SourceData/Tables/CanBalasc/ESP_CAN_stand_md.csv')
plant_md <- read.csv('SourceData/Tables/CanBalasc/ESP_CAN_plant_md.csv')
species_md <- read.csv('SourceData/Tables/CanBalasc/ESP_CAN_species_md.csv')


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
  Value = c("Can Balasc",
            "Spain",
            "ESP_CAN",
            "Elisenda Sánchez-Costa (IDAEA-CSIC)",
            "",
            "",
            41.43099,
            2.0736,
            270,
            0.86,
            90,
            "Shales and granite",
            "Sandy loam",
            17.0,
            585,
            "Mixed forest dominated by Q. ilex",
            3.2,
            "10.1016/j.agrformet.2015.03.012",
            "Quercus ilex, Quercus pubescens, Pinus halepensis, Arbutus unedo",
            "SpParamsES",
            "2011-2012",
            "2011-2012")
)


# 2. TERRAIN DATA ---------------------------------------------------------
terrainData <- data.frame(
  latitude = 41.43099,
  elevation = 270,
  aspect = 90,
  slope = 0.86 # < 2%, calculado a partir de una pendiente del 1.5%
)

# 3. TREE DATA ----------------------------------------------------------
treeData <- data.frame(
  Species = c("Arbutus unedo", "Pinus halepensis", "Quercus pubescens", "Quercus ilex"), # A.unedo / P.halepensis / Q.pubescens / Q.ilex
  DBH = c(9.6, 33.7, 12, 11.9),
  Height = c(810, 1710, 960, 1020),
  N = c(76, 53, 150, 1150),
  Z50 = c(390, 300, 529, 529),
  Z95 = c(1470,1200,2287,2287)
)
f = emptyforest()
f$treeData = treeData
treeData$LAI <- species_LAI(f, SpParamsES)
treeData$LAI <- treeData$LAI*(3.2/sum(treeData$LAI)) ## CORRECT LAI = 3.2
rm(f)

# 4. SHRUB DATA -----------------------------------------------------------
# shrubData <- data.frame(
#   Species = numeric(0), 
#   Cover = numeric(0),
#   Height = numeric(0),
#   Z50 = numeric(0),
#   Z95 = numeric(0)
# )
shrubData <- data.frame(
  Species = c("Arbutus unedo", "Phillyrea angustifolia", "Pistacia lentiscus", "Quercus ilex",  "Viburnum spp."), # Arbutus unedo, Phillyrea angustifolia, Pistacea lentiscus, Quercus ilex, Viburnum spp.
  Cover = c(4.83, 7.25, 13.5, 9.67, 9.67),
  Height = c(174, 153.33, 118.33, 78, 138.33),
  Z50 = rep(NA, 5),
  Z95 = rep(NA, 5)
)

# 5. SEED DATA ------------------------------------------------------------
# there is no seed info

# 6. MISC DATA ------------------------------------------------------------
miscData <- data.frame(
  ID = 'CANBALASC',
  SpParamsName = "SpParamsES",
  herbCover = 5, herbHeight = 20,
  Validation = 'global_transp', Definitive = 'Yes'
)

# 7. SOIL DATA ------------------------------------------------------------
# Obetnidos de los datos de Antoine
# 
# DESCRIPCIÓ DEL SÒL DE LA PARCELA PERMANENT D’ALZINAR DE CAN BALASC (COLLSEROLA)
# Xavier Domene, Pol Oliveras, Anna Àvila, Irene Fraile, Josep M. Alcañiz
# Març 2019
#
# 0-300 mm <-> A/B [9-25/30]
# 300-400 mm <-> B
# 400-600 mm <-> Ct
# 600-4000 mm <-> Ct (rock)
soilData <- data.frame(
  widths = c(300, 700, 1000,2500),
  clay = c(20.23, 24.58, 27.66, 27.66),
  sand = c(48.9, 52.4, 45.6,45.6),
  om = c(2.7, 1, 0.61, 0.61),
  bd = c(1.5, 1.5,1.5,1.5),#?
  rfc = c(20,30,85,90)
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
                   WindSpeed = mean(ws, na.rm = TRUE),
                   Radiation = (sum((sw_in * 900), na.rm = TRUE)/(24*3600)), # W/m2, a W/m2 en el día
                   Precipitation = sum(precip, na.rm = TRUE)) |>
  dplyr::mutate(Radiation = Radiation*3600*24/1000000) |> # w/m2 to MJ/m2/day
  dplyr::mutate_at(dplyr::vars(dplyr::ends_with('Humidity')),
                   dplyr::funs(replace(., . > 100, 100))) |>
  dplyr::mutate_at(dplyr::vars(dplyr::ends_with('Speed')),
                   dplyr::funs(replace(., is.nan(.), NA))) |>
  as.data.frame()
meteoData <- meteoData[!is.na(meteoData$dates),] 




# 9. CUSTOM PARAMS --------------------------------------------------------
AU_index = SpParamsES$SpIndex[SpParamsES$Name=="Arbutus unedo"]
PH_index = SpParamsES$SpIndex[SpParamsES$Name=="Pinus halepensis"]
QP_index = SpParamsES$SpIndex[SpParamsES$Name=="Quercus pubescens"]
QI_index = SpParamsES$SpIndex[SpParamsES$Name=="Quercus ilex"]

AU_cohname = paste0("T1_", AU_index)
PH_cohname = paste0("T2_", PH_index)
QP_cohname = paste0("T3_", QP_index)
QI_cohname = paste0("T4_", QI_index)

au <- 1
ph <- 2
qp <- 3
qi<- 4
customParams <- data.frame(
  Species = c("Arbutus unedo", "Pinus halepensis", "Quercus pubescens", "Quercus ilex",
              "Phillyrea angustifolia", "Pistacia lentiscus", "Viburnum spp."),
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

Al2As_sp = c(mean(10000/As2Al[c(10,11,13,14)]), 
             mean(10000/As2Al[c(2,20,21)]), 
             mean(10000/As2Al[c(12,18:19)]),
             mean(10000/As2Al[c(4:9)]))
customParams$Al2As[1:4] = Al2As_sp


customParams$VCleaf_kmax[ph] <- 4.0
customParams$VCleaf_kmax[qi] <- 2.63

customParams$VCroot_P12[ph] <- -1.0
customParams$VCroot_P50[ph] <- -1.741565
customParams$VCroot_P88[ph] <- -2.301482


slope <- 46
P50 <- -4.79
P88 <- P50 + log((100.0/88.0)-1.0)*(25.0/slope)
P12 <- P50 + log((100.0/12.0)-1.0)*(25.0/slope)
customParams$VCstem_P12[ph] <- P12
customParams$VCstem_P50[ph] <- P50
customParams$VCstem_P88[ph] <- P88
customParams$VCstem_slope[ph] <- slope

slope <- 30
P50 <- -6.4
P88 <- P50 + log((100.0/88.0)-1.0)*(25.0/slope)
P12 <- P50 + log((100.0/12.0)-1.0)*(25.0/slope)
customParams$VCstem_P12[qi] <- P12
customParams$VCstem_P50[qi] <- P50
customParams$VCstem_P88[qi] <- P88
customParams$VCstem_slope[qi] <- slope


customParams$LeafEPS[qi] <- 15 
customParams$LeafPI0[qi] <- -2.5
customParams$LeafAF[qi] <- 0.4
customParams$LeafEPS[ph] <- 5.31 
customParams$LeafPI0[ph] <- -1.5
customParams$LeafAF[ph] <- 0.6

customParams$StemEPS[qi] <- 15 
customParams$StemPI0[qi] <- -2.5
customParams$StemAF[qi] <- 0.4
customParams$StemEPS[ph] <- 5
customParams$StemPI0[ph] <- -1.65
customParams$StemAF[ph] <- 0.4

customParams$Gswmax[qi] <- 0.220 
customParams$Gswmax[ph] <- 0.2175 
customParams$Gswmin[qi] <- 0.002 
customParams$Gswmin[ph] <- 0.001 


# P. angustifolia	P.latifolia	Qilex	Phalepensis	Phalepensis_OLD
# P12_gs	-2.2	 -1.65	-1	-1.36	-1.36
# P88_gs	-3.8	 -2.5	-2.7	 -2.14	-2.33
customParams$Gs_slope[qi] <- (88.0 - 12.0)/(2.7 - 1);
customParams$Gs_P50[qi] <- -1.0 + log(0.12/0.88)/(customParams$Gs_slope[qi]/25)
customParams$Gs_slope[ph] <- (88.0 - 12.0)/(2.14 - 1.36);
customParams$Gs_P50[ph] <- -1.36 + log(0.12/0.88)/(customParams$Gs_slope[ph]/25)


# 
# # par_QI = hydraulics_psi2Weibull(-6.5,-8.5)
# par_leaf_PN = SpParamsES[SpParamsES$Name=="Pinus nigra", c("VCleaf_c", "VCleaf_d")]
# customParams <- data.frame(
#   SpIndex = c(AU_index, PH_index, QP_index, QI_index),
#   Cohort = c(AU_cohname, PH_cohname, QP_cohname, QI_cohname),
#   # Species specific
#   g = c(0.8,1.0,0.8,0.8),
#   Kmax_stemxylem = c(NA,NA,0.7,0.4), 
#   VCleaf_kmax= c(4,4,5,2.63),
#   VCleaf_c = c(NA, par_leaf_PN$VCleaf_c, NA,5.41),
#   VCleaf_d = c(NA, par_leaf_PN$VCleaf_d, NA,-4.18),
#   Jmax298 = c(NA, NA, 150, NA),
#   Gwmax = c(NA, NA, 0.2, NA),
#   LeafPI0 = c(NA,NA,NA,-2.66),
#   LeafEPS = c(NA,NA,NA,10.57),
#   LeafAF = c(NA,NA,NA,0.43),
#   
#   # VCstem_c = c(NA,NA,NA,par_QI[["c"]]),
#   # VCstem_d = c(NA,NA,NA,par_QI[["d"]]),
#   
#   LAI_live = c(0.1, 0.3, 0.3, 2.5),
#   LAI_expanded = c(0.1, 0.3, 0.3, 2.5)
# )

# 10. MEASURED DATA --------------------------------------------------------
# sapflow data, está en cm3 cm-2 h-1 of leaf area, y el timestep es 15 minutos, así que tenemos que
# multiplicar por 15*60 segundos para los l/m-2 en el timestep.
# Agrego los datos de sapflow por día, promedio todos los árboles 
transp_data_temp <- sapf_data |>
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('ESP_CAN')),
                   dplyr::funs(.*0.25*1e4/1e3))|>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  dplyr::group_by(dates) |>
  dplyr::summarise_at(dplyr::vars(dplyr::starts_with('ESP_CAN')),
                      dplyr::funs(sum(., na.rm = TRUE))) |>
  # remove the zeroes generated previously
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('ESP_CAN')),
                   dplyr::funs(replace(., . == 0, NA)))
transp_data_temp$E_Au <- rowMeans(transp_data_temp[,19:22], na.rm=TRUE)
transp_data_temp$E_Ph <- rowMeans(transp_data_temp[,9:11], na.rm=TRUE)
transp_data_temp$E_Qi <- rowMeans(transp_data_temp[,12:18], na.rm=TRUE)
transp_data_temp$E_Qp <- rowMeans(transp_data_temp[,2:8], na.rm=TRUE)
transp_data_temp2 <- transp_data_temp |>
  dplyr::select(dates, E_Au, E_Ph, E_Qi, E_Qp)
names(transp_data_temp2)[2:5] <-paste0("E_", c(AU_cohname, PH_cohname, QP_cohname, QI_cohname))


measuredData <- env_data |>
  dplyr::mutate(dates = date(as_datetime(TIMESTAMP, tz = 'Europe/Madrid'))) |>
  dplyr::select(dates, swc_shallow) |>
  dplyr::group_by(dates) |>
  dplyr::summarise(SWC = mean(swc_shallow, na.rm = TRUE)) |>
  dplyr::left_join(transp_data_temp2, by = 'dates') 

# 11. SIMULATION/EVALUATION PERIOD ---------------------------------------------------
simulation_period <- seq(as.Date("2011-01-01"),as.Date("2012-12-31"), by="day")
evaluation_period <- seq(as.Date("2011-01-01"),as.Date("2012-12-31"), by="day")
meteoData <- meteoData |> filter(dates %in% simulation_period)
measuredData <- measuredData |> filter(dates %in% evaluation_period)

# 12. REMARKS -------------------------------------------------------------
remarks <- data.frame(
  Title = c('Soil',
            'Vegetation',
            'Sapflow'),
  Remark = c('Soil description from local samples',
             'Understory composed of multiple species',
             'No scaling required (already per leaf area) but tree selection may be required')
)

# 13. SAVE DATA IN FOLDER -------------------------------------------------
folder_name <- file.path('Sites_data', 'CANBALASC')

write.table(siteData, file = file.path(folder_name, 'CANBALASC_siteData.txt'),
            row.names = FALSE, sep = '\t')

write.table(treeData, file = file.path(folder_name, 'CANBALASC_treeData.txt'),
            row.names = FALSE, sep = '\t')

write.table(shrubData, file = file.path(folder_name, 'CANBALASC_shrubData.txt'),
            row.names = FALSE, sep = '\t')

write.table(miscData, file = file.path(folder_name, 'CANBALASC_miscData.txt'),
            row.names = FALSE, sep = '\t')

write.table(meteoData, file = file.path(folder_name, 'CANBALASC_meteoData.txt'),
            row.names = FALSE, sep = '\t')

write.table(soilData, file = file.path(folder_name, 'CANBALASC_soilData.txt'),
            row.names = FALSE, sep = '\t')

write.table(terrainData, file = file.path(folder_name, 'CANBALASC_terrainData.txt'),
            row.names = FALSE, sep = '\t')

write.table(customParams, file = file.path(folder_name, 'CANBALASC_customParams.txt'),
            row.names = FALSE, sep = '\t')

write.table(measuredData, file = file.path(folder_name, 'CANBALASC_measuredData.txt'),
            row.names = FALSE, sep = '\t')
 
write.table(remarks, file = file.path(folder_name, 'CANBALASC_remarks.txt'),
            row.names = FALSE, sep = '\t')



