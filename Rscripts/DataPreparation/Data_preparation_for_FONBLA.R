## Font blanche Q. ilex/P. halepensis/Phillyrea latifolia data script
library(medfate)
library(meteoland)
library(dplyr)
library(lubridate)
library(readxl)
data("SpParamsMED")


# 0. LOAD DATA and METADATA -----------------------------------------------
env_data2008 <- read_xlsx("SourceData/Tables/fontblanche/meteofontblanche.xlsx", sheet = "2008")
env_data2009 <- read_xlsx("SourceData/Tables/fontblanche/meteofontblanche.xlsx", sheet = "2009")
env_data2010 <- read_xlsx("SourceData/Tables/fontblanche/meteofontblanche.xlsx", sheet = "2010")
env_data2011 <- read_xlsx("SourceData/Tables/fontblanche/meteofontblanche.xlsx", sheet = "2011")
env_data2012 <- read_xlsx("SourceData/Tables/fontblanche/meteofontblanche.xlsx", sheet = "2012")
env_data2013 <- read_xlsx("SourceData/Tables/fontblanche/meteofontblanche.xlsx", sheet = "2013")
env_data2014 <- read_xlsx("SourceData/Tables/fontblanche/meteofontblanche.xlsx", sheet = "2014")
env_data2015 <- read_xlsx("SourceData/Tables/fontblanche/meteofontblanche.xlsx", sheet = "2015")
env_data2016 <- read_xlsx("SourceData/Tables/fontblanche/meteofontblanche.xlsx", sheet = "2016")
env_data2017 <- read_xlsx("SourceData/Tables/fontblanche/meteofontblanche.xlsx", sheet = "2017")
env_data2018 <- read_xlsx("SourceData/Tables/fontblanche/meteofontblanche.xlsx", sheet = "2018")
sapf_data2014 <-read_xlsx("SourceData/Tables/fontblanche/2014_TD_sap_velocity_water_potential.xlsx")
plant_md <- read.csv('SourceData/Tables/fontblanche/TD.csv', sep=" ")


QI_index = SpParamsMED$SpIndex[SpParamsMED$Name=="Quercus ilex"]
PL_index = SpParamsMED$SpIndex[SpParamsMED$Name=="Phillyrea latifolia"]
PH_index = SpParamsMED$SpIndex[SpParamsMED$Name=="Pinus halepensis"]

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
  Value = c("Font-Blanche",
            "France",
            "",
            "",
            "FR-Fbn",
            "Nicolas Martin-StPaul (INRAE)",
            43.24,
            5.68,
            420,
            0,
            0,
            "Cretaceous limestone",
            "Clay loam",
            13.5,
            722,
            "Mixed forest with P. halepensis and Q. ilex",
            2.0,
            "Quercus ilex, Pinus halepensis, Phillyrea latifolia",
            "2014",
            "10.1016/j.agrformet.2021.108472")
)

# 2. TERRAIN DATA ---------------------------------------------------------
terrainData <- data.frame(
  latitude = 43.24,
  elevation = 420,
  aspect = 0, # Flat
  slope = 0 # Flat
)

# 3. TREE DATA ----------------------------------------------------------
plant_md = plant_md[plant_md$espece %in% c("cv", "pa", "ph"),]
plant_md$Ht = apply(plant_md[,c("ht2009","ht2011")],1, function(x) {mean(x,na.rm=T)})
plant_md$DBH = apply(plant_md[,c("dbh2009","dbh2011")],1, function(x) {mean(x,na.rm=T)})
treeData = data.frame(Species = NA, N =1, DBH = plant_md$DBH, 
                      Height = plant_md$Ht,
                      Z50 = 200, Z95 = 1000)
treeData$Species[plant_md$espece=="cv"] = "Quercus ilex"
treeData$Species[plant_md$espece=="pa"] = "Pinus halepensis"
treeData$Species[plant_md$espece=="ph"] = "Phillyrea latifolia"
for(sp in c("Quercus ilex","Pinus halepensis","Phillyrea latifolia")) {
  td = treeData[treeData$Species==sp,]
  td$DBH[is.na(td$DBH)] = mean(td$DBH, na.rm=T)
  td$Height[is.na(td$Height)] = mean(td$Height, na.rm=T)
  treeData[treeData$Species==sp,] = td
}
treeData$N = 10000*1/625
f = emptyforest()
f$treeData = treeData
treeData$LAI = plant_LAI(f, SpParamsMED)
treeDataDetailed = treeData
tapply(treeDataDetailed$LAI, treeDataDetailed$Species, sum)
sum(treeDataDetailed$LAI)

treeData <- data.frame(
  Species = names(tapply(treeData$DBH, treeData$Species, mean)), 
  DBH = tapply(treeData$DBH, treeData$Species, mean),
  Height = tapply(treeData$Height, treeData$Species, mean),
  N = tapply(treeData$N, treeData$Species, sum),
  Z50 = c(390,300,500),  
  Z95 = c(1470,1200,2287),  
  row.names = NULL
)
f = emptyforest()
f$treeData = treeData
treeData$LAI <- species_LAI(f, SpParamsMED)
treeData$LAI <- treeData$LAI*(2.7/sum(treeData$LAI)) ## CORRECT LAI = 2.7
rm(f)

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
  ID = 'FONBLA',
  SpParamsName = "SpParamsMED",
  herbCover = 10, herbHeight = 20,
  Validation = 'global', Definitive = 'Yes'
)

# 7. SOIL DATA ------------------------------------------------------------
# As Puechabon
soilData <- data.frame(
  widths = c(300, 700, 1000, 2500),
  clay = c(39, 39, 39, 39),
  sand = c(26, 26, 26, 26), #14
  om = c(6, 3, 1, 1),
  bd = c(1.45, 1.45, 1.45, 1.45),
  rfc = c(50,65,90,95)
)
s=soil(soilData)
sum(soil_waterExtractable(s, model="VG", minPsi = -4))


# 8. METEO DATA -----------------------------------------------------------
env_data = rbind(env_data2008, env_data2009, env_data2010, env_data2011, env_data2012, env_data2013,
                 env_data2014, env_data2015, env_data2016, env_data2017, env_data2018)
dates = env_data$date
vps_Tmean = numeric(length(dates))
vps_Tmin = numeric(length(dates))
vps_Tmax = numeric(length(dates))
for(i in 1:nrow(env_data)) {
  vps_Tmean[i] = meteoland::utils_saturationVP(env_data$Ta_mean[i])
  vps_Tmin[i] = meteoland::utils_saturationVP(env_data$Ta_min[i])
  vps_Tmax[i] = meteoland::utils_saturationVP(env_data$Ta_max[i])
}
vpa = vps_Tmean*env_data$Rh_mean/100
meteoData <- data.frame(dates = dates,
                        MeanTemperature = env_data$Ta_mean,
                        MinTemperature = env_data$Ta_min,
                        MaxTemperature = env_data$Ta_max,
                        MeanRelativeHumidity = env_data$Rh_mean,
                        MinRelativeHumidity = pmax(0, 100*vpa/vps_Tmax),
                        MaxRelativeHumidity = pmin(100, 100*vpa/vps_Tmin),
                        WindSpeed = env_data$WS_17m_mean/3.6, # From km/h to m/s
                        Precipitation = env_data$P,
                        Radiation = env_data$Rg_tot)
meteo_sel = complete.cases(meteoData)
meteoData <- meteoData[meteo_sel,]


# 9. CUSTOM PARAMS --------------------------------------------------------
PL_cohname = paste0("T1_", PL_index)
PH_cohname = paste0("T2_", PH_index)
QI_cohname = paste0("T3_", QI_index)
pl <- 1
ph <- 2
qi <- 3
customParams <- data.frame(
  Species = c("Phillyrea latifolia", "Pinus halepensis", "Quercus ilex"),
  # Cohort = c(PL_cohname,PH_cohname, QI_cohname),
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
  Al2As = c(NA,631,1540.671)) # PH-CB = 631, QI-FRAPUE = 1540.671)

customParams$VCleaf_kmax[pl] <- 3.0
customParams$VCleaf_kmax[ph] <- 4.0
customParams$VCleaf_kmax[qi] <- 2.63

customParams$VCroot_P12[ph] <- -1.0
customParams$VCroot_P50[ph] <- -1.741565
customParams$VCroot_P88[ph] <- -2.301482

slope <- 11
P50 <- -6.5
P88 <- P50 + log((100.0/88.0)-1.0)*(25.0/slope)
P12 <- P50 + log((100.0/12.0)-1.0)*(25.0/slope)
customParams$VCstem_P12[pl] <- P12
customParams$VCstem_P50[pl] <- P50
customParams$VCstem_P88[pl] <- P88
customParams$VCstem_slope[pl] <- slope

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


customParams$LeafEPS[pl] <- 12.38 
customParams$LeafPI0[pl] <- -2.13
customParams$LeafAF[pl] <- 0.5
customParams$LeafEPS[qi] <- 15 
customParams$LeafPI0[qi] <- -2.5
customParams$LeafAF[qi] <- 0.4
customParams$LeafEPS[ph] <- 5.31 
customParams$LeafPI0[ph] <- -1.5
customParams$LeafAF[ph] <- 0.6

customParams$StemEPS[pl] <- 12.38 
customParams$StemPI0[pl] <- -2.13
customParams$StemAF[pl] <- 0.4
customParams$StemEPS[qi] <- 15 
customParams$StemPI0[qi] <- -2.5
customParams$StemAF[qi] <- 0.4
customParams$StemEPS[ph] <- 5
customParams$StemPI0[ph] <- -1.65
customParams$StemAF[ph] <- 0.4

customParams$Gswmax[pl] <- 0.220 
customParams$Gswmax[qi] <- 0.220 
customParams$Gswmax[ph] <- 0.2175 
customParams$Gswmin[pl] <- 0.002 
customParams$Gswmin[qi] <- 0.002 
customParams$Gswmin[ph] <- 0.001 


# P. angustifolia	P.latifolia	Qilex	Phalepensis	Phalepensis_OLD
# P12_gs	-2.2	 -1.65	-1	-1.36	-1.36
# P88_gs	-3.8	 -2.5	-2.7	 -2.14	-2.33
customParams$Gs_slope[pl] <- (88.0 - 12.0)/(2.5 - 1.65);
customParams$Gs_P50[pl] <- -1.65 + log(0.12/0.88)/(customParams$Gs_slope[pl]/25)
customParams$Gs_slope[qi] <- (88.0 - 12.0)/(2.7 - 1);
customParams$Gs_P50[qi] <- -1.0 + log(0.12/0.88)/(customParams$Gs_slope[qi]/25)
customParams$Gs_slope[ph] <- (88.0 - 12.0)/(2.14 - 1.36);
customParams$Gs_P50[ph] <- -1.36 + log(0.12/0.88)/(customParams$Gs_slope[ph]/25)


# 10. MEASURED DATA --------------------------------------------------------
measuredData<-data.frame(dates = dates, 
                         SWC = env_data$RE_TD/100, 
                         SWC.err = NA,
                         ETR = env_data$ETR)
row.names(measuredData)<- measuredData$dates
measuredData <- measuredData[meteo_sel,]
measuredData[as.character(sapf_data2014$doy), paste0("E_", PH_cohname)] = as.numeric(sapf_data2014$Mean_Sapflow_velocity_Pinus_halepensis_TD)
measuredData[as.character(sapf_data2014$doy), paste0("E_", PH_cohname, "_err")] = as.numeric(sapf_data2014$SD_Sapflow_velocity_Pinus_halepensis_TD)
measuredData[as.character(sapf_data2014$doy), paste0("E_", QI_cohname)] = as.numeric(sapf_data2014$Mean_Sapflow_velocity_Quercus_ilex_TD)
measuredData[as.character(sapf_data2014$doy), paste0("E_", QI_cohname, "_err")] = as.numeric(sapf_data2014$SD_Sapflow_velocity_Quercus_ilex_TD)
measuredData[as.character(sapf_data2014$doy), paste0("PD_", PH_cohname)] = as.numeric(sapf_data2014$`MEAN_Pinus_halepensis_ PBASE_TD`)
measuredData[as.character(sapf_data2014$doy), paste0("PD_", PH_cohname, "_err")] = as.numeric(sapf_data2014$SD_Pinus_halepensis_PBASE_TD)
measuredData[as.character(sapf_data2014$doy), paste0("PD_", QI_cohname)] = as.numeric(sapf_data2014$`MEAN_Quercus_ilex_PBASE TD`)
measuredData[as.character(sapf_data2014$doy), paste0("PD_", QI_cohname, "_err")] = as.numeric(sapf_data2014$SD_Quercus_ilex_PBASE_TD)
measuredData[as.character(sapf_data2014$doy), paste0("MD_", PH_cohname)] = as.numeric(sapf_data2014$MEAN_Pinus_halepensis_PMIN_TD)
measuredData[as.character(sapf_data2014$doy), paste0("MD_", PH_cohname, "_err")] = as.numeric(sapf_data2014$SD_Pinus_halepensis_PMIN_TD)
measuredData[as.character(sapf_data2014$doy), paste0("MD_", QI_cohname)] = as.numeric(sapf_data2014$MEAN_Quercus_ilex_PMIN_TD)
measuredData[as.character(sapf_data2014$doy), paste0("MD_", QI_cohname, "_err")] = as.numeric(sapf_data2014$SD_Quercus_ilex_PMIN_TD)
row.names(measuredData)<-NULL


# 11. EVALUATION PERIOD ---------------------------------------------------
# Select evaluation dates
d = as.Date(meteoData$dates)
meteoData <- meteoData[(d>="2014-01-01") & (d<="2014-12-31"),] #Select years
measuredData <- measuredData[(d>="2014-01-01") & (d<="2014-12-31"),] #Select years



# 12. REMARKS -------------------------------------------------------------
remarks <- data.frame(
  Title = c('Soil',
            'Vegetation'),
  Remark = c('',
             '')
)


# 13. SAVE DATA IN FOLDER -------------------------------------------------
folder_name <- file.path('Sites_data', 'FONBLA')

write.table(siteData, file = file.path(folder_name, 'FONBLA_siteData.txt'),
            row.names = FALSE, sep = '\t')

write.table(treeData, file = file.path(folder_name, 'FONBLA_treeData.txt'),
            row.names = FALSE, sep = '\t')

write.table(shrubData, file = file.path(folder_name, 'FONBLA_shrubData.txt'),
             row.names = FALSE, sep = '\t')

write.table(miscData, file = file.path(folder_name, 'FONBLA_miscData.txt'),
            row.names = FALSE, sep = '\t')

write.table(meteoData, file = file.path(folder_name, 'FONBLA_meteoData.txt'),
            row.names = FALSE, sep = '\t')

write.table(soilData, file = file.path(folder_name, 'FONBLA_soilData.txt'),
            row.names = FALSE, sep = '\t')

write.table(terrainData, file = file.path(folder_name, 'FONBLA_terrainData.txt'),
            row.names = FALSE, sep = '\t')

write.table(customParams, file = file.path(folder_name, 'FONBLA_customParams.txt'),
            row.names = FALSE, sep = '\t')

write.table(measuredData, file = file.path(folder_name, 'FONBLA_measuredData.txt'),
            row.names = FALSE, sep = '\t')

write.table(remarks, file = file.path(folder_name, 'FONBLA_remarks.txt'),
            row.names = FALSE, sep = '\t')



