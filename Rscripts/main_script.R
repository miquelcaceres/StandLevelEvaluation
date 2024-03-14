
## data preparation
sites = c("WOMBAT", "EUCFACE",
          "FRAPUE", "FRAHES", "FONTAINEBLEAU","FONBLA",
          "ISRYAT",
          "MITRA",
          "ESPRIN", "QVALLCEBRE", "PVALLCEBRE", "PRADES", "CANBALASC", "ESPALTARM", "RONDA",
          "DAVOS",
          "USAMORSF", "SEVILLETA")
## Model runs & evaluation
wd = getwd()
medfatereports::run_reports(sites= sites, model="spwb", 
                            confs="granier", wd = wd)
medfatereports::run_reports(sites= sites, model="spwb", 
                            confs="sperry", wd = wd)
medfatereports::run_reports(sites= sites, model="spwb", 
                            confs="cochard", wd = wd)

# medfatereports::run_reports(sites= sites, model="growth",
#                             confs="granier", wd = wd)
# medfatereports::run_reports(sites= sites, model="growth",
#                             confs="sperry", wd = wd)
# medfatereports::run_reports(sites= sites, model="growth",
#                             confs="cochard", wd = wd)
