
## data preparation
sites = c("FRAPUE", "FRAHES", "ISRYAT", "ESPRIN", 
          "QVALLCEBRE", "PVALLCEBRE", "PRADES", "FONBLA", 
          "CANBALASC", "ESPALTARM", "RONDA")
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
