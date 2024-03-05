
## data preparation
sites = c("FRAPUE", "FRAHES", "ISRYAT", "QVALLCEBRE", "PVALLCEBRE", "PRADES", "ESPRIN", "FONBLA", "CANBALASC", "ESPALTARM")
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
