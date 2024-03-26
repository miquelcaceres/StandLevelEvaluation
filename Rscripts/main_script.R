
## data preparation
all_sites = c("WOMBAT", "EUCFACE",
          "SOROE",
          "FRAPUE", "FRAHES", "FONTAINEBLEAU","FONBLA",
          "ISRYAT",
          "COLLELONGO",
          "MITRA",
          "ESPRIN", "QVALLCEBRE", "PVALLCEBRE", "PRADES", "CANBALASC", "ESPALTARM", "RONDA",
          "DAVOS", "LOTSCHENTAL",
          "USAMORSF", "SEVILLETA")
## Model runs & evaluation
wd = getwd()
for(site in all_sites) {
  print(site)
  medfatereports::run_reports(sites= site, model="spwb",
                              confs="granier", wd = wd)
  medfatereports::run_reports(sites= site, model="spwb", 
                              confs="sperry", wd = wd)
  medfatereports::run_reports(sites= site, model="spwb", 
                              confs="sureau", wd = wd)
}
