# SETUP ####
library(readxl)
library(tidyverse)

# custom functions
fix_list_lengths <- function(x){lapply(x, `length<-`, max(lengths(x)))}


# LOAD METADATA
dat <- read_xlsx("./metadata/SraRunTable_Marine_MG.xlsx",guess_max = 100000) %>% 
  janitor::clean_names()

# FIX UP LATITUDE AND LONGITUDE ####

# find all variables with lat and lon
# since different people put that info in different variables
lat_cols <- names(dat) %>% grep(pattern = "lat")
lon_cols <- names(dat) %>% grep(pattern = "lon")
names(dat)[c(lat_cols,lon_cols)]

lat_lon_cols <- 
c("lat_lon",
  "geographic_location_latitude","geographic_location_longitude", 
  "latitude_start","latitude_end","longitude_start","longitude_end")

# N and E are + | S and W are -
dat %>% 
  dplyr::select(all_of(lat_lon_cols)) %>% 
  skimr::skim()


          # standard lat_lon column values

# lat_lon values that are non-standard
# convert to NA
dat$lat_lon[which(dat$lat_lon %>% 
                    str_split(" ") %>% map(length) %>% unlist() != 4)]
# repair list lengths
lat_lon <- 
  dat$lat_lon %>% str_split(" ") %>% 
  fix_list_lengths()


# get sign from N/S
lat.sign <- lat_lon %>% map(2) %>% unlist()
lat.sign <- ifelse(lat.sign == "N", 1,-1)


lon.sign <- lat_lon %>% map(4) %>% unlist()
lon.sign <- ifelse(lon.sign == "E", 1,-1)

lat.from.lat_lon <- 
  lat_lon %>% map(1) %>% as.numeric() * lat.sign

lon.from.lat_lon <- 
  lat_lon %>% map(3) %>% as.numeric() * lon.sign

lat.from.lat_lon 
lon.from.lat_lon

          # geographic_location columns
lat.from.geoglocation <- 
dat$geographic_location_latitude %>% as.numeric
lon.from.geoglocation <- 
dat$geographic_location_longitude %>% as.numeric

# match existing values from geoglocation with lat and lon from 'lat_lon'
lat.from.lat_lon[which(!is.na(lat.from.geoglocation))] <- 
  lat.from.geoglocation[which(!is.na(lat.from.geoglocation))]

lon.from.lat_lon[which(!is.na(lon.from.geoglocation))] <- 
  lon.from.geoglocation[which(!is.na(lon.from.geoglocation))]

dat$latitude <- lat.from.lat_lon
dat$longitude <- lon.from.lat_lon

names(dat)
dat %>% skimr::skim() %>% as.data.frame()

# Remove non-illumina reads (convert to NA)
dat$instrument %>% grep(pattern="Illumina",value = TRUE,invert = TRUE) %>% unique
dat <- 
  dat %>% 
  mutate(instrument = case_when(platform != "ILLUMINA" ~ NA,
                                TRUE ~ platform))
dat <- 
dat %>% 
  dplyr::filter(instrument == "ILLUMINA")


# Remove any transcriptome samples (convert to NA)
dat <- 
  dat %>% 
  mutate(library_source = case_when(library_source %in% c("METATRANSCRIPTOMIC") ~ NA,
                                TRUE ~ library_source))
dat <- 
dat %>% 
  dplyr::filter(library_source %in% c("GENOMIC","METAGENOMIC"))


# Examine isolation_source (free response, so tokenize and code into groups)
dat$isolation_source <- str_to_lower(dat$isolation_source) # remove capitals

# get feel for all values present
dat$isolation_source %>% unique %>% 
  paste(collapse = " ") %>% 
  str_remove_all("\\\\") %>% # remove backslashes (who puts these in SRA!?)
  str_remove_all(",") %>% 
  str_remove_all("\\(") %>% 
  str_remove_all("\\)") %>% 
  strsplit(" ") %>% table %>% 
  as.data.frame() %>% 
  arrange(Freq)

dat$isolation_source <- 
dat$isolation_source %>% 
  str_remove_all("\\\\") %>% # remove backslashes (who puts these in SRA!?)
  str_remove_all(",") %>% 
  str_remove_all("\\(") %>% 
  str_remove_all("\\)")


# fix depth (meters)
dat$depth <- 
dat$depth %>% 
  strsplit("[^[:alnum:]]+") %>% 
  map_chr(1) %>% 
  str_remove("m") %>% 
  as.numeric()

dat$env_biome %>% unique
dat$env_feature %>% unique
dat$environment_biome %>% unique
env <- dat %>% dplyr::select(starts_with("env"),isolation_source)
names(env)


# generalize the environmental features
dat$environment <- 
env %>% 
  mutate(all_env = paste(env_biome,env_broad_scale,env_local_scale,
                         env_feature,environment_biome,
                         environment_feature,environment_material,isolation_source,sep="_")) %>% 
  pluck("all_env") %>% 
  str_remove_all("NA_") %>% 
  ifelse(. == "NA",NA,.)



# subset to clean(ish), useful columns
clean_cols <- c("bio_project","bio_sample","experiment","sra_study",
                "latitude","longitude","geo_loc_name_country_continent",
                "isolation_source","host","depth","environment")
dat <- 
  dat %>% 
  dplyr::select(all_of(clean_cols))

# remove samples with bad data (discovered through exploration in subsequent scripts)
# convert bio_project to NA
dat[dat$bio_project == "PRJEB37465",][,"latitude"] <- NA

# write out 'environment' to csv to build a manual dictionary
# data.frame(environment = dat$environment %>% unique) %>%
#   write_csv("./metadata/environmental_dictionary2.csv")
# read in ontology dictionary
env_dict <- read_csv("./metadata/environmental_dictionary2.csv")
# use it to clean up "environment" names
# see https://raw.githubusercontent.com/EnvironmentOntology/envo/master/envo.obo for ontology info

env_dict
# use dicitonary to add more general columns for environment
dat$material <- plyr::mapvalues(dat$environment,from = env_dict$environment, to = env_dict$material)
dat$location <- plyr::mapvalues(dat$environment,from = env_dict$environment, to = env_dict$location)
dat$notes <- plyr::mapvalues(dat$environment,from = env_dict$environment, to = env_dict$notes)
# dat %>% View
# dat[dat$longitude < -120 & dat$longitude > -200,]

# clean up data with all missing metadata (pulled manually from SRA)
dat[dat$bio_project == "PRJEB61010","material"] <- "water"
dat[dat$bio_project == "PRJEB61010","location"] <- "neritic"

dat[dat$bio_project == "PRJNA291491","material"] <- "water"
dat[dat$bio_project == "PRJNA291491","location"] <- "neritic" 
 
dat[dat$bio_project == "PRJEB42974","material"] <- "water"
dat[dat$bio_project == "PRJEB42974","location"] <- "oceanic"

dat[dat$bio_project == "PRJEB49968","material"] <- "water"
dat[dat$bio_project == "PRJEB49968","location"] <- "oceanic"

dat[dat$bio_project == "PRJNA266680","material"] <- "water"
dat[dat$bio_project == "PRJNA266680","location"] <- "neritic"

dat[dat$bio_project == "PRJNA819259","material"] <- "sediment"
dat[dat$bio_project == "PRJNA819259","location"] <- "neritic"

dat[dat$bio_project == "PRJNA637983","material"] <- "water"
dat[dat$bio_project == "PRJNA637983","location"] <- "oceanic"

dat[dat$bio_project == "PRJNA291491","material"] <- "water"
dat[dat$bio_project == "PRJNA291491","location"] <- "oceanic"

dat[dat$bio_project == "PRJNA385736","material"] <- "unknown"
dat[dat$bio_project == "PRJNA385736","location"] <- "neritic"

dat[dat$longitude < -100 & dat$bio_project == "PRJNA385736","material"] <- "unknown"
dat[dat$longitude < -100 & dat$bio_project == "PRJNA385736","location"] <- "oceanic"

dat[dat$longitude > 155 & dat$longitude < 175 & dat$latitude > -25 & dat$latitude < -15 & dat$bio_project == "PRJNA385736","material"] <- "unknown"
dat[dat$longitude > 155 & dat$longitude < 175 & dat$latitude > -25 & dat$latitude < -15 & dat$bio_project == "PRJNA385736","location"] <- "oceanic"

x <- which(dat$latitude > 75 & dat$latitude < 80 & dat$longitude > -170 & dat$longitude < -150 & dat$bio_project == "PRJNA681031")
dat[x,"material"] <- "water"
dat[x,"location"] <- "oceanic"

dat[dat$longitude < -15 & dat$bio_project == "PRJEB41592","location"] <- "neritic"

x <- which(dat$bio_project == "PRJNA291491" & dat$latitude > 30 & dat$latitude < 50 & dat$longitude > 100 & dat$longitude < 150)
dat[x,"location"] <- "neritic"

saveRDS(dat,"./metadata/cleaned_metadata.RDS")
