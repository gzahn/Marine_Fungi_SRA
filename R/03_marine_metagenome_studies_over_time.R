
# Marine DNA collections over time

# Entrez search string
# marine[All Fields] AND "marine metagenome"[orgn] AND ("biomol dna"[Properties] AND "strategy wgs"[Properties])
# downloaded metadata on Nov 13, 2023

library(tidyverse)
library(readxl)

dat <- read_xlsx("./metadata/Marine_Metagenome_all_Metadata.xlsx") %>% 
  janitor::clean_names()

dat$collection_date
# Clean up collection_date column ####

# compile all excel dates
excel_dates <- janitor::excel_numeric_to_date(as.numeric(dat$collection_date))

# convert dat$collection_date where previous excel dates lived into NA
dat$collection_date[which(!is.na(excel_dates))] <- NA

# clean up slashes to dashes
dat$collection_date <- dat$collection_date %>% str_replace_all("/","-")

# fix odd projects that just listed year
just_year <- which(nchar(dat$collection_date) == 4)
dat[just_year,"collection_date"] <- paste0(dat$collection_date[just_year],"-01")
dat$collection_date %>% unique


# compile all dates already in Posix
posix_dates <- dat$collection_date %>% as.POSIXct(format='%Y-%m-%d')
# convert posix dates to NA
dat$collection_date[which(!is.na(posix_dates))] <- NA

# convert a bunch of ways to say "missing" to NA
dat$collection_date[which(dat$collection_date %in% 
                            c("cot collected","missing",
                              "not applicable","uncalculated",
                              "not collected"))] <- NA

dat$collection_date
# clean up all 2015-05 style dates
# compile simplified YYYY-mm dates and convert to posixct
x <- paste0(dat$collection_date,"-01")
unique(x)
# x[x=="NA-01"] <- NA
simplified_dates <- x %>% as.POSIXct(format='%Y-%m-%d')

# how many collection_date observations still missing?
which(
  is.na(excel_dates) &
    is.na(posix_dates) &
    is.na(simplified_dates)
)

compiled_dates <- 
  data.frame(excel_dates,posix_dates,simplified_dates) %>% 
  mutate(new = paste(excel_dates,posix_dates,simplified_dates) %>% 
           str_remove_all(" ") %>% 
           str_remove_all("NA") %>% 
           as.Date()) %>% pluck("new")

# overwrite all with cleaned dates
dat$collection_date <- compiled_dates


dat %>% 
  dplyr::filter(collection_date > 1980) %>% 
  ggplot(aes(x=collection_date)) +
  geom_histogram()

