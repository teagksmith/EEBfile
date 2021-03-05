#This file loads Teagan's 2018 file and adds the other years
library(here)
here() #here is now set to your root directory with .Rproj file

library(tidyverse)
library(janitor)
library(data.table)
library(skimr)
library(here)
library(readxl)



# load travel data --------------------------------------------------------

coral_travel <- read_csv(here("data", "data for analysis.csv"))

coral_travel
names(coral_travel)

#what are the common airport codes? 
coral_travel %>% 
  select(contains("code")) %>% 
  tabyl(affiliation_airport_code) %>% 
  arrange(-n) %>% 
  head()


coral_travel %>% 
  select(contains("code")) %>% 
  tabyl(field_airport_code) %>% 
  arrange(-n) %>% 
  head()

#load all years data from Sam repo
all_years <- read_csv(here("data","one-row-by-study-for-Teagan-02152021.csv"))
all_years

all_years %>% 
  tabyl(pub_year)


# import data from 2018 teagan work ---------------------------------------

#grab 2018 airport codes from Teagan file 
names(coral_travel)

data_2018 <- coral_travel %>% 
  select(article_id, 
         affiliation,
         affiliation_airport, 
         affiliation_airport_code, 
         field_study_location,
         field_country,
         field_airport,           
         field_airport_code) %>% 
  distinct()


#join
names(all_years)
names(data_2018)

check <- all_years %>% 
  #filter(pub_year == 2018) %>% 
  left_join(data_2018, 
            by = c("article_id", 
                   "fieldstudy_country" = "field_country")) #and country

check %>% 
  tabyl(affiliation_airport_code)

names(check)

# check %>% 
#   filter(affiliation.x != affiliation.y) %>% 
#   select(article_id, 
#          contains("affiliation")) %>% 
#   view()


# make all years data -----------------------------------------------------
#reorg
data_for_teagan <- check %>% 
  select(-affiliation.y) %>% 
  rename("affiliation" = affiliation.x)

data_for_teagan

data_for_teagan %>% 
  tabyl(pub_year)

data_for_teagan %>% 
  tabyl(field_airport_code) %>% 
  arrange(-n)

#add in some usual suspects field codes
data_for_teagan <- data_for_teagan %>% 
  mutate(affiliation_airport_code = case_when(
    str_detect(affiliation, "James Cook Univ") ~ "TSV",
    str_detect(affiliation, "Townsville") ~ "TSV",
    str_detect(affiliation, "Mombasa") ~ "MBA",
    str_detect(affiliation, "Queensland") ~ "BNE",
    str_detect(affiliation, "Veracruz") ~ "VER", 
    str_detect(affiliation, "Noumea") ~ "NOU", 
    str_detect(affiliation, "Bronx") ~ "LGA", 
    str_detect(affiliation, "Simon Fraser Univ") ~ "YVR", 
    str_detect(affiliation, "Univ Rhode Isl") ~ "PVD", 
    str_detect(affiliation, "Copenhagen") ~ "CPH", 
    str_detect(affiliation, "Rosenstiel Sch Marine") ~ "MIA", 
    str_detect(affiliation, "Santa Cruz") ~ "SPC", 
    TRUE ~ affiliation_airport_code))

#check airport codes now
data_for_teagan %>% 
  tabyl(affiliation_airport_code) %>% 
  arrange(-n) %>% 
  slice(1:10)

#check common affiliations to add code
data_for_teagan %>% 
  filter(is.na(affiliation_airport_code)) %>% 
  tabyl(affiliation) %>% 
  arrange(-n) %>% 
  slice(1:10)

#check field options
names(data_for_teagan)

data_for_teagan %>% 
  tabyl(fieldstudy_country) %>% 
  arrange(-n) %>% 
  slice(1:10)

data_for_teagan %>% 
  tabyl(field_study_location) %>% #needs to add to all studies to add field airport codes
  arrange(-n) %>% 
  slice(1:10)

#rearrange columns for easier analysis 
names(data_for_teagan)

data_for_teagan <- data_for_teagan %>% 
  select(article_id:study_type_2, 
         starts_with("affiliation"), 
         starts_with("field"))

data_for_teagan

#remove domestic article lines, so where affiliation country = field country
data_for_teagan

# data_for_teagan %>% 
#   filter(affiliation_country == fieldstudy_country) %>% 
#   filter(pub_year == 2018) %>% 
#   view() #looks reasonable

?write_csv

data_for_teagan %>% 
  filter(affiliation_country != fieldstudy_country) %>% 
  write_csv(here("data", "all years for teagan with 2018 codes_no domestic.csv"), 
            na = "")

