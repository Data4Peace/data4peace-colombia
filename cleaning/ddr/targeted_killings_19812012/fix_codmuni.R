###########################################################################################################################
#
#  File Name:		  fix_codmuni.R
#  Author:		    Billy Matthias 
#  Email:		      b.matthias@utexas.edu
#  Purpose:		    pull in municipality codes that aren't in the "asesinatos_selectivos_19812012.csv" file
#  Last Updated:	08-May-2017 
#  Data Used:		  08-May-2017
#  Data Source:	  1) "asesinatos_selectivos_19812012_resaved.csv" (imported than immediately exported data from OpenRefine - to create consistent encoding across data files)
#                 2) "asesinatos_selectivos_refined-name-key.csv" 3) "fix_codmuni_openrefine.csv"
#  Data Output:   1) 
#
###########################################################################################################################
## TABLE OF CONTENTS:
#  I. Setup
#  II. Merge original data w/ the refined key
#  III. Fix redunancies
#  IV. Summary stats
#  V. Export


## NOTES:
#
#
#


##############
#  I. SETUP  # 
##############

rm(list=ls())
library(tidyverse)
library(stringi)

# path to raw data
dir.rawdata <-"/Users/wtmatthias/Google Drive/data4peace-colombia/raw_data/ddr" # add in github url to pull data from
# path to assassination data
dir.cleaning <- "/Users/wtmatthias/Google Drive/data4peace-colombia/cleaning/ddr/asesinatos_selectivos_19812012"
dir.cleaned <- "/Users/wtmatthias/Google Drive/data4peace-colombia/cleaning/00_cleaned/ddr"

# read in original assassination data w/out municipality codes
getwd()
setwd(dir.rawdata)
orig <- read_csv(file = "asesinatos_selectivos_19812012_resaved.csv")
# orig <- read_csv(file = "asesinatos_selectivos_19812012.csv", col_names = TRUE, col_types = NULL, locale(asciify = TRUE)) # alternative spec
# str(orig)

# read in refined_name_key from 'cluster' cmd in Open Refine
setwd(dir.clean)
name_key <- read_csv(file = "asesinatos_selectivos_refined-name-key.csv") # contains refined_name_key created in OpenRefine
                                                                          # matches this key with the muni names from 'orig' above
# read in the codmuni file with 'refined_name_key'                                                                          
codmuni <- read_csv("fix_codmuni_openrefine.csv") # file w/ codmuni and refined_name_key (created in OpenRefine)


################################################
#  II. MERGE ORIGINAL DATA W/ THE REFINED KEY  # 
################################################

## REMOVE DUPLICATES FOR CORRECT JOIN
name_key <- name_key %>% distinct(municipality, .keep_all = TRUE)
codmuni <- codmuni %>% filter(is_character(refined_name_key) & !is.na(codmpio))

## GET RID OF DAMN NON-ASCII CHARACTERS
orig$municipality <- stri_trans_general(orig$municipality, "latin-ascii") # takes all the non-ascii characters and makes them easier
name_key$municipality <- stri_trans_general(name_key$municipality, "latin-ascii")
name_key$refined_name_key <- stri_trans_general(name_key$refined_name_key, "latin-ascii")
codmuni$municipio <- stri_trans_general(codmuni$municipio, "latin-ascii")
codmuni$refined_name_key <- stri_trans_general(codmuni$refined_name_key, "latin-ascii")


## MERGE
# join the muni code dataset w/ the key that will join w/ the assassination data values
codmuni <- left_join(codmuni, name_key, by = "refined_name_key")
codmuni <- codmuni %>% distinct(codmpio, .keep_all = TRUE)

# join the muni codes in w/ assassination data
orig2 <- left_join(orig, codmuni, by = "municipality")


############################
#  III. FIX REDUNDANCIES   # 
############################
## NOTES:
#         - data isn't the best b/c only municipality names were given; no pairing w/ the department name
#         - matching algorithms in OpenRefine collaposed multiple department-municipalities into the same unit (muni-year)
#         - no way to perfect this right now
#         - need to drop the redundant observations where a municipality name is counted multiple times
#         - original assassination data has 6091 observations (1981-2012)

# filter out the rows where assassination data is matched w/ muni code (codmuni)
orig2 <- orig2 %>% select(-codmpio.x) %>%
                    rename(codmuni = codmpio.y) %>%
                    filter(!is.na(codmuni))

# count of distinct codmuni-year observations = 5282
orig2$yrcodmuni <- with(orig2, paste0(as.character(year), as.character(codmuni)))
orig2 %>% summarise(n_distinct(as.numeric(yrcodmuni)))

# keep only distinct codmuni-year observations
orig2 <- orig2 %>% distinct(yrcodmuni, .keep_all = TRUE)

# count of distinct municipality-year observations = 5593
orig2$yrmuni <- with(orig2, paste0(as.character(year), as.character(municipality)))
orig2 %>% summarise(n_distinct(yrmuni))

# keep only distinct codmuni-year observations
orig3 <- orig2 %>% distinct(yrmuni, .keep_all = TRUE)


########################
#  IV. SUMMARY STATS   # 
########################
## NOTES:
#         - summary stats on the original assassination data

# municipalities represented in assassinations data = 871
orig %>% summarise(n_distinct(municipality))


################
#  V. EXPORT   #
################
targetkill <- orig3 %>% rename(targeted_kill = victimas) %>%
                   select(year, targeted_kill, coddept, codmuni, codpoblado, department, municipio)
str(targetkill)

setwd(dir.cleaned)
write_csv(targetkill, "targeted_killings_19812012_v01.csv")

