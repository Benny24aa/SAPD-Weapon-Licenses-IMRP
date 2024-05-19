###################################################################################
# Welcome to the SAPD Weapon License Script and Salary Outgoings Calculator       #
# This script is designed to do the following:                                    #
# *Allocate a player's column to a faction based on SAPD/SASF/NOOSE staff rosters #
# *Calculate how much in total SAPD is spending over a 2 week period in salaries  #
# *Create a ratio to see how this varies across total players with above 0 hours  #
##########
#########################################################################

# Packages Loaded

library(readxl)
library(dplyr)
library(fs)
library(purrr)
library(data.table)
library(tidyr)
library(janitor)
library(stringr)
library(readr)

#Set's a unique file location for reference file
ReferenceFileLocation <-"C:/Users/harle/OneDrive/Desktop/IMRP 2024 Improved  War Census/Weapon License Files/Weapon License Dashboard R Script/SAPD-Weapon-Licenses-IMRP/SAPD Reference File.csv"

#Loads in file reference file for the first time
SAPDReferenceFile <- read.csv(ReferenceFileLocation)

#Apply clean_names to reference file and then add SAPD to each individual's name
SAPDReferenceFile <- SAPDReferenceFile |>
  clean_names() |>
  mutate(faction_name = "SAPD")

#Removing text that are not necessary in dataframe

SAPDReferenceFile <- SAPDReferenceFile |> 
  mutate(name = gsub("<a0><a0>","",name)) |> 
  mutate(playtime_2_weeks = gsub("hours","",playtime_2_weeks))|> 
 mutate(playtime_2_weeks = gsub(" ","",playtime_2_weeks))

SAPDReferenceFile[c('badge_number', 'rank')] <- str_split_fixed(SAPDReferenceFile$rank, ']', 2)

SAPDReferenceFile <- SAPDReferenceFile |>
  mutate(rank = gsub("  ", "", rank))|>
  mutate(badge_number = gsub("!","",badge_number))|>
  mutate(badge_number = gsub("D","",badge_number))|>
  mutate(badge_number = gsub("S","",badge_number))|>
  mutate(badge_number = gsub("H","",badge_number)) |>
  mutate(badge_number = gsub("B","",badge_number)) |>
  mutate(badge_number = gsub("XXX","",badge_number)) |>
  mutate(badge_number = gsub("Police Cadet","",badge_number)) |>
  mutate(badge_number = gsub("Reserve Officer","",badge_number)) |>
  mutate(badge_number = gsub("Reserve Officer I","",badge_number)) |>
  mutate(badge_number = gsub("enior Cadet","",badge_number)) |>
  mutate(badge_number = gsub("I","",badge_number)) |>
  mutate(badge_number = gsub("Retard l", "", badge_number))|>
  mutate(badge_number = gsub("C","",badge_number)) |>
  mutate(badge_number = gsub("Police adet","",badge_number)) |>
  mutate(badge_number = gsub("enior adet","",badge_number)) |>
  mutate(badge_number = gsub("D","",badge_number))


#### Lack of badge number report


#Activity Report Over Last 2 Weeks


 
