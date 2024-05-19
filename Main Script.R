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

########################################
### Activity Report based on 2 weeks ###
########################################

# Error Types based on activity
SAPDReferenceFile <- SAPDReferenceFile |>
  mutate(
    Activity_Zero = playtime_2_weeks == 0 ,
    Activity_Bare_Minimum = playtime_2_weeks > 0 & playtime_2_weeks <= 20,
    Activity_Well = playtime_2_weeks >20 & playtime_2_weeks <=40,
    Activity_Great = playtime_2_weeks > 40
  )

#Given Critera to search for when considering activity boundaries

SAPDReferenceFile <- SAPDReferenceFile |>
  mutate(Activity_Type = case_when(
    Activity_Zero == TRUE ~ "Inactive",
    Activity_Bare_Minimum == TRUE ~ "Needs Improvement",
    Activity_Well == TRUE ~ "Good",
    Activity_Great == TRUE ~ "Very Good"
  ))

### Removes columns related to conditions for activity

SAPDReferenceFile <- SAPDReferenceFile |>
  select(-Activity_Zero) |>
  select(-Activity_Bare_Minimum)|>
  select(-Activity_Well)|>
  select(-Activity_Great)

######################################################
### Preparing Data for Lack of Badge Number Report ###
######################################################

#Removes activity type from previous report
SAPDReferenceFile <- SAPDReferenceFile |>
  select(-Activity_Type) 


#Splits rank and badge into two sections
SAPDReferenceFile[c('badge_number', 'rank')] <- str_split_fixed(SAPDReferenceFile$rank, ']', 2)

#Removes aspects of badge_number that aren't required and filters out Police Cadets as they don't have a number
SAPDReferenceFile <- SAPDReferenceFile |>
  mutate(rank = gsub("  ", "", rank))|>
  mutate(badge_number = gsub("!","",badge_number))|>
  mutate(badge_number = gsub("D","",badge_number))|>
  mutate(badge_number = gsub("H","",badge_number)) |>
  mutate(badge_number = gsub("B","",badge_number)) |>
  mutate(badge_number = gsub("XXX","",badge_number)) |>
  mutate(badge_number = gsub("I","",badge_number)) |>
 filter(badge_number != "Police Cadet")

###################################################################################
#### Valid Players who passed data inspection - ready for weapon license report ###
###################################################################################
SAPDReferenceFile <- SAPDReferenceFile |>
  filter(badge_number > 0)|>
  filter(rank != "" )

SAPDReferenceFile <- SAPDReferenceFile |>
mutate(badge_number = gsub("C","",badge_number)) |>
  mutate(badge_number = gsub("D","",badge_number))|>
mutate(badge_number = gsub("S","",badge_number))



 
