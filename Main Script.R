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

SAPDReferenceFile <- SAPDReferenceFile |> 
  mutate(name = gsub("<a0><a0>","",name))
