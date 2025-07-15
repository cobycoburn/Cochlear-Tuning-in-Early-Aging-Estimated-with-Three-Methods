################################################################################
## Audiogram data ##
################################################################################

# For plotting LRF data & associated (estimated) Qerbs for Tuning Manuscript.

# Author: Courtney Glavin
# Last Updated: April 11, 2024
# Version: 2.0 

##############
## Set up R ##
##############

## Load in data
df <- read.csv('D:/Analysis/Tuning Manuscript/Data (Final)/audioData_2024-05-13.csv') # data
te <- read.csv('D:/Analysis/Tuning Manuscript/Data (Final)/participantTestEar.csv') # test ear of each participant 

################################################################################
## Data wrangling ##
################################################################################

# Remove participants from data frame who are not included in other data analysis (did not meet inclusion criteria for study)
df <- df %>%
  filter(ID != 'CCG_B_007') %>%
  filter(ID != 'CCG_B_039') %>% 
  #filter(ID != 'CCG_B_048') %>%
  filter(ID != 'CCG_C_081') %>% 
  filter(ID != 'CCG_C_061')

# Rename mislabeled participants
df$ID[df$ID == "CCG_A_015"] <- "CCG_A_015_discontinued"
df$ID[df$ID == "CCG_B_29"] <- "CCG_B_029"

# Merge dfs
df <- df %>% 
  left_join(te, by="ID")

# Filter by test ear
df <- df %>% 
  mutate(testEarThreshold = ifelse(testEar == 'R', Right, Left)
  )

# Remove _ from participantIDs 
df$ID <- gsub("_", "", df$ID)

# Create PTA column from audiogram data (0.5, 2, and 4 kHz)
pta <- df %>%
  filter(Frequency %in% c(500, 1000, 2000)) %>%
  mutate(selected_ear = ifelse(testEar == 'L', Left, Right)) %>% 
  group_by(ID) %>%
  summarise(pta = mean(selected_ear, na.rm = TRUE)) %>%
  ungroup() 

df <- df %>%
  left_join(pta, by = "ID")

newdf <- df
