##
# Determining Nest Success 
# Species: RAZO
# Created: May 15, 2026
# Creators: Abigail Muscat, add your names!

#databases
library(dplyr)
library(lubridate)
library(tidyr)

#links/resources:
#https://www.listendata.com/2017/03/if-else-in-r.html
#https://github.com/tidyverse/dplyr/issues/3768
#https://stackoverflow.com/questions/58443566/what-does-na-rm-true-actually-means
#https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/as.Date --> did not end up using
#https://www.rdocumentation.org/packages/cropgrowdays/versions/0.2.2/topics/day_of_year --> did not end up using
#https://www.geeksforgeeks.org/r-language/how-to-use-is-not-na-in-r/
#https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/NA 

# Creating file directory location ----

dir <- "/Volumes/T7 Shield/UofM Masters/NestSuccess/Raw Files"
## Set "Fname" to be the unique identifier for each file needed

Fname_nest <- "2023_RAZOprod"
Fname_weight <- "ChickWeights_prelim"

# Read in data ----
weight_data <- read.csv(file.path(dir,paste(Fname_weight,".csv", sep = "")))
prod_data <- read.csv(file.path(dir,paste(Fname_nest,".csv", sep = "")))

# might need to add in some code to adjust dates and such here

# need hatch date, make hatch interval, fledge interval (C_Last - N_First), how long MFC and how long PFC
#min and max age?
#death date
#fledged status

# adjust prod file ----
#remove comments columns:
prod_data <- subset(prod_data, select = -c(Exclude., Field.Comment.., Notes))

#mess with prod file - make it so the columns are labelled by DOY
meta <- prod_data[1, ]      # first row = DOY row
data <- prod_data[-1, ]     # everything else = nests

doy_vals <- as.numeric(meta[1 , -(1:6)])  
colnames(data)[-(1:6)] <- as.character(doy_vals) # adding a minus excludes that column, so skip to column 7 which is where DOY should start

# force ALL DOY columns to character (this fixed an error I was getting when trying to pivot)
data[ , -(1:6)] <- lapply(data[ , -(1:6)], as.character)

#filter RAZO
RAZO_proddata <- data %>%
  filter(Species %in% c("RAZO")) 



#pivot prod data longer instead of horizontal ----
nest_long <- RAZO_proddata %>%
  pivot_longer(
    cols = -(1:6),
    names_to = "DOY",
    values_to = "Stage"
  ) %>%
  mutate(DOY = as.numeric(DOY)) %>%
  filter(!is.na(Stage), Stage != "")

#prod status options ----
#let's figure out all the possible prod options -- once we have all years would need to go through this again, I just used 2023 as a tester year
unique(nest_long$Stage)
#ATPU in a RAZO crevice I am assuming means we saw a puffin so I won't touch that.
#E options: E, A*E, 2AE, 2E, AE, 
#Cw or Ep options: A*Ep, AEp, Cw
# C options: 2AD, A*D, AD, AFE, D, FE, C, F, AF, 2AFE, 2D, 2F, 2FE, A*F, A*FE,

chick_stages <- c(
  "AD", "A*D", "AFE", "D", "FE", "C", "F",
  "AF", "2AFE", "2D", "2F", "2FE", "A*F", "A*FE"
)

# add unique name to each nest ----         
#add unique identified
nest_long <- nest_long %>%
  unite("Nest_unique", Nest.ID, Chamber, Plot, remove = FALSE)

# how many nests had chicks ever - gives idea of how many won't make the cut for fledge success (either unsuccessful or unknown)
nest_long %>%
  group_by(Nest_unique) %>%
  summarize(
    has_chicks = any(Stage %in% chick_stages)
  ) %>%
  filter(!has_chicks)

# hatch date (E_last, C_first, Ep or Cw date), death date, etc ----

hatch_dates <- nest_long %>%
  group_by(Nest_unique) %>%
  summarize(
    #last egg date
    E_last = if(any(Stage %in% c("E", "A*E", "2AE", "2E", "AE"))) {
      max(DOY[Stage %in% c("E", "A*E", "2AE", "2E", "AE")], na.rm = TRUE)
    } else {
      NA_real_
    },
    #first chick date
    C_first = if(any(Stage %in% chick_stages)) {
      min(DOY[Stage %in% chick_stages], na.rm = TRUE)
    } else {
      NA_real_
    },
    #last chick date
    C_last = if(any(Stage %in% chick_stages)) {
      max(DOY[Stage %in% chick_stages], na.rm = TRUE)
    } else {
      NA_real_
    },
    #ep/cw date
    Ep_date = if(any(Stage %in% c("Ep", "Cw", "A*Ep"))) {
      min(DOY[Stage %in% c("Ep", "Cw", "A*Ep")], na.rm = TRUE)
    } else {
      NA_real_
    }, 
    #death date
    Death_date = if(any(Stage == "X")) {
      min(DOY[Stage == "X"], na.rm = TRUE)
    } else {
      NA_real_
    }, 
    #last day of check
    Last_check = max(DOY), #NOT DOING WHAT WE WANT
    
    #empty nest observation 
    N_first = if(any(Stage == "N")) min(DOY[Stage == "N"], na.rm = TRUE) else NA_real_, # first time nest empty
    
    N_last = if(any(Stage == "N")) max(DOY[Stage == "N"], na.rm = TRUE) else NA_real_, # last time nest marked empty
    
    N_after_chick = if(!is.na(C_last) && # this makes it so we narrow in on Ns after chick, so those Ns that may mean fledge; filters out nests where chicks were never recorded; first empty nest observation that occurs after chick was present
      any(Stage == "N" & DOY > C_last, na.rm = TRUE) #is there any "N" after last chick date
    ) {
      min(DOY[Stage == "N" & DOY > C_last], na.rm = TRUE) #first empty nest date after chick
    } else { # if not return NA
      NA_real_
      },
    
    
  ) %>%
  mutate( #calculate HatchDate by taking the midpoint between E last and C first, unless there is a Ep date that use that
    HatchInterval = as.numeric(C_first - E_last),
    
    HatchDate = case_when(
      !is.na(Ep_date) ~ Ep_date,
      !is.na(E_last) & !is.na(C_first) ~
        E_last + (C_first - E_last)/2,
      TRUE ~ NA_real_
    ),
    
    F_interval = N_after_chick - C_last # calculate Fledge interval same as excel algorithm
  ) %>%
  
  #get chick rearing duration (ages); ! is.na exclude nests with specific date (e.g. death date), adding ! makes it opposite, so returns TRUE is there is a value; ! can be not equal to
  mutate(
    
    C_min_age = case_when(
      !is.na(Death_date) ~ NA_real_, # exclude nests with death date
      !is.na(C_first) & !is.na(C_last) ~ C_last - C_first, #last day seen chick - first = minimum chick rearing duration
      TRUE ~ NA_real_
    ),
    
    C_max_age = case_when(
      !is.na(Death_date) ~ NA_real_, 
      !is.na(Last_check) & !is.na(E_last) ~ N_after_chick - E_last, #first look for E_last and use that - priority 1
      !is.na(Last_check) & !is.na(C_first) ~ N_after_chick - C_first, # if no E_last do this - priority 2 (
      TRUE ~ NA_real_
    )
  )



#observed feather stage duration (not biological) ----
feather_groups <- list(
  down = c("D", "DFC"),
  mid  = c("FE", "PFC"),
  final = c("F", "MFC")
)


feather_lookup <- tibble(
  Stage = unlist(feather_groups),
  feather_stage = rep(names(feather_groups), lengths(feather_groups))
)
  

feather_durations <- nest_long %>%
  inner_join(feather_lookup, by = "Stage") %>%
  group_by(Nest_unique, feather_stage) %>%
  summarize(
    start = min(DOY),
    end   = max(DOY),
    duration = end - start,
    .groups = "drop"
  )

feather_wide <- feather_durations %>%
  select(Nest_unique, feather_stage, duration) %>%
  pivot_wider(
    names_from = feather_stage,
    values_from = duration
  )

prod_mainfile <- hatch_dates %>%
  left_join(feather_wide, by = "Nest_ID")



#fledging weight ----
chick_weight_info <- function(prod_mainfile, weight_data,
                                    fledge_weight = 195) {
  #pull in weight information
  
  weight_hits <- weight_data %>%
    filter(!is.na(Weight)) %>%
    mutate(reached_fledge = Weight >= fledge_weight) %>%
    group_by(Nest_ID) %>%
    summarize(
      ever_fledged_weight = any(reached_fledge),
      fledge_weight_day = if(any(reached_fledge)) {
        min(DOY[reached_fledge])
      } else {
        NA_real_
      },
      max_weight = if(any(!is.na(Weight))) max(Weight, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    )
}

  #join weights into main data
  prod_mainfile <- prod_mainfile %>%
    left_join(weight_hits, by = "Nest_ID")

  
### Classification Code.----
#classify fledging status 
classify_fledging <- function(prod_mainfile,
                              min_duration = 16,
                              reasonable_duration = 13,
                              fledge_weight = 195,
                              FE_duration = 11,
                              F_duration = 6) {
  
  # return values for if and when chick reached fledge weight
  prod_mainfile %>%
    mutate(
  #weight flags
          reached_fledge_weight = ever_fledged_weight,
          fledge_weight_day = fledge_weight_day,
          max_weight = max_weight,
          
      

