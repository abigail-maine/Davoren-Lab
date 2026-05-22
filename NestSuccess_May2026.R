##
# Determining Nest Success 
# Species: RAZO
# Created: May 15, 2026
# Creators: Abigail Muscat, add your names!
# A few notes as I have been going: in order to have each individual unique I combined Nest.ID Chamber and Plot. This was a bit of trial an error but I discovered that plots can have the same nest IDs. I made a chick stage df with as many possible statuses as I could think of, but we will likely need to add more as we encounter them (same for feathers). Within the hatch date function I added a calculation to determine the final check, but realized it is more nuanced (it can be last NC, except if there are times we miss a nest one time and check it the next). I am honestly not sure if the date of last check is needed. I was thinking of it as a measure of if we stopped monitoring early due to failure or nothing at all there.The logic progression for classifications might need tweaking, and accounting for NAs. I did a trail run and examined if I agreed with the classification or not (see file I sent). There may be room for adjustments there. Some parts could likely be reorganized.



#load packages
library(dplyr) # for summarizing and mutating everything
library(lubridate) # for dates
library(tidyr) # for pivoting dataframes

#links/resources:
#dplyr cheat sheet on sharepoint!! https://umanitoba.sharepoint.com/:b:/r/sites/DavorenLab/Shared%20Documents/R%20Code%20Tutorials/Cheat%20Sheets/data-transformation.pdf?csf=1&web=1&e=8pE1us 
#https://www.listendata.com/2017/03/if-else-in-r.html
#https://github.com/tidyverse/dplyr/issues/3768
#https://stackoverflow.com/questions/58443566/what-does-na-rm-true-actually-means
#https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/as.Date --> did not end up using
#https://www.rdocumentation.org/packages/cropgrowdays/versions/0.2.2/topics/day_of_year --> did not end up using
#https://www.geeksforgeeks.org/r-language/how-to-use-is-not-na-in-r/
#https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/NA 
#https://dplyr.tidyverse.org/reference/coalesce.html 

# Creating file directory location ----

dir <- "/Volumes/T7 Shield/UofM Masters/NestSuccess/Raw Files"
## Set "Fname" to be the unique identifier for each file needed

Fname_nest <- "2023_RAZOprod"

Fname_weight <- "chick_weights"

# Read in data ----
weight_data <- read.csv(file.path(dir,paste(Fname_weight,".csv", sep = "")))
prod_data <- read.csv(file.path(dir,paste(Fname_nest,".csv", sep = "")))

# might need to add in some code to adjust dates and such here, make columns consistent?

# need hatch date, make hatch interval, fledge interval (C_Last - N_First), how long MFC and how long PFC
#min and max age?
#death date
#fledged status

#adjust and filter weights file ----
str(weight_data)

#convert to DOY
weight_data <- weight_data %>%
  mutate(
    date_clean = trimws(date_clean),
    date_clean = as.Date(date_clean, format = "%m/%d/%y"),
    DOY = lubridate::yday(date_clean)
  )

str(weight_data)

weight_data <- weight_data %>% # filtering out RAZO 2019 since no nest Ids so nothing is uniquely identifying nests, can change if we know the nests, when 2019 was included incorrect weights were being pulled for that year
  filter(!(species == "RAZO" & year == 2019))

#fledging weight ----
chick_weight_info <- function(weight_data,
                              fledge_weight) {
  #pull in weight information
  
  weight_hits <- weight_data %>%
    filter(!is.na(chick_weight)) %>%
    mutate(reached_fledge = chick_weight >= fledge_weight) %>%
    group_by(year, species, nest_id, plot) %>%
    summarize(
      ever_fledged_weight = any(reached_fledge),
      fledge_weight_day = if(any(reached_fledge)) { # first DOY chick reached fledge weight
        min(DOY[reached_fledge])
      } else {
        NA_real_
      },
      fledge_weight_value = if(any(reached_fledge)) { # weight on first DOY above or equal to fledge weight
        chick_weight[which(DOY == min(DOY[reached_fledge]))][1]
      } else {
        NA_real_
      },
      max_weight = if(any(!is.na(chick_weight))) max(chick_weight, na.rm = TRUE) else NA_real_, # max weight reached
      .groups = "drop"
    )
  weight_hits
}


fledge_weightdf <- chick_weight_info(weight_data, fledge_weight = 195)


#fledge_weightdf <- weight_df %>% # realized for classification we actually want to keep FALSE in df
  #filter(ever_fledged_weight %in% c("TRUE"))


fledge_weightdf <- fledge_weightdf %>% # fix weight column names for later 
  rename(Year = year, 
         Species = species, 
         Nest.ID = nest_id, 
         Plot = plot)

# adjust prod file ----
#remove comments columns:
prod_data <- subset(prod_data, select = -c(Exclude., Field.Comment.., Notes))


#mess with prod file - make it so the columns are labelled by DOY (this was the case when I downloaded a field file directly which had dates on the top row, but might be ale to avoid this when setting up a file with all prod data compiled)
meta <- prod_data[1, ]      # first row = DOY row
data <- prod_data[-1, ]     # everything else = nests

doy_vals <- as.numeric(meta[1 , -(1:6)])  
colnames(data)[-(1:6)] <- as.character(doy_vals) # adding a minus excludes that column, so skip to column 7 which is where DOY should start

# force ALL DOY columns to character (this fixed an error I was getting when trying to pivot)
data[ , -(1:6)] <- lapply(data[ , -(1:6)], as.character)

#filter RAZO
RAZO_proddata <- data %>%
  filter(Species %in% c("RAZO")) %>%
  filter(!Control...Experiment...Not.Used %in% c("Not Used")) #in the all sp year file this column is called Type, change it out when using full file; removing crevices not used

# standardize plot names
unique(RAZO_proddata$Plot)

RAZO_proddata$Plot[RAZO_proddata$Plot == "RAZO colony"] <- "MAIN"
RAZO_proddata$Plot[RAZO_proddata$Plot == "colony"] <- "MAIN"
RAZO_proddata$Plot[RAZO_proddata$Plot == "RAZO Jumble"] <- "JUMBLE"

unique(RAZO_proddata$Plot)

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
#figure out all the possible prod options -- once we have all years would need to go through this again, I just used 2023 as a tester year
unique(nest_long$Stage)
#ATPU in a RAZO crevice I am assuming means we saw a puffin so I won't touch that.
#E options: E, A*E, 2AE, 2E, AE, 
#Cw or Ep options: A*Ep, AEp, Cw
# C options: 2AD, A*D, AD, AFE, D, FE, C, F, AF, 2AFE, 2D, 2F, 2FE, A*F, A*FE,

chick_stages <- c(
  "AD", "A*D", "AFE", "D", "FE", "C", "F",
  "AF", "2AFE", "2D", "2F", "2FE", "A*F", "A*FE"
)

#can make ones for other things too, I did chick because it had the most options and manually entered the others below

# add unique name to each nest ----         
#add unique identified
nest_long <- nest_long %>%
  unite("Nest_unique", Nest.ID, Chamber, Plot, remove = FALSE)

# how many nests had chicks ever - gives idea of how many won't make the cut for fledge success (either unsuccessful or unknown)
has_chicksFalse <- nest_long %>%
  group_by(Nest_unique, Year) %>% # doing year in case there is reoccuring Nest_uniques
  summarize(
    has_chicks = any(Stage %in% chick_stages)
  ) %>%
  filter(!has_chicks)

# hatch date (E_last, C_first, Ep or Cw date), death date, etc ----

hatch_dates <- nest_long %>%
  group_by(Nest_unique, Year) %>% # doing year in case there is reoccuring Nest_uniques
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
    Death_date = if(any(Stage %in% c("X", "Eb", "Ex"))) {
      min(DOY[Stage %in% c("X", "Eb", "Ex")], na.rm = TRUE)
    } else {
      NA_real_
    }, 
    #last day of check
    Last_check = max(DOY[!is.na(Stage)]), #NOT DOING WHAT WE WANT, I was thinking knowing if we stopped checking early due to failure or N might be worth knowing, but not sure if needed, we never use it again anyways
    
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
    
    U_after_chick = if ( #look for U after chick if no N recorded
      !is.na(C_last) &&
      !any(Stage == "N" & DOY > C_last, na.rm = TRUE) &&
      any(Stage == "U" & DOY > C_last, na.rm = TRUE)
    ) {
      min(DOY[Stage == "U" & DOY > C_last], na.rm = TRUE)
    } else {
      NA_real_
    },
    
    .groups = "drop") %>%
  mutate( #calculate HatchDate by taking the midpoint between E last and C first, unless there is a Ep date that use that
    HatchInterval = as.numeric(C_first - E_last),
    
    HatchDate = case_when(
      !is.na(Ep_date) ~ Ep_date,
      !is.na(E_last) & !is.na(C_first) ~
        E_last + (C_first - E_last)/2,
      TRUE ~ NA_real_
    ),
    
    F_interval_N = N_after_chick - C_last, # calculate Fledge interval same as excel algorithm
    F_interval_U = U_after_chick - C_last # calculate Fledge interval same as excel algorithm
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
      !is.na(E_last) ~ 
        coalesce(N_after_chick, U_after_chick) - E_last, #first look for E_last and use that - priority 1
      !is.na(C_first) ~ 
        coalesce(N_after_chick, U_after_chick) - C_first, # if no E_last do this - priority 2 (
      TRUE ~ NA_real_
    )
  )

# add metadata back
hatch_dates <- hatch_dates %>%
  left_join(
    nest_long %>%
      distinct(Nest_unique, Species, Chamber, Nest.ID, Plot, Control...Experiment...Not.Used), #again may need to change to 'Type', also we filtered by Species earlier so species may be redundant here
    by = "Nest_unique" ) %>%
      dplyr::select( # reorder so the identifying columns comes first
        Nest_unique,
        Species,
        Chamber,
        Nest.ID,
        Plot,
        Control...Experiment...Not.Used,
        everything()
      )


#observed feather stage duration (not biological) ----
feather_groups <- list(
  down = c("D", "DFC", "AD", "A*D", "2AD"),
  featheremerge  = c("FE", "PFC", "AFE", "2AFE", "A*FE"),
  feathered = c("F", "MFC", "AF", "2AF", "A*F")
)


feather_lookup <- tibble( #table of feather groups
  Stage = unlist(feather_groups),
  feather_stage = rep(names(feather_groups), lengths(feather_groups))
)
  

feather_durations <- nest_long %>%
  inner_join(feather_lookup, by = "Stage") %>% #join feather groups with nest_long to calculate how long each feather status was noted for each chick
  group_by(Nest_unique, feather_stage) %>%
  summarize(
    start = min(DOY),
    end   = max(DOY),
    duration = end - start +1, # add one to make inclusive, if only one day then it is not 0
    .groups = "drop"
  )

feather_wide <- feather_durations %>% #convert long df to wide format
  select(Nest_unique, feather_stage, duration) %>%
  pivot_wider(
    names_from = feather_stage,
    values_from = duration
  )



RAZO_prod_mainfile <- hatch_dates %>% #add in feather duration info to hatch date info
  left_join(feather_wide, by = "Nest_unique")




#join weights into main data 
RAZO_prod_mainfile <- RAZO_prod_mainfile %>%
    left_join(fledge_weightdf, by = c("Year", "Species", "Nest.ID", "Plot"))



  
### Classification Code.----

classification_df <- RAZO_prod_mainfile %>%
  mutate(
    
    # Failure conditions (Unsuccessful)
    fail_death = !is.na(Death_date),
    
    fail_age = C_min_age < 13, # is this too harsh?
    
    fail_feather_missing =
      is.na(featheremerge) & is.na(feathered)   # no feather development info at all
    
  ) %>%
    
  mutate(
    nest_success_class = case_when( # I only used C_min_age, could consider adding in C_max_age?
      
      # Unsuccessful 
      fail_death |
        fail_age |
        fail_feather_missing ~ "Unsuccessful",
      
      #Almost certainly successful
      !is.na(N_after_chick) & #left out the U for very certain ones
        C_min_age >= 16 &
        !is.na(feathered) & #could change this to be the duration of feather rather than presence
        ever_fledged_weight == TRUE  ~ "Almost certaintly successful",
      
      #Most likely successful
      (!is.na(N_after_chick) | !is.na(U_after_chick)) & 
        C_min_age >= 16 &
        !is.na(feathered) & #could change this to be the duration of feather rather than presence
        is.na(ever_fledged_weight)  ~ "Most likely successful", #weight is unknown, 
              
      #Probably successful
        (!is.na(N_after_chick) | !is.na(U_after_chick)) & 
        C_min_age >= 16 &
        (ever_fledged_weight == TRUE | featheremerge >= 11 | feathered <= 6) ~ "Probably successful",
      
      #Decent chance successful
      (!is.na(N_after_chick) | !is.na(U_after_chick)) & 
        C_min_age >= 13 &
        (ever_fledged_weight == TRUE | featheremerge >= 11 | feathered <= 6) ~ "Decent chance successful", 
      
      #Possibly successful
      C_min_age >= 13 &
        (featheremerge >= 11 | feathered <= 6) ~ "Possibly successful", #removed weight requirement since it is redundant and is included in probably and decent chance
      
      #Aspiring Fledger
      (is.na(N_after_chick) | is.na(U_after_chick)) & #some birds that do have an N or U are falling into this because they don't meet the feathered thresholds of possibly successful
        (ever_fledged_weight == FALSE |featheremerge <6 | feathered >10) ~ "Aspiring Fledger", 
      
      #Unknown/Insufficient Data
      is.na(C_min_age) & is.na(featheremerge) & is.na(feathered) ~ "no_data",
      C_min_age < 13 ~ "too_young",
      TRUE ~ "unclassified_other"))
    
      
      
write.csv(classification_df, "classification2023_test.csv", row.names = FALSE)
      
      

# Making the classification into a function where we could input values to quickly make changes might be beneficial potentially. I abandoned it only because I was getting confused what was what when setting it up.
#classify fledging status 
classify_fledging <- function(prod_mainfile, # making function so these values are easy to change out (min duration = average minimum chick-rearing duration, reasonable duration is lower chick-rearing duration, FE is feathers emerging status duration, F is feathered)
                              min_duration,
                              reasonable_duration,
                              FE_duration,
                              F_duration) {}
      
  classify_fledging(RAZO_prod_mainfile, min_duration = 16, reasonable_duration = 13, FE_duration = 11, F_duration = 6)
  
  
  
      
    ### OLD CODE
    has_fledged_signal = !is.na(N_after_chick),
    has_death = !is.na(Death_date),
    
    min_age_ok = C_min_age >= 16,
    reasonable_age_ok = C_min_age >= 13,
    
    feather_score = case_when (
      (featheremerge >= 11 | feathered <= 6) ~ 2, 
      (featheremerge >= 6 & featheremerge < 11) | (feathered > 6 & feathered <= 10) ~1
      (featheremerge <6 | feathered >10), 
      TRUE ~ 0
    ), 
    
    age_score = case_when (
      has_fledged_signal & 
    )

    strong_PFCFEfeather_dev = featheremerge >= 11 , # PFC/FE duration > or equal to 11
    strong_MFCFfeather_dev  = feathered <= 6, #MFC/F duration < or equal to 6 since this stage is short that we see
    strong_feather_signal = strong_MFCFfeather_dev | strong_PFCFEfeather_dev,
    
    weak_PFCFEfeather_dev = featheremerge <6, #not FE reasonably long enough
    weak_MFCFfeather_dev = feathered >10, # too much uncertainty, maybe too long interval between checks
    
    weak_feather_signal = weak_MFCFfeather_dev | weak_PFCFEfeather_dev,
    
    neutral_PFCFE = featheremerge >= 6 & featheremerge < 11,
    neutral_MFCF  = feathered > 6 & feathered <= 10,
    
    neutral_feather_signal = neutral_PFCFE | neutral_MFCF
    
  )





  
         
      

