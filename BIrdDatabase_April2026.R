### 
# Database Query of Bird Data
# Davoren Lab
# Created April 27, 2026
# Creator: Abigail Muscat
# Purpose: to create query information from bird banding, FTA, feather sampling, and tagging mainfiles to answer a question (e.g. What tags need to be retrieved, which birds need sex results?)

# load packages
library(dplyr)

#load files - download mainfiles from sharepoint and convert to csv
dir <- "/Users/abigail/R Studio/MSc_Project_2026" # change for your directory 

Fname1 <- "Banding_data_ALLYEARS_MAINFILE_20260421"
Fname2 <- "Tagging_Metadata_ALL_YEARS_20260421"
Fname4 <- "SexResults_20260428"

old_bandingdata <- read.csv(file.path(dir,paste(Fname1,".csv", sep = "")))
bandingdata <- old_bandingdata[, 1:46]
old_taggingdata <- read.csv(file.path(dir,paste(Fname2,".csv", sep = "")))
taggingdata <- old_taggingdata[, 1:29]
sexresults <- read.csv(file.path(dir,paste(Fname4,".csv", sep = "")))

str(sexresults)
str(bandingdata)

#combine sex results and banding and keep sex results from the sex results file
# next step is because the sexresutls are the truth file for sex, banding data has sex but it might be incorrect or partially filled

birds_final <- bandingdata %>%
  left_join(sexresults, by = "Band.Number")


#keep the new sex column added
names(birds_final)
birds_final <- birds_final %>%
  select(c(1:16, 18:46, 50))

names(birds_final)



#check what values are under each test
unique(bandingdata$Blood.Sample.Taken)
unique(bandingdata$Feather.Sample.Taken)
unique(bandingdata$Tests.Performed.Desc)

#make blanks NA
bandingdata <- bandingdata %>%
  mutate(
    Blood.Sample.Taken = na_if(Blood.Sample.Taken, ""),
    Feather.Sample.Taken = na_if(Feather.Sample.Taken, ""),
    Tests.Performed.Desc = na_if(Tests.Performed.Desc, "")
  )

#confirm it worked
unique(bandingdata$Blood.Sample.Taken)
unique(bandingdata$Feather.Sample.Taken)
unique(bandingdata$Tests.Performed.Desc)

#summarize TRUE/FALSE whether each bird (one row per band number) recieved the three types of tests (blood, feather, and FTA) - fecal is not accounted for here, can add it if interested
banding_summary <- bandingdata %>%
  group_by(Band.Number) %>%
  summarize(
    Blood_Collected = any(Blood.Sample.Taken == "Y", na.rm = TRUE),
    Feather_Collected = any(Feather.Sample.Taken == "Y", na.rm = TRUE),
    FTA_Test = any(Tests.Performed.Desc == "FTA", na.rm = TRUE),
    .groups = "drop"
  )

#add banding summary to birds_final (no longer one row per band number)
birds_final <- birds_final %>%
  left_join(banding_summary, by = "Band.Number")


#need to make sure all dataframes are joined by the same column
taggingdata <- taggingdata %>% # rename column Band to Band.Number
  rename(Band.Number = Band)

str(taggingdata)  
str(birds_final)

unique(taggingdata$Tag.ID) # check for blanks

taggingdata <- taggingdata %>% # to be safe, change blanks to NAs
  mutate(Tag.ID = na_if(Tag.ID, ""))

tagging_summary <- taggingdata %>% #create TRUE/FALSE whether each bird was ever tagged and count the number of tags per bird
  group_by(Band.Number) %>%
  summarize(
    Tagged = TRUE,
    N_Tags = n_distinct(Tag.ID, na.rm = TRUE),
    .groups = "drop"
  )

#add tagging to birds_final
birds_final <- birds_final %>%
  left_join(tagging_summary, by = "Band.Number")


#now we need to figure out which ones need retrieval (making TRUE/FALSE column like the sample collected columns above)
unique(taggingdata$TimeLocalRetrieve)

taggingdata <- taggingdata %>% # make blanks NA
  mutate(TimeLocalRetrieve = na_if(TimeLocalRetrieve, ""))
unique(taggingdata$TimeLocalRetrieve)

unique(taggingdata$TimeLocalRetrieve) # L means lost, N is that tag was likely retrieved although no record

tag_status <- taggingdata %>% # determine tag status, if NA is in TimeLocalRetrieve then that tag needs retrieval (more or less)
  group_by(Band.Number) %>%
  summarize(
    Tag_Retrieval_Needed = any(is.na(TimeLocalRetrieve)),
    .groups = "drop"
  )

birds_final_tag <- birds_final %>%
  left_join(tag_status, by = "Band.Number")


#now we make our files
names(birds_final_tag)

# to make the Core Table Bird Information file we need Band.Number, prefix, suffix, Sex.y, Blood_Collected, Feather_Collected, FTA_Test, Tagged, N_Tags, and Tag_Retrieval_Needed
birdcoretable =subset(birds_final_tag, select = c(Species.x, Band.Number, prefix.x, suffix.x, Sex.y, Blood_Collected, Feather_Collected, FTA_Test, Tagged, N_Tags, Tag_Retrieval_Needed))

birdcoretable <- birdcoretable %>% # make column names pretty
  rename(Species = Species.x,
         prefix = prefix.x, 
         suffix = suffix.x, 
         Sex = Sex.y)

write.csv(birdcoretable, "core_bird_table_20260421.csv", row.names = FALSE) #change date based on date of creation

#make tags to retrieve file, need to add back in some burrow information (there may be a better way to do this earlier)
birds_final_tag_info <- birds_final_tag %>% # join back in tagging data, you will get a warning about duplicate rows that is expected
  left_join(taggingdata, by = "Band.Number")

names(birds_final_tag_info) # take the columns we are interested in, kept whether data was downloaded as a secondary check and since GPS can download remotely 
tags_to_retrieve =subset(birds_final_tag_info, select = c(Species.x, Band.Number, prefix.x, suffix.x, Burrow.ID, Bird.Field.ID, Sex.y, Tag.ID, Tag.Model, TimeUTCdeploy, TimeLocalDeploy,TimeLocalRetrieve, Data.Downloaded., Blood_Collected, Feather_Collected, FTA_Test, Tagged, N_Tags, Tag_Retrieval_Needed))

tags_to_retrieve  <- tags_to_retrieve %>% # filter those with tag retrieval needed = TRUE and where retrieve time is NA (not retrieved), we are removing lost ones here
  filter(Tag_Retrieval_Needed == "TRUE") %>% 
  filter(is.na(TimeLocalRetrieve))

tags_to_retrieve_final <- tags_to_retrieve %>% #because of duplication, some rows are identical. Remove those here
  arrange(Band.Number) %>%
  distinct() 

write.csv(tags_to_retrieve, "Tags to Retrieve YEAR.csv", row.names = FALSE) #add year at end

