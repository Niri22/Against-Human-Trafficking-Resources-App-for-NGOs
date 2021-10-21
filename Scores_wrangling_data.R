###Librar##
library(dplyr)
library(readr)
library(data.table)
library(DT)

###load data###
Scores <- read_csv("can_scores_quintiles_EN.csv")
PRCDDA_lang_long <- read_csv("2016_92-151_XBB.csv")

###Clean data ###

#IN PRCDDA, need to cut out Yukon, Northwest Territores and Nunavat because 
#we don't have data for them in the scores file

Clean1 <- subset.data.frame(PRCDDA_lang_long, 
                            PRCDDA_lang_long$`PRename/PRanom` == "Newfoundland and Labrador")

provincelst <- c("Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia", 
                 "New Brunswick", "Quebec", "Ontario", "Manitoba", "Saskatchewan", 
                 "Alberta", "British Columbia")
for(prov in 2:length(provincelst)) {
  ProvinceSub <- subset.data.frame(PRCDDA_lang_long, 
                                   PRCDDA_lang_long$`PRename/PRanom` == provincelst[prov])
  Clean1 <- rbind(Clean1, ProvinceSub)
}

#Now, we only require data from PRCDDA which tells us details about location that can displayed

Clean2 <- Clean1 %>% select("DAuid/ADidu", "DArplamx/ADlamx", "DArplamy/ADlamy", "DArplat/Adlat",
                            "DArplong/ADlong", "PRename/PRanom", "PRfname/PRfnom", 
                            "CDname/DRnom", "CDtype/DRgenre")

##remove repeated rows
Clean3 <-unique.data.frame(Clean2)

##rename "DAuid/ADidu" to PRCDDA and change name of lat and lang
Clean3 <- rename(Clean3, PRCDDA = "DAuid/ADidu")
Clean3 <- rename(Clean3, Lattitude = "DArplat/Adlat")
Clean3 <- rename(Clean3, Longitude = "DArplong/ADlong")

###Merge Clean3 and Scores dataset
Clean4 <- merge.data.frame(Scores, Clean3, by = "PRCDDA")

###Save as new dataframe ### 
write.csv(Clean4, "CDI_lang_lat.csv")
