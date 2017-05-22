library("dplyr")

#sahie_2014 <- read.csv(file.path("Data/", "sahie_2014.csv"),sep = ",", skip=79, strip.white = TRUE, na.strings=c(""))
#sahie_2015 <- read.csv(file.path("Data/", "sahie_2015.csv"),sep = ",", skip=79, strip.white = TRUE, na.strings=c(""))
#hcahps_201404_201503 <- read.csv(file.path("Data", "HCAHPS_Hospital_201404-201503.csv"),sep = ",", strip.white = TRUE, na.strings=c("", "Not Available"))
#readmit_201307_201406 <- read.csv(file.path("Data", "Readmissions_and_Deaths 201307-201406_Hospital.csv"),sep = ",", strip.white = TRUE, na.strings=c("", "Not Available"))
#readmit_201407_201506 <- read.csv(file.path("Data", "Readmissions_and_Deaths 201407-201506_Hospital.csv"),sep = ",", strip.white = TRUE, na.strings=c("", "Not Available"))

sahie_2014_clean <- read.csv(file.path("Data/", "sahie_2014.csv"),stringsAsFactors = FALSE, sep = ",", skip=79, strip.white = TRUE, na.strings=c("")) %>%
  filter(agecat == 0, racecat == 0, sexcat == 0, iprcat == 0, county_name != "NA") %>%
  select(year, state_name, county_name, PCTELIG)

sahie_2014_clean$PCTELIG <- as.numeric(sahie_2014_clean$PCTELIG)
sahie_2014_clean$state_name <- as.factor(state.abb[match(sahie_2014_clean$state_name, state.name)])
sahie_2014_clean$county_name <- as.factor(toupper(sahie_2014_clean$county_name))


sahie_2015_clean <- read.csv(file.path("Data/", "sahie_2015.csv"),stringsAsFactors = FALSE, sep = ",", skip=79, strip.white = TRUE, na.strings=c("")) %>%
  filter(agecat == 0, racecat == 0, sexcat == 0, iprcat == 0, county_name != "NA") %>%
  select(year, state_name, county_name, as.numeric(PCTELIG))
  
sahie_2015_clean$PCTELIG <- as.numeric(sahie_2015_clean$PCTELIG)
sahie_2015_clean$state_name <- as.factor(state.abb[match(sahie_2015_clean$state_name, state.name)])
sahie_2015_clean$county_name <- as.factor(toupper(sahie_2015_clean$county_name))

hcahps_clean <- hcahps_201404_201503 <- read.csv(file.path("Data", "HCAHPS_Hospital_201404-201503.csv"),stringsAsFactors = FALSE, sep = ",", strip.white = TRUE, na.strings=c("", "Not Available")) %>%
  filter(HCAHPS.Measure.ID == "H_COMP_6_Y_P", County.Name != "NA", HCAHPS.Answer.Percent != "NA") %>%
  select(Measure.Start.Date, Measure.End.Date, State, County.Name, HCAHPS.Measure.ID, HCAHPS.Answer.Percent)

hcahps_clean$Measure.End.Date <- as.Date(hcahps_clean$Measure.End.Date, "%m/%d/%Y")
hcahps_clean$Measure.Start.Date <- as.Date(hcahps_clean$Measure.Start.Date, "%m/%d/%Y")
hcahps_clean$State <- as.factor(hcahps_clean$State)
hcahps_clean$County.Name <- as.factor(hcahps_clean$County.Name)

readmit_201307_201406_clean <- read.csv(file.path("Data", "Readmissions_and_Deaths 201307-201406_Hospital.csv"),stringsAsFactors = FALSE, sep = ",", strip.white = TRUE, na.strings=c("", "Not Available")) %>%
  filter(Measure.ID == "READM_30_HOSP_WIDE", County.Name != "NA", Score != "NA" ) %>%
  select(Measure.Start.Date, Measure.End.Date, State, County.Name, Measure.ID, Score)

readmit_201307_201406_clean$Measure.Start.Date <- as.Date(gsub( "7/1/13", "07/01/2013"  ,readmit_201307_201406_clean$Measure.Start.Date), "%m/%d/%Y")
readmit_201307_201406_clean$Measure.End.Date <- as.Date(gsub( "6/30/14", "06/30/2014"  ,readmit_201307_201406_clean$Measure.Start.Date), "%m/%d/%Y")
readmit_201307_201406_clean$State <- as.factor(readmit_201307_201406_clean$State)
readmit_201307_201406_clean$County.Name <- as.factor(readmit_201307_201406_clean$County.Name)

readmit_201407_201506_clean <- read.csv(file.path("Data", "Readmissions_and_Deaths 201407-201506_Hospital.csv"),stringsAsFactors = FALSE, sep = ",", strip.white = TRUE, na.strings=c("", "Not Available")) %>%
  filter(Measure.ID == "READM_30_HOSP_WIDE", County.Name != "NA", Score != "NA" ) %>%
  select(Measure.Start.Date, Measure.End.Date, State, County.Name, Measure.ID, Score)

readmit_201407_201506_clean$Measure.Start.Date <- as.Date(gsub( "7/1/14", "07/01/2014"  ,readmit_201407_201506_clean$Measure.Start.Date), "%m/%d/%Y")
readmit_201407_201506_clean$Measure.End.Date <- as.Date(gsub( "6/30/15", "06/30/2015"  ,readmit_201407_201506_clean$Measure.Start.Date), "%m/%d/%Y")
readmit_201407_201506_clean$State <- as.factor(readmit_201407_201506_clean$State)
readmit_201407_201506_clean$County.Name <- as.factor(readmit_201407_201506_clean$County.Name)


str(sahie_2014_clean)
str(sahie_2015_clean)
str(hcahps_clean)
str(readmit_201307_201406_clean)
str(readmit_201407_201506_clean)