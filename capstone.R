library("dplyr")
library("tidyr")
library("ggplot2")

sahie_2014_clean <- read.csv(file.path("Data/", "sahie_2014.csv"),stringsAsFactors = FALSE, sep = ",", skip=79, strip.white = TRUE, na.strings=c("")) %>%
  filter(agecat == 0, racecat == 0, sexcat == 0, iprcat == 0, county_name != "NA") %>%
  select(year, state_name, county_name, PCTELIG)

sahie_2014_clean$PCTELIG <- as.numeric(sahie_2014_clean$PCTELIG)
sahie_2014_clean$state_name <- as.factor(state.abb[match(sahie_2014_clean$state_name, state.name)])
sahie_2014_clean$county_name <- as.factor(toupper(sahie_2014_clean$county_name))
sahie_2014_clean$county_name <- as.factor(gsub(" COUNTY", "",sahie_2014_clean$county_name))
sahie_2014_clean$county_name <- as.factor(gsub(" PARISH", "",sahie_2014_clean$county_name))

sahie_2015_clean <- read.csv(file.path("Data/", "sahie_2015.csv"),stringsAsFactors = FALSE, sep = ",", skip=79, strip.white = TRUE, na.strings=c("")) %>%
  filter(agecat == 0, racecat == 0, sexcat == 0, iprcat == 0, county_name != "NA") %>%
  select(year, state_name, county_name, as.numeric(PCTELIG))
  
sahie_2015_clean$PCTELIG <- as.numeric(sahie_2015_clean$PCTELIG)
sahie_2015_clean$state_name <- as.factor(state.abb[match(sahie_2015_clean$state_name, state.name)])
sahie_2015_clean$county_name <- as.factor(toupper(sahie_2015_clean$county_name))
sahie_2015_clean$county_name <- as.factor(gsub(" COUNTY", "",sahie_2015_clean$county_name))
sahie_2015_clean$county_name <- as.factor(gsub(" PARISH", "",sahie_2015_clean$county_name))


hcahps_clean <- read.csv(file.path("Data", "HCAHPS_Hospital_201404-201503.csv"),stringsAsFactors = FALSE, sep = ",", strip.white = TRUE, na.strings=c("", "Not Available", "Not Applicable", "FEWER THAN 50")) %>%
  select(Provider.ID, State, County.Name, HCAHPS.Measure.ID, HCAHPS.Linear.Mean.Value) %>%
  spread(HCAHPS.Measure.ID, HCAHPS.Linear.Mean.Value)

#hcahps_final$Measure.End.Date <- as.Date(hcahps_clean$Measure.End.Date, "%m/%d/%Y")
#hcahps_final$Measure.Start.Date <- as.Date(hcahps_clean$Measure.Start.Date, "%m/%d/%Y")
hcahps_final$State <- as.factor(hcahps_clean$State)
hcahps_final$County.Name <- as.factor(hcahps_clean$County.Name)

str(hcahps_final)

readmit_201307_201406_clean <- read.csv(file.path("Data", "Readmissions_and_Deaths 201307-201406_Hospital.csv"),stringsAsFactors = FALSE, sep = ",", strip.white = TRUE, na.strings=c("", "Not Available")) %>%
  filter(Measure.ID == "READM_30_HOSP_WIDE", County.Name != "NA", Score != "NA" ) %>%
  select(Measure.Start.Date, Measure.End.Date, State, County.Name, Measure.ID, Score)

#readmit_201307_201406_clean$Measure.Start.Date <- as.Date(gsub( "7/1/13", "07/01/2013"  ,readmit_201307_201406_clean$Measure.Start.Date), "%m/%d/%Y")
#readmit_201307_201406_clean$Measure.End.Date <- as.Date(gsub( "6/30/14", "06/30/2014"  ,readmit_201307_201406_clean$Measure.End.Date), "%m/%d/%Y")
readmit_201307_201406_clean$State <- as.factor(readmit_201307_201406_clean$State)
readmit_201307_201406_clean$County.Name <- as.factor(readmit_201307_201406_clean$County.Name)

readmit_201407_201506_clean <- read.csv(file.path("Data", "Readmissions_and_Deaths 201407-201506_Hospital.csv"),stringsAsFactors = FALSE, sep = ",", strip.white = TRUE, na.strings=c("", "Not Available")) %>%
  filter(Measure.ID == "READM_30_HOSP_WIDE", County.Name != "NA", Score != "NA" ) %>%
  select(Measure.Start.Date, Measure.End.Date, State, County.Name, Measure.ID, Score)

#readmit_201407_201506_clean$Measure.Start.Date <- as.Date(gsub( "7/1/14", "07/01/2014"  ,readmit_201407_201506_clean$Measure.Start.Date), "%m/%d/%Y")
#readmit_201407_201506_clean$Measure.End.Date <- as.Date(gsub( "6/30/15", "06/30/2015"  ,readmit_201407_201506_clean$Measure.End.Date), "%m/%d/%Y")
readmit_201407_201506_clean$State <- as.factor(readmit_201407_201506_clean$State)
readmit_201407_201506_clean$County.Name <- as.factor(readmit_201407_201506_clean$County.Name)

sahie_final <- inner_join(sahie_2014_clean, sahie_2015_clean, by=c("state_name" = "state_name", "county_name" = "county_name")) %>%
  mutate( combined_sahie_metric = (PCTELIG.x*0.75)+(PCTELIG.y*0.25)) %>%
  select(state_name, county_name, PCTELIG.x, PCTELIG.y, combined_sahie_metric) %>%
  group_by(State = state_name, County.Name = as.factor(county_name) ) %>%
  summarise(county_unins_average = mean(combined_sahie_metric))

readmit_final <- inner_join(readmit_201307_201406_clean, readmit_201407_201506_clean, by=c( "State" = "State", "County.Name" = "County.Name")) %>%
  mutate(combined_readmit_metric = (Score.x*0.25)+(Score.y*0.75)) %>%
  select(State, County.Name, Score.x, Score.y, combined_readmit_metric) %>%
  group_by(State = as.factor(State), County.Name = as.factor(County.Name)) %>%
  summarise(county_readmit_average = mean(combined_readmit_metric))

hcahps_select_target <- c("H_CLEAN_LINEAR_SCORE", "H_COMP_1_LINEAR_SCORE", "H_COMP_2_LINEAR_SCORE", "H_COMP_3_LINEAR_SCORE", "H_COMP_4_LINEAR_SCORE", "H_COMP_5_LINEAR_SCORE", "H_COMP_6_LINEAR_SCORE", "H_COMP_7_LINEAR_SCORE", "H_HSP_RATING_LINEAR_SCORE", "H_QUIET_LINEAR_SCORE", "H_RECMND_LINEAR_SCORE")
hcahps_final <- hcahps_clean %>% 
  group_by(State, County.Name) %>%
  summarise_each(funs(mean(.,na.rm = TRUE)), one_of(hcahps_select_target))

data_final <- inner_join(readmit_final, hcahps_final, by = c("State" = "State", "County.Name" = "County.Name")) %>%
  inner_join(., sahie_final, by = c("State" = "State", "County.Name" = "County.Name"))


ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

for(i in 4:15) {
analyze_data <- data_final[c(3,i)]
lm1 <- lm(county_readmit_average ~ ., data=analyze_data)
print(colnames(data_final[i]))
print(summary(lm1)$adj.r.squared)
}


analyze_data <- data_final[c(3,5,6,7,10,11,15)]
lm2 <- lm(county_readmit_average ~ ., data=analyze_data)
summary(lm2)

ggplotRegression(lm2)


##############################################################

readmit_201307_201406_clean <- read.csv(file.path("Data", "Readmissions_and_Deaths 201307-201406_Hospital.csv"),stringsAsFactors = FALSE, sep = ",", strip.white = TRUE, na.strings=c("", "Not Available")) %>%
  filter(Measure.ID == "READM_30_HOSP_WIDE", Score != "NA" ) %>%
  select(Provider.ID, Score)

readmit_201307_201406_clean$Provider.ID <- as.factor(readmit_201307_201406_clean$Provider.ID)
str(readmit_201307_201406_clean)

readmit_201407_201506_clean <- read.csv(file.path("Data", "Readmissions_and_Deaths 201407-201506_Hospital.csv"),stringsAsFactors = FALSE, sep = ",", strip.white = TRUE, na.strings=c("", "Not Available")) %>%
  filter(Measure.ID == "READM_30_HOSP_WIDE", Score != "NA" ) %>%
  select(Provider.ID, Score)
  
readmit_201407_201506_clean$Provider.ID <- as.factor(readmit_201407_201506_clean$Provider.ID)
str(readmit_201407_201506_clean)

readmit_final <- inner_join(readmit_201307_201406_clean, readmit_201407_201506_clean, by=c( "Provider.ID" = "Provider.ID")) %>%
  mutate(combined_readmit_metric = (Score.x*0.25)+(Score.y*0.75)) %>%
  select(Provider.ID, combined_readmit_metric)
str(readmit_final)

hcahps_select_target <- c("H_CLEAN_LINEAR_SCORE", "H_COMP_1_LINEAR_SCORE", "H_COMP_2_LINEAR_SCORE", "H_COMP_3_LINEAR_SCORE", "H_COMP_4_LINEAR_SCORE", "H_COMP_5_LINEAR_SCORE", "H_COMP_6_LINEAR_SCORE", "H_COMP_7_LINEAR_SCORE", "H_HSP_RATING_LINEAR_SCORE", "H_QUIET_LINEAR_SCORE", "H_RECMND_LINEAR_SCORE")
hcahps_clean <- read.csv(file.path("Data", "HCAHPS_Hospital_201404-201503.csv"),stringsAsFactors = FALSE, sep = ",", strip.white = TRUE, na.strings=c("", "Not Available", "Not Applicable", "FEWER THAN 50")) %>%
  filter(HCAHPS.Measure.ID %in% hcahps_select_target) %>%
  select(Provider.ID, HCAHPS.Measure.ID, HCAHPS.Linear.Mean.Value) %>%
  spread(HCAHPS.Measure.ID, HCAHPS.Linear.Mean.Value)

hcahps_clean$Provider.ID <- as.factor(hcahps_clean$Provider.ID)


str(hcahps_clean)



data_final <- inner_join(readmit_final, hcahps_clean, by = c("Provider.ID" = "Provider.ID"))
View(data_final)

analyze_data <- data_final[c(2:13)]
lm2 <- lm(combined_readmit_metric ~ ., data=analyze_data)
summary(lm2)

ggplotRegression(lm2)

analyze_data <- data_final[c(6,10,11,12,13,15,16)]
lm1 <- lm(combined_readmit_metric ~ ., data=analyze_data)
summary(lm1)



for(i in 9:19) {
  analyze_data <- data_final[c(6,i)]
  lm1 <- lm(combined_readmit_metric ~ ., data=analyze_data)
  print(colnames(data_final[i]))
  print(summary(lm1)$adj.r.squared)
} 

