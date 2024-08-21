#Monitoring the Future Survey 2022
#Clean and analyze the MTF22 survey
#Written by Sangam Shrestha

################################################################################

#### Loading ####

#load the packages
library(haven)
library(survey)
library(tidyverse)
library(magrittr)
library(plyr)
library(tableone)
library(dplyr)
library(MatchIt)
library(ggplot2)
library(survival)
library(car)
library(gtsummary)
library(optmatch)

################################################################################

#### cleaning ####

#lets load the MTA RDA files
load("Data/Raw/38882-0001-Data.rda")
load("Data/Raw/38882-0005-Data.rda")

#list the columns we want
cols1 <- c("RESPONDENT_ID", "ARCHIVE_WT", "V3", "RESPONDENT_AGE", "V2150", "V2151", "V2179", "V7780")
cols5 <- c("RESPONDENT_ID", "V4491", "V4492")

#create the databases with the columns we want
data1 <- subset(da38882.0001, select = cols1)
data5 <- subset(da38882.0005, select = cols5)

#get only needed rows
data1 <- data1[data1$V3 %in% c("(4) FORM 4:(4)"),] #keep only DS5 students
data1 <- data1[!(is.na(data1$V7780)),] #keep only students with no missing vape use values
data1$vape <- ifelse(data1$V7780=="(1) NEVER:(1)", 0, 1)
data1$vape <- as.factor(data1$vape)

#merge the files
all_data <- merge(x = data1, y = data5, by = "RESPONDENT_ID", all.x = TRUE)
all_data <- subset(all_data, select = -c(V3, V7780))

#clean the variables into only being numbers 
cleaning_cat <- c("RESPONDENT_AGE", "V2150", "V2151", "V2179", "V4491", "V4492") 
all_data[cleaning_cat] <- lapply(all_data[cleaning_cat], as.character)
all_data[cleaning_cat] <- lapply(all_data[cleaning_cat], function(x) substr(x, 2,2))
all_data[cleaning_cat] <- lapply(all_data[cleaning_cat], as.numeric)
all_data[cleaning_cat] <- lapply(all_data[cleaning_cat], function(x) x-1)
all_data[cleaning_cat] <- lapply(all_data[cleaning_cat], as.factor)

#clean up some of the variables for DS5
all_data$V4491 <- as.numeric(all_data$V4491)
all_data$V4492 <- as.numeric(all_data$V4492)
all_data$feeling <- ifelse(all_data$V4491>all_data$V4492, all_data$V4491, all_data$V4492)
all_data$feeling <- ifelse(all_data$feeling>1,0,1)
all_data$feeling <- as.factor(all_data$feeling)

#clean up sex var
all_data$V2150 <- as.numeric(all_data$V2150)
all_data$V2150 <- ifelse(all_data$V2150>2, 2, all_data$V2150-1)
all_data$V2150 <- as.factor(all_data$V2150)

all_data <- subset(all_data, select = -c(V4491, V4492))

#flip variables
all_data$V2151 <- ifelse(all_data$V2151==1, 0, ifelse(all_data$V2151==0,1,ifelse(all_data$V2151==2, 2 ,all_data$V2151)))
all_data$V2151 <- as.factor(all_data$V2151)

all_data$V2179 <- ifelse(all_data$V2179 %in% c(8,7),0, ifelse(all_data$V2179 %in% c(6,5,4),1,ifelse(all_data$V2179 %in% c(3,2,1), 2, ifelse(all_data$V2179==0,3,all_data$V2179))))

all_data$V2179 <- as.factor(all_data$V2179)

#rename
names(all_data)[names(all_data) == "RESPONDENT_AGE"] <- "Age"
names(all_data)[names(all_data) == "V2150"] <- "Sex"
names(all_data)[names(all_data) == "V2151"] <- "RaceEthnicity"
names(all_data)[names(all_data) == "V2179"] <- "Grade"

#final cleanup
all_data2 <- all_data

all_data <- all_data %>% drop_na()
all_data <- as.data.frame(all_data)

################################################################################

#### Analysis ####

#univariate analysis first
svy_design <- svydesign(id=~RESPONDENT_ID, weights=~ARCHIVE_WT, data=all_data2)

#get variables of interest
cat2 <- c("vape", "Age", "Sex", "RaceEthnicity", "Grade")

#table 1
unweightedtable<-CreateTableOne(data=all_data2, strata="feeling", vars = cat2, test=TRUE, addOverall = TRUE, includeNA = TRUE)
print(unweightedtable$CatTable, showAllLevels = TRUE)
summary(unweightedtable)
table1f <- print(unweightedtable$CatTable, showAllLevels = TRUE, format = "f", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
table1f

weightedtable <-svyCreateTableOne(strata = "feeling", vars = cat2, data = svy_design, test = TRUE, addOverall = TRUE)
print(weightedtable$CatTable, showAllLevels = TRUE)
summary(weightedtable)
table1p <- print(weightedtable$CatTable, showAllLevels = TRUE, format = "p", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
table1p

#Save to a CSV file
write.csv(table1f, file = "Output/table1f.csv")
write.csv(table1p, file = "Output/table1p.csv")

#table 2

#initial logistic regression model
basic_model <- glm(vape~feeling , data = all_data, family="binomial")
basic_model
exp(cbind(OR = coef(basic_model), confint(basic_model)))

basic_model_adjusted <- glm(vape~feeling + Age + Sex + RaceEthnicity + Grade , data = all_data, family="binomial")
basic_model_adjusted
exp(cbind(OR = coef(basic_model_adjusted), confint(basic_model_adjusted)))

####PS####

#matching
m.outPS <- matchit(feeling ~ Age + Sex + RaceEthnicity + Grade, 
                   data = all_data, method="full", ratio = 1,
                   replace = FALSE)
m.outPS

summary(m.outPS)
plot(summary(m.outPS))
plot(m.outPS,type="jitter")
plot(m.outPS,type="histogram")

matchedPS<-match.data(m.outPS)

#get data for table 1 for PS
matchedPStable1<-subset(matchedPS, select = -c(subclass, RESPONDENT_ID , ARCHIVE_WT, distance))

Table1V2<-CreateTableOne(data=matchedPStable1, strata="feeling", vars = cat2, test=TRUE, addOverall = TRUE, includeNA = TRUE)

table1psf <- print(Table1V2$CatTable, showAllLevels = TRUE, format = "f", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
table1psf

table1psp <- print(Table1V2$CatTable, showAllLevels = TRUE, format = "p", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
table1psp


#Save to a CSV file
write.csv(table1psf, file = "Output/table1psf.csv")
write.csv(table1psp, file = "Output/table1psp.csv")

matchedPS$vape <- as.numeric(matchedPS$vape)

#conditional logistic regression model 1
fit.clr <- clogit(vape ~ feeling + strata(subclass), data = matchedPS)
fit.clr
#Regression OR with CI
OR.CI <- cbind("OR" = exp(coef(fit.clr)), exp(confint(fit.clr)))
round(OR.CI, 3)
