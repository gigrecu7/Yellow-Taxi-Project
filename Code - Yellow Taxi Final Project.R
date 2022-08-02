#Read the file
JanData <- read.csv('yellow_tripdata_2020-01.csv')


#QUESTION 1 - What one-hour window in January 2020 saw the highest number of taxi pickups

 
library(dplyr)

#Use this code like this for the different questions
#Here we choose the columns we need based on the question 
JanNewData1 <- select(JanData, tpep_pickup_datetime)

#Checking missing values
library(questionr)
freq.na(JanNewData1)

#Structure of data
str(JanNewData1)

install.packages("lubridate")
library(lubridate)


#Split up the pickup column into ymd_hms
JanNewData1$tpep_pickup_datetime <- ymd_hms(JanNewData1$tpep_pickup_datetime)
JanNewData1$PMonth <- factor(month(JanNewData1$tpep_pickup_datetime))
JanNewData1$PDay <- factor(day(JanNewData1$tpep_pickup_datetime))
JanNewData1$PHour <- factor(hour(JanNewData1$tpep_pickup_datetime))

#Here we find the amount of pickups per hour
pickuphourfreq <- count(JanNewData1, c(PHour))

#This shows us the histogram of the data.
library(ggplot2)
ggplot(JanNewData1, aes(x=PHour)) + 
  geom_bar(stat ="count") + geom_bar(color="black", fill="gold")


#Answer to question 1:
#Based from the histogram the highest number of pickups happened 
#in the one hour window of 5:30pm - 6:30pm. 

###########################################################################################

#To further look into pickup frequency data
#What if we focus on a particular day? Jan 1

#We are expecting the most amount of pickups to happen at the beginning of the day because 
#people are celebrating New Years. So hours 12:00am - 2:00am
Jan1Data <- filter(JanNewData1, PMonth==1, PDay ==1)
ggplot(Jan1Data, aes(x=PHour)) + geom_bar(stat ="count") + 
  geom_bar(color="darkorange", fill="cornflowerblue")

#We can see from the plot that our prediction was right. The most amount of pick ups
#happen at the start of the day because people are celebrating. So cool!


###########################################################################################

#We wanted to know more! How did COVID affect the number of pickups?

#Read file
MarchData <- read.csv('yellow_tripdata_2020-03.csv')

#Picking the variables to use
MarchNewData1 <- select(MarchData, tpep_pickup_datetime)

#Checking missing values
library(questionr)
freq.na(MarchNewData1)

#Structure of data
str(MarchNewData1)

install.packages("lubridate")
library(lubridate)

#Split up the pickup column into ymd_hms
MarchNewData1$tpep_pickup_datetime <- ymd_hms(MarchNewData1$tpep_pickup_datetime)
MarchNewData1$PMonth <- factor(month(MarchNewData1$tpep_pickup_datetime))
MarchNewData1$PDay <- factor(day(MarchNewData1$tpep_pickup_datetime))
MarchNewData1$PHour <- factor(hour(MarchNewData1$tpep_pickup_datetime))


#Here we look at number of pickups on a regular day in March before COVID shutdown occured 
RegularDayData <- filter(MarchNewData1, PMonth==3, PDay ==3)
ggplot(RegularDayData, aes(x=PHour)) + 
  geom_bar(stat ="count") + geom_bar(color="palegoldenrod", fill="rosybrown1")

#We see on the y-axis that on a regular day, taxi pickups reach 15,000
#When we look at days that shutdown in NY occured, we are expecting to see a decrease
#of the numbers on the y-axis

#Day shutdown occured in NY - 03/20
COVIDData <- filter(MarchNewData1, PMonth==3, PDay ==20)
ggplot(COVIDData, aes(x=PHour)) + 
  geom_bar(stat ="count") + geom_bar(color="ivory4", fill="darkred")

#As we were expecting, the numbers on the y-axis dropped dramatically.
#They dropped from at most 15,000 pickups in an hour to at most 2,000 pick ups in an hour.

#A few days after shutdown occured in NY - 03/29
#We are expecting to see an even lower amount of pickups as shutdown goes into full affect
#and people are starting to realize this is actually happening.
COVIDData <- filter(MarchNewData1, PMonth==3, PDay ==29)
ggplot(COVIDData, aes(x=PHour)) + 
  geom_bar(stat ="count") + geom_bar(color="ivory4", fill="darkred")

#Just as we expected, it was even lower! The highest number on the y-axis dropped 
#to 400 meaning the most number of pickups during that day reached 400 in one hour.

#Overall, the most number of pickups in an hour dropped from 15,000 to 400 in an hour.
#That is a huge drop of 14,600 pickups.


###########################################################################################

#One more curiosity we had was to see how much the yellow taxis recovered from COVID.

#The latest data we found was the month of June, and that was still when things,
#were mostly shutdown

#Read file
JuneData <- read.csv('yellow_tripdata_2020-06.csv')

#Picking the variables to use
JuneNewData1 <- select(JuneData, tpep_pickup_datetime)

#Checking missing values
library(questionr)
freq.na(JuneNewData1)

#Structure of data
str(JuneNewData1)

install.packages("lubridate")
library(lubridate)

#Split up the pickup column into ymd_hms
JuneNewData1$tpep_pickup_datetime <- ymd_hms(JuneNewData1$tpep_pickup_datetime)
JuneNewData1$PMonth <- factor(month(JuneNewData1$tpep_pickup_datetime))
JuneNewData1$PDay <- factor(day(JuneNewData1$tpep_pickup_datetime))
JuneNewData1$PHour <- factor(hour(JuneNewData1$tpep_pickup_datetime))


#We are expecting a slight increase of the numbers on the y-axis 
COVIDRecoveryData <- filter(JuneNewData1, PMonth==06, PDay ==30)
ggplot(COVIDRecoveryData, aes(x=PHour)) + geom_bar(stat ="count") + 
  geom_bar(color="plum2", fill="seagreen2")

#We can see a bit of an increase on the y-axis as the most number of pickups reached
#2,000 in an hour. That is an increase of 1,600 pickups from the lowest number during 
#COVID. The number now should be higher as restrictions eased up but data for July, August,
#September, and October is not available.


#################################
#QUESTION 2 - Accordingto ww.nyc.gov,the meter(fare_amount) is calculated using a 
#$2.50 base fare plus $0.50 per 1/5 mile when traveling above 12mph or per 60 
#seconds in slow traffic or when the vehicle is stopped. Use this information 
#to build a model that predicts fare_amount as a function of trip duration, distance, 
#and any other features you feel are appropriate. Were you able to successfully 
#reverse engineer the fare rules?

#Selecting the variables needed for Question 2
JanNewdata2 <- select(JanData, trip_distance, passenger_count, fare_amount, 
                  extra, mta_tax, tolls_amount, congestion_surcharge)

#Structure of JanNewdata2
str(JanNewdata2)

#Changing passaengercount from Int to NUM
JanNewdata2$passenger_count <- as.numeric(JanNewdata2$passenger_count)

#Check to see missing values
library(questionr)
freq.na(JanNewdata2)

#Deletes all the rows with missing values
JanNewdata2 <- JanNewdata2 [ complete.cases(JanNewdata2), ]

#Check to see missing values again
library(questionr)
freq.na(JanNewdata2)

#Finding the correlation bewtween basefair and variables chosen
linearmod <- glm(formula = fare_amount ~., family = gaussian, data=JanNewdata2)
summary(linearmod)


########################################
#QUESTION 3 - Construct a probabilistic model to predict the drop-off borough given 
#relevant input features from the data. Using this model, what is your estimate of 
#the probability that a passenger is dropped off in Queens given she's picked up in 
#Manhattan at 10:12 AM on a Tuesday? 

#Response variable for a classification algorithm has to be a categorical variable (factor)
#Y (reponse) variable has to be of 0-1 type
#Logistic, KNN, SVM

#Selecting the variables needed for Question 3
JanNewdata3 <- select(JanData,tpep_pickup_datetime, PULocationID, DOLocationID)


#With these two lines of code, we extract the locationID to match the Boroughs
TimeZoneData <- read.csv('taxi+_zone_lookup.csv')

#Selecting the variables needed
TimeZoneData1 <- select(TimeZoneData, LocationID, Borough)

#Cluster in centers of 5 because of the 5 Boroughs for DOLocationID
cluster <- kmeans(JanNewdata3$DOLocationID, centers=5)

#Create new column to show in which Borough the dropoffs occured
JanNewdata3$BoroughDropOff <- as.factor(cluster$cluster)

#Cluster in centers of 5 because of the 5 Boroughs for PULocationID
cluster <- kmeans(JanNewdata3$PULocationID, centers=5)

#Create new column to show in which Borough the pickups occured
JanNewdata3$BoroughPickUp <- as.factor(cluster$cluster)


#Here we match all the DOLocationIDs that are assigned to Staten Island as Borough #5
JanNewdata3$BoroughDropOff <- ifelse(JanNewdata3$DOLocationID %in% c('23', '156', '187', '206', '251', 
                    '245', '115', '22', '6', '214', '118', '99', '172', '176', '110', 
                    '109', '84', '5', '204', '44'), 5, JanNewdata3$BoroughDropOff)


#Here we match all the DOLocationIDs that are assigned to Queens as Borough #4
JanNewdata3$BoroughDropOff <- ifelse(JanNewdata3$DOLocationID %in% c('2', '7' , 
'8', '9', '10', '15', '16', '19', '27', 
'28', '30','38', '53', '56', '57', '64', '70', '73', 
'82', '83', '86', '92', '93', '95', '96', '98', '101', 
'102', '117', '121', '122', '124', '129', '130', '131', 
'132', '134', '135', '138', '139', '145', '146', '157', 
'160', '171', '173', '175', '179', '180', '191', '192', 
'193', '196', '197', '198', '201', '203', '205', '207', 
'215', '216', '218', '219', '223', '226', '252', '253', '258', '260'), 4, JanNewdata3$BoroughDropOff) 



#Here we match all the DOLocationIDs that are assigned to Manhattan as Borough #3
JanNewdata3$BoroughDropOff <- ifelse(JanNewdata3$DOLocationID %in% c('4', 
'12', '13', '24', '41', '42', '43', '45', '48', '50', '68', 
'74', '75', '79', '87', '88', '90', '100', '103', '104', '105', 
'107', '113', '114', '116', '120', '125', '127', '128', '137', 
'140', '141', '142', '143', '144', '148', '151', '152', '153', 
'158', '161', '162', '163', '164', '166', '170', '186', '194', 
'202', '209', '211', '224', '229', '230', '231', '232', '233', 
'234', '236', '237', '238', '239', '243', '244', '246', '249', 
'261', '262', '263'), 3, JanNewdata3$BoroughDropOff)



#Here we match all the DOLocationIDs that are assigned to Brooklyn as Borough #2
JanNewdata3$BoroughDropOff <- ifelse(JanNewdata3$DOLocationID %in% c('11', '14', '17', '21', '22', '25',
'26', '29', '33', '34', '35', '36', '37', '39', '40', 
'49', '52', '54', '55', '61', '62', '63', '65', '66', '67', '71', '72', '76', '77', '80',
'85', '89', '91', '97', '106', '108', '111', '112', '123', '133', '149', '150', '154',
'155', '165', '177', '178', '181', '188', '189', '190', '195', '210', '217', '222', 
'225', '227', '228', '255', '256', '257'), 2, JanNewdata3$BoroughDropOff)


#Here we match all the DOLocationIDs that are assigned to Bronx as Borough #1
JanNewdata3$BoroughDropOff <- ifelse(JanNewdata3$DOLocationID %in% c('3', '18', '20', '31', '32', '46', '47', '51', '58', '59', '60', '69', '78', '81', '94', 
'119', '126', '136', '147', '159', '167', '168', '169', '174', '182', '183', '184', '185', 
'199', '200', '208', '212', '213', '220', '235', '240', '241', '242', '247', '248', '250', 
'254', '259'), 1, JanNewdata3$BoroughDropOff)



#Here we match all the PULocationID that are assigned to  as Borough #5
JanNewdata3$BoroughPickUp <- ifelse(JanNewdata3$PULocationID %in% c('23', '156', '187', '206', '251', 
      '245', '115', '22', '6', '214', '118', '99', '172', '176', '110', 
     '109', '84', '5', '204', '44'), 5, JanNewdata3$BoroughPickUp)


#Here we match all the PULocationIDs that are assigned to Queens as Borough #4
JanNewdata3$BoroughPickUp<- ifelse(JanNewdata3$PULocationID %in% c('2', '7' , 
         '8', '9', '10', '15', '16', '19', '27', 
         '28', '30','38', '53', '56', '57', '64', '70', '73', 
        '82', '83', '86', '92', '93', '95', '96', '98', '101', 
        '102', '117', '121', '122', '124', '129', '130', '131', 
        '132', '134', '135', '138', '139', '145', '146', '157', 
        '160', '171', '173', '175', '179', '180', '191', '192', 
  '193', '196', '197', '198', '201', '203', '205', '207', 
'215', '216', '218', '219', '223', '226', '252', '253', '258', '260'), 4, JanNewdata3$BoroughPickUp) 


#Here we match all the PULocationIDs that are assigned to Manhattan as Borough #3
JanNewdata3$BoroughPickUp <- ifelse(JanNewdata3$PULocationID %in% c('4', 
  '12', '13', '24', '41', '42', '43', '45', '48', '50', '68', 
  '74', '75', '79', '87', '88', '90', '100', '103', '104', '105', 
  '107', '113', '114', '116', '120', '125', '127', '128', '137', 
  '140', '141', '142', '143', '144', '148', '151', '152', '153', 
   '158', '161', '162', '163', '164', '166', '170', '186', '194', 
  '202', '209', '211', '224', '229', '230', '231', '232', '233', 
   '234', '236', '237', '238', '239', '243', '244', '246', '249', 
    '261', '262', '263'), 3, JanNewdata3$BoroughPickUp)



#Here we match all the PULocationIDs that are assigned to Brooklyn as Borough #2
JanNewdata3$BoroughPickUp <- ifelse(JanNewdata3$PULocationID %in% c('11', '14', '17', '21', '22', '25',
 '26', '29', '33', '34', '35', '36', '37', '39', '40', 
'49', '52', '54', '55', '61', '62', '63', '65', '66', '67', '71', '72', '76', '77', '80',
 '85', '89', '91', '97', '106', '108', '111', '112', '123', '133', '149', '150', '154',
 '155', '165', '177', '178', '181', '188', '189', '190', '195', '210', '217', '222', 
 '225', '227', '228', '255', '256', '257'), 2, JanNewdata3$BoroughPickUp)


#Here we match all the PULocationIDs that are assigned to Bronx as Borough #1
JanNewdata3$BoroughPickUp <- ifelse(JanNewdata3$PULocationID %in% c('3', '18', '20', '31', '32', '46', '47', '51', '58', '59', '60', '69', '78', '81', '94', 
 '119', '126', '136', '147', '159', '167', '168', '169', '174', '182', '183', '184', '185', 
 '199', '200', '208', '212', '213', '220', '235', '240', '241', '242', '247', '248', '250', 
 '254', '259'), 1, JanNewdata3$BoroughPickUp)



#Checking missing values
library(questionr)
freq.na(JanNewdata3)


#Split up the pickup column into ymd_hms for the Tuesday 10:12am part
JanNewdata3$tpep_pickup_datetime <- ymd_hms(JanNewdata3$tpep_pickup_datetime)
JanNewdata3$PMonth <- factor(month(JanNewdata3$tpep_pickup_datetime))
JanNewdata3$PDay <- factor(day(JanNewdata3$tpep_pickup_datetime))
JanNewdata3$PHour <- factor(hour(JanNewdata3$tpep_pickup_datetime))
JanNewdata3$PMinute <- factor(minute(JanNewdata3$tpep_pickup_datetime))

#Filtering the data to find the amount of instances pickups occured on any Tuesday
#in the month at 10:12am
TuesdayData <- filter(JanNewdata3, PMonth==1, PDay==c(7,14,21,28), 
                      PHour==10, PMinute == 12)

#168 instances


#Filtering the data to find the amount of instances pickups in Manhattan occured on any Tuesday
#in the month at 10:12am and the drop off was in Queens
ProbData <- filter(TuesdayData, PMonth==1, PDay==c(7,14,21,28), 
                      PHour==10, PMinute == 12, BoroughDropOff==4, BoroughPickUp==3)

#Only 1 instance that this occurred



#Probability of this happening is 1/168 = 0.00595 or 0.6%

#This is because from TuesdayData, we have 168 instances that a pick up occurs on a 
#Tuesday at 10:12am. We then filter TuesdayData to ProbData. ProbData shows us how many
#instances of getting picked up in Manhattan at 10:12am and dropped of in Queens. We only
#one instance of this happening; therefore, we divide 1 by the total amount pickups that
#can happen on a Tuesday at 10:12am (168) and get 0.6%

