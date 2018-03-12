########################################################################################################
# Data Analysis on World Development Indicators (Surajudeen Abdulrasaq MLDM M1) 
# It presents the most current and accurate global development data available,
# and includes national, regional and global estimates.
# Quesions :  What are the factors affecting global developments?
#             What country or region are developing faster?
#             What years are the country or region develop most
#             Is it possible to predict global development in 2018 using this data?
# Data Available at https://data.worldbank.org/data-catalog/world-development-indicators (World Bank)
#########################################################################################################


#SET PATH & Clear Space
path = ("~/R-Projects/world_devel")
setwd(path)
rm(list=ls(all = TRUE))


# READ ALL DATASET PROVIDED AND EXAMINE THEM
library(dplyr)
library(readr)

world_dev <- read.csv('WDIData.csv', check.names = FALSE)
summary(world_dev)


# Check Structure & Rename Column for easy analysis 

str(world_dev)   #415800 obs. of  63 variables:

world_dev <- rename(world_dev, Indicator_Code = "Indicator Code", Indicator_Name = "Indicator Name", 
      Country_Code = "Country Code", Country_Name = "Country Name")

#################################################################################
# The Year Column (1960 1961 1962.....) is spread Across Rows.
# This will be difficults to deal with So i will Combine Multiple Columns 
# of Data into a Single Column to create the "Year" Necesary for efficent analysis
##################################################################################
# install.packages("tidyr")
library(tidyr)
world_dev <- gather(world_dev, Year, Values, -Country_Name, -Country_Code, -Indicator_Name, -Indicator_Code)

head(world_dev)
# Remove NA data and check statiscs with the structure
world_dev <- na.omit(world_dev)
#summary(world_dev)
str(world_dev)
head(world_dev)
tail(world_dev)


########################################################################################
# I regroup dataset using the Indicator_Code & Idicator_Name as an 
# important factor so we can easily visualise countries
# affected by this indicatos and the affected years the no of years
world_indi <- world_dev %>%
  group_by(Indicator_Code, Indicator_Name) %>%
  summarise(Num_of_Countries = n_distinct(Country_Code),
            Num_of_Years     = n_distinct(Year),
            FirstYear    = min(Year),
            LastYear     = max(Year))
world_indi$Indicator_Name <- sub("\\$", "dollar", world_indi$Indicator_Name)# replace $ symbol with dollar
str(world_indi)
summary(world_indi)
#write.csv(world_indi, file = 'world_indi.csv')

############################################################################################
# Now we have a set of new set of analyse data(world_indi) which tell us the NO. of countries affected by particular indicators
# The Number of years it affect them and from what year to what year this are spread out.
# Lets examine Maximum Number of countries and the Maximum Num of years Afected
# to know some very importants indicators
############################################################################################

max(world_indi$Num_of_Countries) # Maximum No of country afected by A OR some particular indicator = 263.
max(world_indi$Num_of_Years)# Maximum No or years this indicator has affected the country = 58 YEARS

# lETS DO SOME VISUALISATION WITH TOP 5 
library(plotrix)
library(ggplot2)

# 
#fixing some names so the plots is readble
world_indi$Indicator_Name[world_indi$Indicator_Name == 'Agricultural land (% of land area)'] = "Agric_Land_area" 
world_indi$Indicator_Name[world_indi$Indicator_Name == 'Agricultural machinery, tractors'] = "Mechanised Agric" 
world_indi$Indicator_Name[world_indi$Indicator_Name == 'Fertilizer consumption (kilograms per hectare of arable land)'] = "Fertilizer_consumption" 
world_indi$Indicator_Name[world_indi$Indicator_Name == 'Agricultural land (sq. km)'] = "Agric_Land_(sq.km)" 
world_indi$Indicator_Name[world_indi$Indicator_Name == 'Arable land (hectares per person)'] = "Arable_Land" 
world_indi$Indicator_Name[world_indi$Indicator_Name == 'Fertilizer consumption (% of fertilizer production)'] = "fertilizer production" 

#X11(display = 'Impotant Indicator', 8,4)
#dev.copy(device = png, filename= 'plot1.png', width =1000, height = 500);
qplot(Indicator_Name,  Num_of_Countries, data = world_indi[1:5,], color= Num_of_Years, geom = c("boxplot", "jitter"))
#dev.off();

# lets do pie plot
#dev.copy(device = png, filename= 'plot2.png', width =1000, height = 500);
pie(world_indi$Num_of_Countries[1:5], labels = world_indi$Indicator_Code, explode = 0.1,main="Pie Chart of Important indicators ")
#dev.off();


# Agriculral land(both in land area and sq. meter.),seem to be very important Indicators 
# But we can explore further to be certain by extracting more information from our data-set
# Since we know the biggest number's of affected countries is 263
# i take a sample of this affect between 200 and 263 countiries to get to know how this indicator affected
# i create a new data-frame called Important_Indicators to take the list of most inportant indicators

Important_indicators <- world_indi[which(world_indi$Num_of_Countries > 200),]
#warning()
summary(Important_indicators)

# From the results Agriculral land is still the prevaling 
#lets visualise with a subset of this Important Indicators using ggplot
library(ggplot2)

#dev.copy(device = png, filename= 'plot3.png', width =1000, height = 500);
qplot(Indicator_Name, Num_of_Countries, data = Important_indicators[1:5,], color = Num_of_Years)
#dev.off();

# visualise using ggplot
#dev.copy(device = png, filename= 'plot4.png', width =1000, height = 500);
ggplot(data=Important_indicators[1:5,], aes(x=Indicator_Name, y=Num_of_Countries, fill=Num_of_Years)) +
  geom_bar(stat="identity", position=position_dodge())
#dev.off()
############################################################################################

# NOW I HAVE TWO DIFFRENT DATASET (world_dev and world_indi) WITH DIFFRENT USEFUL INFORMATION
# Now Combine the two dataset(world_indi and world_dev) in to single dataframe, this is 
# neccesary to see if there can be any surprise discovery,  i will analyse further before
# finally try to predict the 2018 outcome

#world_indi <- world_indi[,-(1:2), drop = FALSE] # Drop the first two col, which are already in world_dev
#str(world_indi)

# Now Combine the data-frame
data.combine <- merge(world_dev, world_indi)
summary(data.combine)
str(data.combine)

#write.csv(data.combine, file = 'final.csv')

# Now i have a new data-frame let me check the importance of values.
table(data.combine$Values) # Values is a very impotant factor

#Check the correlation of values and year by visualisation
# we plot to based on values of each indicator to be sure we are good
# the visualisation shows that most countries improove in Access to clean fuels and technologies for cooking
# through the years
#dev.copy(device = png, filename= 'plot8.png', width =1000, height = 700);

ggplot(data=data.combine[1200:1211,], aes(x=Indicator_Name, y=Values, fill=Year)) +
  geom_bar(stat="identity", position=position_dodge())+
  facet_wrap(~Country_Name)+
  ggtitle(' most countries improove in Access to clean fuels and technologies for cooking through the years')
#dev.off()


#lets visualise using MAP for some Indicators and years involve
# we ramdomly visualise some top most important indicator

indicator_name <- 'EG.CFT.ACCS.ZS'
year <- 2013

my_indicator <- data.combine[data.combine$Indicator_Code==indicator_name & data.combine$Year==year,]
#write_csv(visual, "visual.csv")

sPDF <- joinCountryData2Map(my_indicator,
                            joinCode = "ISO3",
                            nameJoinColumn = "Country_Code",
                            verbose = TRUE)

png("map0.png", width=900, height=550)
mapCountryData(sPDF, nameColumnToPlot='Values', mapTitle='Access to clean fuels and technologies(Most Developing Countries Improove in this aspects) 2015')
dev.off()


###########################################################################
# End of Data Analysis Now lets start Exploratory Modelling
###########################################################################


# Laod packages
library(readr)
library(rworldmap)
library('ggplot2')
library('forecast')
library(knitr)
library(plotly)
library(DT)
library(highcharter)
library(formattable)
library(countrycode)
library(maps)

# Pick indicators of interest and check how nations fares according to 
# the indicators

child_death <- filter(data.combine, Indicator_Code=='SH.DTH.MORT')
write_csv(child_death , 'child_death .csv')
my_model <- read_csv("child_death .csv")
my_model$Values <- round(my_model$Values, digits = 2)

titlelab <- "Reseach and Developments expenditure (Europe)"
ylabl <- "Reseach and Developments expenditure(Europe)"


NORTH_AMERICA <- filter(my_model, Country_Code == 'HND' | Country_Code == 'CRI' | Country_Code == 'SLV' |
               Country_Code == 'GTM' | Country_Code == 'NIC'| Country_Code == 'HND' | Country_Code == 'CRI' | Country_Code == 'SLV' |
               Country_Code == 'GTM' | Country_Code == 'NIC' | Country_Code == 'PAN')


SOUTH_AMERICA <- filter(my_model, Country_Code=='BRA' | Country_Code=='VEN' | 
               Country_Code=='COL' | Country_Code=='CHL' | 
               Country_Code=='ARG')

AFRICA <- filter(my_model, Country_Code=='DZA' | 
               Country_Code=='NGA' | Country_Code=='ZAF'|
               Country_Code=='EGY' | Country_Code=='ZAR')


EUROPE <- filter(my_model, Country_Code=='FIN' | Country_Code=='SWE'
              |Country_Code=='DNK' | Country_Code=='NOR'
              |Country_Code=='ISL' | Country_Code=='DEU' | Country_Code=='ITA' |
                Country_Code=='GBR' | Country_Code=='FRA'| 
                Country_Code == 'ESP')

ASIA <- filter(my_model, Country_Code=='CHN' | Country_Code=='JPN' |
                  Country_Code=='IDN' | Country_Code=='IND' |
                  Country_Code=='KOR'| Country_Code=='SAU' | Country_Code=='KAZ' |
                  Country_Code=='IRN' | Country_Code=='RUS' |
                  Country_Code=='PRK' )



others <- filter(my_model, Country_Code=='USA' | Country_Code=='AUS'
                 |Country_Code=='CAN' | Country_Code=='MEX'
                 |Country_Code=='NZL'| Country_Code == 'HND' | Country_Code == 'CRI')


# plot to visualise

##Europe

# Continent by continent plot
titleformat <- theme(plot.title = element_text(colour = "brown", size = 14, face = "bold") )
my_plot <- ggplot(AEU, aes(Year, Values) ) +
  geom_line(size = 1, aes(color = Country_Name) ) +
  xlab('') +
  ylab(ylabl) + 
  facet_grid(.~Country_Name) +
  titleformat +
  ggtitle('Child death (age 1-5) YEAR 2000-2015 (Europe)') +
  theme(axis.text.x = element_text(face="bold", color="#993333", 
                                   size=7, angle=30))

#ggplotly(plot_4)

tmpFile <- tempfile(fileext = ".png")
export(ggplotly(plot_4), file = tmpFile)
browseURL(tmpFile)

###################################

# Comparative between Country

country_Compare = function(my_ind, countries, data.combine) {

  #check if country code and indicator is valid
  if(!any(my_ind == data.combine$Indicator_Code)) {
    stop("Please check the indicator and try again")
  }
  
  for(c in countries) {
    if(!any(c == data.combine$Country_Code)) {
      stop(sprintf("Country code does not exist", c))
    }   
  }
  
  filtered = data.combine[data.combine$Indicator_Code == my_ind & data.combine$Country_Code == countries, ]
 
  for(c in countries) {
    if(!any(c == filtered$Country_Code))
      warning(sprintf("There's No such indicator this country %s", c))
  }
  
  years = min(filtered$Year):max(filtered$Year)
  #Initialize an empty matrix with results
  results = matrix(NA, length(years), length(countries))
  rownames(results) = years
  colnames(results) = countries
  #loop through and fill in values if they exist
  for (i in 1:length(years)) {
    for (j in 1:length(countries)) {
      value = filtered$Values[filtered$Country_Code == countries[j] & filtered$Year == years[i]]
      if (length(value) > 0) {
        results[i,j] = value
      }
    }
  }
 
  matplot(years,results, type = "p", pch = 21:20 + length(countries), main = filtered$Indicator_Name[1])
  #titleformat <- theme(plot.title = element_text(colour = "brown", size = 14, face = "bold") )
  legend(max(years)-10, max(results, na.rm = TRUE), countries, pch = 21:20 + length(countries), col = 1:length(countries),bty = "n" )
}
      
# Call the compare function and insert two countries code
country_Compare("SP.MTR.1519.ZS", c( "FRA", "GBR"), my_model)

#train randomforesst with default parameter Year and values

#write_csv(AEU , 'aeu.csv')

aeu_model <- read.csv('aeu.csv')
aeu_model <-  na.omit(aeu_model)
library(randomForest)
rf.train <- aeu_model[c('Values', 'Year')]
rf.label <- as.factor(aeu_model$Country_Code)

set.seed(1234)
rf <- randomForest(x = rf.train, y = rf.label, importance = TRUE, ntree = 2000)
rf
#varImpPlot(rf)
plot(rf)
########################################################################
# Now lets do cross validation this nessecary to avoid OVERFITTING
# and help to evaluate our error rate against unseen data.
#########################################################################

test.model <- aeu_model[400:557, c('Values', 'Year')]
test.model <- na.omit(test.model)
# lets predict 
rf.preds <- predict(rf, test.model)
table(rf.preds)
plot(rf.preds)



