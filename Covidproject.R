# Yusuke Shiota 


# Libraly
library(dplyr)
library(ggplot2)
library(grid)



# COVID-19 

#read the CSV file
covidData <- read.csv("cowid-covid-data.csv")
head(covidData)

# Q2

# Aggregate the data to extract sum of new cases by location
totalcases_in_eachcountry <- aggregate(covidData$new_cases, by=list(Location=covidData$location), FUN=sum,na.rm=TRUE)

#Subset only countries required
totalcases_in_eachcountry <-subset(totalcases_in_eachcountry, Location == "Australia"|Location == "New Zealand"
                                   |Location == "Sweden"|Location == "Russia"|Location == "United Kingdom"
                                   |Location == "Sweden"|Location == "Russia"|Location == "United States")
totalcases_in_eachcountry

#Visualize the data
barplot(height = totalcases_in_eachcountry$x, names.arg = totalcases_in_eachcountry$Location, col=c("darkblue"))



# Q3

#Subset only location, date, ,total_cases
q3data <-covidData %>% select(location, date, ,total_cases)

#Subset only countries required
q3data <- subset(q3data, location == "Australia"|location == "China"|location == "New Zealand"
                 |location == "Spain" |location == "Sweden"|location == "Russia"|location == "United Kingdom"
                 |location == "United States")

#Drop the rows if the total cases < 100
q3data <- subset(q3data,  total_cases>=100)

#Convert date to days since 2019-12-31
q3data$NumDays  <- difftime(q3data$date,"2019-12-31" ,units="days")
head(q3data)

#Remove the scientific e notation 
options(scipen=10000)
#Create the ggplot
ggplot(data=q3data, aes(x=NumDays, y=total_cases, group=location)) +
  geom_line(aes(color=location ,linetype="solid"))

# Q4

# Aggregate the life_expectancy by location
Life_Expectancy <- aggregate(covidData$life_expectancy, by=list(Location=covidData$location),FUN=mean,na.rm=TRUE)

# Store column x to another column
Life_Expectancy$lifeE <- Life_Expectancy$x

# Delete the column x
Life_Expectancy <- subset( Life_Expectancy, select = -x )
head(Life_Expectancy)

# Aggregate the total_cases_per_million
totalcases_perMillion <- aggregate(covidData$total_cases_per_million, by=list(Location=covidData$location), FUN=sum,na.rm=TRUE)
head(totalcases_perMillion)

# Store column x to another column
totalcases_perMillion$permillion <- totalcases_perMillion$x

# Delete the column x
totalcases_perMillion <- subset( totalcases_perMillion, select = -x )


# Transform "totalcases_perMillion" to "totalcases_per_HundredThousand" 
totalcases_perMillion <- transform(totalcases_perMillion, total_cases_per_HundredThousand = permillion / 10)

#Add life expectancy
totalcases_perMillion$LifeExpectancy <- Life_Expectancy$lifeE
head(totalcases_perMillion)

options(scipen=10000)
ggplot(totalcases_perMillion, aes(x=LifeExpectancy, y=total_cases_per_HundredThousand)) + 
  geom_point() 



