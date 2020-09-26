# Download archive file, if it does not exist

if(!(file.exists("summarySCC_PM25.rds") && 
     file.exists("Source_Classification_Code.rds"))) { 
  archiveFile <- "NEI_data.zip"
  if(!file.exists(archiveFile)) {
    archiveURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    download.file(url=archiveURL,destfile=archiveFile,method="curl")
  }  
  unzip(archiveFile) 
}

## load data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# View the data imported
head(NEI)
head(SCC)

#Load the packages used in the exploratory analysis
library(ggplot2)
library(plyr)

## Converting "year", "type", "Pollutant", "SCC", "fips" to factor
colToFactor <- c("year", "type", "Pollutant","SCC","fips")
NEI[,colToFactor] <- lapply(NEI[,colToFactor], factor)

head(levels(NEI$fips))

## The levels have NA as "   NA", so converting that level back to NA
levels(NEI$fips)[1] = NA
NEIdata<-NEI[complete.cases(NEI),]
colSums(is.na(NEIdata))

#Question 1

totalEmission <- aggregate(Emissions ~ year, NEIdata, sum)
totalEmission

## Plotting the total Emissions over time using a base plotting

barplot(
  (totalEmission$Emissions)/10^6,
  names.arg=totalEmission$year,
  xlab="Year",
  ylab="PM2.5 Emissions (10^6 Tons)",
  main="Total PM2.5 Emissions From All US Sources"
)