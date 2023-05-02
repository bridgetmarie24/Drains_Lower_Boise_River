## ------------------ ##
## RETRIEVE USGS DATA ##
## ------------------ ##


# install.packages('dataRetrieval')
library(dataRetrieval)

#Input gauge site numbera and the parameter ID for discharge
siteNumber <- "13213072"
info <- readNWISsite(siteNumber)
parameterCd <- "00060"

# Import raw daily data
rawdailydata <- readNWISdv(siteNumber, parameterCd, '2016-11-30', '2022-06-28')
rawdailydata$DiversionName <- 'Sand Run Gulch'

# Export csv file
write.csv(file="~/Desktop/CASCWork/sandrungulch.csv", rawdailydata)

#Input gauge site numbera and the parameter ID for discharge
siteNumber <- "132109867"
info <- readNWISsite(siteNumber)
parameterCd <- "00060"

# Import raw daily data
rawdailydata <- readNWISdv(siteNumber, parameterCd, '2016-11-30', '2022-06-28')
rawdailydata$DiversionName <- 'East Hartley Gulch'

# Export csv file
write.csv(file="~/Desktop/CASCWork/easthartley.csv", rawdailydata)

#Input gauge site numbera and the parameter ID for discharge
siteNumber <- "13212890"
info <- readNWISsite(siteNumber)
parameterCd <- "00060"

# Import raw daily data
rawdailydata <- readNWISdv(siteNumber, parameterCd, '1971-01-01', '2022-06-28')
rawdailydata$DiversionName <- 'Dixie Drain'

# Export csv file
write.csv(file="~/Desktop/CASCWork/dixiedrain.csv", rawdailydata)

#Input gauge site numbera and the parameter ID for discharge
siteNumber <- "13212549"
info <- readNWISsite(siteNumber)
parameterCd <- "00060"

# Import raw daily data
rawdailydata <- readNWISdv(siteNumber, parameterCd, '2016-11-30', '2022-06-28')
rawdailydata$DiversionName <- 'Conway Gulch'

# Export csv file
write.csv(file="~/Desktop/CASCWork/conway.csv", rawdailydata)



