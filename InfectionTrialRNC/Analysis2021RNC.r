###############################################################################################################################################################
                     ########################## Red needle cast (RNC), caused by Phytophthora pluvialis and Phytophthora kernoviae ##########################
                               ##########################    Data supplied by Stuart Fraser Forest Protection   ##########################
                               ########################## Two independent sites in each of Kaingaroa and Kinleith forests. ##############
                                ############  Plants introduced into forest and removed. Sampled two weekly over two years
                                   ############   Task code F70320. RNC score classes 1=<10% of foliage, 2=10-50%, 3 = >50%
                                     #####Data location: Q:\Resilient Forests\RA3.2 Epidemiology of foliar pathogens\historical_data\New_timing of infection 2017_2019
                                     ##### Analysis updated in Jan 2021. Use NIWA data to predict weather station data prior to Dec 2018
                                     #### Exchange fortnights are very close to evenly arranged for all four sites. 

##rmarkdown::render("PaperResults1.rmd", output_file="MethodsResultsRNC.docx", quiet=TRUE)

###Clear workspace and set locations for data and packages
rm(list=ls()); setwd("/home/sean/ScionBiometrics/InfectionTrialRNC"); RPackages = "/home/sean/RPackages" ; .libPaths("/home/sean/RPackages")
packages.1  <- c("car", "imputeTS","Amelia", "DescTools", "Evapotranspiration", "lubridate", "lme4", "plotrix", "RColorBrewer", "openxlsx", "devtools", "merTools", "nlme", "MuMIn", "EnvStats", "polynom", "rms", "lattice", "rmarkdown", "randomForest"); ##An object for packages 
invisible(lapply(packages.1, function(x) require(x, character.only = T, quietly = T)))
## display.brewer.all(colorblindFriendly = TRUE); display.brewer.pal(n = 8, name = 'Paired'); brewer.pal(n = 4, name = "Dark2")

##exch.dat  <- as.data.frame(openxlsx::read.xlsx("Data/All exchange plant data.xlsx", detectDates = TRUE)) ##UpdatedPCR.xlsx
##exch.dat  <- as.data.frame(openxlsx::read.xlsx("NovData/All field control plant data Nov.xlsx", detectDates = TRUE))[ , 1:12] ##Data missing from Nov data
##exch.dat  <- as.data.frame(openxlsx::read.xlsx("Data/All exchange plant data.xlsx", detectDates = TRUE)) ##Modified 10/12/2020, 19:24:40, FraserS
exch.dat  <- read.csv("Data/ExchangeData.csv") ##!!Different dims. Converted from All exchange plant data.xlsx file Modified 10/12/2020, 19:24:40, FraserS

####Date format conversions
exch.dat$Assessment.date  <- as.Date(exch.dat$Assessment.date, "%m/%d/%Y")  ##as.POSIXct
exch.dat$Fortnight.start.date  <- as.Date(exch.dat$Fortnight.start.date, "%m/%d/%Y") 
exch.dat$Fortnight.end.date  <- as.Date(exch.dat$Fortnight.end.date, "%m/%d/%Y")
exch.dat$foliage  <- exch.dat$Younger.foliage.score_Updated + exch.dat$Older.foliage.score_Updated 
exch.dat  <- exch.dat[is.na(exch.dat$foliage) == FALSE, ]

## svg(file = "TimeToSymptomaticRNC.svg")
## hist(assessment.rnc$Batch.assessment)
## dev.off()
##

  ############################################################################################################################################
       ####################################################         Pull out pcr data and format  ###################################
pcr.assessment.pp  <- exch.dat[exch.dat$Pp==1 & is.na(exch.dat$Pp)==FALSE, c("Replicate", "Batch.assessment", "Pp")]
pcr.assessment.pp  <-  pcr.assessment.pp[base::order(pcr.assessment.pp$Replicate, pcr.assessment.pp$Batch.assessment, decreasing = FALSE), ]
pcr.assessment.pp  <- pcr.assessment.pp[duplicated(pcr.assessment.pp$Replicate)==FALSE, ]

pcr.assessment.pk  <- exch.dat[exch.dat$Pk==1, c("Replicate", "Batch.assessment", "Pk")]
pcr.assessment.pk  <-  pcr.assessment.pk[base::order(pcr.assessment.pk$Replicate, pcr.assessment.pk$Batch.assessment, decreasing = FALSE), ]
pcr.assessment.pk  <- pcr.assessment.pk[duplicated(pcr.assessment.pk$Replicate)==FALSE, ]

pcr.dat  <- exch.dat[is.na(exch.dat$Sampled)==FALSE, c("Year", "Site", "Replicate", "Fortnight.exposed", "Pp", "Pk")]
pcr.dat[pcr.dat$Year==2, "Fortnight.exposed"]  <- pcr.dat[pcr.dat$Year==2, "Fortnight.exposed"] + 25
pcr.duplicated.Pk  <- pcr.dat[duplicated(pcr.dat[, c("Site", "Replicate", "Fortnight.exposed")]) & pcr.dat$Pk != 0, c("Site", "Replicate", "Fortnight.exposed")] ##
for ( i in 1:nrow(pcr.duplicated.Pk)) {
    pcr.dat[pcr.dat$Site == pcr.duplicated.Pk[i, "Site"] & pcr.dat$Fortnight.exposed == pcr.duplicated.Pk[i, "Fortnight.exposed"] & pcr.dat$Replicate == pcr.duplicated.Pk[i, "Replicate"],  "Pk"]  <-  1
}
pcr.duplicated.Pp  <- pcr.dat[duplicated(pcr.dat[, c("Site", "Replicate", "Fortnight.exposed")]) & pcr.dat$Pp != 0, c("Site", "Replicate", "Fortnight.exposed")] ##
for ( i in 1:nrow(pcr.duplicated.Pp)) {
    pcr.dat[pcr.dat$Site == pcr.duplicated.Pp[i, "Site"] & pcr.dat$Fortnight.exposed == pcr.duplicated.Pp[i, "Fortnight.exposed"] & pcr.dat$Replicate == pcr.duplicated.Pp[i, "Replicate"],  "Pp"]  <-  1
}
pcr.dat  <-  pcr.dat[duplicated(pcr.dat[, c("Site", "Replicate", "Fortnight.exposed")])==FALSE, ]  ###All duplicates removed, but PCT records of rnc kept
pcr.dat.pp  <- as.data.frame.table(tapply(pcr.dat$Pp, list(pcr.dat$Site, pcr.dat$Fortnight.exposed), sum))
names(pcr.dat.pp)  <- c("Site", "Fortnight.exposed", "Pp")
pcr.dat.pk  <- as.data.frame.table(tapply(pcr.dat$Pk, list(pcr.dat$Site, pcr.dat$Fortnight.exposed), sum))
names(pcr.dat.pk)  <- c("Site", "Fortnight.exposed", "Pk")
pcr.dat  <-  merge(pcr.dat.pp, pcr.dat.pk, by=c("Site", "Fortnight.exposed"))
pcr.dat$Fortnight.exposed  <-  as.numeric(as.character(pcr.dat$Fortnight.exposed))

############################################################################################################################################################
  ###############################         Load and compile Kaingaroa and Kinleith Weather data      ####################################################

###Data needs to be compiled from several worksheets in an xcel spreadsheet
options("openxlsx.datetimeFormat" = "dd-mm-yyyy hh:mm:ss") ## 11/28/2017 11:00:00
sheet.names  <-  openxlsx::getSheetNames("Data/Kinleith_Kaingaroa_Met_data_master.xlsx")
kaing.pre18  <-  openxlsx::read.xlsx("Data/Kinleith_Kaingaroa_Met_data_master.xlsx", sheet="Kaing Hour 28Nov17-24Oct18",
                                     detectDates = TRUE, startRow=5, colNames=FALSE)[,c(1,7,5,20,12,19,17)] ##PAR is photosynthetically active radiation. is avg. RH is smp
names(kaing.pre18)   <- c("time", "temp", "par", "rain", "rh", "wind", "wet") 
kaing.pre18$time  <- convertToDateTime(kaing.pre18$time, origin = "1900-01-01") ###Converts Xcel format date to R format
kaing  <-  openxlsx::read.xlsx("Data/Kinleith_Kaingaroa_Met_data_master.xlsx", sheet="Kaing Hour 26Oct18-current",
                                      startRow=5, colNames=FALSE)[,c(1,17,5,13,18,14,11)] ###Rain missing
names(kaing)  <- c("time", "temp", "par", "rain", "rh", "wind", "wet")
kaing$site  <- "Kaingaroa"
kaing$time  <- convertToDateTime(kaing$time, origin = "1900-01-01") ###Converts Xcel format date to R format

####Load and compile Kinleith Weather data
kin.pre18  <-  openxlsx::read.xlsx("Data/Kinleith_Kaingaroa_Met_data_master.xlsx", sheet="Kin Hourly 29Nov17-24May18",
                                     detectDates = TRUE, startRow=5, colNames=FALSE)[,c(1,7,5,19,12,20,17)] ##PAR is avg. RH is smp
names(kin.pre18)   <- c("time", "temp", "par", "rain", "rh", "wind", "wet")
kin.pre18$time  <- convertToDateTime(kin.pre18$time, origin = "1900-01-01") ###Converts Xcel format date to R format

kin  <-  openxlsx::read.xlsx("Data/Kinleith_Kaingaroa_Met_data_master.xlsx", sheet="Kin Hour 24May18-current",
                                      startRow=5, colNames=FALSE)[,c(1,16,5,12,17,13,11)] ###Rain missing
names(kin)  <- c("time", "temp", "par", "rain", "rh", "wind", "wet")
kin$site  <- "Kinleith"
kin$time  <- convertToDateTime(kin$time, origin = "1900-01-01") ###Converts Xcel format date to R format

###Produce one data frame with weather data
weather.dat  <- rbind(kaing, kin)[ , c("site", "time", "temp", "par", "rain", "rh", "wind", "wet")]      

weather.dat  <-  weather.dat[weather.dat$time > as.Date("2018-12-01"), ]

######Summarise leaf wetness measurements. Proportion of measurements above 150 
wet.kaing  <- as.data.frame(rbind(as.matrix(kaing[, c(1,7)]), as.matrix(kaing.pre18[, c(1,7)])))
wet.kaing$site  <- "Kaingaroa" 
wet.kin  <- as.data.frame(rbind(as.matrix(kin[, c(1,7)]), as.matrix(kin.pre18[, c(1,7)])))
wet.kin$site  <- "Kinleith" 
wet.dat  <-  rbind(wet.kaing, wet.kin)
wet.dat$time  <-  as.Date(wet.dat$time); wet.dat$wet  <-  as.numeric(as.character(wet.dat$wet))
wet.dat[wet.dat$time %in% c("NAN","INF"), "wet"]  <- 151 ## NA, NAN Inf are dry
wet.dat[is.na(wet.dat$wet)==FALSE & wet.dat$wet <= 150, "wet"]  <- 1
wet.dat[is.na(wet.dat$wet)==TRUE | wet.dat$wet > 150 , "wet"]  <- 0
wet.dat  <- wet.dat[order(wet.dat$site, wet.dat$time), ]

############################################################################################################################################################
  ##########################################  Combine and summarise Field, Exchange and Weather data   ####################################################

#### Relationalise exhange data. Raw data has some individual plant data repeated for each assessment 
####remove duplicates, but ensure that if the duplicates had rnc detected that is recorded
duplicated.with.rnx  <- exch.dat[duplicated(exch.dat[, c("Site", "Replicate", "Fortnight.start.date")])==TRUE & exch.dat$foliage!=0, c("Site", "Replicate", "Fortnight.start.date")]
for ( i in 1:nrow(duplicated.with.rnx)) {
exch.dat[exch.dat$Site == duplicated.with.rnx[i, "Site"] & exch.dat$Fortnight.start.date == duplicated.with.rnx[i, "Fortnight.start.date"] & exch.dat$Replicate == duplicated.with.rnx[i, "Replicate"], "foliage"]  <-  1
}

plant.dat  <- exch.dat[duplicated(exch.dat[, c("Site", "Replicate", "Fortnight.start.date")])==FALSE,
                       c("Site", "Replicate", "Fortnight.start.date", "Fortnight.end.date", "foliage")]
plant.dat$Site  <- as.character(plant.dat$Site)
plant.dat.reps  <- as.data.frame.table(tapply(plant.dat$Replicate, list(plant.dat$Site, plant.dat$Fortnight.start.date), length))
names(plant.dat.reps)  <-  c("Site", "Fortnight.start.date", "n.reps")
plant.dat.rnc.pres  <- as.data.frame.table(tapply(plant.dat$foliage, list(plant.dat$Site, plant.dat$Fortnight.start.date), sum))
names(plant.dat.rnc.pres)  <-  c("Site", "Fortnight.start.date", "n.rnc")
plant.dat  <-  merge(plant.dat.rnc.pres[is.na(plant.dat.rnc.pres$n.rnc)==FALSE, ], plant.dat.reps[is.na(plant.dat.reps$n.reps)==FALSE, ])
plant.dat$rnc  <- plant.dat$n.rnc / plant.dat$n.reps  
plant.dat$rnc  <- plant.dat$n.rnc / plant.dat$n.reps  
plant.dat$Fortnight.start.date  <- as.Date(plant.dat$Fortnight.start.date, format="%Y-%m-%d")  ##POSIXct
plant.dat$Site  <-  as.character(plant.dat$Site)
exch.dat$Fortnight.start.date  <- as.Date(exch.dat$Fortnight.start.date, format="%Y-%m-%d")

plant.dat  <- merge(plant.dat, exch.dat[duplicated(exch.dat[, c("Site", "Fortnight.start.date")])==FALSE, c("Site", "Fortnight.start.date", "Fortnight.end.date")],
                           by = c("Site", "Fortnight.start.date"), all.x=TRUE, all.y=FALSE)

plant.dat.kai  <- plant.dat[plant.dat$Site %in% c("Blue", "Red"), ]
##plant.dat.kai  <-  plant.dat.kai[duplicated(plant.dat.kai[, c("Site", "Fortnight.start.date", "Fortnight.end.date")])==FALSE, ]  
weather.means.kai  <- as.data.frame(matrix(NA,  nrow = length(1:nrow(plant.dat.kai)), ncol = 7, byrow = FALSE))
names(weather.means.kai)  <- c("Site", "Fortnight.start.date", "Fortnight.end.date", "temp", "par", "rain", "rh")
weather.means.kai[, c("Site","Fortnight.start.date", "Fortnight.end.date")]  <- plant.dat.kai[, c("Site","Fortnight.start.date", "Fortnight.end.date")]
weather.means.kai$Site  <- as.character(weather.means.kai$Site);  weather.means.kai$Fortnight.start.date  <- as.POSIXct(weather.means.kai$Fortnight.start.date)
weather.means.kai$Fortnight.end.date  <- as.POSIXct(weather.means.kai$Fortnight.end.date)

plant.dat.kin  <- plant.dat[plant.dat$Site %in% c("Green", "Yellow"), ]
##plant.dat.kin  <-  plant.dat.kin[duplicated(plant.dat.kin[, c("Site", "Fortnight.start.date", "Fortnight.end.date")])==FALSE, ]  
weather.means.kin  <- as.data.frame(matrix(NA,  nrow = length(1:nrow(plant.dat.kin)), ncol = 7, byrow = FALSE))
names(weather.means.kin)  <- c("Site", "Fortnight.start.date", "Fortnight.end.date", "temp", "par", "rain", "rh")
weather.means.kin[, c("Site","Fortnight.start.date", "Fortnight.end.date")]  <- plant.dat.kin[, c("Site","Fortnight.start.date", "Fortnight.end.date")]
weather.means.kin$Site  <- as.character(weather.means.kin$Site);  weather.means.kin$Fortnight.start.date  <- as.POSIXct(weather.means.kin$Fortnight.start.date)
weather.means.kin$Fortnight.end.date  <- as.POSIXct(weather.means.kin$Fortnight.end.date)

##############  Calculate proportion of leaf wetness for two weeks during exchange

#wet.means  <- as.data.frame(matrix(NA,  nrow = length(1:nrow(plant.dat)), ncol = 4, byrow = FALSE))
wet.means  <-  as.data.frame(plant.dat[, c("Site", "Fortnight.start.date", "Fortnight.end.date")])
wet.means$wet  <-  NA; wet.means$Site  <- as.character(plant.dat$Site);  wet.means$Fortnight.start.date  <- as.POSIXct(plant.dat$Fortnight.start.date)
wet.means.kai  <- wet.means[wet.means$Site %in% c("Blue", "Red"), ] ; wet.means.kin  <- wet.means[wet.means$Site %in% c("Green", "Yellow"), ] 
                                        #wet.means$Fortnight.end.date  <- as.POSIXct(wet.means$Fortnight.end.date)
for (i in 1:nrow(wet.means.kai)) {
            temp.wet.n  <-  length(wet.dat[wet.dat$site == "Kaingaroa" & wet.dat$time > as.POSIXct(wet.means.kai[i,
                "Fortnight.start.date"]) &  wet.dat$time < as.POSIXct(wet.means.kai[i, "Fortnight.end.date"]) & wet.dat$wet==1, "wet"])
            temp.wet.tot  <- length(wet.dat[wet.dat$site == "Kaingaroa" & wet.dat$time > as.POSIXct(wet.means.kai[i,
                "Fortnight.start.date"]) &  wet.dat$time < as.POSIXct(wet.means.kai[i, "Fortnight.end.date"]) , "wet"])
            wet.means.kai[i, "wet"]  <- temp.wet.n / temp.wet.tot
 }

for (i in 1:nrow(wet.means.kin)) {
            temp.wet.n  <-  length(wet.dat[wet.dat$site == "Kinleith" & wet.dat$time > as.POSIXct(wet.means.kin[i,
                "Fortnight.start.date"]) &  wet.dat$time < as.POSIXct(wet.means.kin[i, "Fortnight.end.date"]) & wet.dat$wet==1, "wet"])
            temp.wet.tot  <- length(wet.dat[wet.dat$site == "Kinleith" & wet.dat$time > as.POSIXct(wet.means.kin[i,
                "Fortnight.start.date"]) &  wet.dat$time < as.POSIXct(wet.means.kin[i, "Fortnight.end.date"]) , "wet"])
            wet.means.kin[i, "wet"]  <- temp.wet.n / temp.wet.tot
 }
wet.means  <-  rbind(wet.means.kai, wet.means.kin)

##############  Calculate mean weather summaries for two weeks during exchange and month prior 

weather.dat  <- weather.dat[is.na(weather.dat$time)== FALSE, ]

for (i in 1:nrow(weather.means.kai)) {
    for (j in  c("temp", "par", "rain", "rh")) {
#        if (plant.dat[ i, "Site"] %in% c("Blue", "Red")==TRUE) { 
            temp.dat.kai  <- mean(weather.dat[weather.dat$site == "Kaingaroa" & is.na(weather.dat$time)==FALSE & weather.dat$time > as.POSIXct(weather.means.kai[i,
                "Fortnight.start.date"]) &  weather.dat$time < as.POSIXct(weather.means.kai[i, "Fortnight.end.date"]), j], na.rm=TRUE)
                  ####  "Fortnight.end.date"]) &  weather.dat$time > (as.POSIXct(plant.dat[i, "Fortnight.end.date"]) %m+% months(-1)), j], na.rm=TRUE)
        weather.means.kai[i, j]  <-  temp.dat.kai
        weather.means.kai$site == "Kaingaroa"
}
}

for (i in 1:nrow(weather.means.kin)) {
    for (j in  c("temp", "par", "rain", "rh")) {
#        if (plant.dat[ i, "Site"] %in% c("Blue", "Red")==TRUE) { 
            temp.dat.kin  <- mean(weather.dat[weather.dat$site == "Kinleith" & is.na(weather.dat$time)==FALSE & weather.dat$time > as.POSIXct(weather.means.kin[i,
                "Fortnight.start.date"]) &  weather.dat$time < as.POSIXct(weather.means.kin[i, "Fortnight.end.date"]), j], na.rm=TRUE)
                  ####  "Fortnight.end.date"]) &  weather.dat$time > (as.POSIXct(plant.dat[i, "Fortnight.end.date"]) %m+% months(-1)), j], na.rm=TRUE)
        weather.means.kin[i, j]  <-  temp.dat.kin
        weather.means.kin$site == "Kinleith"
}
}

weather.means  <-  rbind(weather.means.kai, weather.means.kin)
weather.means  <- merge(weather.means, wet.means[, c("Site", "Fortnight.start.date", "wet")], by = c("Site", "Fortnight.start.date"))

#####Plot each variable to check summarisation procedure. 
for (j in c("temp", "par", "rain", "rh")){
pdf(paste("Check", j, "FortnightlyHourly.pdf", sep=""))
par(mfrow = c(2,1))
for (i in c("Kinleith", "Kaingaroa")){
plot(weather.dat[is.na(weather.dat[, j]) == FALSE & weather.dat$site==i, j ] ~ weather.dat[is.na(weather.dat[, j]) == FALSE & weather.dat$site==i, "time"],
         type="n", bty="l", ylab=j, xlab="", main= i)
points(weather.dat[is.na(weather.dat[, j]) == FALSE & weather.dat$site==i, j ] ~ weather.dat[is.na(weather.dat[, j]) == FALSE & weather.dat$site==i, "time"],
       cex=0.25, pch=19)
if (i == "Kinleith") {
site.col  <- "Green"
} else {
site.col  <- "Red"
}
lines(weather.means[weather.means$Site==site.col, j] ~ as.POSIXct(weather.means[weather.means$Site==site.col, "Fortnight.start.date"]), col=site.col, lwd=2)
}
dev.off()
}


pdf("CheckLeafWetnessFortnightlyHourly.pdf")
par(mfrow = c(2,1))
for (i in c("Kinleith", "Kaingaroa")){
plot(weather.means[weather.means$Site==site.col, "wet"] ~ as.POSIXct(weather.means[weather.means$Site==site.col, "Fortnight.start.date"]),
         type="n", bty="l", ylab="wet", xlab="", main= i)
if (i == "Kinleith") {
site.col  <- "Green"
} else {
site.col  <- "Red"
}
points(weather.means[weather.means$Site==site.col, "wet"] ~ as.POSIXct(weather.means[weather.means$Site==site.col, "Fortnight.start.date"]),
       cex=0.5, pch=19, col= site.col)
}
dev.off()


## ###############   Summarise other weather means by fortnight
## weather.means  <- as.data.frame(matrix(NA,  nrow = length(1:nrow(plant.dat)), ncol = 8, byrow = FALSE))
## names(weather.means)  <- c("Site", "Fortnight.start.date", "Fortnight.end.date", "temp", "par", "rain", "rh", "wet")
## weather.means$Site  <- as.character(weather.means$Site);  weather.means$Fortnight.start.date  <- as.POSIXct(weather.means$Fortnight.start.date)
## weather.means$Fortnight.end.date  <- as.POSIXct(weather.means$Fortnight.end.date)

## ##############  Calculate mean weather summaries for two weeks during exchange
## for (i in 1:nrow(plant.dat)) {
##     for (j in  c("temp", "par", "rain", "rh", "wet")) {
##         if (plant.dat[ i, "Site"] %in% c("Blue", "Red")==TRUE) { 
##             temp.dat  <- mean(weather.dat[weather.dat$site == "Kaingaroa" & is.na(weather.dat$time)==FALSE & weather.dat$time > as.POSIXct(plant.dat[i,
##                 "Fortnight.start.date"]) &  weather.dat$time < as.POSIXct(plant.dat[i, "Fortnight.end.date"]), j], na.rm=TRUE)
##                   ####  "Fortnight.end.date"]) &  weather.dat$time > (as.POSIXct(plant.dat[i, "Fortnight.end.date"]) %m+% months(-1)), j], na.rm=TRUE)
##             weather.means[i, j]  <-  temp.dat
## }
##         else {
##     temp.dat  <- mean(weather.dat[weather.dat$site == "Kinleith" & is.na(weather.dat$time)==FALSE & weather.dat$time < as.POSIXct(plant.dat[i,
##                 "Fortnight.start.date"]) &  weather.dat$time < as.POSIXct(plant.dat[i, "Fortnight.end.date"]), j], na.rm=TRUE)
##                   ####  "Fortnight.end.date"]) &  weather.dat$time > (as.POSIXct(plant.dat[i, "Fortnight.end.date"]) %m+% months(-1)), j], na.rm=TRUE)
##          weather.means[i, j]  <-  temp.dat
##     }
            
##  weather.means[i, c("Site", "Fortnight.start.date")]  <- plant.dat[i,  c("Site", "Fortnight.start.date")]
## }}

plant.dat  <- merge(plant.dat, weather.means[, c("Site", "Fortnight.start.date", "temp", "par", "rain", "rh", "wet")], by=c("Site", "Fortnight.start.date"), all.x=TRUE, all.y=TRUE)

##########################################################################################################################################################
         ##################################################   Calculate Evapotranspiration   ################################################## 

##plant.dat[is.na(plant.dat$temp)==FALSE, c("Site", "Fortnight.start.date", "temp", "par", "rain", "rh")]
transpiration.dat  <- data.frame(Station= weather.dat$site)
transpiration.dat$Year  <- year(weather.dat$time); transpiration.dat$Month  <- month(weather.dat$time); transpiration.dat$Day  <- day(weather.dat$time);
transpiration.dat$Hour  <- hour(weather.dat$time); transpiration.dat$Date  <- date(weather.dat$time) 
transpiration.dat$Temp  <- weather.dat$temp ; transpiration.dat$RH  <- weather.dat$rh 
transpiration.dat$Precip  <- weather.dat$rain; transpiration.dat$u2  <- weather.dat$wind  
transpiration.dat$Rs  <- (weather.dat$par/220000) * (60*60) ###Convert milli mols sec to Megajoules in hour (m2 is constant). In diffuse daylight 1 micromol = 0.22 joule. 
transpiration.dat  <- transpiration.dat[is.na(transpiration.dat$Station)== FALSE, ]

####Edit contants  
data(constants) ##load constants from package Evapotranspiration
constants$lat  <-  -38.44 # degrees for Kaingaroa,
constants$lat_rad  <-  -0.6763 #  latitude in radians 
constants$Elev  <-  500 # ground elevation above mean sea level
constants$z  <-  3  #height of wind instrument = 10m for Kent Town station,
## Kaingaroa E 1904620 N 5739810. 540 m asl. -38.44 S
## Kinleith E 1859152 N 5756472   450 m -38.31 S

for (i in c("Kaingaroa", "Kinleith")) { ##i  <-  "Kinleith"
temp.dat  <- transpiration.dat[transpiration.dat$Station== i, ] ## & is.na(transpiration.dat$Precip)==FALSE, ]
evap.dat <- ReadInputs(varnames=c("Temp", "RH","Precip","u2","Rs"), temp.dat,  constants, 
                       timestep = "subdaily", stopmissing=c(80,80,60),interp_missing_days = TRUE, interp_missing_entries = TRUE,
                       interp_abnormal = FALSE, missing_method = "monthly average", abnormal_method = NULL)
penman.evap <- ET(evap.dat, constants, save.csv="no") ### Call function for  Penman model
assign(paste("penman.evap", i, sep="."), data.frame(site=i,date = as.Date(rownames(as.data.frame(penman.evap$ET.Daily)), format="%Y-%m-%d"), evapo.trans = as.data.frame(penman.evap$ET.Daily)[,1]))
       }
penman.evap  <- rbind(penman.evap.Kaingaroa, penman.evap.Kinleith)

##############  Calculate mean Evapotranspiration for two weeks during exchange 

plant.dat$evapo.trans  <-  as.numeric(NA)

for (i in 1:nrow(plant.dat)) {
            if (plant.dat[ i, "Site"] %in% c("Blue", "Red")==TRUE) { 
            temp.dat  <- mean(penman.evap[penman.evap$site == "Kaingaroa" & penman.evap$date > plant.dat[i,
                "Fortnight.start.date"] &  penman.evap$date < plant.dat[i, "Fortnight.end.date"], "evapo.trans"], na.rm=TRUE)
            plant.dat[i, "evapo.trans"]  <-  temp.dat
}
        else {
temp.dat  <- mean(penman.evap[penman.evap$site == "Kinleith" & penman.evap$date > plant.dat[i,
                "Fortnight.start.date"] &  penman.evap$date < plant.dat[i, "Fortnight.end.date"], "evapo.trans"], na.rm=TRUE)
            plant.dat[i, "evapo.trans"]  <-  temp.dat
    } }

dat  <- plant.dat[, c("Site","Fortnight.start.date", "rnc", "temp", "evapo.trans")]

## ##########################################################################################################################################################
##          ##################################################   Calculate Raindays from NIWA data  ################################################## 
###NIWA is National Atmospheric Water Administration of New Zealand
niwa.dat  <-  openxlsx::read.xlsx("Data/NIWA data.xlsx", sheet="Sheet1",  detectDates = TRUE, startRow=1, colNames=TRUE)##
niwa.dat$Date <- as.Date(niwa.dat$Date, "%Y.%m.%d")
names(niwa.dat)  <- c("Agent", "Site","Date", "solar.MJm2", "RH", "Earth.Temp",  "Rain", "Soil.Moisture", "Vapour.Pressure.hPa",
                      "evapo.trans",  "pressure.hPa",  "Min.Air.Temp",  "Max.Air.Temp",  "Wind.Speed.MS")            
niwa.dat$rain.day  <-  0; niwa.dat[niwa.dat$Rain > 0.1, "rain.day"]  <-  1

for (i in unique(niwa.dat$Site)){  
start.date.seq  <- c(as.Date(min(dat[dat$Site==i, "Fortnight.start.date"])) -  rev(seq(14, 364, 14)), unique(as.Date(dat[dat$Site==i, "Fortnight.start.date"])))
end.date.seq  <- start.date.seq +14
date.seq  <-  data.frame(start = start.date.seq, end = end.date.seq, rain.days = NA, temp = NA, min.temp = NA, max.temp=NA, evapo.trans = NA)
date.seq$fortnight  <- seq(-25,length(date.seq$start)-26,1) 


for (j in 1:nrow(date.seq)) { 
date.seq[j, "rain.days"]  <- sum(niwa.dat[niwa.dat$Site== i & niwa.dat$Date >= date.seq[j,"start"] & niwa.dat$Date < date.seq[j,"end"], "rain.day"])  
}

for (j in 1:nrow(date.seq)) { 
date.seq[j, "temp"]  <- mean(niwa.dat[niwa.dat$Site== i & niwa.dat$Date >= date.seq[j,"start"] & niwa.dat$Date < date.seq[j,"end"], "Earth.Temp"])  
}

for (j in 1:nrow(date.seq)) { 
date.seq[j, "min.temp"]  <- mean(niwa.dat[niwa.dat$Site== i & niwa.dat$Date >= date.seq[j,"start"] & niwa.dat$Date < date.seq[j,"end"], "Min.Air.Temp"])  
}

for (j in 1:nrow(date.seq)) { 
date.seq[j, "max.temp"]  <- mean(niwa.dat[niwa.dat$Site== i & niwa.dat$Date >= date.seq[j,"start"] & niwa.dat$Date < date.seq[j,"end"], "Max.Air.Temp"])  
}
    
for (j in 1:nrow(date.seq)) { 
date.seq[j, "evapo.trans"]  <- mean(niwa.dat[niwa.dat$Site== i & niwa.dat$Date >= date.seq[j,"start"] & niwa.dat$Date < date.seq[j,"end"], "evapo.trans"])  
}

date.seq$Site  <- i

assign(paste("niwa.", i, sep=""), date.seq)
}

niwa.summary  <- rbind(niwa.Green, niwa.Yellow, niwa.Blue, niwa.Red)

names(dat)  <- c("Site","start", "rnc", "temp","evapo.trans")        
names(niwa.summary)  <-  c("start", "end", "niwa.rain.days", "niwa.temp", "niwa.min.temp", "niwa.max.temp", "niwa.evapo.trans", "fortnight", "Site")       
dat  <- merge(dat[, c("Site","start", "rnc", "temp","evapo.trans")], niwa.summary, by = c("Site", "start"), all.x=TRUE, all.y=TRUE)

lm.evap  <- lm(evapo.trans ~ Site + niwa.evapo.trans + niwa.temp + niwa.min.temp + niwa.max.temp, data=dat)
step.lm.evap  <- step(lm.evap, direction = "both", trace = 0)
dat[is.na(dat$temp)==TRUE, "evapo.trans"]  <-  predict(step.lm.evap, dat[is.na(dat$temp)==TRUE, ])

lm.temp  <- lm(temp ~ Site + niwa.temp + niwa.min.temp + niwa.max.temp, data=dat)
step.lm.temp  <- step(lm.temp, direction = "both", trace = 0)
dat[is.na(dat$temp)==TRUE, "temp"]  <-  predict(step.lm.temp, dat[is.na(dat$temp)==TRUE, ])

#####Produce a dataframe with a series of lags of NIWA data

##lagged.dat  <-  as.data.frame(expand.grid(lag=0:20, evapo.trans=NA, temp=NA))
## lag.dat  <- data.frame(lag = NA, Site = NA, fortnight = NA, rnc = NA, temp = NA, evapo.trans = NA)
## for (i in 0:25) { ##  i  <- 10
##     ##for (k in unique(dat$Site)){ k  <- "Red"

## temp.bysite  <- dat[dat$Site == k, c("Site", "fortnight", "rnc")]
##     temp.bysite  <- cbind(temp.bysite[temp.bysite$fortnight %in% seq(1-i,max(temp.bysite$fortnight)-i, 1),
##                           dat[dat$Site == k & dat$fortnight %in% seq(1-i,max(temp.bysite$fortnight)-i, 1),  c("temp", "evapo.trans")])
## temp.bysite  <- cbind(data.frame(lag = rep(i, nrow(temp.bysite))), temp.bysite)
## lag.dat  <- rbind(lag.dat, temp.bysite)    
 
## }}

###Create empty columns of lagged evaop.trans and temp
rnc.pres.dat  <-  dat[dat$fortnight %in% c(1:max(dat$fortnight)), ] ###Exclude rows where RNC was not collected
dat.lags  <- as.data.frame(matrix(data = NA, nrow = nrow(rnc.pres.dat), ncol = 4 * 26)) ###Empty data frame to fill
names(dat.lags)  <- c(paste("EvapoTrans.", 1:26, sep=""), paste("Temp.", 1:26, sep=""), paste("RainDays.", 1:26, sep=""), paste("MaxTemp.", 1:26, sep="")) ##variable names with 26 lags (yr)
dat.ts  <- cbind(rnc.pres.dat[, c("Site", "fortnight", "start", "end", "rnc",  "temp", "niwa.max.temp", "evapo.trans", "niwa.rain.days")], dat.lags) ##Incorporate time series data for proportion of RNC infections 

for (k in unique(dat.ts$Site)) { ###Fill empty lag variables for four sites
temp.dat.ts  <- dat[dat$Site==k, c("fortnight", "evapo.trans", "temp", "niwa.max.temp", "niwa.rain.days")]
names(temp.dat.ts)  <- c("fortnight", "EvapoTrans", "Temp", "MaxTemp", "RainDays")
    
for (i in 1:26) { ### Max 26 fortnightly lags. Or one year. Fortnight 1 in niwa data is the first fortnight. 0 is the first lag back.
for (j in c("EvapoTrans", "Temp", "MaxTemp", "RainDays")) {
    dat.ts[dat.ts$Site==k, paste(j, i, sep=".")]  <-  temp.dat.ts[temp.dat.ts$fortnight %in% seq(1-i,53-i), j ]
}}}

names(wet.means)  <-  c("Site", "start", "Fortnight.end.date", "wet")                 

dat  <-  merge(dat.ts, wet.means[, c("Site", "start", "wet")], by = c("Site", "start"), all.x=TRUE, all.y=TRUE) 

## ##########################################################################################################################################################
      #######################################          Include month and quarter before fortnight.     #######################################
###One month prior to each fortnight
dat.mth  <- as.data.frame(matrix(data = NA, nrow = nrow(dat), ncol = 3 ))
names(dat.mth)  <-  c("rain.days.mnth", "temp.mnth", "evapo.trans.mnth")
dat.mth  <- as.data.frame(cbind(dat[, c("Site", "fortnight")], dat.mth))
dat.mth[,-1] <- as.data.frame(lapply(dat.mth[,-1], function(x) as.numeric(as.character(x))))  ##Ensure all columns are numeric

for (k in unique(dat.mth$Site)) {    
 for (i in 1:53) {
rain.out  <- sum(niwa.summary[niwa.summary$Site== k & niwa.summary$fortnight <= i & niwa.summary$fortnight >= i-1, "niwa.rain.days" ])
dat.mth[dat.mth$Site==k & dat.mth$fortnight==i,  "rain.days.mnth"]  <-  rain.out

temp.out  <- mean(niwa.summary[niwa.summary$Site== k & niwa.summary$fortnight <= i & niwa.summary$fortnight >= i-1, "niwa.temp"])
dat.mth[dat.mth$Site==k & dat.mth$fortnight==i,  "temp.mnth"]  <-  temp.out

evap.out  <- mean(niwa.summary[niwa.summary$Site== k & niwa.summary$fortnight <= i & niwa.summary$fortnight >= i-1, "niwa.evapo.trans"])
dat.mth[dat.mth$Site==k & dat.mth$fortnight==i,  "evapo.trans.mnth"]  <-  evap.out
}}

###One quarter (6 fortnights) prior to each fortnight
dat.qrt  <- as.data.frame(matrix(data = NA, nrow = nrow(dat), ncol = 4 * 4 ))
names(dat.qrt)  <- c(paste("rain.days.qrt.", 1:4, sep=""), paste("temp.qrt.", 1:4, sep=""), paste("max.temp.qrt.", 1:4, sep=""), paste("evapo.trans.qrt.", 1:4, sep=""))
dat.qrt  <- as.data.frame(cbind(dat.ts[, c("Site", "fortnight")], dat.qrt))
dat.qrt[,-1] <- as.data.frame(lapply(dat.qrt[,-1], function(x) as.numeric(as.character(x))))  ##Ensure all columns are numeric
quarter.index  <- data.frame(lag.1= 1:6, lag.2= 7:12, lag.3= 13:18, lag.4= 19:24) ###An index with values to pool months by for quarters
 
for (k in unique(dat.qrt$Site)) { 
     for (i in 1:53) {
         for (j in 1:4) {
rain.out  <- sum(niwa.summary[niwa.summary$Site== k & niwa.summary$fortnight %in% c(i - quarter.index[, j]), "niwa.rain.days"])            
dat.qrt[dat.qrt$Site==k & dat.qrt$fortnight==i,  paste("rain.days.qrt.", j , sep="")]  <-  rain.out
 
temp.out  <- mean(niwa.summary[niwa.summary$Site== k & niwa.summary$fortnight %in% c(i - quarter.index[, j]), "niwa.temp"])            
dat.qrt[dat.qrt$Site==k & dat.qrt$fortnight==i,  paste("temp.qrt.", j , sep="")]  <-  temp.out
 
max.temp.out  <- mean(niwa.summary[niwa.summary$Site== k & niwa.summary$fortnight %in% c(i - quarter.index[, j]), "niwa.max.temp"])            
dat.qrt[dat.qrt$Site==k & dat.qrt$fortnight==i,  paste("max.temp.qrt.", j , sep="")]  <-  max.temp.out
 
evap.out  <- mean(niwa.summary[niwa.summary$Site== k & niwa.summary$fortnight %in% c(i - quarter.index[, j]), "niwa.evapo.trans"])            
dat.qrt[dat.qrt$Site==k & dat.qrt$fortnight==i,  paste("evapo.trans.qrt.", j , sep="")]  <-  evap.out
 }}}

dat  <- merge(dat, dat.mth, by = c("fortnight", "Site"))
dat  <- merge(dat, dat.qrt, by = c("fortnight", "Site"))
dat  <-  dat[order(dat$Site, dat$fortnight), ]

                     ######################################################################################################
                       ##################################        Calculate Season     ##################################

spring.dates  <- c(dat[dat$start >= as.Date("2017-11-20") & dat$start <= as.Date("2017-11-30"),"start" ],
                   dat[dat$start >= as.Date("2018-09-01") & dat$start <= as.Date("2018-11-30"),"start" ],
                   dat[dat$start >= as.Date("2019-09-01") & dat$start <= as.Date("2019-11-30"),"start" ])
dat[dat$start %in% spring.dates, "Season"]  <-  "spring"

summer.dates  <- c(dat[dat$start >= as.Date("2017-12-01") & dat$start <= as.Date("2018-02-28"),"start" ],
                   dat[dat$start >= as.Date("2018-12-01") & dat$start <= as.Date("2019-02-28"),"start" ],
                   dat[dat$start >= as.Date("2019-12-01") & dat$start <= as.Date("2020-02-29"),"start" ]) ##last exchange start "2019-12-19"
dat[dat$start %in% summer.dates, "Season"]  <-  "summer"

autumn.dates  <- c(dat[dat$start >= as.Date("2018-03-01") & dat$start <= as.Date("2018-05-31"),"start" ],
                   dat[dat$start >= as.Date("2019-03-01") & dat$start <= as.Date("2019-05-31"),"start" ])
dat[dat$start %in% autumn.dates, "Season"]  <-  "autumn"

winter.dates  <- c(dat[dat$start >= as.Date("2018-06-01") & dat$start <= as.Date("2018-08-31"),"start" ],
                   dat[dat$start >= as.Date("2019-06-01") & dat$start <= as.Date("2019-08-31"),"start" ])
dat[dat$start %in% winter.dates, "Season"]  <-  "winter"


   ################################################################################################################################################################
              ################################                     STATISTICAL ANALYSIS                ################################  
   ################################################################################################################################################################

## library(pls)
## rf.formula  <- as.formula(paste(names(dat)[5], "~", "site + season + ", paste(names(dat)[-c(1:5, length(names(dat)), length(names(dat))-1, length(names(dat))-2)], collapse=" + ")))

## plsFit <- plsr(rf.formula, data = dat)


set.seed(123) ; dat$site  <- as.numeric(as.factor(dat$Site)); dat$season  <- as.numeric(as.factor(dat$Season)) ###Convert Site and Season to numeric
index.dat <- sample(1:nrow(dat), size = 0.7*nrow(dat)) 
train.dat <- dat[index.dat,] ; test.dat <- dat[-index.dat,] ## # subset data to include only the elements in the index. nrow(train.dat); nrow(test.dat)

rf.formula  <- as.formula(paste(names(dat)[5], "~", "site + season + ", paste(names(dat)[-c(1:5, length(names(dat)), length(names(dat))-1, length(names(dat))-2)], collapse=" + ")))

length.variables  <- length(c("site", "season", (names(dat)[-c(1:5, length(names(dat)), length(names(dat))-1, length(names(dat))-2)])))

rf <- randomForest(rf.formula, data = train.dat, importance = TRUE, ntree=500)
# How many trees are needed to reach the minimum error estimate? 
# This is a simple problem; it appears that about 100 trees would be enough. 
which.min(rf$mse)
# Plot rf to see the estimated error as a function of the number of trees
# (not running it here)
# plot(rf) 
 
imp.rf <- as.data.frame(sort(importance(rf)[,1],decreasing = TRUE),optional = T) # Importance function to calculate the importance of each variable
names(imp.rf) <- "MSE"
imp.rf$Predictor  <- rownames(imp.rf)

test.pred.forest <- predict(rf,test.dat) # Predict and evaluate on the test set

RMSE.forest <- sqrt(mean((test.pred.forest-test.dat$rnc)^2))
MAE.forest <- mean(abs(test.pred.forest-test.dat$rnc))

rf.full <- randomForest(rf.formula, data = dat, importance = TRUE, ntree=500)
dat$rf.fit <- predict(rf.full, data=dat)
R2.forest  <-  1 - sum((dat$rnc - dat$rf.fit)^2)/sum((dat$rnc-mean(dat$rnc))^2)


#lm.formula  <-  as.formula(paste(names(dat)[5], "~", paste((names(dat)[-c(1,3:5)]), collapse=" + ")))
##evap.formula  <-  as.formula(paste(names(dat)[5], "~", paste((names(dat)[-c(2:6,8, 34:87)]), collapse=" + ")))
##temp.formula  <-  as.formula(paste(names(dat)[5], "~", paste((names(dat)[-c(2:5,7:34, 61:87)]), collapse=" + ")))
selected.formula  <-  as.formula(paste(names(dat)[5], "~", paste(rownames(imp.rf)[1:10] , collapse=" + "))) ##"Site + Season +", 
lm.1  <-  lm(selected.formula, data = dat) ##BEST. Lowest AIC
## lm.1  <-  lm(lm.formula, data = dat) ##BEST. Lowest AIC
step.lm  <-  step(lm.1, direction = "both", trace = 0)

dat$lm.fit  <- predict(step.lm, data=dat)
RMSE.lm  <-  ModelMetrics::rmse(dat$rnc, dat$lm.fit)
R2.lm  <-  1 - sum((dat$rnc - dat$lm.fit)^2)/sum((dat$rnc-mean(dat$rnc))^2)
lm.aic  <-  AIC(step.lm)

##summary(lm(rnc ~ wet, data=dat)  ##Wetness has little effect

####An Anova table for display
anova.tab  <- as.data.frame(anova(step.lm))
names(anova.tab)  <- c("df", "SumSq", "MeanSq", "Fvalue",  "P")
anova.tab.coefs  <- anova.tab[-c(nrow(anova.tab)), c("df", "MeanSq", "Fvalue",  "P")]
anova.tab  <-  rbind(anova.tab.coefs[order(anova.tab.coefs$MeanSq, decreasing = TRUE), ],
                     anova.tab[nrow(anova.tab),c("df", "MeanSq", "Fvalue",  "P")])
anova.tab  <- round(anova.tab[, c("MeanSq", "Fvalue",  "P")], digits=3)

#reduced.formula  <-  as.formula(paste(names(dat)[5], "~", paste(rownames(anova.tab)[1:4], collapse=" + ")))


####Output key data frames to a summary file
save(dat, imp.rf, length.variables, RMSE.forest, R2.forest, RMSE.lm, R2.lm, anova.tab, file="SeptSummaryRNC.rds") ###Saves data frames for import using RNCtrial.r

explanation  <- as.data.frame(rbind("Tables in this spreadsheet are intended to pasted into a word document.",
                                    "Variables included in random forests RNC analysis are:",
                                    as.character(rf.formula)))

# Create a blank workbook
wb.out <- createWorkbook("TabulatedResultsRNC")
addWorksheet(wb.out, "Explanation") # Add some sheets to the workbook
addWorksheet(wb.out, "Table1"); addWorksheet(wb.out, "Table2")

# Write the data to the sheets
writeData(wb.out, sheet = "Explanation", x = explanation)
writeData(wb.out, sheet = "Table1", x = imp.rf[imp.rf$MSE > 5, c("Predictor",  "MSE")])
writeData(wb.out, sheet = "Table2", x = anova.tab, rowNames = TRUE)

# Export the file
saveWorkbook(wb.out, "TabulatedResultsRNC.xlsx", overwrite=TRUE)

##dat$lm.fit  <- predict(lm.1, data=dat)
dat$less.fit <- predict(lm.1, data=dat)

site.cols  <- data.frame(Site = c("Red", "Blue", "Green",  "Yellow"),
                         site.cols = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"), site.names = c("Goudies", "Low Level", "Tar Hill",  "Kaki"))
site.cols <- as.data.frame(lapply(site.cols, function(x) as.character(x)))  ##Ensure all columns are character
site.cols$site.cols  <- as.character(site.cols$site.cols); site.cols$site.names  <- as.character(site.cols$site.names)
pcr.dat <- merge(pcr.dat, site.cols, by="Site")


## For confirmation
## "Goudies" = Red Site #E41A1C
## "Low Level" = Blue Site #377EB8
## "Tar Hill" = Green Site #4DAF4A
## "Kaki" = Yellow Site #984EA3 (Now purple)

 
        ##############################################################################################################################
             ############################################      Figures 1 and 2.   ##########################################


## png(file = "ObservedPredictedRNCweatherStn.png", width=800)#, width=20cm)
## pdf(file = "ObservedPredictedRNCweatherStn.pdf")##, width=20cm)#
svg(file = "Fig1ObservedPredictedRNCweatherStn.svg")
par(mar = c(7, 4, 4, 2) + 0.1, mfrow = c(3,1))
plot(rnc ~ fortnight, data=dat, type="n", bty="l", ylab="Proportion of plants infected", xlab="", ylim=(c(0,1.25)), xlim=(c(0,52)), axes=FALSE) ##main="Observed",
axis(1, at=seq(1,51, 2), labels = FALSE)#c(substr(month.abb[11:12], 1,1), rep(substr(month.abb, 1,1), 2)), cex=.5)
text(seq(1,51, 2), par("usr")[3] - 0.15, srt = 45, adj = 0.9,  labels = c(month.abb[11:12], rep(month.abb, 2)), xpd = TRUE, cex=0.75) ## 
text(c(15, 40), par("usr")[3] - .4, srt = 0, adj = 1,  labels = c("2018", "2019"), xpd = TRUE) ## Plot x axis labels at default tick marks

## axis(1, at = c(0, 26, 52 ), labels = c("Nov. 2017", "Nov. 2018", "Dec. 2018"), tick = TRUE, lwd=2, tck=-0.05)
## axis(1, at = seq(2,52,2), labels = FALSE, tick = TRUE, lwd=1, tck=-0.025)
axis(2, at = c(0,0.5,1), labels = c(0,0.5,1), tick = TRUE, lwd=1, tck=-0.05)
axis(2, at = c(0,0.25,0.75), labels = FALSE, tick = TRUE, lwd=0.75, tck=-0.025)

for (i in unique(dat$Site)) {
    lines(rnc ~ fortnight, data=dat[dat$Site==i,], lwd=1.5, col= as.character(site.cols[site.cols$Site==i, "site.cols"]))
    }

pcr.dat$Site  <- as.character(pcr.dat$Site); ##pcr.dat[pcr.dat$Site=="Yellow", "Site"]  <-  "Yellow2"
points(jitter(rep(.9, nrow(pcr.dat))) ~ jitter(pcr.dat$Fortnight.exposed), cex=0.5, col=pcr.dat$site.cols, pch=1)
pp.dat  <- pcr.dat[is.na(pcr.dat$Pp)==FALSE & pcr.dat$Pp!=0,]; pk.dat  <- pcr.dat[is.na(pcr.dat$Pk)==FALSE & pcr.dat$Pk!=0,]
points(rep(1.15, nrow(pp.dat)) ~ jitter(pp.dat$Fortnight.exposed), col=pp.dat$site.cols, pch=2) 
points(rep(1.05, nrow(pk.dat)) ~ jitter(pk.dat$Fortnight.exposed), col=pk.dat$site.cols, pch=6) 
text(44, .87, label = "Sampled", cex=0.75); text(44, 1.025, label = "P. pluvialis", cex=0.75); text(44, 1.2, label = "P. kernoviae", cex=0.75)
mtext("a) Observed", cex=0.75, side=2, line=2, las=2, adj=0, padj=-6)

plot(rnc ~ fortnight, data=dat, type="n", bty="l", ylab="Proportion of plants infected", xlab="", ylim=(c(0,1)), xlim=(c(0,52)), axes=FALSE) ##main="Predicted - Random Forests"
axis(1, at=seq(1,51, 2), labels = FALSE)#c(substr(month.abb[11:12], 1,1), rep(substr(month.abb, 1,1), 2)), cex=.5)
text(seq(1,51, 2), par("usr")[3] - 0.15, srt = 45, adj = 0.9,  labels = c(month.abb[11:12], rep(month.abb, 2)), xpd = TRUE, cex=0.75) ## 
text(c(15, 40), par("usr")[3] - .4, srt = 0, adj = 1,  labels = c("2018", "2019"), xpd = TRUE) ## Plot x axis labels at default tick marks
## axis(1, at = c(0, 26, 52 ), labels = c("Nov. 2017", "Nov. 2018", "Dec. 2018"), tick = TRUE, lwd=2, tck=-0.05)
## axis(1, at = seq(2,52,2), labels = FALSE, tick = TRUE, lwd=1, tck=-0.025)
axis(2, at = c(0,0.5,1), labels = c(0,0.5,1), tick = TRUE, lwd=1, tck=-0.05)
axis(2, at = c(0,0.25,0.75), labels = FALSE, tick = TRUE, lwd=0.75, tck=-0.025)
mtext("b) Predicted - random forests", cex=0.75, side=2, line=2, las=2, adj=0, padj=-6)

for (i in unique(dat.ts$Site)) {
        lines(rf.fit ~ fortnight, data= dat[dat$Site==i, ], lwd=1.5, col=site.cols[site.cols$Site==i, "site.cols"])
}

## reduced.formula  <-  as.formula(paste(names(dat)[5], "~", paste(rownames(anova.tab)[1:4], collapse=" + ")))
## lm.reduced  <-  lm(reduced.formula, data = dat) ##
## dat$less.fit  <- predict(lm.reduced, data=dat)

plot(rnc ~ fortnight, data=dat, type="n", bty="l", ylab="Proportion of plants infected", xlab="", ylim=(c(0,1)), xlim=(c(0,52)), axes=FALSE) ##main="Predicted - Reduced OLS"
axis(1, at=seq(1,51, 2), labels = FALSE)#c(substr(month.abb[11:12], 1,1), rep(substr(month.abb, 1,1), 2)), cex=.5)
text(seq(1,51, 2), par("usr")[3] - 0.15, srt = 45, adj = 0.9,  labels = c(month.abb[11:12], rep(month.abb, 2)), xpd = TRUE, cex=0.75) ## 
text(c(15, 40), par("usr")[3] - .4, srt = 0, adj = 1,  labels = c("2018", "2019"), xpd = TRUE) ## Plot x axis labels at default tick marks
## axis(1, at = c(0, 26, 52 ), labels = c("Nov. 2017", "Nov. 2018", "Dec. 2018"), tick = TRUE, lwd=2, tck=-0.05)
## axis(1, at = seq(2,52,2), labels = FALSE, tick = TRUE, lwd=1, tck=-0.025)
axis(2, at = c(0,0.5,1), labels = c(0,0.5,1), tick = TRUE, lwd=1, tck=-0.05)
axis(2, at = c(0,0.25,0.75), labels = FALSE, tick = TRUE, lwd=0.75, tck=-0.025)
mtext("c) Predicted - reduced OLS", cex=0.75, side=2, line=2, las=2, adj=0, padj=-6)

for (i in unique(dat.ts$Site)) {
        lines(less.fit ~ fortnight, data= dat[dat$Site==i, ], lwd=1.5, col=site.cols[site.cols$Site==i, "site.cols"])
}

dev.off()

#png(file = "WeatherStnObservations.png", width=800)#, width=20cm)
svg(file = "Fig2WeatherStnObservations.svg")
par(mar = c(7, 4, 4, 2) + 0.1, mfrow = c(3,1))
###Plot Evapo Transpiration for one year prior
plot(evapo.trans ~ fortnight, data=dat, type="n", bty="l", ylab="Penman index", xlab="", ylim=(c(0,3)), xlim=(c(0,52)), axes=FALSE) #main="Evapo-transpiration"
axis(1, at=seq(1,51, 2), labels = FALSE)#c(substr(month.abb[11:12], 1,1), rep(substr(month.abb, 1,1), 2)), cex=.5)
text(seq(1,51, 2), par("usr")[3] - 0.5, srt = 45, adj = 0.9,  labels = c(month.abb[11:12], rep(month.abb, 2)), xpd = TRUE, cex=0.75) ## 
text(c(15, 40), par("usr")[3] - 1.25, srt = 0, adj = 1,  labels = c("2018", "2019"), xpd = TRUE) ## Plot x axis labels at default tick marks
axis(2, at = 0:3, labels = 0:3, tick = TRUE)
mtext("a) Evapo-transpiration", cex=.75, side=2, line=2, las=2, adj=0, padj=-6)
for (i in unique(dat$Site)) {
    lines(evapo.trans ~ fortnight, data=dat[dat$Site==i,], lwd=1.5, col=site.cols[site.cols$Site==i, "site.cols"])
}

plot(temp ~ fortnight, data=dat, type="n", bty="l", ylab="Temperature", xlab="", ylim=(c(0,24)), xlim=(c(0,52)), axes=FALSE) #main="Air temp"
axis(1, at=seq(1,51, 2), labels = FALSE)#c(substr(month.abb[11:12], 1,1), rep(substr(month.abb, 1,1), 2)), cex=.5)
text(seq(1,51, 2), par("usr")[3] - 3.7, srt = 45, adj = 0.9,  labels = c(month.abb[11:12], rep(month.abb, 2)), xpd = TRUE, cex=0.75) ## 
text(c(15, 40), par("usr")[3] - 9.5, srt = 0, adj = 1,  labels = c("2018", "2019"), xpd = TRUE) ## Plot x axis labels at default tick marks
axis(2, at = c(0,12,24), labels = c(0,12,24), tick = TRUE)
mtext("b) Air temperature", cex=.75, side=2, line=2, las=2, adj=0, padj=-6)
for (i in unique(dat$Site)) {
    lines(temp ~ fortnight, data=dat[dat$Site==i,], lwd=1.5, col=site.cols[site.cols$Site==i, "site.cols"])
}

plot(wet ~ fortnight, data=dat, type="n", bty="l", ylab="Wetness proportion", xlab="", ylim=(c(0,1)), xlim=(c(0,52)), axes=FALSE) #main="Leaf wetness"
axis(1, at=seq(1,51, 2), labels = FALSE)#c(substr(month.abb[11:12], 1,1), rep(substr(month.abb, 1,1), 2)), cex=.5)
text(seq(1,51, 2), par("usr")[3] - 0.15, srt = 45, adj = 0.9,  labels = c(month.abb[11:12], rep(month.abb, 2)), xpd = TRUE, cex=0.75) ## 
text(c(15, 40), par("usr")[3] - .4, srt = 0, adj = 1,  labels = c("2018", "2019"), xpd = TRUE) ## Plot x axis labels at default tick marks
axis(2, at = c(0,0.5,1), labels = c(0,0.5,1), tick = TRUE)
mtext("c) Leaf wetness", cex=0.75, side=2, line=2, las=2, adj=0, padj=-6)
for (i in unique(dat$Site)) {
    lines(wet ~ fortnight, data=dat[dat$Site==i,], lwd=1.5, col=site.cols[site.cols$Site==i, "site.cols"])
}
dev.off()


##    ###########################################################################################################################################################
##                 ###############################  Previous model comparisons ##############################################################
## rf.formula  <-  as.formula(paste(names(dat.omit)[6], "~", paste((names(dat.omit)[-c(1, 2,6)]), collapse=" + "))) ###Site excluded
## ##lme.formula  <-  as.formula(paste(names(dat.omit)[6], "~", paste((names(dat.omit)[-c(1,2,6)]), collapse=" + "))) 
## lme1  <-  lme(lm.formula, data = dat.omit, random = ~ 1 | Site,  corr = corSpatial(form = ~ fortnight, type ="exponential", nugget = F), method = "ML") 
## lme2  <-  lme(rf.formula, data = dat.omit, random = ~ 1 | Site,  corr = corSpatial(form = ~ fortnight, type ="exponential", nugget = F), method = "ML") 
## lme3  <-  lme(lm.formula, data = dat.omit, random = ~ 1 | Site, method = "ML")
## gls1  <-  gls(lm.formula, data= na.omit(dat), correlation = corAR1(form = ~fortnight|Site), method="ML", na.action=na.omit)##Best GLS model
## gls2  <-  gls(rf.formula, data= na.omit(dat), correlation = corAR1(form = ~fortnight|Site), method="ML", na.action=na.omit) 
## lm.1  <-  lm(lm.formula, data = dat.omit) ##BEST. Lowest AIC
## lm.2  <-  lm(rf.formula, data = dat.omit) 
## step.lme1  <- MASS::stepAIC(lme1, direction="both", trace=0)
## step.lme2  <- MASS::stepAIC(lme2, direction="both", trace=0)

## dat.omit$lme.fit  <- predict(step.lme1, data=dat.omit)
## lme.rmse  <-  ModelMetrics::rmse(dat.omit$RNC, dat.omit$lme.fit)
## lme.r2  <-  1 - sum((dat.omit$RNC - dat.omit$lme.fit)^2)/sum((dat.omit$RNC-mean(dat.omit$RNC))^2)
## lme.aic  <-  AIC(step.lme1)
## step.lme3  <- MASS::stepAIC(lme3, direction="both", trace=0)
## step.gls1  <- MASS::stepAIC(gls1, direction="both", trace=0)
## step.gls2  <- MASS::stepAIC(gls2, direction="both", trace=0)
## dat.omit$gls.fit  <- predict(step.gls1, data=dat.omit)
## gls.rmse  <-  ModelMetrics::rmse(dat.omit$RNC, dat.omit$gls.fit)
## gls.r2  <-  1 - sum((dat.omit$RNC - dat.omit$gls.fit)^2)/sum((dat.omit$RNC-mean(dat.omit$RNC))^2)
## gls.aic  <-  AIC(step.gls1)

## mod.1  <- MASS::stepAIC(lm.1, direction="both", trace=0) ###Best Model
## step.lm2  <- MASS::stepAIC(lm.2, direction="both", trace=0)

## dat.omit$lm.fit  <- predict(mod.1, data=dat.omit)
## lm.rmse  <-  ModelMetrics::rmse(dat.omit$RNC, dat.omit$lm.fit)
## lm.r2  <-  1 - sum((dat.omit$RNC - dat.omit$lm.fit)^2)/sum((dat.omit$RNC-mean(dat.omit$RNC))^2)
## lm.aic  <-  AIC(mod.1)

## rf.1  <- randomForest(rf.formula, data=dat.omit)
## dat.omit$rf.fit  <- rf.1$predicted  ####Prediction from the random forest model (model excludes na values)
## rf.rmse  <-  ModelMetrics::rmse(dat.omit$RNC, dat.omit$rf.fit)
## rf.r2  <-  1 - sum((dat.omit$RNC - dat.omit$rf.fit)^2)/sum((dat.omit$RNC-mean(dat.omit$RNC))^2)

## anova(step.lme1, step.lme2, step.lme3, step.gls1, step.gls2, mod.1, step.lm2)


## ####An Anova table for display
## anova.tab  <- as.data.frame(anova(mod.1))
## names(anova.tab)  <- c("df", "SumSq", "MeanSq", "Fvalue",  "P")
## anova.tab.coefs  <- anova.tab[-c(1,22), c("df", "MeanSq", "Fvalue",  "P")]
## anova.tab  <-  rbind(anova.tab[1, c("df", "MeanSq", "Fvalue",  "P")], anova.tab.coefs[order(anova.tab.coefs$MeanSq, decreasing = TRUE), ],
##                      anova.tab[22,c("df", "MeanSq", "Fvalue",  "P")])
## anova.tab  <- round(anova.tab[, c("MeanSq", "Fvalue",  "P")], digits=3)


##     ######################################### Check Weather temperature data from Kinleith ########################################################
## pdf("FortnightlyHourlyTemp.pdf")
## par(mfrow = c(2,1))
## for (i in c("Kinleith", "Kaingaroa")){
## plot(temp ~ time, data=weather.dat[weather.dat$site== i, ], type="n", bty="l", ylab="Air temperature", xlab="", main= i)
## points(temp ~ time, data=weather.dat[weather.dat$site==i, ], cex=0.25, pch=19)

## if (i == "Kinleith") {
## site.col  <- "Green"
## } else {
## site.col  <- "Red"
## }
## lines(dat[dat$Site==site.col, "temp"] ~ as.POSIXct(dat[dat$Site==site.col, "start"]), col=site.col, lwd=2)
## }
## dev.off()


        ##############################################################################################################################
             ############################################      Figure 3.   ##########################################
                      ######################### Analysis of time until infection #######################################

exch.dat$seedling.rep  <- paste(exch.dat$Site, exch.dat$Replicate, exch.dat$Fortnight.exposed, sep="-")
assessment.summary  <- as.integer(summary(tapply(exch.dat$Batch.assessment, exch.dat$seedling.rep, max))[c(1,6,4)])
assessment.rnc  <- exch.dat[exch.dat$Older.foliage.score_Updated > 0, c("seedling.rep", "Batch.assessment", "Fortnight.start.date")]
assessment.rnc  <-  assessment.rnc[base::order(assessment.rnc$seedling.rep, assessment.rnc$Batch.assessment, decreasing = FALSE), ]
assessment.rnc  <- assessment.rnc[duplicated(assessment.rnc$seedling.rep)==FALSE, ]
assessment.summary.rnc  <- c(summary(assessment.rnc$Batch.assessment)[c(1,6,4)], sd(assessment.rnc$Batch.assessment))

#fortnight.start  <- unique(paste(dat$fortnight, dat$start))
fortnight.start.dat <- data.frame(fortnight =  as.integer(as.character(substr(unique(paste(dat$fortnight, dat$start)), 1,
                                                                              regexpr(" ", unique(paste(dat$fortnight, dat$start)))-1))),
                                  Fortnight.start.date = as.Date(substr(unique(paste(dat$fortnight, dat$start)), regexpr(" ", unique(paste(dat$fortnight, dat$start)))+1,
                                                     nchar(unique(paste(dat$fortnight, dat$start)))), "%Y-%m-%d"))
fortnight.start.dat$fortnight.days  <- round((as.numeric(unique(fortnight.start.dat$Fortnight.start.date) - +
                                                        min(unique(fortnight.start.dat$Fortnight.start.date))) /14) +1, digits=1)

assessment.rnc  <- merge(assessment.rnc, fortnight.start.dat, by= "Fortnight.start.date", all.x=T, all.y=F)
assessment.rnc$Site  <- substr(assessment.rnc$seedling.rep, 1, regexpr("-", assessment.rnc$seedling.rep)-1)

assessment.rnc  <- merge(assessment.rnc, dat[, c("Site", "fortnight", "temp", "niwa.max.temp",  "evapo.trans", "wet", "Season")],
                         by=c("Site", "fortnight"), all.x=T, all.y=F)

###Summarise delay until detection by fortnight
delay.fort  <- as.data.frame.table(tapply(assessment.rnc$Batch.assessment, list(assessment.rnc$fortnight, assessment.rnc$Site), mean))
names(delay.fort)  <- c("fortnight", "site", "mean.delay")
delay.fort  <-  delay.fort[is.na(delay.fort$mean.delay)==FALSE, ] 
delay.fort  <-  delay.fort[order(delay.fort$fortnight), ]
delay.fort$fortnight  <-  as.numeric(as.character(delay.fort$fortnight))
##t.test(delay.fort$mean.delay ~ delay.fort$inf.stage)

assessment.rnc.lm  <- lm(Batch.assessment ~  Site + Season + temp + niwa.max.temp + evapo.trans + wet , data = assessment.rnc)              
anova(step(assessment.rnc.lm))
coef(assessment.rnc.lm)

## svg("BarplotRNCassessments.svg")
## barplot(t(table(assessment.rnc$fortnight, assessment.rnc$Batch.assessment)), ylab="Frequency of symptomatic seedlings", xlab="Fortnight exchanged")
## dev.off()

rnc.n.dat  <- as.data.frame(table(assessment.rnc$fortnight, assessment.rnc$Batch.assessment))
names(rnc.n.dat)  <- c("fortnight", "assessment", "freq"); rnc.n.dat$fortnight  <-  as.integer(as.character(rnc.n.dat$fortnight))
rnc.n.dat  <-  rnc.n.dat[rnc.n.dat$freq>0, ]; rnc.n.dat$assessment  <-  as.integer(as.character(rnc.n.dat$assessment))

svg("Fig3DotPlotRNCassessments.svg")
par(mar = c(7, 4, 4, 2) + 0.1)  ## Increase bottom margin to make room for rotated labels
date.labels  <- c("Nov 2017","Nov 2018","Nov 2019")
plot(rep(6, 52) ~ c(1:52),  type="n", bty="l", ylab="Fortnightly assessment", xaxt="n", xlab="", ylim=c(0.75, 6), xlim=c(0, 52))
axis(1, at=seq(1,51, 2), labels = FALSE)#c(substr(month.abb[11:12], 1,1), rep(substr(month.abb, 1,1), 2)), cex=.5)
text(seq(1,51, 2), par("usr")[3] - 0.25, srt = 45, adj = 1,  labels = c(month.abb[11:12], rep(month.abb, 2)), xpd = TRUE, cex=0.75) ## 
text(c(15, 40), par("usr")[3] - .75, srt = 0, adj = 1,  labels = c("2018", "2019"), xpd = TRUE) ## Plot x axis labels at default tick marks

##text(c(1, 26, 52), par("usr")[3] - 0.25, srt = 45, adj = 1,  labels = date.labels, xpd = TRUE) ## Plot x axis labels at default tick marks
##axis(1, at=c(1, 26, 52), labels = c("Nov 2017","Nov 2018","Nov 2019"))
points(rnc.n.dat$assessment ~ rnc.n.dat$fortnight, cex = rnc.n.dat$freq/ 2, col = "grey", pch=19)

## for (i in unique(delay.fort$site)) {
## lines(mean.delay ~ fortnight, data=delay.fort[delay.fort$site==i,], cex=0.75, col= as.character(site.cols[site.cols$Site==i, "site.cols"])) #pch=19
##     }

legend("topright", legend=c("N = 1", "N = 2", "N = 5", "N = 10"), pch=19, cex=0.75, pt.cex=c(0.5,1,2.5,5), col="grey",
       title="Frequency", bty="n", x.intersp=4, y.intersp=2.5)

dev.off()

svg("D##ateVsFortnight.svg"); plot(dat$fortnight ~ dat$start); dev.off()

## rnc.mean.dat  <- as.data.frame.table(tapply(assessment.rnc$Batch.assessment, list(assessment.rnc$Site, assessment.rnc$fortnight), mean, na.rm=TRUE))
## names(rnc.mean.dat)  <- c("site", "fortnight", "assessment.mean"); rnc.mean.dat  <-  rnc.mean.dat[is.na(rnc.mean.dat$assessment.mean)==FALSE, ]


## svg("Fig3DotPlotRNCassessments.svg")
## par(mar = c(7, 4, 4, 2) + 0.1)  ## Increase bottom margin to make room for rotated labels
## plot(rep(5, 52) ~ c(1:52),  type="n", bty="l", ylab="Mean assessment scores", xaxt="n", xlab="", ylim=c(0.75, 6), xlim=c(0, 52))
## axis(1, at=seq(1,51, 2), labels = FALSE)#c(substr(month.abb[11:12], 1,1), rep(substr(month.abb, 1,1), 2)), cex=.5)
## text(seq(1,51, 2), par("usr")[3] - 0.25, srt = 45, adj = 1,  labels = c(month.abb[11:12], rep(month.abb, 2)), xpd = TRUE, cex=0.75) ## 
## text(c(15, 40), par("usr")[3] - .75, srt = 0, adj = 1,  labels = c("2018", "2019"), xpd = TRUE) ## Plot x axis labels at default tick marks

## for (i in unique(delay.fort$site)) {
## ##i  <- "Yellow" 
## lines(assessment.mean ~ fortnight, data=rnc.mean.dat[rnc.mean.dat$site==i,], cex=0.75, col= as.character(site.cols[site.cols$Site==i, "site.cols"])) #pch=19
## }

## dev.off()






        ##############################################################################################################################
             ############################################      Figures 4 A and B.   ########################################## 
                    ####################      Time until symptoms apparent after infection      ########################
bucket.dat  <- as.data.frame(openxlsx::read.xlsx("NovData/Bait bucket data.xlsx", detectDates = TRUE)) ##UpdatedPCR.xlsx
bucket.dat.sum  <- as.data.frame(expand.grid(date = as.Date(unique(bucket.dat$End.date)), site = unique(bucket.dat$SIte), assessments=20))
bucket.dat.Pp  <- as.data.frame.table(tapply(bucket.dat$Pp,  list(bucket.dat$End.date, bucket.dat$SIte),  sum))
names(bucket.dat.Pp)  <- c("date", "site", "pp.n")
bucket.dat.sum  <-  merge(bucket.dat.sum, bucket.dat.Pp, by=c("date", "site"), all.x=TRUE, all.y=TRUE)
bucket.dat.Pk  <- as.data.frame.table(tapply(bucket.dat$Pk,  list(bucket.dat$End.date, bucket.dat$SIte),  sum))
names(bucket.dat.Pk)  <- c("date", "site", "pk.n")
bucket.dat.sum  <-  merge(bucket.dat.sum, bucket.dat.Pk, by=c("date", "site"), all.x=TRUE, all.y=TRUE)
bucket.dat.sum$pp  <-  bucket.dat.sum$pp.n / bucket.dat.sum$assessments ; bucket.dat.sum$pk  <-  bucket.dat.sum$pk.n / bucket.dat.sum$assessments
bucket.dat.sum$fortnight  <- as.integer(abs(difftime("2017-11-20", bucket.dat.sum$date))/14) ##fortnights from start of study
#bucket.dat.sum[is.na(bucket.dat.sum$pp)==FALSE & bucket.dat.sum$pp==0, "pp"]  <-  jitter(bucket.dat.sum[is.na(bucket.dat.sum$pp)==FALSE & bucket.dat.sum$pp==0, "pp"])  


svg("Fig4aBucketAssessmentsPP.svg")

par(mar = c(7, 4, 4, 2) + 0.1, mfrow = c(2,1))  ## Increase bottom margin to make room for rotated labels
plot(rep(1, 26) ~ c(1:26),  type="n", bty="l", ylab="Proportion of buckets infected", xaxt="n", xlab="", ylim=c(0, 0.225), xlim=c(0, 26))
axis(1, at=seq(0,26, 2), labels = FALSE)
text(seq(0,26, 2), rep(-0.0385,3), srt = 45, adj = 0.9,  labels = c(month.abb[11:12], month.abb[1:12]), xpd = TRUE, cex=0.75) ## 
text(15, -0.09, srt = 0, adj = 1,  labels = "2018", xpd = TRUE) ## Plot x axis labels at default tick marks
##axis(1, at=seq(0,26, 2), labels = FALSE)
##text(c(0, 13, 26), par("usr")[3] - 0.01 , srt = 45, adj = 1,  labels = date.labels, xpd = TRUE) ## Plot x axis labels at default tick marks
for (i in unique(site.cols)) {
    points(jitter(bucket.dat.sum$pp, amount=0.0025) ~ jitter(bucket.dat.sum$fortnight, amount = 1), pch=19, cex=1.5, col = as.character(site.cols[site.cols$Site==i, "site.cols"]))
    }
mtext("a)", cex=1, side=2, line=2, adj=1, padj=-9, las=2)

##dev.off()


##svg("BucketAssessmentsPK.svg")
##par(mar = c(7, 4, 4, 2) + 0.1)  ## Increase bottom margin to make room for rotated labels

plot(rep(1, 26) ~ c(1:26),  type="n", bty="l", ylab="Proportion of buckets infected", xaxt="n", xlab="", ylim=c(0, 0.2), xlim=c(0, 26))
axis(1, at=seq(0,26, 2), labels = FALSE)
text(seq(0,26, 2), rep(-0.0385,3), srt = 45, adj = 0.9,  labels = c(month.abb[11:12], month.abb[1:12]), xpd = TRUE, cex=0.75) ## 
text(15, -0.09, srt = 0, adj = 1,  labels = "2018", xpd = TRUE) ## Plot x axis labels at default tick marks
for (i in unique(site.cols)) {
    points(jitter(bucket.dat.sum$pk, amount=0.0025) ~ jitter(bucket.dat.sum$fortnight, amount = 1), pch=19, cex=1.5, col = as.character(site.cols[site.cols$Site==i, "site.cols"]))
    }
mtext("b)", cex=1, side=2, line=2, adj=1, padj=-9, las=2)

dev.off()


        ##############################################################################################################################
             ############################################      Figure 5.   ########################################## 

###Figure 5 not required. dect.dat  <- read.csv("NovData/Fig5data.csv") ##

## Figure 5. Comparison of sensitivity of detection by automated high-throughput qPCR and isolation onto Phytophthora-selective media for a subset of 64 samples from the first phase of the trial.

Trial period. For analysis there weren't two distinctive trial periods. I analysed all data in one model with time as a continous variable. So it might be missleading to have such a strong emphasis on two phases.  



## What conditions?

## Studies with some disease pathogens have demonstrated that under certain conditions foliage may be not or only slightly infected even if propagules are present

## However, more research data are required to endorse or modify this supposition. In particular, does infection occur over the full period that inoculum is available?


##     Methods second para
##     Sites or plots?

##     Solar radiation was included in initial analyses, along with wind, humidity and rainfall. But in the end we settled on temp and evapo transpiration. Evap o wsa provided by NIWA directly. For weather station data we used solar radiation along with other parameters to calculate evapo transpiration.

## Stuart is correct when he explains why we only used a subset of data. After a lot of work we lost confidence in the earlier data set, and used NIWA data instead. It was a good solution. Also remember we used the hourly weather station data, not the shorter periods available.






## Shouldn’t we have a formula or something demonstrating the chosen model(s)?    
## I don’t understand the difference between this (28% explanation) and the previous model (36% explanation), but presumably it makes sense.



## I think greater detail on the axes would be good in all figures – maybe monthly dates?

##     I’m calling the three graphs a, b, c (and likewise in the other Figures. Need to incorporate these into the graphs.

##                                           True? Evapotranspiration is calculated/estimated from met data??

## Sean, could you please make a graph similar to that in Fig. 1 which includes pathogen ID – I’ll supply the data.


## NIWA climate data were summarized for each fortnightly exchange period and a linear model was used to predict and plot weather station evapotranspiration and air temperature data during the course of the trial.




## Stuart, I’ve written up this section from Sean’s report. However, I feel there’s more work to do for which more information is needed. There may be appropriate information in Sean’s GitHub link, but I can’t open it. Maybe Sean should review what’s written here?

##  I’m not a biometrician/statistician/modeler but intuitively this first sentence seems odd (but I could be wrong). Is just evapotranspiration “predicted” (or estimated, calculated)? Temperature (and also leaf wetness in the Figures – Fig. 2) were actual met station data? Where does the NIWA data come in here? These data were plotted.
##  This is the first time that evapotranspiration is mentioned in the paper. Some explanation is needed.
## How was all this done? I don’t think it’s adequate just to state (as Sean does): “R analysis code and data is available from the authors and on a github repository”. I’ve put this statement in newly added Supplementary Data (appended to this document). However, it’s not really enough for a reader, in my view (and I could not make his link open). I’ve added “…and plot..” to Sean’s sentence here.
## The first two paragraphs here need reassessing and ?perhaps combining appropriately?
##  In the Methods we record that each seedling was assessed fortnightly for 12 weeks before being discarded (in case symptoms took time to develop after being returned from the field). We need to describe how the data from each of the six assessments were put together. Was it just presence or absence in at least one assessment during the 12 weeks of evaluation before plants were discarded? That’s how I’ve written it.
## Later: we also distinguished younger and older needles – need to consider/say something about this?


## Was the symptom data binary or converted into a proportion? Did you transform the proportion data before analysis?



## "Analysis methods section
## For each fortnightly seedling exchange period meteorological station data were used to calculate and plot mean leaf wetness, mean air temperature and the mean for Penman's evapotranspiration index (Penman 1948).

## Models were constructed to relate the appearance of symptoms of RNC on foliage of exchange plants during the study period to NIWA and meteorological station weather data. Plants were considered symptomatic if symptoms were recorded at least once during assessments after being returned from the field. Fortnightly lag variables were constructed, so that the proportion of seedlings at each exchange period that developed RNC symptoms at each site could be compared with historical weather (Table S1). Lag variables of time periods T1 to T26 represented weather from 1 to 26 fortnights prior to seedling exchanges. Lag variables of Q1 to Q4 were calculated for 1 to 4 quarter years prior to each exchange period. Because variables for predicting RNC at each exchange period at each site were highly correlated, randomForest analyses were used to identify the most important weather variables (Liaw and Wiener 2002).

## Generalised mixed effects models, generalised least square (GLS) models and ordinary least square (OLS) models were compared for best fit to data. GLS models included a serial correlation matrix to test and allow for the effect of serial autocorrelation. An automated stepwise procedure was used to choose the minimum adequate model, selected using Akaike information criteria (AIC). The most parsimonious model which adequately predicted the relationship between important weather variables and the proportion of RNC infected seedlings was an ordinary least squares regression. Inclusion of variables identifying the season when RNC was measured, the site, or temporal autocorrelation, did not significantly improve the most parsimonious models. Adjusted R2 values were calculated following Nakagawa et al. (2017).



## Comparison of pathogen detection data from qPCR and isolation onto selective media was assessed with a McNemar’s Test of contingency table for P. pluvialis and P. kernoviae separately. A continuity correction was applied due to low numbers of positives.

## Data from bait traps were plotted without analysis.

