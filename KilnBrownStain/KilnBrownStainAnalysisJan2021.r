rm(list=ls()) #Clear the workspace of all objects so R has a clean start
setwd("/home/sean/ScionBiometrics2020/KilnBrownStain")
RPackages = "/home/sean/RPackages" ; .libPaths("/home/sean/RPackages")
## RPackages = "/home/sean/R/x86_64-pc-linux-gnu-library/3.6" ; .libPaths("/home/sean/R/x86_64-pc-linux-gnu-library/3.6")
##install.packages(c("sqldf"), RPackages,  dependencies=TRUE, repos = "http://cran.stat.auckland.ac.nz/")
##remove.packages("rlang", RPackages)
##library(devtools); install_github("marchtaylor/sinkr"); ##install_github("https://github.com/marchtaylor/sinkr") 
library(openxlsx); library(kableExtra); library(nlme); library(lme4); library(emmeans); library(xtable); library(MASS) ###Editing outputs: library(sinkr); library(rtf);

s.e. <- function(x) sqrt(var(na.omit(x))/length(na.omit(x)))
l.s.d <-  function(x) (sqrt(var(na.omit(x))/length(na.omit(x))))*(qt(0.95,(length(na.omit(x)))))
ci.95 <- function(x) { (qt(0.975,length(x)-1)) * (s.e.(x))}
##install.packages(c("rtf"), RPackages,  dependencies=TRUE, repos = "http://cran.stat.auckland.ac.nz/") library(MASS);
  ##############################################################################################################################################################
         #######################################################   Analysis undertaken August 2020   #######################################################
#################################################################################################################################################################

##rmarkdown::render("/home/sean/ScionBiometrics2020/KilnBrownStain/KBSresultsOct.rmd", output_file="KilnBrownStainTrialResultsOct2020.html")
##rmarkdown::render("/home/sean/ScionBiometrics2020/KilnBrownStain/KBSresultsOct.rmd", output_file="KilnBrownStainTrialResultsOct2020.docx")

## 18th Sept 2020 email:
## 1/ Confirm that the colour data from the 5 boards measured on the colour meter (directly L*a*b*) is the same as the colour data derived from the photos of 60 boards via RGB and conversion to L*a*b*.  Once this is confirmed, use the full data set (60 boards) for the analysis.  Each board will have 3 secondary kiln drying variants plus kiln dried only.
## 2/  Investigate which variable to do the analysis with.  Choices would be  del L (lightness),  del E (root of the sum of squares formula) or    mG (mean greyscale). If one was better than the other two, use that. If all three equally significant then it probably doesn’t matter much. If L was dominant over a and b then Del L is good.  If L is not dominant, the either of the aggregate terms Del E or mG would work.
## 3/ Analyse effect of DW/secondary drying vs kiln drying only on KBS development. Do analysis for  Roughsawn – Deep planed and  Planed-KBS – Deep planed

##Note: The colour meter measures L*a*b* directly.  From the photos can calculate RGB and hence L*a*b*.  If its established that del E is the same both ways then can use either. Same comment for L*. mG is in a slightly different position as it is just from RGB and thus photo derived.  It is independent of colour meter

source("DataJuly2020/CIEDE.R") ###Load function used to calculate Delta L "CIEDE". Written by Rosie Sargent
source("DataJuly2020/CIEDEi.R") ###When DplyR not available this function uses indexing
###Import data updated in Oct 2020. Supplied via email 2nd October from Bernie Dawson. 
##dat  <- as.data.frame(read.csv("CupMCoct.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", skip=8))
##Updated by Bernie 23rdNov. Included some data previously summarised by Rosie and error corrections
dat  <- as.data.frame(read.csv("DataNov2020/CupMC24Nov2020.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, skip=8))

##dat  <- as.data.frame(read.csv("DataOct2020/Nov cup data with MC corrected cup.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, skip=8))

bigboard.dat <- as.data.frame(read.csv("DataOct2020/Bigboard for stats RS.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, skip=1))
names(bigboard.dat)[names(bigboard.dat)=="bigboard"]  <- "big.board"
bigboard.dat$ring.orientation  <-  NA; bigboard.dat$compression.wood  <- NA; bigboard.dat$sawing  <- NA

###Recode Ring Orientation
bigboard.dat[bigboard.dat$Ring.orientation == "F", "ring.orientation"]  <- 0; bigboard.dat[bigboard.dat$Ring.orientation == "I", "ring.orientation"]  <- 45
bigboard.dat[bigboard.dat$Ring.orientation == "Q", "ring.orientation"]  <- 90
###Recode Compression Wood
bigboard.dat[bigboard.dat$Compression.wood=="N", "compression.wood"]  <- 0; bigboard.dat[bigboard.dat$Compression.wood=="M", "compression.wood"]  <- 10
bigboard.dat[bigboard.dat$Compression.wood=="S", "compression.wood"]  <- 20 

###Recode days since sawing
bigboard.dat[bigboard.dat$Post.sawing == "<8" , "sawing"] <- 8; bigboard.dat[bigboard.dat$Post.sawing == "13-15", "sawing"] <- 15

###Additional predictor variabels at the board replication level
## board.dat  <- as.data.frame(read.csv("DataJuly2020/Bigboard for stats RS.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", skip=1))
## names(board.dat)[1]  <-  "big.board"
colmeter.dat  <- as.data.frame(read.csv("DataJuly2020/Colour calibration for R.csv", header = TRUE, sep = ",",
                                        quote = "\"", dec = ".", fill = TRUE, comment.char = "", skip=1))
###Description of predictor variables in board.dat and col.dat
##desc.dat <- as.data.frame(read.csv("DataJuly2020/Description of DW cup data.csv", header=TRUE, sep=",", quote="\"", dec=".", fill = TRUE, comment.char = "", skip=0))
### When in the cup measurment data referes to if the cup measurement was measured dry or steamed. In board data when refers to all boards were green. When removed.
### "Max.MC" and "MC" from big board data not relevant to dried mesaurements since from green boards
##board.dat  <-  within(board.dat, rm("When"))

############################# Summarise l a and b from meter readings. Five boards only, five measurements each rep.                                        
delta.colmeter  <- merge(colmeter.dat[colmeter.dat$Region=="KBS", c("Board", "Rep", "L.", "a.", "b.")],
                       colmeter.dat[colmeter.dat$Region=="Core", c("Board", "Rep", "L.", "a.", "b.")], by=c("Board", "Rep"))
names(delta.colmeter)  <- c("Board", "Rep", "L", "a", "b", "Li", "ai", "bi") ##col.kbs.core
#col.kbs.core$board.rep  <-  paste(col.kbs.core$Board, col.kbs.core$Rep,sep=".")
delta.colmeter  <- CIEDE.i(delta.colmeter)[, c("Board", "Rep", "DE", "L", "Li")]
delta.colmeter$DeltaL  <- delta.colmeter$L - delta.colmeter$Li
delta.colmeter.DE  <- as.data.frame.table(tapply(delta.colmeter$DE, delta.colmeter$Board, mean))
names(delta.colmeter.DE)  <- c("Board", "DE.meter")
delta.colmeter.DL  <- as.data.frame.table(tapply(delta.colmeter$DeltaL, delta.colmeter$Board, mean))
names(delta.colmeter.DL)  <- c("Board", "DL.meter")
delta.colmeter  <- merge(delta.colmeter.DE, delta.colmeter.DL, by="Board", all.x=TRUE, all.y=TRUE)
dat  <- merge(dat, delta.colmeter, by="Board", all.x=TRUE, all.y=TRUE)
## dat  <- merge(dat, board.dat[, c("big.board", "Basic.Density", "Ring.width", "Slope.of.grain", "Distance.from.pith", "X..sat", "Ring.orientation", "Compression.wood")],
##               by = "big.board", all.x=TRUE, all.y=TRUE)
##names(dat)[6]  <- "DW"
###DW. Dewatering or Kiln dried treatments. Drying temp is 90 or 240 C. 

## Del E = (del L^2 + del a^2 + Del b^2)^.05
## Del L = (L(KBS) – L(deep-planed)

################ Summarise l a and b from photographs. All boards, but only one estimate from photos                                        
photo.dat  <- as.data.frame(read.csv("DataJuly2020/RGB KBS boards.csv", header = TRUE, sep = ",",
                                        quote = "\"", dec = ".", fill = TRUE, comment.char = "", skip=1))
delta.photo  <- merge(photo.dat[photo.dat$Region=="KBS", c("Board", "L.RGB", "a.RGB", "b.RGB")],
                       photo.dat[photo.dat$Region=="Core", c("Board", "L.RGB", "a.RGB", "b.RGB")], by="Board")
names(delta.photo)  <- c("Board", "L", "a", "b", "Li", "ai", "bi") ##col.kbs.core
#col.kbs.core$board.rep  <-  paste(col.kbs.core$Board, col.kbs.core$Rep,sep=".")
delta.dat  <- CIEDE.i(delta.photo)[, c("Board", "DE")]
names(delta.dat)  <- c("Board", "DE.photo")
delta.dat  <- merge(delta.dat, merge(photo.dat[photo.dat$Region=="KBS", c("Board", "L.RGB", "Face", "mGV")],
                       photo.dat[photo.dat$Region=="Core", c("Board", "L.RGB", "mGV")], by="Board"), by="Board")
delta.dat$Delta.L.rgb  <-  delta.dat$L.RGB.x - delta.dat$L.RGB.y
delta.dat$Delta.mGV  <-  delta.dat$mGV.x - delta.dat$mGV.y

dat.rough  <- dat ## Save for the rough=deep planed analysis below
dat  <- merge(dat, delta.dat[,c("Board", "DE.photo", "Face", "Delta.L.rgb", "Delta.mGV")], by="Board", all.x=TRUE, all.y=TRUE)

####Change names after 8th October request by Bernie. And again 25th Nov
names(dat)[19:20]  <-  c("MC.gradient.after.drying", "MC.gradient.after.sec.steam")
names(dat)[24:27]   <-  c("Hot.MC.KD.DRY","Cold.MC.rep.1.KD", "Cold.MC.rep.2.KD", "Cold.MC.rep.3.KD")
names(dat)[c(6,20)]    <- c("Green MC", "MC.after.sec.steam")

############## Compare computer with meter vs photo on del L.

summary(lm(log(Delta.L.rgb) ~ log(DL.meter), data=dat))
summary(lm(Delta.L.rgb ~ DL.meter, data=dat))

svg("PhotoMeterDL.svg")
plot(Delta.L.rgb ~ DL.meter, data= dat, ylab="Delta L photo", xlab="Delta L meter",  bty="l", type="n", cex.axis = 1, cex.lab =1, 
      xlim=c(min(-15), max(3)), ylim=c(min(-15), max(3))) ##xaxt='n',
points(Delta.L.rgb ~ DL.meter, data=dat, pch=19)
abline(lm(Delta.L.rgb ~ DL.meter, data= dat, lwd = 1.5))
lines(c(-15,3), c(-15,3), lty= "dashed")
dev.off()





######### Calculate Moisture Content for DW, Sec Dry (Dry), Sec Dry (Steamed), KD (Steamed).

dat$mc.tot  <-  NA; dat$mc.grad  <-  NA
###Sumarise Dewatered
dat[is.na(dat$ccc)==FALSE, "mc.tot"]  <-  rowMeans(dat[is.na(dat$ccc)==FALSE, c("DW.MC.rep.1", "DW.MC.rep.2", "DW.MC.rep.3")], na.rm = TRUE)
dat[is.na(dat$ccc)==FALSE, "mc.grad"]  <-  dat[is.na(dat$ccc)==FALSE, "MC.gradient.after.DW..I.O."]

##Summarise Secondry Dried
dat[is.na(dat$ccc.1)==FALSE | is.na(dat$hhh)==FALSE | is.na(dat$hcc)==FALSE, "mc.tot"]  <- +
    dat[is.na(dat$ccc.1)==FALSE | is.na(dat$hhh)==FALSE | is.na(dat$hcc)==FALSE, "MC.after.secondary.drying"]  
dat[is.na(dat$ccc.1)==FALSE | is.na(dat$hhh)==FALSE | is.na(dat$hcc)==FALSE, "mc.grad"]  <- +
    dat[is.na(dat$ccc.1)==FALSE | is.na(dat$hhh)==FALSE | is.na(dat$hcc)==FALSE, "MC.gradient.after.drying"]  

##Summarise Secondry Dried and Steamed
dat[is.na(dat$ccc.2)==FALSE | is.na(dat$hhh.1)==FALSE | is.na(dat$hcc.1)==FALSE, "mc.tot"]  <- +
    dat[is.na(dat$ccc.2)==FALSE | is.na(dat$hhh.1)==FALSE | is.na(dat$hcc.1)==FALSE, "MC.after.sec.steam"]  
dat[is.na(dat$ccc.2)==FALSE | is.na(dat$hhh.1)==FALSE | is.na(dat$hcc.1)==FALSE, "mc.grad"]  <- +
    dat[is.na(dat$ccc.2)==FALSE | is.na(dat$hhh.1)==FALSE | is.na(dat$hcc.1)==FALSE, "MC.gradient.after.steaming..I.O..1"]  

##Summarise Kiln Dried and Steamed
dat[is.na(dat$ccc.3)==FALSE | is.na(dat$hhh.2)==FALSE | is.na(dat$hcc.2)==FALSE, "mc.tot"]  <- +
    rowMeans(dat[is.na(dat$ccc.3)==FALSE | is.na(dat$hhh.2)==FALSE | is.na(dat$hcc.2)==FALSE, c("Cold.MC.rep.1.KD", "Cold.MC.rep.2.KD", "Cold.MC.rep.3.KD")], na.rm = TRUE)

dat[is.na(dat$ccc.3)==FALSE | is.na(dat$hhh.2)==FALSE | is.na(dat$hcc.2)==FALSE, "mc.grad"]  <- +
    dat[is.na(dat$ccc.3)==FALSE | is.na(dat$hhh.2)==FALSE | is.na(dat$hcc.2)==FALSE, "MC.gradient.after.steaming..I.O..2"]

dat$mc.tot  <- round(dat$mc.tot, digits=1)

##dat$MC.grad  <- dat$Inner.MC - dat$Outer.MC


##########Rough sawn results
################ Summarise l a and b from photographs. Comparing Rough sawn with deep planed from photos                                        
delta.photo.rough  <- merge(photo.dat[photo.dat$Region=="Roughsawn", c("Board", "L.RGB", "a.RGB", "b.RGB")],
                       photo.dat[photo.dat$Region=="Core", c("Board", "L.RGB", "a.RGB", "b.RGB")], by="Board")
names(delta.photo.rough)  <- c("Board", "L", "a", "b", "Li", "ai", "bi") ##col.kbs.core
#col.kbs.core$board.rep  <-  paste(col.kbs.core$Board, col.kbs.core$Rep,sep=".")
delta.dat.rough  <- CIEDE.i(delta.photo.rough)[, c("Board", "DE")]
names(delta.dat.rough)  <- c("Board", "DE.photo")

delta.dat.rough  <- merge(delta.dat.rough, merge(photo.dat[photo.dat$Region=="Roughsawn", c("Board", "L.RGB", "Face", "mGV")],
                       photo.dat[photo.dat$Region=="Core", c("Board", "L.RGB", "mGV")], by="Board"), by="Board")
delta.dat.rough$Delta.L.rgb  <-  delta.dat.rough$L.RGB.x - delta.dat.rough$L.RGB.y
delta.dat.rough$Delta.mGV  <-  delta.dat.rough$mGV.x - delta.dat.rough$mGV.y

dat.rough  <- merge(dat.rough, delta.dat.rough[,c("Board", "DE.photo", "Face", "Delta.L.rgb", "Delta.mGV")], by="Board", all.x=TRUE, all.y=TRUE)
dat.rough$dry  <-  paste(dat.rough$DW.KD, dat.rough$Secondary.drying.temp, sep="-")
dat.rough[is.na(dat.rough$Secondary.drying.temp)==TRUE, "dry"] <- "DW-50"


svg("RoughDeltaL.svg")
#par(mar = c(7, 4, 4, 2) + 0.1)  ## Increase bottom margin to make room for rotated labels
plot(dat.rough$Delta.L.rgb ~ as.factor(dat.rough$dry))
dev.off()











##dat.rough$MC.grad  <- dat.rough$Inner.MC - dat.rough$Outer.MC

## svg("RoughDeltaE.svg")
## plot(DE.photo.y ~ DE.photo.x, data=dat.rough, ylab="Delta E photo rough sawn", xlab="Delta E photo (Deep planed - KBS)",  bty="l", type="n", cex.axis = 1, cex.lab =1, 
##       xlim=c(min(0), max(15)), ylim=c(min(0), max(15))) ##xaxt='n',
## points(DE.photo.y ~ DE.photo.x, data=dat.rough, pch=19)
## abline(lm(DE.photo.y ~ DE.photo.x, data=dat.rough), lwd = 1.5)
## ##lines(c(-2,3), c(-2,3), lty= "dashed")
## dev.off()
    

save(dat, dat.rough, file="SavedDat.rds") #, dat.rough
write.csv(dat, "OctCompiledData.csv", row.names=FALSE, na = "")
##write.csv(dat.rough, "RoughsawnData.csv", row.names=FALSE)


 ############################### ############################### ############################### ############################### ###############################
                              ###############################    Restructure Data for Cup Analysis    ############################### 
                                ## This stacks Hot and Cold Measurements of Cup in mm

### Could add Basic.Density.1 and  Distance.from.pith.y as covariates. Tried but neither had a significant effect.
cup.index  <- data.frame(cup.measure = as.character(c("ccc", "ccc.1", "ccc.2", "ccc.3", "hhh", "hhh.1", "hhh.2", "hcc", "hcc.1", "hcc.2")),
             cup.dry = as.character(c("Dewater", "Sec.Dry", "KD.Dry", "KD.Steamed", "Sec.Dry", "Sec.Steamed", "KD.Dry", "Sec.Dry", "Sec.Steamed", "KD.Dry")))

##i  <- "ccc"

dat.cup  <-  as.data.frame(matrix(NA, nrow = 0, ncol = 10, dimnames = list(NULL, c("Board", "big.board", "Week", "mc.tot", "mc.grad", "Delta.L.rgb",
                                                               "cup", "temp", "Measure", "Dry"))))
for (i in cup.index$cup.measure){ 
    dat.cup  <- rbind(dat.cup, cbind(dat[is.na(dat[, i])==FALSE,  c("Board", "big.board", "Week", "mc.tot", "mc.grad", "Delta.L.rgb")], 
                        data.frame(cup=dat[is.na(dat[ , i])==FALSE,  i], temp = dat[is.na(dat[ , i])==FALSE, "Secondary.drying.temp"],
                                   Measure = toupper(i), Dry = cup.index[cup.index$cup.measure == i, "cup.dry"])))
}
                 
## ###Previous reformatting before above loop written
##   dat.cup  <- rbind(data.frame(cup=dat[is.na(dat$ccc)==FALSE,  "ccc"],   Measure = "CCC", Dry = "Dewater", Temp=20,
##                                mc.grad = dat[is.na(dat$ccc)==FALSE, "MC.gradient.after.DW..I.O."], Delta.L.rgb = dat[is.na(dat$ccc)==FALSE, "Delta.L.rgb"]), 
##    data.frame(cup=dat[is.na(dat$ccc.1)==FALSE, c("ccc.1")], Measure= "CCC.1", Dry = "Sec.Dry",   Temp=dat[is.na(dat$ccc.1)==FALSE, c("Secondary.drying.temp")],
##            mc.grad = dat[is.na(dat$ccc.1)==FALSE, "MC.gradient.after.drying"], Delta.L.rgb = dat[is.na(dat$ccc.1)==FALSE, "Delta.L.rgb"]), 
##    data.frame(cup=dat[is.na(dat$ccc.2)==FALSE, c("ccc.2")], Measure= "CCC.2",   Dry = "KD.Steamed", Temp=dat[is.na(dat$ccc.2)==FALSE, c("Secondary.drying.temp")],
##            mc.grad = dat[is.na(dat$ccc.2)==FALSE, "MC.gradient.after.steaming..I.O..2"], Delta.L.rgb = dat[is.na(dat$ccc.2)==FALSE, "Delta.L.rgb"]), 
##    data.frame(cup=dat[is.na(dat$ccc.3)==FALSE, c("ccc.3")], Measure= "CCC.3",   Dry = "KD.Steamed", Temp=dat[is.na(dat$ccc.3)==FALSE, c("Secondary.drying.temp")],
##            mc.grad = dat[is.na(dat$ccc.3)==FALSE, "MC.gradient.after.steaming..I.O..2"], Delta.L.rgb = dat[is.na(dat$ccc.3)==FALSE, "Delta.L.rgb"]), 
##    data.frame(cup=dat[is.na(dat$hhh)==FALSE,  c("hhh")], Measure = "HHH", Dry = "Sec.Dry", Temp=dat[is.na(dat$hhh)==FALSE, c("Secondary.drying.temp")],
##            mc.grad = dat[is.na(dat$hhh)==FALSE, "MC.gradient.after.drying"], Delta.L.rgb = dat[is.na(dat$hhh)==FALSE, "Delta.L.rgb"]), 
##    data.frame(cup=dat[is.na(dat$hhh.1)==FALSE,  c("hhh.1")], Measure = "HHH.1", Dry = "Sec.Steamed",   Temp=dat[is.na(dat$hhh.1)==FALSE, c("Secondary.drying.temp")],
##            mc.grad = dat[is.na(dat$hhh.1)==FALSE, "MC.gradient.after.steaming..I.O..1"], Delta.L.rgb = dat[is.na(dat$hhh.1)==FALSE, "Delta.L.rgb"]),
##    data.frame(cup=dat[is.na(dat$hhh.2)==FALSE,  c("hhh.2")], Measure = "HHH.2", Dry = "KD.Steamed",   Temp=dat[is.na(dat$hhh.2)==FALSE, c("Secondary.drying.temp")],
##            mc.grad = dat[is.na(dat$hhh.2)==FALSE, "MC.gradient.after.steaming..I.O..2"], Delta.L.rgb = dat[is.na(dat$hhh.2)==FALSE, "Delta.L.rgb"]), 
##    data.frame(cup=dat[is.na(dat$hcc)==FALSE,  c("hcc")], Measure = "HCC", Dry = "Sec.Dry",   Temp=dat[is.na(dat$hcc)==FALSE, c("Secondary.drying.temp")],
##            mc.grad = dat[is.na(dat$hcc)==FALSE, "MC.gradient.after.drying"], Delta.L.rgb = dat[is.na(dat$hcc)==FALSE, "Delta.L.rgb"]), 
##    data.frame(cup=dat[is.na(dat$hcc.1)==FALSE,  c("hcc.1")], Measure = "HCC.1", Dry = "Sec.Steamed",   Temp=dat[is.na(dat$hcc.1)==FALSE, c("Secondary.drying.temp")],
##            mc.grad = dat[is.na(dat$hcc.1)==FALSE, "MC.gradient.after.steaming..I.O..1"], Delta.L.rgb = dat[is.na(dat$hcc.1)==FALSE, "Delta.L.rgb"]), 
##    data.frame(cup=dat[is.na(dat$hcc.2)==FALSE,  c("hcc.2")], Measure = "HCC.2", Dry = "KD.Steamed",   Temp=dat[is.na(dat$hcc.2)==FALSE, c("Secondary.drying.temp")], 
##               mc.grad = dat[is.na(dat$hcc.2)==FALSE, "MC.gradient.after.steaming..I.O..2"], Delta.L.rgb = dat[is.na(dat$hcc.2)==FALSE, "Delta.L.rgb"])) 

dat.cup[dat.cup$Measure== "CCC", "temp"]  <- 50
dat.cup$Measure  <- substr(dat.cup$Measure, 1, 3)  ###Reformat Cup measurement identifier so that CCC, HCC and HHH can be compared
#names(dat.cup)[names(dat.cup)=="MC.gradient.after.DW..I.O."]  <- "MC.Grad"
dat.cup$Delta.L.rgb  <- round(dat.cup$Delta.L.rgb, digits=1)

dat.cup  <- merge(dat.cup, bigboard.dat[, c("big.board", "Ring.width",  "Slope.of.grain",  "Distance.from.pith", "Basic.Density", "sawing", "compression.wood", "ring.orientation")], by="big.board", all.x=TRUE, all.y=TRUE)

## ##################################################################################################################################################################
##          ###################################   Re Analyse Cup Data following conversion to percent %   ######################################################

##For the  AS/NZS standard for cup, cup is % not mm.  % Cup stress = ( cup (mm)/ board width in mm) x 100.
## And Steve’s moisture adjusted cup test do not have KD.dry and KD.steamed. Still only KD.steamed
#####Convert cup in mm to %
board.width.dat  <-  as.data.frame(openxlsx::read.xlsx("/home/sean/ScionBiometrics2020/KilnBrownStain/DataOct2020/BoardWidth.xlsx",
                                                sheet = "Sheet1", startRow = 2)) ##Delete columns BQ onwards prior to import. detectDates = TRUE,
names(board.width.dat)  <- c("big.board", "width")

dat.cup  <- merge(dat.cup, board.width.dat, by = "big.board")

dat.cup$cup.percent  <-  (dat.cup$cup / dat.cup$width) * 100

write.csv(dat.cup, "PercentCupData.csv", row.names=FALSE, na = "")
save(dat.cup, file="SavedOctDat.rds") #, dat.rough
write.csv(cup.index, "OctCupIndex.csv", row.names=FALSE, na = "")


   ## ################## ################## ################## ################## ################## ################## ################## ################ ##
####Bernies December 2020 work requests via emails 

wb.out <- createWorkbook("TabulatedResultsPercentCup")
addWorksheet(wb.out, "Explanation") # Add a sheet to the workbook
addWorksheet(wb.out, "TableA"); addWorksheet(wb.out, "TableB"); addWorksheet(wb.out, "TableD");
explanation  <- as.data.frame(c("Tables in this spreadsheet are intended to pasted into a word document.",
                                    ""))
writeData(wb.out, sheet = "Explanation", x = explanation)  # Write the data to the sheets

### Measure. Table A. CCC, HHH, HCC 
dat.cup$measure  <- as.character(dat.cup$Measure); dat.cup[dat.cup$measure=="CCC", "measure"] <- "1"; dat.cup[dat.cup$measure=="HCC", "measure"] <- "2"
dat.cup[dat.cup$measure=="HHH", "measure"] <- "3"; dat.cup$measure  <-  as.integer(dat.cup$measure)

lm.measure  <-  lm(cup.percent ~ Measure, data = dat.cup)



ls.mean.measure  <-  emmeans(lm.measure, specs = pairwise ~ Measure); 
anova.cup.Dry  <- anova(lm.measure)      

mean.cup.measure  <- data.frame(Mesaure = c("CCC", "HCC", "HHH"), measure = 1:3, mean.cup = tapply(dat.cup$cup.percent, dat.cup$measure, mean),
                            lsd = tapply(dat.cup$cup.percent, dat.cup$measure, l.s.d), ls.mean = (as.data.frame(ls.mean.measure$emmeans))$emmean,
                            lower.lsd = tapply(dat.cup$cup.percent, dat.cup$measure, mean) - tapply(dat.cup$cup.percent, dat.cup$measure, l.s.d),
                            upper.lsd = tapply(dat.cup$cup.percent, dat.cup$measure, mean) + tapply(dat.cup$cup.percent, dat.cup$measure, l.s.d),
                            se = tapply(dat.cup$cup.percent, dat.cup$measure, s.e.), ci.95 = tapply(dat.cup$cup.percent, dat.cup$measure, ci.95),
                            lower.ci = tapply(dat.cup$cup.percent, dat.cup$measure, mean) - tapply(dat.cup$cup.percent, dat.cup$measure, ci.95),
                            upper.ci = tapply(dat.cup$cup.percent, dat.cup$measure, mean) + tapply(dat.cup$cup.percent, dat.cup$measure, ci.95),
                            sd = tapply(dat.cup$cup.percent, dat.cup$measure, sd), n = as.integer(table(dat.cup$measure)),
                            Statistic = c("df = ", "F = ", "P = "), results = c(paste(anova(lm.measure)[1, 1], anova(lm.measure)[2, 1], sep=", "),
                            anova(lm.measure)[1, 4], anova(lm.measure)[1, 5]))

writeData(wb.out, sheet = "TableA", x = mean.cup.measure)

svg("MeasureVsCupPercent.svg")
plot(cup.percent ~ measure, data = dat.cup,  type="n", bty="l", ylab= "Cup (%)", xlab=" ", main= "", xlim=c(min(0.5), max(3.5)), ylim=c(min(0), max(5)), xaxt='n')
axis(1, at = c(1, 2, 3 ), labels = c("CCC", "HCC", "HHH"), tick = TRUE, lwd=2, tcl=-0.5)
points(cup.percent ~ jitter(measure), data = dat.cup, pch=19, cex=0.25, col="grey25")
points(mean.cup ~ measure, data= mean.cup.measure, pch=19, cex=1.05, col="black") 
arrows(x0= mean.cup.measure$measure, y0= mean.cup.measure$lower.ci, x1= mean.cup.measure$measure, y1= mean.cup.measure$upper.ci,
       code=3, angle=90, length=0.1, lty=1, lwd=1.5)
abline(h=c(0.5, 1, 2, 3 ,4), lty=3, lwd = 0.5, col="grey25")
text(rep(3.6, 5), c(0.5, 1, 2, 3 ,4)+ 0.1,  LETTERS[1:5]) 
dev.off()

#### Dry. Table B. Secondary drying. 
dat.cup$dry  <- as.character(dat.cup$Dry); dat.cup[dat.cup$dry=="Dewater", "dry"] <- "1"; dat.cup[dat.cup$dry=="KD.Dry", "dry"] <- "2";
dat.cup[dat.cup$dry=="KD.Steamed", "dry"] <- "3"; dat.cup[dat.cup$dry=="Sec.Dry", "dry"] <- "4"; dat.cup[dat.cup$dry=="Sec.Steamed", "dry"] <- "5";
dat.cup$dry  <-  as.integer(dat.cup$dry)

lm.dry  <-  lm(cup.percent ~ Dry, data = dat.cup)
ls.mean.dry  <-  emmeans(lm.dry, specs = pairwise ~ Dry); 
anova.cup.Dry  <- anova(lm.dry)      

mean.cup.dry  <- data.frame(Dry = c("Dewater", "KD.Dry", "KD.Steamed", "Sec.Dry", "Sec.Steamed"), dry = 1:5,
                                        mean.cup = tapply(dat.cup$cup.percent, dat.cup$Dry, mean),
                            lsd = tapply(dat.cup$cup.percent, dat.cup$dry, l.s.d), ls.mean = (as.data.frame(ls.mean.dry$emmeans))$emmean,
                            lower.lsd = tapply(dat.cup$cup.percent, dat.cup$dry, mean) - tapply(dat.cup$cup.percent, dat.cup$dry, l.s.d),
                            upper.lsd = tapply(dat.cup$cup.percent, dat.cup$dry, mean) + tapply(dat.cup$cup.percent, dat.cup$dry, l.s.d),
                            lower.ci = tapply(dat.cup$cup.percent, dat.cup$dry, mean) - tapply(dat.cup$cup.percent, dat.cup$dry, ci.95),
                            upper.ci = tapply(dat.cup$cup.percent, dat.cup$dry, mean) + tapply(dat.cup$cup.percent, dat.cup$dry, ci.95),
                            se = tapply(dat.cup$cup.percent, dat.cup$dry, s.e.), ci.95 = tapply(dat.cup$cup.percent, dat.cup$dry, ci.95),
                            sd = tapply(dat.cup$cup.percent, dat.cup$dry, sd), n = as.integer(table(dat.cup$dry)),
                            Statistic = c("df = ", "F = ", "P = ", " ", " "), results = c(paste(anova(lm.dry)[1, 1], anova(lm.dry)[2, 1], sep=", "),
                                                                                anova(lm.dry)[1, 4], anova(lm.dry)[1, 5], " ", " "))
writeData(wb.out, sheet = "TableB", x = mean.cup.dry)

svg("DryVsCupPercent.svg")
plot(cup.percent ~ dry, data = dat.cup,  type="n", bty="l", ylab= "Cup (%)", xlab=" ", main= "", xlim=c(min(0.5), max(5.5)), ylim=c(min(0), max(5)), xaxt='n')
axis(1, at = 1:5, labels = c("Dewater", "KD.Dry", "KD.Steamed", "Sec.Dry", "Sec.Steamed"), tick = TRUE, lwd=2, tcl=-0.5)
points(cup.percent ~ jitter(dry), data = dat.cup, pch=19, cex=0.25, col="grey25")
points(mean.cup ~ dry, data= mean.cup.dry, pch=19, cex=1.05, col="black") 
arrows(x0= mean.cup.dry$dry, y0= mean.cup.dry$lower.ci, x1= mean.cup.dry$dry, y1= mean.cup.dry$upper.ci,
       code=3, angle=90, length=0.1, lty=1, lwd=1.5)
abline(h=c(0.5, 1, 2, 3 ,4), lty=3, lwd = 0.5, col="grey25")
text(rep(5.6, 5), c(0.5, 1, 2, 3 ,4)+ 0.1,  LETTERS[1:5]) 
dev.off()


### Table C. Not sure if Bernie really wants hhh.1, hhh.2 etc differentiated
### Interaction of Measure and Dry. Table C. 
lm.MeasureDry  <-  lm(cup.percent ~ Measure * Dry, data = dat.cup); ls.mean.MeasureDry  <-  emmeans(lm.MeasureDry, specs = pairwise ~ Measure + Dry )
anova.MeasureDry  <- as.data.frame(anova(lm.MeasureDry))  ; anova.MeasureDry  <-  cbind(rownames(anova.MeasureDry), anova.MeasureDry)    

emmeans.MeasureDry  <- as.data.frame(ls.mean.MeasureDry$emmeans)
emmeans.MeasureDry  <- emmeans.MeasureDry[is.na(emmeans.MeasureDry$emmean)==FALSE, ]

contrasts.MeasureDry  <- as.data.frame(ls.mean.MeasureDry$contrasts)
contrasts.MeasureDry  <- contrasts.MeasureDry[is.na(contrasts.MeasureDry$estimate)==FALSE, ]

mean.cup.MeasureDry  <-  tapply(dat.cup$cup.percent, list(dat.cup$Measure, dat.cup$Dry), mean)
lsd.cup.MeasureDry  <-  tapply(dat.cup$cup.percent, list(dat.cup$Measure, dat.cup$Dry), l.s.d)
ci.cup.MeasureDry  <-  tapply(dat.cup$cup.percent, list(dat.cup$Measure, dat.cup$Dry), ci.95)
se.cup.MeasureDry  <-  tapply(dat.cup$cup.percent, list(dat.cup$Measure, dat.cup$Dry), s.e.)
sd.cup.MeasureDry  <-  tapply(dat.cup$cup.percent, list(dat.cup$Measure, dat.cup$Dry), sd)
n.cup.MeasureDry  <-  table(dat.cup$Measure, dat.cup$Dry)

MeasureDry.tab  <- data.frame(MeasureDry = c("CCC-Dewater", "CCC-KD.Dry", "CCC-KD.Steamed", "CCC-Sec.Dry",
                               "HCC-KD.Dry", "HCC-Sec.Dry", "HCC-Sec.Steamed", "HHH-KD.Dry", "HHH-Sec.Dry", "HHH-Sec.Steamed"), meas.dry = c(1:10),      
                             mean.cup = c(mean.cup.MeasureDry[1, 1:4], mean.cup.MeasureDry[2, c(2,4,5)], mean.cup.MeasureDry[3, c(2,4,5)]),
                             ci = c(ci.cup.MeasureDry[1, 1:4], ci.cup.MeasureDry[2, c(2,4,5)], ci.cup.MeasureDry[3, c(2,4,5)]),
                             lsd = c(lsd.cup.MeasureDry[1, 1:4], lsd.cup.MeasureDry[2, c(2,4,5)], lsd.cup.MeasureDry[3, c(2,4,5)]),
                             se = c(se.cup.MeasureDry[1, 1:4], se.cup.MeasureDry[2, c(2,4,5)], se.cup.MeasureDry[3, c(2,4,5)]),
                             sd = c(sd.cup.MeasureDry[1, 1:4], sd.cup.MeasureDry[2, c(2,4,5)], sd.cup.MeasureDry[3, c(2,4,5)]),                               
                             n = c(n.cup.MeasureDry[1, 1:4], n.cup.MeasureDry[2, c(2,4,5)], n.cup.MeasureDry[3, c(2,4,5)]))  

MeasureDry.tab$lower.ci  <-  MeasureDry.tab$mean - MeasureDry.tab$ci; MeasureDry.tab$upper.ci  <-  MeasureDry.tab$mean + MeasureDry.tab$ci

addWorksheet(wb.out, "TableC"); addWorksheet(wb.out, "TableCanova"); addWorksheet(wb.out, "TableCemmeans"); addWorksheet(wb.out, "TableCcontrasts")
writeData(wb.out, sheet = "TableC", x = MeasureDry.tab)
writeData(wb.out, sheet = "TableCanova", x = anova.MeasureDry)
writeData(wb.out, sheet = "TableCemmeans", x = emmeans.MeasureDry)
writeData(wb.out, sheet = "TableCcontrasts", x = contrasts.MeasureDry)

dat.cup$MeasureDry  <-  NA
dat.cup[dat.cup$Measure=="CCC" & dat.cup$Dry=="Dewater",  "MeasureDry"]  <- 1
dat.cup[dat.cup$Measure=="CCC" & dat.cup$Dry=="KD.Dry",  "MeasureDry"]  <- 2
dat.cup[dat.cup$Measure=="CCC" & dat.cup$Dry=="KD.Steamed",  "MeasureDry"]  <- 3
dat.cup[dat.cup$Measure=="CCC" & dat.cup$Dry=="Sec.Dry",  "MeasureDry"]  <- 4

dat.cup[dat.cup$Measure=="HCC" & dat.cup$Dry=="KD.Dry",  "MeasureDry"]  <- 5
dat.cup[dat.cup$Measure=="HCC" & dat.cup$Dry=="Sec.Dry",  "MeasureDry"]  <- 6
dat.cup[dat.cup$Measure=="HCC" & dat.cup$Dry=="Sec.Steamed",  "MeasureDry"]  <- 7

dat.cup[dat.cup$Measure=="HHH" & dat.cup$Dry=="KD.Dry",  "MeasureDry"]  <- 8
dat.cup[dat.cup$Measure=="HHH" & dat.cup$Dry=="Sec.Dry",  "MeasureDry"]  <- 9
dat.cup[dat.cup$Measure=="HHH" & dat.cup$Dry=="Sec.Steamed",  "MeasureDry"]  <- 10


svg("MeasureDryVsCupPercent.svg")
par(mar = c(7, 4, 4, 2) + 0.1, mfrow = c(1,1))  ## Increase bottom margin to make room for rotated labels
plot(cup.percent ~ MeasureDry, data = dat.cup,  type="n", bty="l", ylab= "Cup (%)", xlab="", main= "", xaxt='n',
     xlim=c(min(0.5), max(10.5)), ylim=c(min(0), max(5)))
##axis(1, at = c(1:10), labels = MeasureDry.tab$Measure, tick = TRUE, lwd=2, tcl=-0.5)
axis(1, at=c(1:10), labels = FALSE, tcl=-0.25)
text(c(1:10), par("usr")[3] - 0.15 , srt = 45, adj = 1,  labels = MeasureDry.tab$MeasureDry, xpd = TRUE, cex=0.7) ## Plot x axis labels at default tick marks
points(cup.percent ~ jitter(MeasureDry), data = dat.cup, pch=19, cex=0.25, col="grey25")
points(mean.cup ~ meas.dry, data= MeasureDry.tab, pch=19, cex=1.05, col="black") 
arrows(x0= MeasureDry.tab$meas.dry, y0= MeasureDry.tab$lower.ci, x1= MeasureDry.tab$meas.dry, y1= MeasureDry.tab$upper.ci, code=3, angle=90, length=0.1, lty=1, lwd=1.5)
abline(h=c(0.5, 1, 2, 3 ,4), lty=3, lwd = 0.5, col="grey25")
text(rep(10.6, 5), c(0.5, 1, 2, 3 ,4)+ 0.1,  LETTERS[1:5]) 
dev.off()



###Temperature. Table D.

lm.temp  <-  lm(cup.percent ~ as.factor(temp), data = dat.cup); ls.mean.temp  <-  emmeans(lm.temp, specs = pairwise ~ temp); anova.cup.temp  <- anova(lm.temp)      

mean.cup.temp  <- data.frame(temp = c(50, 70, 90, 120), mean.cup = tapply(dat.cup$cup.percent, dat.cup$temp, mean),
                            lsd = tapply(dat.cup$cup.percent, dat.cup$temp, l.s.d), ls.mean = (as.data.frame(ls.mean.temp$emmeans))$emmean,
                            lower.lsd = tapply(dat.cup$cup.percent, dat.cup$temp, mean) - tapply(dat.cup$cup.percent, dat.cup$temp, l.s.d),
                            upper.lsd = tapply(dat.cup$cup.percent, dat.cup$temp, mean) + tapply(dat.cup$cup.percent, dat.cup$temp, l.s.d),
                            lower.ci = tapply(dat.cup$cup.percent, dat.cup$temp, mean) - tapply(dat.cup$cup.percent, dat.cup$temp, ci.95),
                            upper.ci = tapply(dat.cup$cup.percent, dat.cup$temp, mean) + tapply(dat.cup$cup.percent, dat.cup$temp, ci.95),
                            se = tapply(dat.cup$cup.percent, dat.cup$temp, s.e.), ci.95 = tapply(dat.cup$cup.percent, dat.cup$temp, ci.95),
                            sd = tapply(dat.cup$cup.percent, dat.cup$temp, sd), n = as.integer(table(dat.cup$temp)),
                            Statistic = c("df = ", "F = ", "P = ", " "), results = c(paste(anova(lm.temp)[1, 1], anova(lm.temp)[2, 1], sep=", "),
                            anova(lm.temp)[1, 4], anova(lm.temp)[1, 5], " "))

writeData(wb.out, sheet = "TableD", x = mean.cup.temp)

svg("TempVsCupPercent.svg")
par(mar = c(5.1, 4.1, 4.1, 5.1), mfrow = c(1,1))  ## Increase right margin to make room for rotated labels
plot(cup.percent ~ temp, data = dat.cup,  type="n", bty="l", ylab= "Cup (%)", xlab=" Temperature (C)", main= "",
     xlim=c(min(40), max(135)), ylim=c(min(0), max(5)), xaxt='n')
axis(1, at = sort(unique(dat.cup$temp)), labels = sort(unique(dat.cup$temp)), tick = TRUE, lwd=2, tcl=-0.5)
points(cup.percent ~ jitter(temp), data = dat.cup, pch=19, cex=0.25, col="grey25")
points(mean.cup ~ temp, data= mean.cup.temp, pch=19, cex=1.05, col="black") 
arrows(x0= mean.cup.temp$temp, y0= mean.cup.temp$lower.ci, x1= mean.cup.temp$temp, y1= mean.cup.temp$upper.ci, code=3, angle=90, length=0.1, lty=1, lwd=1.5)
abline(h=c(0.5, 1, 2, 3 ,4), lty=3, lwd = 0.5, col="grey25")
text(rep(130, 5), c(0.5, 1, 2, 3 ,4)+ 0.1,  LETTERS[1:5]) 
dev.off()

### Interaction of Temp and Dry. Table E. 
lm.TempDry  <-  lm(cup.percent ~ as.factor(temp) * Dry, data = dat.cup); ls.mean.TempDry  <-  emmeans(lm.TempDry, specs = pairwise ~ as.factor(temp) + Dry )
anova.TempDry  <- as.data.frame(anova(lm.TempDry))  ; anova.TempDry  <-  cbind(rownames(anova.TempDry), anova.TempDry)    

emmeans.TempDry  <- as.data.frame(ls.mean.TempDry$emmeans)
emmeans.TempDry  <- emmeans.TempDry[is.na(emmeans.TempDry$emmean)==FALSE, ]

contrasts.TempDry  <- as.data.frame(ls.mean.TempDry$contrasts)
contrasts.TempDry  <- contrasts.TempDry[is.na(contrasts.TempDry$estimate)==FALSE, ]

mean.cup.TempDry  <-  tapply(dat.cup$cup.percent, list(dat.cup$temp, dat.cup$Dry), mean)
lsd.cup.TempDry  <-  tapply(dat.cup$cup.percent, list(dat.cup$temp, dat.cup$Dry), l.s.d)
ci.cup.TempDry  <-  tapply(dat.cup$cup.percent, list(dat.cup$temp, dat.cup$Dry), ci.95)
se.cup.TempDry  <-  tapply(dat.cup$cup.percent, list(dat.cup$temp, dat.cup$Dry), s.e.)
sd.cup.TempDry  <-  tapply(dat.cup$cup.percent, list(dat.cup$temp, dat.cup$Dry), sd)
n.cup.TempDry  <-  table(dat.cup$temp, dat.cup$Dry)


TempDry.tab  <- data.frame(TempDry = c("Dewater-50", "Sec.Dry-70", "Sec.Steamed-70", "KD.Dry-90", "KD.Steamed-90", "Sec.Dry-90", "Sec.Steamed-90",
                                       "Sec.Dry-120", "Sec.Steamed-120"), temp.dry = c(1:9), 
                           mean.cup = c(mean.cup.TempDry[1, 1], mean.cup.TempDry[2, c(4,5)], mean.cup.TempDry[3, c(2,3,4,5)], mean.cup.TempDry[4, c(4,5)]),
                           lsd.cup = c(lsd.cup.TempDry[1, 1], lsd.cup.TempDry[2, c(4,5)], lsd.cup.TempDry[3, c(2,3,4,5)], lsd.cup.TempDry[4, c(4,5)]),
                           sd.cup = c(sd.cup.TempDry[1, 1], sd.cup.TempDry[2, c(4,5)], sd.cup.TempDry[3, c(2,3,4,5)], sd.cup.TempDry[4, c(4,5)]),
                           ci.cup = c(ci.cup.TempDry[1, 1], ci.cup.TempDry[2, c(4,5)], ci.cup.TempDry[3, c(2,3,4,5)], ci.cup.TempDry[4, c(4,5)]),
                           se.cup = c(se.cup.TempDry[1, 1], se.cup.TempDry[2, c(4,5)], se.cup.TempDry[3, c(2,3,4,5)], se.cup.TempDry[4, c(4,5)]),
                           n.cup = c(n.cup.TempDry[1, 1], n.cup.TempDry[2, c(4,5)], n.cup.TempDry[3, c(2,3,4,5)], n.cup.TempDry[4, c(4,5)])) 

TempDry.tab$lower.ci  <-  TempDry.tab$mean - TempDry.tab$ci; TempDry.tab$upper.ci  <-  TempDry.tab$mean + TempDry.tab$ci

addWorksheet(wb.out, "TableE"); addWorksheet(wb.out, "TableEanova"); addWorksheet(wb.out, "TableEemmeans"); addWorksheet(wb.out, "TableEcontrasts")
writeData(wb.out, sheet = "TableE", x = TempDry.tab)
writeData(wb.out, sheet = "TableEanova", x = anova.TempDry)
writeData(wb.out, sheet = "TableEemmeans", x = emmeans.TempDry)
writeData(wb.out, sheet = "TableEcontrasts", x = contrasts.TempDry)
saveWorkbook(wb.out, "TabulatedResultsCupPercent.xlsx", overwrite=TRUE)


dat.cup$TempDry  <-  NA
dat.cup[dat.cup$temp== 50 & dat.cup$Dry=="Dewater",  "TempDry"]  <- 1
dat.cup[dat.cup$temp== 70 & dat.cup$Dry=="Sec.Dry",  "TempDry"]  <- 2
dat.cup[dat.cup$temp== 70 & dat.cup$Dry=="Sec.Steamed",  "TempDry"]  <- 3
dat.cup[dat.cup$temp== 90 & dat.cup$Dry=="KD.Dry",  "TempDry"]  <- 4
dat.cup[dat.cup$temp== 90 & dat.cup$Dry=="KD.Steamed",  "TempDry"]  <- 5
dat.cup[dat.cup$temp== 90 & dat.cup$Dry=="Sec.Dry",  "TempDry"]  <- 6
dat.cup[dat.cup$temp== 90 & dat.cup$Dry=="Sec.Steamed",  "TempDry"]  <- 7
dat.cup[dat.cup$temp== 120 & dat.cup$Dry=="Sec.Dry",  "TempDry"]  <- 8
dat.cup[dat.cup$temp== 120 & dat.cup$Dry=="Sec.Steamed",  "TempDry"]  <- 9



svg("TempDryVsCupPercent.svg")
par(mar = c(7, 4, 4, 2) + 0.1, mfrow = c(1,1))  ## Increase bottom margin to make room for rotated labels
plot(cup.percent ~ TempDry, data = dat.cup,  type="n", bty="l", ylab= "Cup (%)", xlab="", main= "", xaxt='n',
     xlim=c(min(0.5), max(9.5)), ylim=c(min(0), max(5)))
##axis(1, at = c(1:10), labels = TempDry.tab$Measure, tick = TRUE, lwd=2, tcl=-0.5)
axis(1, at=c(1:9), labels = FALSE, tcl=-0.25)
text(c(1:9), par("usr")[3] - 0.15 , srt = 45, adj = 1,  labels = TempDry.tab$TempDry, xpd = TRUE, cex=0.7) ## Plot x axis labels at default tick marks
points(cup.percent ~ jitter(TempDry), data = dat.cup, pch=19, cex=0.25, col="grey25")
points(mean.cup ~ temp.dry, data= TempDry.tab, pch=19, cex=1.05, col="black") 
arrows(x0= TempDry.tab$temp.dry, y0= TempDry.tab$lower.ci, x1= TempDry.tab$temp.dry, y1= TempDry.tab$upper.ci, code=3, angle=90, length=0.1, lty=1, lwd=1.5)
abline(h=c(0.5, 1, 2, 3 ,4), lty=3, lwd = 0.5, col="grey25")
text(rep(10.6, 5), c(0.5, 1, 2, 3 ,4)+ 0.1,  LETTERS[1:5]) 
dev.off()














## ################ Summarise l a and b from photographs. All boards, but only one estimate from photos. This section copied from previous analysis script           
## photo.dat  <- as.data.frame(read.csv("DataJuly2020/RGB KBS boards.csv", header = TRUE, sep = ",",
##                                         quote = "\"", dec = ".", fill = TRUE, comment.char = "", skip=1))
## delta.photo  <- merge(photo.dat[photo.dat$Region=="KBS", c("Board", "L.RGB", "a.RGB", "b.RGB")],
##                        photo.dat[photo.dat$Region=="Core", c("Board", "L.RGB", "a.RGB", "b.RGB")], by="Board")
## names(delta.photo)  <- c("Board", "L", "a", "b", "Li", "ai", "bi") ##col.kbs.core
## #col.kbs.core$board.rep  <-  paste(col.kbs.core$Board, col.kbs.core$Rep,sep=".")
## delta.dat  <- CIEDE(delta.photo)[, c("Board", "DE")]
## names(delta.dat)  <- c("Board", "DE.photo")
## dat.cup  <- merge(dat.cup, delta.dat, by="Board", all.x=TRUE, all.y=TRUE)

## The data in Colour calibration for R .csv is the colour meter data.  This is direct measurement of colour  as L, a and b. There are 255 points for 5 boards namely 10, 21, 48, 49 and 59.  There are only 70/60 for the first two weeks.  This gave good data.  You were going to check if there was a difference between pith-facing or bark facing boards.

## The graphs (attached)  showed that del E or simply the L values show that there is no colour difference for deep planed,.  What they do show is that in the KBS region data, the kiln dried only boards have higher del E values or lower L values proving that dewatering stopped KBS,

## Alongside this data in the s/s is the RGB data.  The mean grey data (mG) is the mean of RGV values and can be thought of as the equivalent of the L values in the colour data.  So this can be compared with the del E and the L data for colour.  The del E RGV could also be compared too.

## In the RGV KBS file there are 604 rows.  For each boards there are no reps just one set of RGV values (no reps becos the data are calculated for a photo of the sample) for the deep-planed zone and one set of the KBS zone and one set for the rough-sawn surface zone. So could analyse this by looking at the mG value  as well as the Del E RGV or even just Lrgv looking at diff within the KBS region ie the effect of dewatering .

##    ######################################################################################################################################################
##         #######################################         Validate the use of photo data for estimating colour  #######################################
## ############################################################################################################################################################
## ## rm(list=ls()) #Clear the workspace of all objects so R has a clean start
## ## setwd("/home/sean/ScionBiometrics2020/KilnBrownStain")
## ## RPackages = "/home/sean/RPackages" ; .libPaths("/home/sean/RPackages")
## ## ####Data manipulation undertaken March to July 2020 is in "dat"
## ## ###load("SavedData.rds") ###Imports data frames: dat, emmeans.DE, step.ccc, manova.table, res.pca, pca.dat
## dat  <- as.data.frame(read.csv("Cup, MC and MC gradient for stats RS.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", skip=1))
## ###Additional predictor variabels at the board replication level
## board.dat  <- as.data.frame(read.csv("DataJuly2020/Bigboard for stats RS.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", skip=1))
## colmeter.dat  <- as.data.frame(read.csv("DataJuly2020/Colour calibration for R.csv", header = TRUE, sep = ",",
##                                         quote = "\"", dec = ".", fill = TRUE, comment.char = "", skip=1))
## ###Description of predictor variables in board.dat and col.dat
## ##desc.dat <- as.data.frame(read.csv("DataJuly2020/Description of DW cup data.csv", header=TRUE, sep=",", quote="\"", dec=".", fill = TRUE, comment.char = "", skip=0))
## ### When in the cup measurment data referes to if the cup measurement was measured dry or steamed. In board data when refers to all boards were green. When removed.
## ### "Max.MC" and "MC" from big board data not relevant to dried mesaurements since from green boards
## ##board.dat  <-  within(board.dat, rm("When"))

## ############################# Summarise l a and b from meter readings. Five boards only, five measurements each rep.                                        
## delta.colmeter  <- merge(colmeter.dat[colmeter.dat$Region=="KBS", c("Board", "Rep", "L.", "a.", "b.")],
##                        colmeter.dat[colmeter.dat$Region=="Core", c("Board", "Rep", "L.", "a.", "b.")], by=c("Board", "Rep"))
## names(delta.colmeter)  <- c("Board", "Rep", "L", "a", "b", "Li", "ai", "bi") ##col.kbs.core
## #col.kbs.core$board.rep  <-  paste(col.kbs.core$Board, col.kbs.core$Rep,sep=".")
## delta.colmeter  <- CIEDE.i(delta.colmeter)[, c("Board", "Rep", "DE")] ###dpyR not working. SO indexing used in CIEDE.i
## delta.colmeter  <- as.data.frame.table(tapply(delta.colmeter$DE, delta.colmeter$Board, mean))
## names(delta.colmeter)  <- c("Board", "DE.meter")
## dat  <- merge(dat, delta.colmeter, by="Board", all.x=TRUE, all.y=TRUE)
## dat  <- merge(dat, board.dat[, c("bigboard", "Basic.Density", "Ring.width", "Slope.of.grain", "Distance.from.pith", "X..sat", "Ring.orientation", "Compression.wood")],
##               by = "bigboard", all.x=TRUE, all.y=TRUE)
## names(dat)[6]  <- "DW"
## ###DW. Dewatering or Kiln dried treatments. Drying temp is 90 or 240 C. 


################ Summarise l a and b from photographs. All boards, but only one estimate from photos                                        
photo.dat  <- as.data.frame(read.csv("DataJuly2020/RGB KBS boards.csv", header = TRUE, sep = ",",
                                        quote = "\"", dec = ".", fill = TRUE, comment.char = "", skip=1))
delta.photo  <- merge(photo.dat[photo.dat$Region=="KBS", c("Board", "L.RGB", "a.RGB", "b.RGB")],
                       photo.dat[photo.dat$Region=="Core", c("Board", "L.RGB", "a.RGB", "b.RGB")], by="Board")
names(delta.photo)  <- c("Board", "L", "a", "b", "Li", "ai", "bi") ##col.kbs.core
#col.kbs.core$board.rep  <-  paste(col.kbs.core$Board, col.kbs.core$Rep,sep=".")
delta.dat  <- CIEDE.i(delta.photo)[, c("Board", "DE")]
names(delta.dat)  <- c("Board", "DE.photo")
delta.dat  <- merge(delta.dat, merge(photo.dat[photo.dat$Region=="KBS", c("Board", "L.RGB", "Face", "mGV")],
                       photo.dat[photo.dat$Region=="Core", c("Board", "L.RGB", "mGV")], by="Board"), by="Board")
delta.dat$Delta.L.rgb  <-  delta.dat$L.RGB.x - delta.dat$L.RGB.y
delta.dat$Delta.mGV  <-  delta.dat$mGV.x - delta.dat$mGV.y

######################################## Compare colour assesment from colour meter and photographs ############################################ 

dat.cols  <- merge(delta.colmeter, delta.dat, by ="Board", all.x=TRUE, all.y=FALSE)

save(dat.cols, file = "ColourComparisons.RDS")

svg("PairsCols.svg")
pairs(dat.cols[, -1])
dev.off()

svg("PhotoMeter.svg")
plot(log(DE.photo) ~ log(DE.meter), data= dat.cols, ylab="Delta L photo (log)", xlab="Delta L meter (log)",  bty="l", type="n", cex.axis = 1, cex.lab =1, 
      xlim=c(min(-2), max(3)), ylim=c(min(-2), max(3))) ##xaxt='n',
points(log(DE.photo) ~ log(DE.meter), data=dat.cols, pch=19)
abline(lm(log(DE.photo) ~ log(DE.meter), data= dat.cols), lwd = 1.5)
lines(c(-2,3), c(-2,3), lty= "dashed")
dev.off()

summary(lm((DE.photo) ~ (DE.meter), data= dat.cols))


## svg(paste("Emmean", i, "svg", sep="."))    
## plot(em.pairs$emmean ~ rownames(em.pairs), ylab=i, xlab="",  bty="l", type="n", cex.axis = 1, cex.lab =1, xaxt='n',
##      xlim=c(min(0), max(length(rownames(em.pairs)))), ylim=c(min(0), max(dat[,i])))
## for (j in 1:length(rownames(em.pairs))) { 
## y.point.pick  <- dat[dat$DryingTreatment == em.pairs[j, "dat.treat"] & dat$Surface == em.pairs[j, "dat.surface"], i]
## selected.colour  <-  as.character(colour.select[colour.select$treatment.col == em.pairs[j,"dat.treat"], "colours.col"])
## points(y.point.pick ~ rep.int(j, times=length(y.point.pick)), pch=19, cex=0.25, col =selected.colour)
## points(em.pairs[j, "emmean"] ~ j, pch=19, cex=1.25, col =selected.colour)
## arrows(j, y0= em.pairs[j, "emmean"], x1 = j, y1 = em.pairs[j, "upper.CL"], length=0.05, angle=90, code=3)   #Upper CI
## arrows(j, y0= em.pairs[j, "emmean"], x1 = j, y1 = em.pairs[j, "lower.CL"], length=0.05, angle=90, code=3)   #Upper CI
## }

## mtext(c("Deep-planed", "Planed-KBS", "Rough-sawn"), side=1, line=1.5, at=c(3,7,11))
## legend(x=1, y=max(dat[,i]), pch=19, cex=1.25, bty="n",
##        legend= as.character(colour.select$treatment.col), col = as.character(colour.select$colours.col))  ##Legend for lines
## dev.off()

## }





    ################################################################################################################################################################
                      ################################### Import data reworked by Steve Riley (allows for MC gradient)   ################################
##dat.reworked  <- as.data.frame(read.csv("/home/sean/ScionBiometrics2020/KilnBrownStain/DataOct2020/CupDataCorrected.csv"))
### Error correction by Bernie 23rd Nov.
dat.reworked  <- as.data.frame(read.csv("/home/sean/ScionBiometrics2020/KilnBrownStain/DataNov2020/CupDataMCcorrected26Nov2020.csv"))

dat.reworked  <- merge(dat.reworked, board.width.dat, by = "big.board")

dat.reworked[dat.reworked$Measure %in% c("CCC.1", "CCC.2", "CCC.2", "CCC.3"), "Measure"]  <- "CCC"
dat.reworked[dat.reworked$Measure %in% c("HHH.1", "HHH.2"), "Measure"]  <- "HHH"
dat.reworked[dat.reworked$Measure %in% c("HCC.2", "HCC.1", "HCC"), "Measure"]  <- "HCC"

dat.reworked$cup.corr  <-  (dat.reworked$cup.corr / dat.reworked$width) * 100; dat.reworked$cup.xtrm  <-  (dat.reworked$cup.xtrm / dat.reworked$width) * 100
dat.90  <- dat.reworked[dat.reworked$temp==90 , ]; dat.dewater  <- dat.reworked[dat.reworked$Dry %in% c("Sec.Dry", "Sec.Steamed"), ]

cup.corr.formula  <- as.formula("cup.corr ~ Dry + Measure  + MC")
cup.xtrm.formula  <- as.formula("cup.xtrm ~ Dry + Measure  + MC")

lm.cup.corr  <- lm(cup.corr.formula , data= na.omit(dat.90[, c("cup.corr", "Dry", "Measure", "MC")])) ##
###step.90  <- step(lm.cup.corr, direction = "both") ##Full model indicated
lm.cup.xtrm  <- lm(cup.xtrm.formula, data= na.omit(dat.90[, c("cup.xtrm", "Dry", "Measure", "MC")])) ##
step.90  <- step(lm.cup.xtrm, direction = "both") ##Full model indicated

raw.mean.cup.Dry  <- as.data.frame(cbind(tapply(dat.90$cup.corr, dat.90$Dry, mean, na.rm=TRUE), tapply(dat.90$cup.corr, dat.90$Dry, s.e.),
                                         tapply(dat.90$cup.xtrm, dat.90$Dry, mean, na.rm=TRUE), tapply(dat.90$cup.xtrm, dat.90$Dry, s.e.)))
raw.mean.cup.Dry  <-  cbind(rownames(raw.mean.cup.Dry), raw.mean.cup.Dry)
names(raw.mean.cup.Dry)  <- c("Treatment", "RawMeanCorr", "S.E. of Corr mean", "RawMeanXtrm", "S.E. of Xtrm mean") 

##ls.mean.cup.corr.Dry  <-  emmeans(lm.cup.corr, specs = pairwise ~ Dry); ls.mean.cup.corr.Dry  <-  emmeans(lm.cup.xtrm, specs = pairwise ~ Dry)
anova.cup.Dry.corr  <- anova(lm.cup.corr) ; anova.cup.Dry.xtrm  <- anova(lm.cup.xtrm)      

# Create a blank workbook to export tables into
wb.out <- createWorkbook("TabulatedResultsDewateringSteveAdjustedMC")
addWorksheet(wb.out, "Explanation") # Add a sheet to the workbook
addWorksheet(wb.out, "Table1"); addWorksheet(wb.out, "Table2"); addWorksheet(wb.out, "Table3")
explanation  <- as.data.frame(c("Tables in this spreadsheet are derived from MC adjusted data. Undertaken by Steve and Bernie Nov 2020.",
                                    "Variables included in models are:",
                                    as.character(cup.corr.formula),
                                "Tables 1, 2 and 3: Raw cup measurement means (%) and ANOVA tables for Kiln Dried and  Dewatered boards at 90 degrees C",
                                "Table 2 is Corr, Table 3 is ANOVA of the xtrm values",
                                "Tables 4, 5 and 6: Raw cup measurement means (%) and ANOVA tables for Dewatered boards with secondary drying"))

# Write the data to the sheets
writeData(wb.out, sheet = "Explanation", x = explanation)
writeData(wb.out, sheet = "Table1", x = raw.mean.cup.Dry)
writeData(wb.out, sheet = "Table2", x = as.data.frame(anova.cup.Dry.corr), rowNames = TRUE)
writeData(wb.out, sheet = "Table3", x = as.data.frame(anova.cup.Dry.xtrm), rowNames = TRUE)

cup.corr.formula  <- as.formula("cup.corr ~ temp + Measure  + MC")
cup.xtrm.formula  <- as.formula("cup.xtrm ~ temp + Measure  + MC")

lm.cup.corr  <- lm(cup.corr.formula , data= na.omit(dat.dewater[, c("cup.corr", "temp", "Measure", "MC")])) ##
##step.90  <- step(lm.cup.corr, direction = "both") ##Full model indicated
lm.cup.xtrm  <- lm(cup.xtrm.formula, data= na.omit(dat.dewater[, c("cup.xtrm", "temp", "Measure", "MC")])) ##
##step.90  <- step(lm.cup.xtrm, direction = "both") ##Full model indicated

raw.mean.cup.temp  <- as.data.frame(cbind(tapply(dat.dewater$cup.corr, dat.dewater$temp, mean, na.rm=TRUE), tapply(dat.dewater$cup.corr, dat.dewater$temp, s.e.),
                                         tapply(dat.dewater$cup.xtrm, dat.dewater$temp, mean, na.rm=TRUE), tapply(dat.dewater$cup.xtrm, dat.dewater$temp, s.e.)))
raw.mean.cup.temp  <-  cbind(rownames(raw.mean.cup.temp), raw.mean.cup.temp)
names(raw.mean.cup.temp)  <- c("Treatment", "RawMeanCorr", "S.E. of Corr mean", "RawMeanXtrm", "S.E. of Xtrm mean") 

##ls.mean.cup.corr.Dry  <-  emmeans(lm.cup.corr, specs = pairwise ~ Dry); ls.mean.cup.corr.Dry  <-  emmeans(lm.cup.xtrm, specs = pairwise ~ Dry)
anova.cup.temp.corr  <- anova(lm.cup.corr) ; anova.cup.temp.xtrm  <- anova(lm.cup.xtrm)      

addWorksheet(wb.out, "Table4"); addWorksheet(wb.out, "Table5"); addWorksheet(wb.out, "Table6")
writeData(wb.out, sheet = "Table4", x = raw.mean.cup.temp)
writeData(wb.out, sheet = "Table5", x = as.data.frame(anova.cup.temp.corr), rowNames = TRUE)
writeData(wb.out, sheet = "Table6", x = as.data.frame(anova.cup.temp.xtrm), rowNames = TRUE)

saveWorkbook(wb.out, "TabulatedResultsAdjustedMCDeWatering.xlsx", overwrite=TRUE)





## lm.cup.full  <- lm(cup ~ Dry + temp + Measure  + mc.tot + mc.grad + Delta.L.rgb + sawing +
##                        Ring.width + Slope.of.grain + Distance.from.pith + Basic.Density + compression.wood + ring.orientation , data= na.omit(dat.cup)) ##
## step(lm.cup.full, direction = "both")

## lm.cup.min  <-  lm(cup ~ Dry +  Measure + mc.grad + Ring.width +  Basic.Density + compression.wood + ring.orientation, data = dat.cup)

## lm.cup.null  <-  lm(cup ~ Dry, data = dat.cup)

## tapply(dat.cup$cup, dat.cup$Dry, mean )
## emmeans(lm.cup.null, specs = pairwise ~ Dry)


## lm.col.null  <-  lm(Delta.L.rgb ~ Dry, data = dat.cup)
## tapply(dat.cup$Delta.L.rgb, dat.cup$Dry, mean, na.rm=TRUE )
## emmeans(lm.col.null, specs = pairwise ~ Dry)




## lm.col.full  <- lm(Delta.L.rgb ~ cup +Dry + Measure  + mc.tot + mc.grad +
##                        Ring.width + Slope.of.grain + Distance.from.pith + Basic.Density + compression.wood + ring.orientation , data= na.omit(dat.cup)) ##
## step(lm.col.full, direction = "both")
## lm.col.min  <- lm(Delta.L.rgb ~  Dry + Ring.width + Basic.Density, data= dat.cup) ##

## tapply(dat.cup$Delta.L.rgb, dat.cup$Dry, mean, na.rm=TRUE)
## emmeans(lm.col.min, specs = pairwise ~ Dry)


## ############### Compare temperature for 

## dat.90  <- dat.cup[dat.cup$temp==90 & dat.cup$Dry != "Dewater", ]
## dat.90$Dry  <- as.factor(as.character(dat.90$Dry))[Drop=TRUE]

## dat.dewater  <- dat.cup[dat.cup$Dry %in% c("Sec.Dry", "Sec.Steamed"), ]
## dat.dewater$Dry  <- as.factor(as.character(dat.dewater$Dry))[Drop=TRUE]

## lm.cup.reduced  <-  lm(cup ~ Dry * temp +  Measure , data = dat.dewater)

## lm.cup.reduced.2  <-  lm(cup ~ Dry , data = dat.dewater)
## plot(cup ~ Dry , data = dat.dewater)
## abline(lm.cup.reduced.2)


## tapply(dat.cup[dat.cup$Dry != "Dewater", "cup" ], dat.cup[dat.cup$Dry != "Dewater", "Dry" ], mean, na.rm=TRUE)
## emmeans(lm.cup.reduced, specs = pairwise ~ Dry * temp)
## emmeans(lm.cup.reduced, specs = pairwise ~ Measure)



## svg("CorrelatedEffects1.svg")
## par(mfrow = c(2,2))
## plot(dat.cup$temp ~ dat.cup$Dry)
## plot(dat.cup$mc.grad ~ dat.cup$Dry)
## plot(dat.cup$Ring.width ~ dat.cup$Dry)
## plot(dat.cup$Basic.Density ~ dat.cup$Dry)
## dev.off()

## svg("CorrelatedEffects2.svg")
## par(mfrow = c(2,1))
## boxplot(dat.cup$compression.wood ~ dat.cup$Dry)
## boxplot(dat.cup$ring.orientation ~ dat.cup$Dry)
## points(dat.cup$ring.orientation ~ dat.cup$Dry)
## dev.off()


## svg("CupDifferencesWood.svg")
## par(mfrow = c(2,2))
## plot(dat.cup$cup ~ dat.cup$compression.wood)
## plot(dat.cup$cup ~ dat.cup$ring.orientation)
## plot(dat.cup$cup ~ dat.cup$Ring.width)
## plot(dat.cup$cup ~ dat.cup$Basic.Density)
## dev.off()

## svg("CupDifferences.svg")
## par(mfrow = c(2,2))
## plot(cup ~ Dry, data= dat.cup)
## plot(cup ~ mc.grad, data= dat.cup)
## ##plot(cup ~ Delta.L.rgb, data= dat.cup)
## plot(cup ~ temp, data= dat.cup)
## plot(cup ~ as.factor(Measure), data= dat.cup)
## dev.off()


## svg("PairsPlot1.svg")    
## pairs(dat[, c("DW.KD", "Max.MC", "Green MC", "Outer.MC.after.DW", "Inner.MC.after.DW", "MC.gradient.after.DW..I.O.")], pch=19, cex=.1)
## dev.off()

## svg("PairsPlot2.svg")
## pairs(dat[, c("DW.KD", "ccc", "hhh", "hhh.1", "ccc.1", "hhh.2", "ccc.2", "ccc.3")], pch=19, cex=.1)
## dev.off()

## svg("PairsPlot3.svg")
## pairs(dat[, c("DW.KD", "DE.meter", "DE.photo", "Delta.L.rgb", "Delta.mGV", "Face", "Ring.width.y", "Slope.of.grain.y")], pch=19, cex=.1)
## dev.off()



###Bernie wants hypothesis tests on cup measurements for: Dewatering vs Dry, KD.Steamed, Sec steamed AND Sec steamed vs KD.Steamed AND Sec.steamed vs Sec.DRy.



## ########################################## ########################################## ##########################################
##         ##########################################   Analysis   ##########################################

## ## 8th of October email 
## ## Dewatering only						
					
## ## 1	Were wood properties consistent over the 5 weeks? Basic Density	Max MC	Green MC	% sat					

## i  <-  "Basic.Density.x"

## dat$Week  <- as.factor(dat$Week)


## lme.1  <-  lme(dat[, i] ~ dat$Week, random=~1|dat$big.board, na.action = na.exclude)

## lm(dat[, i] ~ as.factor(dat$Week))


## svg("WoodPropertyConsistencyPlot.svg")
## par(mfrow = c(2, 2))
## for (i in c("Basic.Density.x", "Max.MC", "Green MC", "X..sat.x")){

##     temp.dat  <- na.omit(dat[, c(i, "Week", "big.board")]);
##     big.board.dat  <- temp.dat[, "big.board"]
##     temp.dat.i  <-  temp.dat[, i]
##     temp.dat.wk  <-  temp.dat[, "Week"]

## ## forumla.lme.1  <- formula(temp.dat.i ~ temp.dat.wk) 
## ## lme.1  <-  lme(forumla.lme.1, random=~1 | big.board.dat)
## forumla.lm.1  <- formula(temp.dat.i ~ temp.dat.wk) 
## lm.1  <-  lm(forumla.lm.1, random=~1 | big.board.dat)
    
##     p.val.i  <- round(summary(aov(temp.dat[, i] ~ temp.dat$Week))[[1]][["Pr(>F)"]][1], digits=3)
##     F.val.i  <- round(summary(aov(dat[, i] ~ as.factor(dat$Week)))[[1]][["F value"]][1], digits=3)
##     p.val.lm    <-  paste("P = ", as.character(round((summary(lm.1))$coefficients[2,4], digits=3)), sep="")
##     plot(dat[, i] ~ as.factor(dat$Week), main = paste(i, "\n F = ", F.val.i, ",  P < ", p.val.i, sep=""), xlab="")
## #    mtext(paste(p.val.lm, collapse="    "), side = 1, padj = 5, cex=0.75)
##     }
## dev.off()


## ## 2	Compare MC  and gradient after 					
## ## 	1) Dewatering 					
## ## 	2) KD 					
## ## 	3) Secondary KD ie DW-KD with drying and steaming options					

## ## 3	Compare cold cup (by temp)					
## ## 	1)	DW (ccc) v Sec dry cup w/o steam (ccc.1) v KD (ccc.2) v KD (ccc.3)				
						
## ## 4	Compare hot cup (by temp)					
## ## 	1)	sec dry no steam (hhh) v sec dry with steam (hhh.1) v KD (hhh.2)				
						
## ## 5	 Compare hcc cup (by temp)					
## ## 	1) 	sec dry no steam (hcc) v sec dry steam (hcc.1) v KD (hcc.2)				
						
## ## 6	Does cuup correlate with other wood props or MC MC gradient					


## "MC.gradient.after.drying"
## "MC.gradient.after.steaming..I.O..1"
## "MC.gradient.after.steaming..I.O..2"


## [







## ##rmarkdown::render("/home/sean/ScionBiometrics2020/KilnBrownStain/ResultsKBSAugust.rmd", output_file="KilnBrownStainTrialResultsAugust2020.html")
## rmarkdown::render("/home/sean/ScionBiometrics2020/KilnBrownStain/ResultsKBSsept.rmd", output_file="KilnBrownStainTrialResultsSept2020.docx")
