library(fenix)
library(rJava)
library(XLConnect)
library(XLConnectJars)
library(RCurl)
source("code/functionsPorcenta.R")

inicio <- "2018-3-1" 
fin    <- "2018-3-31"


downloadDailyFishing(directory = NULL, startDate = inicio, endDate = fin, specie = "jurel", saveFile = TRUE)
downloadDailyFishing(directory = NULL, startDate = inicio, endDate = fin, specie = "caballa", saveFile = TRUE)
downloadDailyFishing(directory = NULL, startDate = inicio, endDate = fin, specie = "anchoveta", saveFile = TRUE)


# install_github("imarpe/imarpe")
library(imarpe)
jur <- imarpe::getFishingData(file = "dailyFishing_jurel_201831_2018331.csv", type = "fisheryinfo", varType = "landing", sp = "jurel", toTons = FALSE)
cab <- imarpe::getFishingData(file = "dailyFishing_caballa_201711_2017121.csv", type = "fisheryinfo", varType = "landing", sp = "caballa", toTons = FALSE)
# anc <- imarpe::getFishingData(file = "dailyFishing_anchoveta_201711_2017110.csv", type = "fisheryinfo", varType = "landing", sp = "anchoveta", toTons = FALSE)

report(jur, daysToPlot = "1")
report(cab, daysToPlot = "1")
# report(anchoveta, daysToPlot = "1")
