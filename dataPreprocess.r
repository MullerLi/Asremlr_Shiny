library(TTR)
library(timeSeries)
library(imputeTS)
library(Hmisc)
library(ggplot2)
library(vars)
library(forecast)
library(magrittr)
library(plotly)
library(reshape2)
library(growthPheno)
# library(nlme)
library(dplyr)

#-----------------------------------------------#
# Step1. define variates #
#-----------------------------------------------#

Pepper.PhenoData.raw <- read.csv("D:/data/space_pepper/Pepper.PhenoData.csv",header = T)
Pepper.PhenoData <- Pepper.PhenoData.raw
Pepper.PhenoData.names <- Pepper.PhenoData %>% names

# ?s?? ?? line
# ???? ?? rep
# Col ?? col
# Pos ?? row
Pepper.PhenoData <- Pepper.PhenoData %>% rename(., line = `?s??`, rep = `????`, col = Col, row = Pos)

# ?s?? + ???? ?? line_id
Pepper.PhenoData <- Pepper.PhenoData %>% mutate(line_id = paste0(line, "_", rep))
Pepper.PhenoData$line_id <- sub("_NA", "", Pepper.PhenoData$line_id)
Pepper.PhenoData <- Pepper.PhenoData %>% relocate(line_id, .after = line)

# rename traits
Pepper.PhenoData <- Pepper.PhenoData %>% relocate(Pepper.PhenoData.names[55:58], .after = timestamp)
Pepper.PhenoData <- Pepper.PhenoData %>% dplyr::select(-(Pepper.PhenoData.names[grep("bin", Pepper.PhenoData.names)]))
parm.col.start <- which( colnames(Pepper.PhenoData) == "Digital.biomass..mm³.")
parm.col.end <- which( colnames(Pepper.PhenoData) == "PSRI.average...")
PhenoParameter   <-  c("Digital.biomass",
                       "greenness" ,
                       "Height_mm",
                       "Height.Max_mm",
                       "hue.average",
                       "Leaf.angle",
                       "Leaf.area",
                       "Leaf.area.index",
                       "Leaf.area.projected",
                       "Leaf.inclination",
                       "Light.penetration.depth",
                       "NDVI",
                       "NPCI",
                       "PSRI")
colnames(Pepper.PhenoData)[parm.col.start:parm.col.end] <- PhenoParameter
Pepper.PhenoData$timestamp <- as.POSIXct(Pepper.PhenoData$timestamp, format="%Y-%m-%d %H:%M")
Pepper.PhenoData$Date <- Pepper.PhenoData$timestamp %>% as.Date()

# ---------------------------------------------------------- #
# Step2. impute by sma
# ---------------------------------------------------------- #

which( colnames(Pepper.PhenoData) == "Digital.biomass")
which( colnames(Pepper.PhenoData) == "PSRI")

plyDataTran <- function(data, timestamp, line_id, ParmRange){
  # data <- data
  # timestamp <- timestamp
  # line_id <- line_id
  # batch <- Batch
  # pramRange <- pramRange
  smooth_parm <- 1
  PhenoParms <- colnames(data[,ParmRange])

  ts_impute_process <- function(data, timestamp, line_id, PhenoParm){
            Bio <- data %>% dplyr::select(line_id, timestamp, PhenoParm)
            Bio$Date <- Bio$timestamp %>% as.Date()
            BioCast <- dcast(Date ~ line_id ,data = Bio, value.var = PhenoParm, mean)

            allDates <- seq.Date(
              min(BioCast$Date %>% as.Date),
              max(BioCast$Date %>% as.Date),
              "day")

            allValues <- merge(
              x=data.frame(Date=allDates),
              y=BioCast,
              all.x=TRUE)
            #------------------------------------------------#
            multi_tsclean <- function(data){
              data_new <- data
              n_col <- ncol(data)
              for(i in 1:n_col){
                data_new[,1] <- data[,1] %>% tsclean
              }
              return(data_new)
            }
            BioCast_ts <- ts(BioCast[,-1],start = min(BioCast$Date),end = max(BioCast$Date))
            BioCast_ts <- BioCast_ts %>% multi_tsclean
            BioCast_ts <- BioCast_ts %>% na_interpolation()
            #------------------------------------------------#
            SMA_process <- function(BioCast_ts){
              BioCast_ts.nRow <- (BioCast_ts %>% dim)[1]
              BioCast_ts.nCol <- (BioCast_ts %>% dim)[2]

              BioSma <- matrix(rep(0,BioCast_ts.nRow*BioCast_ts.nCol), ncol = BioCast_ts.nCol)
              for (i  in  seq(BioCast_ts.nCol)){
                BioSma[,i] <- SMA(BioCast_ts[,i], n = smooth_parm)
              }
              BioSma <- BioSma%>% as.data.frame()
              colnames(BioSma) <- (BioCast %>% names)[-1]

              BioCast_ts <- ts(BioSma,start = min(allDates),end = max(allDates))
              return(BioCast_ts)
            }
            BioCast_ts <- SMA_process(BioCast_ts)

            Bio_ply <- data.frame(BioCast_ts,allDates) %>% melt(id = "allDates",
                                                                varnames = line_id,
                                                                value.var = PhenoParm)

            colnames(Bio_ply) <- c("Date", line_id, PhenoParm)
            return(Bio_ply )
  }

  for(i in length(ParmRange)){
    data_ts_impute <- ts_impute_process(data, timestamp, line_id, PhenoParms[i])
    data <- left_join(data, data_ts_impute, by = c("Date", line_id))%>%
      rename_at(vars(ends_with(".y")),~ sub(".y", "", .x)) %>%
      select_at(vars(-ends_with(".x")))
  }
  return(data)
}

Pepper.PhenoData <- plyDataTran(Pepper.PhenoData,"timestamp", "line_id" ,16:29)


# ---------------------------------------------------------- #
# Step3. Add DAP xDAP
# ---------------------------------------------------------- #

Add_DAP <- function(data){
  data$DAP <- data$Date - data$Date %>% min
  data$DAP <- data$DAP %>% as.integer()
  data$xDAP <- data$Date %>% as.integer() - data$Date %>% median() %>% as.integer()
  data <- data %>% relocate(c(Date, DAP, xDAP), .after = timestamp)
  return(data)
}

Pepper.PhenoData <- Pepper.PhenoData %>% Add_DAP()

write.csv(Pepper.PhenoData, "Pepper.PhenoData.spaceAug.csv", row.names = F)




#####################################################################################
data <- read.csv("D:/data/SETapp/data/Pepper.PhenoData.spaceAug.csv", header = T)

######################
#?@?@data filter     #
######################
library(dplyr)
data_filter <- function(data, variable, genotype, DAP, timerange){

  newdata <-data %>% dplyr::select(variable, genotype, DAP) %>%
    dplyr::filter(DAP >= (timerange[1]) & DAP <= (timerange[2]))
  colnames(newdata) <- c("variable", "genotype", "DAP")
  newdata <-newdata %>% dplyr::group_by(genotype, DAP) %>%
            summarise_at(vars(variable), list(variable = mean)) %>% ungroup
  return(newdata)
}


data_filter(data, "Digital.biomass", "line_id", "DAP", c(3,30))

######################
#?@model.smoothed    #
######################
library(growthPheno)
model.smoothed <- function(data, smooth.method, smooth.df){
  SET.dat <- data %>%
    splitContGRdiff(., responses = "variable",
                    INDICES="genotype", which.rates = c("AGR", "RGR"),
                    times.factor="DAP")
  SET.dat <- splitSplines(SET.dat, response = "variable", x ="DAP",
                          INDICES = "genotype",
                          smoothing.method = smooth.method, df = smooth.df)
  SET.dat <- splitSplines(SET.dat, response = paste0("variable",".AGR"), x ="DAP",
                          INDICES = "genotype",
                          smoothing.method = smooth.method, df = smooth.df)
  SET.dat <- splitSplines(SET.dat, response = paste0("variable",".RGR"), x ="DAP",
                          INDICES = "genotype",
                          smoothing.method = smooth.method, df = smooth.df)
  return(SET.dat)
}
data_filter(data, "Digital.biomass", "line", "DAP", c(3,30)) %>% model.smoothed(., "dir", 5) %>% ply_model(., "AGR")

######################
#?@smoothed plot    #
######################
library(plotly)
ply_model <- function(data, type){
  plot_ly(
    data = data,
    x = ~DAP,
    y = ~ paste0("variable.",type) %>% get(),
    split = ~genotype,
    type = 'scatter',
    mode = 'lines',
    connectgaps = TRUE)
  # )%>%
  #   layout(title = title,
  #          xaxis = list(title = "DAP"),
  #          yaxis = list(title = paste0(variable,type)))
}
# data4dev <- data_filter(data, "Digital.biomass", "line", "DAP", c(3,30)) %>% model.smoothed(., "direct", 5)
#
# data4dev %>% model.smoothed(., "direct", 5) %>% ply_model(., "AGR")

######################
#?@median.dev plot   #
######################
# data_filter(data, "Digital.biomass", "line", "DAP", c(10,30)) %>% model.smoothed(., "direct", c(5,10)) %>% median.dev.plot(., "RGR", c(5,10))

median.dev.plot <- function(data, trait.types, smooth.df){

  #'## Probe the smoothing methods and DF
  smooth.dat <- probeSmoothing(data = data, response = "variable",
                               xname =  "DAP" , times.factor= "DAP",
                               individuals = "genotype",
                               na.x.action = "exclude",
                               na.y.action = "exclude",
                               facet.x = ".", facet.y = ".",
                               trait.types = c("response","AGR", "RGR"),
                               smoothing.methods = c("dir","log"),
                               df = smooth.df, x = "DAP", get.rates = TRUE,
                               which.plots = "none", deviations.plots = "none")
  SET.dat <- merge(data, smooth.dat)
  # SET.dat <- merge(SET.dat, logist.sub, all.x = TRUE)
  SET.dat <- SET.dat %>% unique()

  #'## Plot the median deviations plots
  #+ "meddevn"
  plotMedianDeviations(data = SET.dat,
                       individuals = "genotype",
                       response = "variable",
                       response.smoothed = "variable.smooth",
                       x = "DAP",
                       xname = "DAP",
                       smoothing.methods = c("dir","log"),
                       df = smooth.df ,
                       facet.x = ".",
                       facet.y = ".",
                       trait.types = trait.types,
                       propn.types = c(0.2)) %>% return()
}
