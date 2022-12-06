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

# 編號 → line
# 重複 → rep
# Col → col
# Pos → row
Pepper.PhenoData <- Pepper.PhenoData %>% rename(., line = `編號`, rep = `重複`, col = Col, row = Pos)

# 編號 + 重複 → line_id
Pepper.PhenoData <- Pepper.PhenoData %>% mutate(line_id = paste0(line, "_", rep))
Pepper.PhenoData$line_id <- sub("_NA", "", Pepper.PhenoData$line_id)
Pepper.PhenoData <- Pepper.PhenoData %>% relocate(line_id, .after = line)

# rename traits
Pepper.PhenoData <- Pepper.PhenoData %>% relocate(Pepper.PhenoData.names[55:58], .after = timestamp)
Pepper.PhenoData <- Pepper.PhenoData %>% dplyr::select(-(Pepper.PhenoData.names[grep("bin", Pepper.PhenoData.names)]))
parm.col.start <- which( colnames(Pepper.PhenoData) == "Digital.biomass..mm糧.")
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

#-----------------------------------------------#
# reshape data for heatmap #
#-----------------------------------------------#

Bio_ply_tile <- function(data, Parm_for_func){
  PhenoData_T <- data
  PhenoParm <- Parm_for_func
  Data.fil <- PhenoData_T
  Data.fil.ag <- aggregate(data = Data.fil,
                           get(PhenoParm) ~ Col+ Pos + 編號,
                           FUN = mean)
  # 加入走道位點
  emptypos <- expand.grid(c(3,8),seq(1:12),"走道", NaN)
  colnames(emptypos) <- c("Col", "Pos", "編號", "get(PhenoParm)")
  Data.fil.ag <- bind_rows(Data.fil.ag, emptypos)


  return(Data.fil.ag)
}


####---------------------------------------------------------------------------------------####
## Step.1 ##
intervalmean <- function(DAP_series, smooth.parm){
  require(dplyr)
  a <- DAP_series %>% unique
  b <- smooth.parm
  mod <- length(a) %/% b
  remain <- length(a) %% b
  group <- c(rep(1:mod, each=b),rep(mod+1, remain))

  group_a <- data.frame(a, group)
  group_summarise <- group_a %>% group_by(group)%>% summarise(groupmean = mean(a))
  group_join <- left_join(group_a, group_summarise, by = "group")
  group_join2 <- left_join(data.frame(a= DAP_series), group_join, by = "a") %>% dplyr::select(-group) %>% rename(DAP=a, DAP_group=groupmean)
  return(group_join2)
}

date.smooth <- function(data, pheno.parm, smooth.parm, key.row, key.id, DAP = DAP){
  data_filted <- data %>% dplyr::select(names(data)[key.row],pheno.parm)
  data_filted$DAP_group <- intervalmean(data_filted$DAP, smooth.parm)$DAP_group

  data_filted$xDAP_group <- data_filted$DAP_group - data_filted$DAP_group %>% median()

  data_filted<- data_filted %>% relocate(DAP_group,xDAP_group, .after = xDAP)
  data_filted <- data_filted %>% group_by({{key.id}}, DAP_group) %>% mutate({{pheno.parm}}:= mean({{pheno.parm}} %>% get()))
  return(data_filted)
}

####---------------------------------------------------------------------------------------####
## Step.2 ##
model.smoothed <- function(data, pheno.parm, smooth.method, smooth.df, key.id, rm.col, DAP_group){
  longit.dat <- data %>% dplyr::select(-rm.col) %>% unique %>%
    splitContGRdiff(., responses = pheno.parm,
                    INDICES=key.id, which.rates = c("AGR", "RGR"),
                    times.factor=DAP_group %>% get())
  SET.dat <- longit.dat
  SET.dat <- splitSplines(SET.dat, response = pheno.parm, x =DAP_group %>% get(),
                          INDICES = key.id,
                          smoothing.method = smooth.method, df = smooth.df)
  SET.dat <- splitSplines(SET.dat, response = paste0(pheno.parm,".AGR"), x =DAP_group %>% get(),
                          INDICES = key.id,
                          smoothing.method = smooth.method, df = smooth.df)
  SET.dat <- splitSplines(SET.dat, response = paste0(pheno.parm,".RGR"), x =DAP_group %>% get(),
                          INDICES = key.id,
                          smoothing.method = smooth.method, df = smooth.df)
  return(SET.dat)
}

####---------------------------------------------------------------------------------------####
## Step.3 ##
ply_model <- function(data, pheno.parm, type, DAP_group, key.id, title){
  plot_ly(
    data = data,
    x = ~DAP_group,
    y = ~pheno.parm %>% paste0(.,type)%>%  get(),
    split = ~key.id %>%  get(),
    type = 'scatter',
    mode = 'lines',
    connectgaps = TRUE
  )%>%
    layout(title = title,
           xaxis = list(title = "DAP"),
           yaxis = list(title = paste0(pheno.parm,type)))
}

####---------------------------------------------------------------------------------------####
## Step.4 ##
median.dev.plot <- function(data, pheno.parm, trait.types, smooth.df, DAP_group, key.id){
  #' logist.sub <- na.omit(data)
  #' formula_grp <- paste0(pheno.parm," ~ ", DAP_group, " | ",  key.id) %>% as.formula
  #' logist.grp <- groupedData(formula=formula_grp ,
  #'                           data = logist.sub)
  #'
  #' #'### Fit the logistics and obtain the fitted values
  #' logist.lis <- nlsList(SSlogis, logist.grp)
  #' logist.sub$Leaf.area.smooth <- fitted(logist.lis)
  #'
  #' #'### Calculate the growth rates from the logistic fits
  #' logist.sub <- splitContGRdiff(logist.sub, responses = paste0(pheno.parm, ".smooth"),
  #'                               INDICES = key.id, which.rates = c("AGR", "RGR"),
  #'                               times.factor = DAP_group)

  responses.smooth <- c(paste0(pheno.parm, ".smooth"),
                        paste0(pheno.parm, ".smooth.AGR"),
                        paste0(pheno.parm, ".smooth.RGR"))

  # responses.logis <- c(paste0(pheno.parm, ".smooth.Logistic"),
  #                      paste0(pheno.parm, ".smooth.AGR.Logistic"),
  #                      paste0(pheno.parm, ".smooth.RGR.Logistic"))

  # names(logist.sub)[match(responses.smooth, names(logist.sub))] <- responses.logis

  #'## Probe the smoothing methods and DF
  smooth.dat <- probeSmoothing(data = data, response = pheno.parm,
                               xname =  DAP_group , times.factor= DAP_group,
                               individuals = key.id,
                               na.x.action = "exclude",
                               na.y.action = "exclude",
                               facet.x = ".", facet.y = ".",
                               trait.types = c("response","AGR", "RGR"),
                               smoothing.methods = c("dir","log"),
                               df = smooth.df, x = DAP_group, get.rates = TRUE,
                               which.plots = "none", deviations.plots = "none"
  )


  SET.dat <- merge(data, smooth.dat)
  # SET.dat <- merge(SET.dat, logist.sub, all.x = TRUE)
  SET.dat <- SET.dat %>% unique()
  #'## Plot the median deviations plots
  #+ "meddevn"
  plotMedianDeviations(data = SET.dat,
                       individuals = key.id,
                       response = pheno.parm,
                       response.smoothed = pheno.parm %>% paste0(.,".smooth"),
                       # extra.smooths = "Logistic",
                       x = DAP_group,
                       xname = DAP_group,
                       # x.title = x.title,
                       smoothing.methods = c("dir","log"),
                       df = smooth.df ,
                       facet.x = ".",
                       facet.y = ".",
                       trait.types = trait.types,
                       propn.types = c(0.2)) %>% return()
}
