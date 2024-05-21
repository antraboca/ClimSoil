### Load packages
library(raster)
library(sp)
library(rgeos)
library(gstat)
library(rgdal)
library(geoR)
library(ggplot2)
library(raster)
library(snow)
library(doSNOW)
library(foreach)
library(doParallel)



r <- raster("C:/Users/aboca/Desktop/OTMED/wise_05min_v12/Grid/smw5by5min/w001001.adf") # soil raster file
#test<-raster("C:/Users/aboca/Desktop/OTMED/WISE5by5min/wise_05min_v12.tif")
#test2<-raster("C:/Users/aboca/Desktop/OTMED/FutureClimate/ip26tn70/ip26tn701.tif")

#### List with files in Gtiff directory
gtif.files <- list.files(path=".", pattern=".tif", all.files=TRUE)

#### This is the function to load rasters so do not change anytying in this function, it should work as it is, I hope (Titia)
load_raster <- function (x) {
    maps <- list()
    for (rast in 1:length(x)) {  
        maps[[rast]] <- raster(x[rast])
    }
    return(maps)
}


#### Correct filename so that there is 01...10, 11, so that it can be correctly read-in 
setwd("C:/Users/aboca/Desktop/OTMED/MIROC/mr85pr70")
gtif.files <- list.files(path=".", pattern=".tif", all.files=TRUE)
fc_prec<- load_raster(gtif.files)
fc_prec.o <- stack(fc_prec)
fc_prec.o

names(fc_prec) <- c( "fc70_prec_01", "fc70_prec_02", "fc70_prec_03" ,
                     "fc70_prec_04", "fc70_prec_05" ,"fc70_prec_06" ,
                     "fc70_prec_07", "fc70_prec_08", "fc70_prec_09",
                     "fc70_prec_10", "fc70_prec_11", "fc70_prec_12")

fc_prec.o <- stack(fc_prec)
fc_prec.o


setwd("C:/Users/aboca/Desktop/OTMED/MIROC/mr85tx70")
#### Correct filename so that there is 01...10, 11, so that it can be correctly read-in 
gtif.files <- list.files(path=".", pattern=".tif", all.files=TRUE)
fc_tmax <- load_raster(gtif.files)
fc_tmax.o <- stack(fc_tmax)
fc_tmax.o

plot(fc_tmax.o)
names(fc_tmax) <- c( "fc70_tmax_01", "fc70_tmax_02", "fc70_tmax_03" ,
                      "fc70_tmax_04", "fc70_tmax_05" ,"fc70_tmax_06" ,
                      "fc70_tmax_07", "fc70_tmax_08", "fc70_tmax_09",
                      "fc70_tmax_10", "fc70_tmax_11", "fc70_tmax_12")

fc_tmax.o <- stack(fc_tmax)
fc_tmax.o

setwd("C:/Users/aboca/Desktop/OTMED/MIROC/mr85tn70")
#### Correct filename so that there is 01...10, 11, so that it can be correctly read-in 
gtif.files <- list.files(path=".", pattern=".tif", all.files=TRUE)
fc_tmin <- load_raster(gtif.files)
fc_tmin.o <- stack(fc_tmin)
fc_tmin.o

names(fc_tmin) <- c( "fc70_tmin_01", "fc70_tmin_02", "fc70_tmin_03" ,
                     "fc70_tmin_04", "fc70_tmin_05" ,"fc70_tmin_06" ,
                     "fc70_tmin_07", "fc70_tmin_08", "fc70_tmin_09",
                     "fc70_tmin_10", "fc70_tmin_11", "fc70_tmin_12")

fc_tmin.o <- stack(fc_tmin)
fc_tmin.o



setwd("C:/Users/aboca/Desktop/OTMED/MIROC/mr85pr70/Output")
detectCores()
cl <- makeCluster(2)   ### Create cluster
registerDoParallel(cl)
getDoParWorkers()

system.time(Prec1 <- foreach(i=1:6, .packages="raster") %dopar% {
  
  ### From january to june, select the  half from the northern hemisphere, 
  # and their equivalent form the southern hemisphere
  
  r1 <- crop(fc_prec[[i]], extent(-180,180,0,90))
  r2 <- crop(fc_prec[[i+6]], extent(-180,180,-90,0))
  m <- merge(r1, r2)
  
  ## Save as Geotiff file file in the output directory
  writeRaster(m, filename = paste0("fc_prec_merged",i,".tif"),
              format = "GTiff", overwrite = T)
  gc()       ### clean garbage
  
})


######################################################################   
####################################################################################

stopCluster(cl)

rm(cl)

###### Now, we want the 

detectCores()
cl <- makeCluster(2)   ### Create cluster
registerDoParallel(cl)
getDoParWorkers()

system.time(Prec2 <- foreach(i=1:6, .packages="raster") %dopar% {
  
  ### From july to december, select the  half from the northern hemisphere,
  ### and their equivalent from the southern hemisphere
  r1 <- crop(fc_prec[[i]], extent(-180,180,-90,0))
  r2 <- crop(fc_prec[[i+6]], extent(-180,180,0,90))
  m <- merge(r1, r2)
  
  ## Save as Geotiff file file in the output directory
  writeRaster(m, filename = paste0("fc_prec_merged",i+6,".tif"),
              format = "GTiff", overwrite = T)
  gc()       ### clean garbage
  
})

##########################################################################   

##################################################################################
stopCluster(cl)

rm(cl)
#### Add 0 before the 1-9 in Output/filenames, so that it can be correctly read-in afterwards

################################################

### Load again
#setwd("C:/Users/aboca/Desktop/OTMED/FutureClimate/ip26pr70/Output")

#### Add 0 before the 1-9 in Output/filenames, so that it can be correctly read-in afterwards

gtif.files <- list.files(path=".", pattern=".tif", all.files=TRUE)

############### Precipitation
fc_prec <- load_raster(gtif.files)
fc_prec.s <- stack(fc_prec)
fc_prec.s

names(fc_prec) <- c( "fc70_prec_01.m", "fc70_prec_02.m", "fc70_prec_03.m" ,
                      "fc70_prec_04.m", "fc70_prec_05.m" ,"fc70_prec_06.m" ,
                      "fc70_prec_07.m", "fc70_prec_08.m", "fc70_prec_09.m",
                      "fc70_prec_10.m", "fc70_prec_11.m", "fc70_prec_12.m")


fc_prec.s <- stack(fc_prec)
fc_prec.s

fc_prec.s2<-crop(fc_prec.s, r)

plot(fc_prec.s)
rm(fc_prec.s)

################################### max temperature
#setwd("C:/Users/aboca/Desktop/OTMED/FutureClimate/ip26tx70/Output")

gtif.files <- list.files(path=".", pattern=".tif", all.files=TRUE)

fc_tmax <- load_raster(gtif.files)
fc_tmax.s <- stack(fc_tmax)
fc_tmax.s


names(fc_tmax) <- c( "fc70_tmax_01.m", "fc70_tmax_02.m", "fc70_tmax_03.m" ,
                     "fc70_tmax_04.m", "fc70_tmax_05.m" ,"fc70_tmax_06.m" ,
                     "fc70_tmax_07.m", "fc70_tmax_08.m", "fc70_tmax_09.m",
                     "fc70_tmax_10.m", "fc70_tmax_11.m", "fc70_tmax_12.m")


fc_tmax.s <- stack(fc_tmax)
fc_tmax.s

fc_tmax.s2<-crop(fc_tmax.s, r)

rm(fc_tmax.s)

################################### min temperature
#setwd("C:/Users/aboca/Desktop/OTMED/FutureClimate/ip26tn70/Output")

gtif.files <- list.files(path=".", pattern=".tif", all.files=TRUE)

fc_tmin <- load_raster(gtif.files)

names(fc_tmin) <- c( "fc70_tmin_01.m", "fc70_tmin_02.m", "fc70_tmin_03.m" ,
                     "fc70_tmin_04.m", "fc70_tmin_05.m" ,"fc70_tmin_06.m" ,
                     "fc70_tmin_07.m", "fc70_tmin_08.m", "fc70_tmin_09.m",
                     "fc70_tmin_10.m", "fc70_tmin_11.m", "fc70_tmin_12.m")


fc_tmin.s <- stack(fc_tmin)
fc_tmin.s

fc_tmin.s2<-crop(fc_tmin.s, r)

rm(fc_tmin.s)

#########################################################################################################################                              Export raster data to a dataframe

fcprec<-as.data.frame(fc_prec.s2, xy=FALSE)
dim(fcprec)
names(fcprec)<-c("prec1", "prec2", "prec3", "prec4", "prec5", "prec6", "prec7", "prec8", "prec9", "prec10", "prec11", "prec12")
names(fcprec)
sapply(fcprec, max, na.rm=T)

fctmin<-as.data.frame(fc_tmin.s2, xy=FALSE)
names(fctmin)<-c("tmin1", "tmin2", "tmin3", "tmin4", "tmin5", "tmin6", "tmin7", "tmin8", "tmin9", "tmin10", "tmin11", "tmin12")

fctmax<-as.data.frame(fc_tmax.s2, xy=FALSE)
names(fctmax)<-c("tmax1", "tmax2", "tmax3", "tmax4", "tmax5", "tmax6", "tmax7", "tmax8", "tmax9", "tmax10", "tmax11", "tmax12")


##########################################################################################################################                                     Read-in bioclimatic variables

setwd("C:/Users/aboca/Desktop/OTMED/MIROC/mr85bi70")

gtif.files <- list.files(path=".", pattern=".tif", all.files=TRUE)

fc_bio <- load_raster(gtif.files)
fc_bio.s <- stack(fc_bio)
fc_bio.s

fc_bio.s2<-crop(fc_bio.s, r)

fcbio<-as.data.frame(fc_bio.s2, xy=TRUE)
names(fcbio)<-c("x","y","Tavg", "MDR", "Isoth", "TempSeason", "Tmax.wm", "Tmin.cm", "TAR", "AvgTempWQ", "AvgTempDQ", "AvgTempWarmQ", "AvgTempColdQ", "AnPrecip", "Prec.wetm", "Prec.drym", "PrecSeason", "PrecWQ", "PrecDQ", "PrecWarmQ","PrecColdQ")

dim(fcbio) #7283520
range(fcbio$Tavg, na.rm=TRUE) #-24.1, +33.8 IPSL; -21.8, +33.8 HadGEM
names(fcbio)


#########################################################################################################################                                                  Earth area

ea<-raster("C:/Users/aboca/Desktop/OTMED/earth_area.tif")
ea<-crop(ea, r)
eadf<-as.data.frame(ea)
dim(eadf)

#########################################################################################################################                        Combine and reduce future climate dataset

fc<-cbind(fcbio, fctmin, fctmax, fcprec, eadf)
dim(fc)
names(fc)

########################################################## Read in adf file

r <- raster("C:/Users/aboca/Desktop/OTMED/wise_05min_v12/Grid/smw5by5min/w001001.adf") 
# soil raster file with SOILMAPUNIT


#################################################### Read in soil proportions

 # soil and prop code is in compositions.r

#########################################################################################################################                                           merge raster and soil data to extract soilmapuni and add to fc, then remove rows with water, rock, and ice and other rows....need to decided whether to remove all rows that I removed for wc or whether to leave them. But if left, then expansion might be due to the fact that more rows are available rather than actual change. 


r2<-as.data.frame(r, xy=TRUE)
dim(r2)

fc2<-cbind(fc, r2)
names(fc2)
colnames(fc2)[62] <- "SOILMAPUNI"
fc2<-fc2[c(1:58, 62)]

#remove rows that have NA in Tavg

fc3<-fc2[complete.cases(fc2$Tavg), ]

write.csv(fc3, "C:/Users/aboca/Desktop/OTMED/MIROC/FC_MIROC_85.csv", row.names=FALSE) # dataset with all available variables and soilmapunit, but needs to be cleaned for actual analysis


########################################################################################################################
rm(ea, eadf, fc, fc_bio, fc_bio.s, fc_bio.s2, fc_prec, fc_prec.o, fc_prec.s, fc_prec.s2, fc_tmax, fc_tmax.o, fc_tmax.s2, fc_tmin, fc_tmin.o, fc_tmin.s2, fc3, fcbio, fcprec, fctmax, fctmin, gtif.files, load_raster, Prec1, Prec2, soil, soil1, Tmax1, Tmax2, Tmin1, Tmin2)
gc()

########################################################################################################################                  READ IN FULL CLIMATE DATASETS FOR ANY FURTHER TRANSFORMATION

setwd("C:/Users/aboca/Desktop/OTMED/MIROC/")
fc2<-read.csv("C:/Users/aboca/Desktop/OTMED/MIROC/FC_MIROC_85.csv")
names(fc2)
head(fc2)

dim(fc2) #2,286,998

total <- merge(fc2,prop,by="SOILMAPUNI")
names(total)
dim(total) # 2,081,893     188


######################################### for subgroups only
myvars <- names(total) %in% c("A","B","C","D","F","G","H","J","K","L","N","O","P","Q","R","S","T","V","W","X","Y","Z","RK","Wr","GL", "WR") 
comps <- total[!myvars]

######################################### for groups
#myvars <- names(total) %in% c("RK","Wr","GL", "WR")
myvars <- names(total) %in% c("Wr", "WR")
comps <- total[!myvars]
###################################################################
dim(comps)
names(comps)


#remove rows with only 0's
clim<-comps[c(1:59)]

#soil<-comps[c(60:163)] # for subgroups
soil<-comps[c(60:187)] #for groups

soil[is.na(soil)] <- 0
comps2<-cbind(clim, soil)
names(comps2)
dim(comps2)

#comps.f<-comps2[apply(comps2[c(60:163)],1,function(z) any(z!=0)),] #removes columns that have only 0's
comps.f<-comps2[apply(comps2[c(60:187)],1,function(z) any(z!=0)),] #removes columns that have only 0's
dim(comps.f)

#comps2$zero<-rowSums(comps2[, -(1:22)])
#length(which(comps2$zero==0))
#df2 <- comps2[rowSums(comps2[, -(1:58)]) > 0, ]
#dim(df2)

#names(soil)
#head(soil)
#soil.f<-soil[apply(soil, 1, function(z) !all(z==0)),] #1,963,168
#soil.f2<-soil[apply(soil, 1, function(z) any(z!=0)),] # 1,963,168 vs 1,958,288 in current climate
#dim(soil.f)
#dim(soil.f2)

dim(comps.f) # 1,963,168 for subgroups; 1,979,754 for groups; 2081893 All      




#########################################################################################################################                                         calculate PET, PEF and other variables

names(comps.f)

#range(comps.f$tmin1) # I think that the average for tmin and tmax is tavg
#range(comps.f$tmax1)
#range(comps.f$Tavg)


# Separate Tmin and Tmax in dataframes of their own, and combine them in a list
Tmin<-comps.f[c(23:34)]
Tmax<-comps.f[c(35:46)]
tempall<-list(Tmin, Tmax)
str(tempall)

Tavglist<-list()

for (i in 1:12){
tempavg<-sapply(tempall,"[",, i) #this actually works, even thoug it has the missing argument sign on the left side. Explanation of code:"[" means "select", and the first comma designates a row (leaving it emtpy is saysing "any"), and the second comma designates columns (1:12); sapply gives a matrix, which can be used to calculate rowmeans
tavg<-apply(tempavg,1, mean) #a better alternative to this is the function rowMeans(tempavg)
Tavglist[[i]]<-tavg
}

sapply(Tavglist, mean) #check the averages of each month and compare with the values from manual selection, just to check whether the approach above was correct
#[1]  -5.039185  12.820656  53.782821 111.309888 166.065852 205.401961 224.126292 214.696568 181.076790 125.928516
#[11]  60.618144  14.079432
# the values need to be divided by 10 for real temp in celsius
#[1]  28.76679  42.00187  80.92908 136.30975 188.16341 222.95698 244.62785 236.92626 200.22917 148.34037  88.38939  48.33390

Tavg<-as.data.frame(Tavglist)
dim(Tavg)
names(Tavg)<-c("tavg1", "tavg2", "tavg3", "tavg4", "tavg5", "tavg6", "tavg7", "tavg8", "tavg9", "tavg10", "tavg11", "tavg12")
head(Tavg)


#####################daylength
library(geosphere)
days<-c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349) #middle-of-month days for each month
dl_months<-data.frame(sapply(days, function(x) daylength(comps.f$y, x))) #calculates the average daylength hours for each month
head(dl_months)
sapply(dl_months, mean)

# separate N and S hempspheres

names(dl_months)<-c("etp1", "etp2", "etp3", "etp4", "etp5","etp6","etp7","etp8","etp9","etp10","etp11","etp12")

dl_months$y<-comps.f$y

north<-dl_months[which(dl_months$y > 0),]
south<-dl_months[which(dl_months$y < 0),]

south2<-south[c(1:12)]
names(south2)<-c("etp7","etp8","etp9","etp10","etp11","etp12", "etp1", "etp2", "etp3", "etp4", "etp5","etp6")

south3<-south2[,c(7,8,9,10,11,12,1,2,3,4,5,6)] #reorder columns in a dataframe
north<-north[c(1:12)]

new_dl<-rbind(north, south2) # the southern hemisphere data are bound to the northern hemisphere data by aligning seasons, meaning January represents January in the northern hemisphere and July in the southern hemisphere. 
sapply(new_dl, mean)

###################################### potential evapotranspiration - PET
###################################### ETP

Tavg<-Tavg/10 # originally in the dataset the values are given as degrees C *10


boxplot(Tavg) #las=2, makes lables appear perpendicular to the x-axis

View(sapply(Tavg, mean))

temp.n0<-Tavg
temp.n0[temp.n0 < 0] <- 0 #replace all negative values with 0 
temp.n0<-round(temp.n0, digits = 3)

sapply(temp.n0, function(x) sum(is.na(x))) #this checks the number of NAs, there are 0 NAs

fun <- function(x) {ifelse(x>0,((x/5)^1.514),0)} # setting the function to get i
i<-sapply(temp.n0,fun) #calculates monthly heat indices


I.n<-rowSums(i, na.rm=FALSE) #The heat index is the sum of the monthly heat indices.
Idf.n<-as.data.frame(I.n)

Idf.n$a<-(6.75 * 10^(-7)) * (Idf.n$I.n)^3 - (7.71 * 10^(-5)) * Idf.n$I.n^2 + 0.01792 * Idf.n$I.n + 0.49239 # calculates the factor by which everything will have to be raised for potential ET in each cell
#a   <- (67.5*10^(-8)*(Ia^3) - 77.1*10^(-6)*(Ia^2) + 0.0179*(Ia) + 0.492)

fun2<-function(y) {16*(10*y/Idf.n$I.n)^Idf.n$a} #function to calculate potential evapotranspiration; If I is 0 then the result can be NA because you have to divide by 0, so in those cases PET is 0

etp.n<-sapply(temp.n0, fun2)
etp.n<-as.data.frame(etp.n)


etp.n2<-data.frame(etp.n*(new_dl/12)) # in the Thornthwaite formula this is represented as L/12
head(etp.n2)
names(etp.n2)<-c("etp1", "etp2", "etp3", "etp4", "etp5","etp6","etp7","etp8","etp9","etp10","etp11","etp12")

sapply(etp.n2, function(x) sum(is.na(x))) #3968 NA's, which need to be converted to 0
etp.n2[is.na(etp.n2)] <- 0



boxplot(etp.n2)
View(sapply(etp.n2, mean))
#boxplot(dum[,(45:56)])
#View(sapply(dum[,(45:56)], mean, na.rm=T))



######################################### effective precipitation - pef
names(comps.f)

prec<-comps.f[c(47:58)]
names(prec)

boxplot(prec)
View(sapply(prec, mean))


Pef<-prec-etp.n2
names(Pef)
names(Pef)<-c("Pef1","Pef2","Pef3","Pef4","Pef5","Pef6","Pef7","Pef8","Pef9","Pef10","Pef11","Pef12")
sapply(Pef, function(x) sum(is.na(x))) #no NA's

boxplot(Pef)
View(sapply(Pef, mean))

# calculate dry months

fprec<-prec - etp.n2
dry.m<-rowSums(fprec<0)
mean(dry.m, na.rm=TRUE) #6.45 for 2.6; 7.139 for 8.5


# cold months

cold.m<-rowSums(Tavg < -3)
range(cold.m)
mean(cold.m) #2.37 for 2.6, 1.93 for 8.5


############################################# 
PefSum <-rowSums(Pef)
petSum <-rowSums(etp.n2)

def<-ifelse(PefSum < 0, PefSum, 0) #there is no true 0 in PefSum because of the number of digits after the period, otherwise I would have had to write "<= 0"
pef_wetm <-ifelse(PefSum>0, PefSum, 0)
mean(pef_wetm) # 124.7269 for 2.6, 93.57 for 8.5

pef_max <- apply(Pef, 1, max)
pef_min <- apply(Pef, 1, min)
pef_range <- pef_max - pef_min
etp_max <- apply(etp.n2, 1, max)
etp_min <- apply(etp.n2, 1, min)
etp_range <- etp_max - etp_min

PefColdQ <- rowSums(Pef[c(1,11,12)])
PefWarmQ <-rowSums(Pef[c(6:8)])




######################################## earth area

cf<-comps.f$earth_area/85863235
range(cf)
cf<-round(cf, digits=2)

#########################################################################################################################                                       Combine final dataset
names(comps.f)
head(comps.f)
dim(comps.f)

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
divten<-comps.f[c(4,5,7:14)] #check that the column number match
head(divten)
divten<-divten/10


#final<-cbind(comps.f[c(1:22)], cold.m, dry.m, petSum, PefSum, def, pef_wetm, pef_max, pef_min, pef_range, etp_max, etp_min, etp_range, PefColdQ, PefWarmQ, cf)

final<-cbind(comps.f[c(1:3)], divten[,(1:2)], comps.f[c(6)], divten[,(3:10)], comps.f[,(15:22)], cold.m, dry.m, petSum, PefSum, def, pef_wetm, pef_max, pef_min, pef_range, etp_max, etp_min, etp_range, PefColdQ, PefWarmQ, cf)

names(final)
dim(final)

sapply(final[c(4:36)], function(x) sum(is.na(x)))
sapply(final[c(4:36)], mean)
#        Tavg           MDR         Isoth    TempSeason       Tmax.wm       Tmin.cm           TAR     AvgTempWQ 
#10.87288338   11.14774270   36.96549390  841.81106512   28.66451052   -6.44923212   35.11369326   17.51551218 

write.csv(final, "C:/Users/aboca/Desktop/OTMED/MIROC/MIROC85_final_AllSoils.csv", row.names=FALSE)

final<-read.csv("C:/Users/aboca/Desktop/OTMED/HadGEM/HadGEM85_final_AllSoils.csv")
sapply(hg85[c(4:36)], mean)

hg26<-read.csv("C:/Users/aboca/Desktop/OTMED/HadGEM/HadGEM26_final_AllSoils.csv")
sapply(hg26[c(4:36)], mean)


soils<- comps.f[c(60:187)]




names(comps.f)
comps.f$Z1<-rowSums(comps.f[c(181:185)]) #I need to sum all the soils that belong to a group


soilgroups<-comps.f[c(187:212)]
names(soilgroups)

names(final)

dim(final)
dim(soilgroups)

###############################################################################################################

#       Sum proportions of gelic and plinthic soils, and create kmat
temp<-subsoils[c(14, 35, 63, 76)] #gelic Bx, Gx, Ox, Rx
head(temp)
range(temp$Gx)
temp$gelic<-rowSums(temp)
range(temp$gelic)

temp2<-subsoils[c(5, 26, 34, 54)] #plinthic Ap,  Fp, Gp,  Lp
names(temp2)
temp2$plint<-rowSums(temp2)
range(temp2$plint)

temp3<-subsoils[c(12, 17,29,36,41,46,52, 73,93, 97)] #calcic Bk, Ck, Gc, Hc, Jc, Kk, Lk, Rc, Xk, Yk
names(temp3)
temp3$calc<-rowSums(temp3)

dfsub$calcic<-temp3$calc


subsoils$gelic<-temp$gelic
subsoils$plint<-temp2$plint
subsoils$calcic<-temp3$calc

###################################################################################################################


write.csv(final, "C:/Users/aboca/Desktop/OTMED/FutureClimate/IPSL85_final_AllSoils.csv", row.names=FALSE)
write.csv(soils, "C:/Users/aboca/Desktop/OTMED/FutureClimate/Soil_All.csv", row.names=FALSE)

write.csv(final, "C:/Users/aboca/Desktop/OTMED/FutureClimate/IPSL85_final_groups.csv", row.names=FALSE)
write.csv(soilgroups, "C:/Users/aboca/Desktop/OTMED/FutureClimate/Soil_Groups.csv", row.names=FALSE)


subsoils<-read.csv("C:/Users/aboca/Desktop/OTMED/FutureClimate/Soil_SubGroups.csv")
dim(subsoils)
soils<-read.csv("C:/Users/aboca/Desktop/OTMED/FutureClimate/Soil_Groups.csv")
dim(soils)
dim(final)


head(dfsub2)
dfsub2<-dfsub[c(1:37)]
names(dfsub2)
fc26<-read.csv("C:/Users/aboca/Desktop/OTMED/FutureClimate/IPSL26_final_subgroups.csv")
dim(fc26)
names(fc26)
head(fc26)

sapply(fc26[c(4:36)], function(x) sum(is.na(x)))

boxplot(dfsub2[,36], fc26[,36])



#fc<-read.csv("C:/Users/aboca/Desktop/OTMED/FutureClimate/IPSL85_final_groups.csv")





#########################################################################################################################################################################################################################################################################################################################################################################
#                                          Current climate
#
#
########################################################################################################################################################################################################################################################################################################################################################################

setwd("C:/Users/aboca/Desktop/OTMED/WorldClim1.4/tmean_5m_bil")
setwd("C:/Users/aboca/Desktop/OTMED/WorldClim1.4/prec_5m_bil")

r <- raster("C:/Users/aboca/Desktop/OTMED/wise_05min_v12/Grid/smw5by5min/w001001.adf") # soil raster file
#test<-raster("C:/Users/aboca/Desktop/OTMED/WISE5by5min/wise_05min_v12.tif")
#test2<-raster("C:/Users/aboca/Desktop/OTMED/FutureClimate/ip26tn70/ip26tn701.tif")

#test<-raster("C:/Users/aboca/Desktop/OTMED/WorldClim1.4/tmean_5m_bil/tmean1.bil")


#### List with files in Gtiff directory
#gtif.files <- list.files(path=".", pattern=".tif", all.files=TRUE)
gtif.files <- list.files(path=".", pattern=".bil", all.files=TRUE) #for worldclim 1.4

#### This is the function to load rasters so do not change anytying in this function, it should work as it is, I hope (Titia)
load_raster <- function (x) {
  maps <- list()
  for (rast in 1:length(x)) {  
    maps[[rast]] <- raster(x[rast])
  }
  return(maps)
}

cc_tavg <- load_raster(gtif.files)
cc_tavg.o <- stack(cc_tavg)
cc_tavg.o

names(cc_tavg) <- c( "cc_tavg_01", "cc_tavg_02", "cc_tavg_03" ,
                     "cc_tavg_04", "cc_tavg_05" ,"cc_tavg_06" ,
                     "cc_tavg_07", "cc_tavg_08", "cc_tavg_09",
                     "cc_tavg_10", "cc_tavg_11", "cc_tavg_12")


#setwd("C:/Users/aboca/Desktop/OTMED/WorldClim2/wc2.0_5m_prec/Output")
#setwd("C:/Users/aboca/Desktop/OTMED/WorldClim1.4/tmean_5m_bil/Output")
setwd("C:/Users/aboca/Desktop/OTMED/WorldClim1.4/prec_5m_bil/Output")

detectCores()
cl <- makeCluster(2)   ### Create cluster
registerDoParallel(cl)
getDoParWorkers()

system.time(Prec1 <- foreach(i=1:6, .packages="raster") %dopar% {
  
  ### From january to june, select the  half from the northern hemisphere, 
  # and their equivalent form the southern hemisphere
  
  r1 <- crop(cc_prec[[i]], extent(-180,180,0,90))
  r2 <- crop(cc_prec[[i+6]], extent(-180,180,-90,0))
  m <- merge(r1, r2)
  
  ## Save as Geotiff file file in the output directory
  writeRaster(m, filename = paste0("cc_prec_merged",i,".tif"),
              format = "GTiff", overwrite = T)
  gc()       ### clean garbage
  
})


######################################################################   
####################################################################################

stopCluster(cl)

rm(cl)

###### Now, we want the 

detectCores()
cl <- makeCluster(2)   ### Create cluster
registerDoParallel(cl)
getDoParWorkers()

system.time(Prec2 <- foreach(i=1:6, .packages="raster") %dopar% {
  
  ### From july to december, select the  half from the northern hemisphere,
  ### and their equivalent from the southern hemisphere
  r1 <- crop(cc_prec[[i]], extent(-180,180,-90,0))
  r2 <- crop(cc_prec[[i+6]], extent(-180,180,0,90))
  m <- merge(r1, r2)
  
  ## Save as Geotiff file file in the output directory
  writeRaster(m, filename = paste0("cc_prec_merged",i+6,".tif"),
              format = "GTiff", overwrite = T)
  gc()       ### clean garbage
  
})

##########################################################################   

##################################################################################


stopCluster(cl)

rm(cl)
#### Add 0 before the 1-9 in Output/filenames, so that it can be correctly read-in afterwards

################################################

### Load again
#setwd("C:/Users/aboca/Desktop/OTMED/WorldClim2/wc2.0_5m_tavg/Output")
#setwd("C:/Users/aboca/Desktop/OTMED/WorldClim2/wc2.0_5m_prec/Output")

setwd("C:/Users/aboca/Desktop/OTMED/WorldClim1.4/tmean_5m_bil/Output")
setwd("C:/Users/aboca/Desktop/OTMED/WorldClim1.4/prec_5m_bil/Output")

gtif.files <- list.files(path=".", pattern=".tif", all.files=TRUE)

############# Tavg
cc_tavg <- load_raster(gtif.files)

names(cc_tavg) <- c( "cc_tavg_01.m", "cc_tavg_02.m", "cc_tavg_03.m" ,
                     "cc_tavg_04.m", "cc_tavg_05.m" ,"cc_tavg_06.m" ,
                     "cc_tavg_07.m", "cc_tavg_08.m", "cc_tavg_09.m",
                     "cc_tavg_10.m", "cc_tavg_11.m", "cc_tavg_12.m")

cc_tavg.s <- stack(cc_tavg)
cc_tavg.s

cc_tavg.s2<-crop(cc_tavg.s, r)
rm(cc_tavg.s)

cctavg<-as.data.frame(cc_tavg.s2, xy=FALSE)
names(cctavg)<-c("tavg1", "tavg2", "tavg3", "tavg4", "tavg5", "tavg6", "tavg7", "tavg8", "tavg9", "tavg10", "tavg11", "tavg12")
head(cctavg)
sapply(cctavg, mean, na.rm=TRUE)

############### Precipitation
setwd("C:/Users/aboca/Desktop/OTMED/WorldClim1.4/prec_5m_bil/Output")

gtif.files <- list.files(path=".", pattern=".tif", all.files=TRUE)
cc_prec <- load_raster(gtif.files)



names(cc_prec) <- c( "cc_prec_01.m", "cc_prec_02.m", "cc_prec_03.m" ,
                     "cc_prec_04.m", "cc_prec_05.m" ,"cc_prec_06.m" ,
                     "cc_prec_07.m", "cc_prec_08.m", "cc_prec_09.m",
                     "cc_prec_10.m", "cc_prec_11.m", "cc_prec_12.m")


cc_prec.s <- stack(cc_prec)
cc_prec.s

cc_prec.s2<-crop(cc_prec.s, r)

rm(cc_prec.s)



ccprec<-as.data.frame(cc_prec.s2, xy=FALSE)
names(ccprec)<-c("prec1", "prec2", "prec3", "prec4", "prec5", "prec6", "prec7", "prec8", "prec9", "prec10", "prec11", "prec12")
head(ccprec)
sapply(ccprec, mean, na.rm=TRUE)

##########################################################################################################################                                     Read-in bioclimatic variables

#setwd("C:/Users/aboca/Desktop/OTMED/WorldClim2/wc2.0_5m_bio")
setwd("C:/Users/aboca/Desktop/OTMED/WorldClim1.4/bio_5m_bil")

#gtif.files <- list.files(path=".", pattern=".tif", all.files=TRUE)
gtif.files <- list.files(path=".", pattern=".bil", all.files=TRUE)

cc_bio <- load_raster(gtif.files)
cc_bio.s <- stack(cc_bio)
cc_bio.s

cc_bio.s2<-crop(cc_bio.s, r)

ccbio<-as.data.frame(cc_bio.s2, xy=TRUE)
names(ccbio)<-c("x","y","Tavg", "MDR", "Isoth", "TempSeason", "Tmax.wm", "Tmin.cm", "TAR", "AvgTempWQ", "AvgTempDQ", "AvgTempWarmQ", "AvgTempColdQ", "AnPrecip", "Prec.wetm", "Prec.drym", "PrecSeason", "PrecWQ", "PrecDQ", "PrecWarmQ","PrecColdQ")

dim(ccbio) #7283520
range(ccbio$Tavg, na.rm=TRUE) #-27.4, +31.6
names(ccbio)


#########################################################################################################################                                                  Earth area

ea<-raster("C:/Users/aboca/Desktop/OTMED/earth_area.tif")
ea<-crop(ea, r)
eadf<-as.data.frame(ea)
dim(eadf)

#########################################################################################################################                        Combine and reduce future climate dataset

cc<-cbind(ccbio, cctavg, ccprec, eadf)
dim(cc)
names(cc)

########################################################## combine with soil raster file

r2<-as.data.frame(r, xy=TRUE)
dim(r2)

cc2<-cbind(cc, r2)
names(cc2)
colnames(cc2)[50] <- "SOILMAPUNI"
cc2<-cc2[c(1:46, 50)]

#remove rows that have NA in Tavg

cc3<-cc2[complete.cases(cc2$Tavg), ]
dim(cc3) #2,289,008; 2,286,998 for worldclim 1.4

write.csv(cc3, "C:/Users/aboca/Desktop/OTMED/WorldClim1.4/CC_WorldClim14.csv", row.names=FALSE) # dataset with all available variables and soilmapunit, but needs to be cleaned for actual analysis


########################################################################################################################
rm(cc, cc_avg.o, cc_bio, cc_bio.s, cc_bio.s2, cc_prec, cc_prec.o, cc_prec.s2, cc_tavg, cc_tavg.o, cc_tavg.s2, cc2, ccbio, ccprec, cctavg, cf, clim, cold.m, comps, comps.f, comps2, def, divten, dry.m, ea, eadf, etp.n, etp_max, etp_min, etp_range, fc_avg.o, fc_bio, fc_bio.s, fc_bio.s2, fc_prec, fc_prec.s, fc_tavg, fc2, fc26, fc26temp, fcbio, final, fprec, fun, fun2, I.n, Idf.n, myvars, Pef, pef_max, pef_min, pef_range, pef_wetm, PefColdQ, PefSum, PefWarmQ, petSum, prec, Prec1, Prec2, prectemp26, prectemp85, r, r2, soil, tavg, Tavg, Tavg1, Tavg2, Tavglist, temp.n0, tempall, tempavg, Tmax, Tmin, total)
  
gc()

#extract the first two letters from soilmapuni as soil descriptors

setwd("C:/Users/aboca/Desktop/OTMED/WorldClim1.4/")
cc2<-read.csv("C:/Users/aboca/Desktop/OTMED/WorldClim1.4/CC_WorldClim14.csv")
names(cc2)


total <- merge(cc2,prop,by="SOILMAPUNI") #prop is from compositions.r
names(total)
dim(total) # 2,081,893     for future climate, 2,080,854 for current climate; 2081893 worldclim 1.4

######################################### for subgroups only
myvars <- names(total) %in% c("A","B","C","D","F","G","H","J","K","L","N","O","P","Q","R","S","T","V","W","X","Y","Z","Wr", "WR") 
#"GL", "RK" - originally I took them out but later realized that I should keep them to train the models
comps <- total[!myvars]

######################################### for groups
myvars <- names(total) %in% c("Wr","WR")
comps <- total[!myvars]
#######################################################

dim(comps)
names(comps)

#remove rows with only 0's
clim<-comps[c(1:47)]

soil<-comps[c(48:153)] # for subgroups
soil<-comps[c(48:175)] #for groups

soil[is.na(soil)] <- 0
comps2<-cbind(clim, soil)
names(comps2)

comps.f<-comps2[apply(comps2[c(48:152)],1,function(z) any(z!=0)),] #removes columns that have only 0's
comps.f<-comps2[apply(comps2[c(48:175)],1,function(z) any(z!=0)),] #removes columns that have only 0's

dim(comps.f) # 1,963,168 for subgroups; 1,979,754 for groups future climate
# 1,962,865 for subgroups current climate; 1,979,447 current climate groups
# 1,963,168 for subgroup worldclim 1.4; 1,979,754 for group worldclim 1.4

#2060459 with GL for subgroups
#2081893 with GL and RK for groups





#########################################################################################################################                                         calculate PET, PEF and other variables

names(comps.f)

###################################### potential evapotranspiration - PET

#####################daylength
library(geosphere)
days<-c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349) #middle-of-month days for each month
dl_months<-data.frame(sapply(days, function(x) daylength(comps.f$y, x))) #calculates the average daylength hours for each month
head(dl_months)
sapply(dl_months, mean)

# separate N and S hempspheres

names(dl_months)<-c("etp1", "etp2", "etp3", "etp4", "etp5","etp6","etp7","etp8","etp9","etp10","etp11","etp12")

dl_months$y<-comps.f$y

dim(comps.f)
north<-dl_months[which(dl_months$y > 0),]
dim(north)
south<-dl_months[which(dl_months$y < 0),]
dim(south)

names(south)

south2<-south[c(1:12)]
names(south2)<-c("etp7","etp8","etp9","etp10","etp11","etp12", "etp1", "etp2", "etp3", "etp4", "etp5","etp6")
names(south2)

south3<-south2[,c(7,8,9,10,11,12,1,2,3,4,5,6)] #reorder columns in a dataframe
names(south3)
north<-north[c(1:12)]
names(north)

new_dl<-rbind(north, south2) # the southern hemisphere data are bound to the northern hemisphere data by aligning seasons, meaning January represents January in the northern hemisphere and July in the southern hemisphere. 
sapply(new_dl, mean)

###################################### ETP
names(comps.f)
Tavg<-comps.f[c(23:34)]
#Tavg<-dum[c(2:13)]

head(Tavg)
Tavg<-Tavg/10
sapply(Tavg, mean)

temp.n0<-Tavg
temp.n0[temp.n0 < 0] <- 0 #replace all negative values with 0 
temp.n0<-round(temp.n0, digits = 3)

sapply(temp.n0, function(x) sum(is.na(x))) #this checks the number of NAs, there are 0 NAs

fun <- function(x) {ifelse(x>0,((x/5)^1.514),0)} # setting the function to get i
i<-sapply(temp.n0,fun) #calculates monthly heat indices


I.n<-rowSums(i, na.rm=FALSE) #The heat index is the sum of the monthly heat indices.
Idf.n<-as.data.frame(I.n)
names(Idf.n)
dim(Idf.n)

Idf.n$a<-(6.75 * 10^(-7)) * (Idf.n$I.n)^3 - (7.71 * 10^(-5)) * Idf.n$I.n^2 + 0.01792 * Idf.n$I.n + 0.49239 # calculates the factor by which everything will have to be raised for potential ET in each cell
#a   <- (67.5*10^(-8)*(Ia^3) - 77.1*10^(-6)*(Ia^2) + 0.0179*(Ia) + 0.492)

fun2<-function(y) {16*(10*y/Idf.n$I.n)^Idf.n$a} #function to calculate potential evapotranspiration; If I is 0 then the result can be NA because you have to divide by 0, so in those cases PET is 0
etp.n<-sapply(temp.n0, fun2)

etp.n2<-data.frame(etp.n*(new_dl/12)) # in the Thornthwaite formula this is represented as L/12
head(etp.n2)
#names(etp.n)<-c("etp1", "etp2", "etp3", "etp4", "etp5","etp6","etp7","etp8","etp9","etp10","etp11","etp12")

sapply(etp.n2, function(x) sum(is.na(x))) #3968 NA's, which need to be converted to 0
etp.n2[is.na(etp.n2)] <- 0

boxplot(etp.n2)
View(sapply(etp.n2, mean))




######################################### effective precipitation - pef
names(comps.f)

prec<-comps.f[c(35:46)]
names(prec)

boxplot(prec)
View(sapply(prec, mean))

Pef<-prec-etp.n2
names(Pef)
names(Pef)<-c("Pef1","Pef2","Pef3","Pef4","Pef5","Pef6","Pef7","Pef8","Pef9","Pef10","Pef11","Pef12")
sapply(Pef, function(x) sum(is.na(x))) #no NA's

boxplot(Pef)
View(sapply(Pef, mean))

# calculate dry months

dry.m<-rowSums(Pef<0)
mean(dry.m, na.rm=TRUE) #6.12 for 2.6; 6.92 for 8.5; 5.9 for current climate-redone



# cold months

cold.m<-rowSums(Tavg < -3)
range(cold.m)
mean(cold.m) #2.3714 for 2.6, 1.93 for 8.5; 2.78 current climate-redone


############################################# 
PefSum <-rowSums(Pef)
petSum <-rowSums(etp.n2)

def<-ifelse(PefSum < 0, PefSum, 0) #there is no true 0 in PefSum because of the number of digits after the period, otherwise I would have had to write "<= 0"
pef_wetm <-ifelse(PefSum>0, PefSum, 0)
mean(pef_wetm) #161.6177 for 2.6, 124.3061 for 8.5

pef_max <- apply(Pef, 1, max)
pef_min <- apply(Pef, 1, min)
pef_range <- pef_max - pef_min
etp_max <- apply(etp.n2, 1, max)
etp_min <- apply(etp.n2, 1, min)
etp_range <- etp_max - etp_min

PefColdQ <- rowSums(Pef[c(1,11,12)])
PefWarmQ <-rowSums(Pef[c(6:8)])

######################################## earth area

cf<-comps.f$earth_area/85863235
range(cf)
cf<-round(cf, digits=2)

#########################################################################################################################                                       Combine final dataset
names(comps.f)
head(comps.f)

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
divten<-comps.f[c(4,5,7:14)] #check that the column number match
head(divten)
divten<-divten/10

# subgroup

final<-cbind(comps.f[c(1:3)], divten[,(1:2)], comps.f[c(6)], divten[,(3:10)], comps.f[,(15:22)], cold.m, dry.m, petSum, PefSum, def, pef_wetm, pef_max, pef_min, pef_range, etp_max, etp_min, etp_range, PefColdQ, PefWarmQ, cf, comps.f[c(48:175)])

# group level


final<-cbind(comps.f[c(1:3)], divten[,(1:2)], comps.f[c(6)], divten[,(3:10)], comps.f[,(15:22)], cold.m, dry.m, petSum, PefSum, def, pef_wetm, pef_max, pef_min, pef_range, etp_max, etp_min, etp_range, PefColdQ, PefWarmQ, cf, comps.f[c(48:175)])

dim(final) #1,962,865; 1,979,447 ; worldclim 1.4 1,963,168; 1,979,754 worldclim 1.4; 2081893 for RK and GL included and all groups and subgroups left
names(final) 

#######################################################################################################################
########################################################################################################################

clim<-final[c(1:37)]
soils<-final[c(38:165)]
soils2<-soils*final$cf
head(soils2)
range(soils2$Rx)
final2<-cbind(clim, soils2)


names(final2)
#       Sum proportions of gelic and plinthic soils, and create kmat
temp<-final2[c(53, 79, 113, 130)] #gelic
head(temp)
range(temp$Gx)
temp$gelic<-rowSums(temp)
range(temp$gelic)

temp2<-final2[c(43,68, 78, 102)] #plinthic
temp2$plint<-rowSums(temp2)
range(temp2$plint)


temp3<-final2[c(51,57, 72, 93,100,152,157)] #calcic
temp3$calc<-rowSums(temp3)

temp4<-final2[c(110,111,112)] # Omin
temp4$Omin<-rowSums(temp4)
names(temp4)

temp5<-final2[c(125:128)] #Rmin
temp5$Rmin<-rowSums(temp5)
names(temp5)

final2$gelic<-temp$gelic
final2$plint<-temp2$plint
final2$calc<-temp3$calc
final2$Omin<-temp4$Omin
final2$Rmin<-temp5$Rmin


write.csv(dfsub, "C:/Users/aboca/Desktop/OTMED/WorldClim1.4/CC_final_subgroups.csv", row.names=FALSE)

#dfall2 is the same as dfall but without RK and GL columns
#names(dfall2)
#dfclim<-dfall2[c(2:37)]
#dfsoil<-dfall2[c(38:168)]


write.csv(dfsoil, "C:/Users/aboca/Desktop/OTMED/WorldClim1.4/CC_final_AllSoils.csv", row.names=FALSE)

dfall<-read.csv("C:/Users/aboca/Desktop/OTMED/WorldClim1.4/CC_final_AllSoils.csv")
clim<-read.csv("C:/Users/aboca/Desktop/OTMED/WorldClim1.4/CC_final_AllSoilsClim.csv")


######################################################################################################################## separate Luvisol and Lixosol


dfsoil<-read.csv("C:/Users/aboca/Desktop/OTMED/WorldClim1.4/CC_AllSoilSum.csv")
names(dfsoil)
dfsoil$lat<-clim$y
dfsoil$lon<-clim$x

dfluv<-dfsoil[c(12, 32,33)]

dfluv$luv<-ifelse((dfluv$lat >= 23.27 | dfluv$lat <= -23.27), dfluv$L, 0)
head(dfluv)
range(dfluv$luv)
hist(dfluv$lix)
dfluv$lix<-ifelse((dfluv$lat <= 23.27 & dfluv$lat >= -23.27), dfluv$L, 0)
head(dfluv)


dflix<-filter(dfluv, lix!= 0)
head(dflix)
range(dflix$lat)
range(dflix$luv)

dfluv2<-filter(dfluv, luv!= 0)
range(dfluv2$luv)
dim(dfluv2)
dim(dflix)
median(dfluv2$lat)


names(soil)
soil$Luv<-dfluv$luv
soil$Lix<-dfluv$lix
range(soil$Luv)
head(soil)
write.csv(soil, "C:/Users/aboca/Desktop/OTMED/WorldClim1.4/CC_AllSoilSum.csv", row.names=FALSE)
#######################################################################################################################

head(dfall)
names(dfall)

dfch<-dfall[c(25:36)]
head(dfch)
dfch<-round(dfch, digits=0)
dfall<-cbind(dfall[c(1:24)], dfch, dfall[c(37:170)])
names(dfall)
head(dfall)
write.csv(dfall, "C:/Users/aboca/Desktop/OTMED/WorldClim1.4/CC_final_AllSoils.csv", row.names=FALSE)


newdf<-read.csv("C:/Users/aboca/Desktop/OTMED/WorldClim2/CC_final_groups.csv")
names(newdf)
sapply(newdf[c(4:37)], mean)

soil<-newdf[c(38:163)]
names(soil)
soil2<-soil*newdf$cf
head(soil2)
range(soil2$Rx) #0-50
final<-cbind(newdf[c(1:37)], soil2)
head(final)
names(final)
dim(final)

write.csv(final, "C:/Users/aboca/Desktop/OTMED/WorldClim2/CC_final_groups.csv", row.names=FALSE)





fctemp<-fc2[1:200,]
dim(fctemp)
head(fctemp)
wctemp<-newdf[,2:3]
head(wctemp)

fc3<-dplyr::inner_join(fc2, wctemp) #theoretically this should extract the rows where x and y match in both datasets, but it gives me only 2016,267 rows

names(newdf)
soilprop<-newdf[c(38:141)]
total<-rowSums(soilprop)
length(total)


stemp<-as.data.frame(test, xy=TRUE)
dim(stemp)
head(stemp)
range(wise_05min_v12)


head(r2)
dim(r2)

tempsoil<-merge(r2, soil, by.x = "band1", by.y = "SUID" )
dim(tempsoil)
head(tempsoil)
dim(fc)
names(fc)
names(newdf)
fc$SOILMAPUNI<-tempsoil$SOILMAPUNI


temp<-newdf[,1:3]
head(temp)
fc3<-dplyr::inner_join(fc, newdf)
