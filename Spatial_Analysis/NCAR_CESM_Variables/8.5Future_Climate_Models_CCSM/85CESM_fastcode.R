#install.packages("ncdf4")
#install.packages("ncdump")
#install.packages("knitr")
#install.packages("tidyselect")
#install.packages("abind")
#install.packages("tiff")

library(ncdf4)
library(tidyselect)
library(ncdump)
library(dplyr)
library(raster)
library(lattice)
library(RColorBrewer)
library(sp)
library(rgdal)
library(knitr)
library(abind)
library(tiff)

###CESM LARGE ENSEMBLE###
#http://www.cesm.ucar.edu/projects/community-projects/LENS/data-sets.html

#######################################################################
#                               PAR                                  #
#######################################################################


##PAR NEAR###

#PAR near (n) 2040 - 2050 Run # 1 - 5 

#Read in ncdf4 files

par_n1 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_PAR/Near/b.e11.BRCP85C5CNBDRD.f09_g16.001.pop.h.PAR_avg.200601-208012.nc")
par_n2 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_PAR/Near/b.e11.BRCP85C5CNBDRD.f09_g16.002.pop.h.PAR_avg.200601-208012.nc")
par_n3 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_PAR/Near/b.e11.BRCP85C5CNBDRD.f09_g16.009.pop.h.PAR_avg.200601-208012.nc")
par_n4 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_PAR/Near/b.e11.BRCP85C5CNBDRD.f09_g16.010.pop.h.PAR_avg.200601-208012.nc")
par_n5 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_PAR/Near/b.e11.BRCP85C5CNBDRD.f09_g16.011.pop.h.PAR_avg.200601-208012.nc")

#spotcheck 

par_n1[["filename"]]
par_n2[["filename"]]
par_n3[["filename"]]
par_n4[["filename"]]
par_n5[["filename"]]

time <- ncvar_get(par_n1, varid = "time")
as.Date(time, origin = "0000-01-01") #see time in class 'calendar'  
#424+120 i.e [424] = "2039-12-22"

#float PAR_avg[nlon,nlat,z_t_150m,time]   (Chunking: [160,192,8,1])  (Compression: shuffle,level 1)
#          long_name: PAR Average over Model Cell
#         units: w/m^2
#        coordinates: TLONG TLAT z_t_150m time
#       grid_loc: 3114
#      cell_methods: time: mean
#     _FillValue: 9.96920996838687e+36
#    missing_value: 9.96920996838687e+36

#############################################################
#                          CURRENT
#############################################################

parc1 <- ncvar_get(par_n1, varid = "PAR_avg", 
                   start = c( 185, 175, z_t = 1, time = 40),
                   count = c( 135, 135, z_t = 5, time = 120))

parc2 <- ncvar_get(par_n2, varid = "PAR_avg", 
                   start = c( 185, 175, z_t = 1, time = 40),
                   count = c( 135, 135, z_t = 5, time = 120))

parc3 <- ncvar_get(par_n3, varid = "PAR_avg", 
                   start = c( 185, 175, z_t = 1, time = 40),
                   count = c( 135, 135, z_t = 5, time = 120))

parc4 <- ncvar_get(par_n4, varid = "PAR_avg", 
                   start = c( 185, 175, z_t = 1, time = 40),
                   count = c( 135, 135, z_t = 5, time = 120))

parc5 <- ncvar_get(par_n5, varid = "PAR_avg", 
                   start = c( 185, 175, z_t = 1, time = 40),
                   count = c( 135, 135, z_t = 5, time = 120))


par_current_master <- abind(parc1, parc2, parc3, parc4, parc5, along = 3)

dim(par_current_master)
par_current_master_mean <- apply(par_current_master, c(1:2), mean)
dim(par_current_master_mean)


plot(par_current_master_mean)
class(par_current_master_mean)


par_current_master_raster <- par_current_master_mean %>% 
  raster() %>% 
  t() %>% 
  flip(direction = 'y')

print(par_current_master_raster)
plot(par_current_master_raster)

###SET EXTENT#####

Caribbean.extent <- extent(-180, -45, -3, 37)

extent(par_current_master_raster) <- Caribbean.extent

par_current_master_raster2 <- setExtent(par_current_master_raster, Caribbean.extent, keepres = TRUE, snap = TRUE)

print(par_current_master_raster2)
plot(par_current_master_raster2)

###RESAMPLE###

resampleFactor <- 3.375 #increase the row(cell) size by 3.375% and reduce the number of rows from 135 to 40
par_current_master_inputRaster <- par_current_master_raster2
inCols <- ncol(par_current_master_inputRaster)
inCols
inRows <- nrow(par_current_master_inputRaster)
inRows
par_current_master_resampledRaster <- raster(ncol=(inCols/1), nrow=(inRows/resampleFactor))
extent(par_current_master_resampledRaster) <- extent(par_current_master_inputRaster)


par_current_master_resampledRaster <- resample(par_current_master_inputRaster,par_current_master_resampledRaster,method='bilinear',filename="par_current.tif",overwrite=TRUE)

print(par_current_master_resampledRaster)
plot(par_current_master_resampledRaster)



#############################################################
#                           NEAR 2040 - 2050
#############################################################

parn1 <- ncvar_get(par_n1, varid = "PAR_avg", 
                   start = c( 185, 175, z_t = 1, time = 424),
                   count = c( 135, 135, z_t = 5, time = 120))

parn2 <- ncvar_get(par_n2, varid = "PAR_avg", 
                   start = c( 185, 175, z_t = 1, time = 424),
                   count = c( 135, 135, z_t = 5, time = 120))

parn3 <- ncvar_get(par_n3, varid = "PAR_avg", 
                   start = c( 185, 175, z_t = 1, time = 424),
                   count = c( 135, 135, z_t = 5, time = 120))

parn4 <- ncvar_get(par_n4, varid = "PAR_avg", 
                   start = c( 185, 175, z_t = 1, time = 424),
                   count = c( 135, 135, z_t = 5, time = 120))

parn5 <- ncvar_get(par_n5, varid = "PAR_avg", 
                   start = c( 185, 175, z_t = 1, time = 424),
                   count = c( 135, 135, z_t = 5, time = 120))


par_near_master <- abind(parn1, parn2, parn3, parn4, parn5, along = 3)

dim(par_near_master)
par_near_master_mean <- apply(par_near_master, c(1:2), mean)
dim(par_near_master_mean)


plot(par_near_master_mean)
class(par_near_master_mean)


par_near_master_raster <- par_near_master_mean %>% 
  raster() %>% 
  t() %>% 
  flip(direction = 'y')

print(par_near_master_raster)
plot(par_near_master_raster)

###SET EXTENT#####

Caribbean.extent <- extent(-180, -45, -3, 37)

extent(par_near_master_raster) <- Caribbean.extent

par_near_master_raster2 <- setExtent(par_near_master_raster, Caribbean.extent, keepres = TRUE, snap = TRUE)

print(par_near_master_raster2)
plot(par_near_master_raster2)

###RESAMPLE###

resampleFactor <- 3.375 #increase the row(cell) size by 3.375% and reduce the number of rows from 135 to 40
par_near_master_inputRaster <- par_near_master_raster2
inCols <- ncol(par_near_master_inputRaster)
inCols
inRows <- nrow(par_near_master_inputRaster)
inRows
par_near_master_resampledRaster <- raster(ncol=(inCols/1), nrow=(inRows/resampleFactor))
extent(par_near_master_resampledRaster) <- extent(par_near_master_inputRaster)


par_near_master_resampledRaster <- resample(par_near_master_inputRaster,par_near_master_resampledRaster,method='bilinear',filename="par_near.tif",overwrite=TRUE)

print(par_near_master_resampledRaster)
plot(par_near_master_resampledRaster)


#PAR Future (f) 2070 - 2080

#############################################################
#                           FAR 2070 - 2080
#############################################################

time <- ncvar_get(par_n1, varid = "time")
as.Date(time, origin = "0000-01-01") #see time in class 'calendar'  
#424+120 i.e [424] = "2039-12-22"
#785 (2070-01-14) to 900 (2079-08-14)

parf1 <- ncvar_get(par_n1, varid = "PAR_avg", 
                   start = c( 185, 175, z_t = 1, time = 785),
                   count = c( 135, 135, z_t = 5, time = 115))

parf2 <- ncvar_get(par_n2, varid = "PAR_avg", 
                   start = c( 185, 175, z_t = 1, time = 785), 
                   count = c( 135, 135, z_t = 5, time = 115))

parf3 <- ncvar_get(par_n3, varid = "PAR_avg", 
                   start = c( 185, 175, z_t = 1, time = 785),
                   count = c( 135, 135, z_t = 5, time = 115))

parf4 <- ncvar_get(par_n4, varid = "PAR_avg", 
                   start = c( 185, 175, z_t = 1, time = 785),
                   count = c( 135, 135, z_t = 5, time = 115))

parf5 <- ncvar_get(par_n5, varid = "PAR_avg", 
                   start = c( 185, 175, z_t = 1, time = 785),
                   count = c( 135, 135, z_t = 5, time = 115))


par_far_master <- abind(parf1, parf2, parf3, parf4, parf5, along = 3)

dim(par_far_master)
par_far_master_mean <- apply(par_far_master, c(1:2), mean)
dim(par_far_master_mean)


plot(par_far_master_mean)
class(par_far_master_mean)


par_far_master_raster <- par_far_master_mean %>% 
  raster() %>% 
  t() %>% 
  flip(direction = 'y')

print(par_far_master_raster)
plot(par_far_master_raster)

###SET EXTENT#####

Caribbean.extent <- extent(-180, -45, -3, 37)

extent(par_far_master_raster) <- Caribbean.extent

par_far_master_raster2 <- setExtent(par_far_master_raster, Caribbean.extent, keepres = TRUE, snap = TRUE)

print(par_far_master_raster2)
plot(par_far_master_raster2)

###RESAMPLE###

resampleFactor <- 3.375 #increase the row(cell) size by 3.375% and reduce the number of rows from 135 to 40
par_far_master_inputRaster <- par_far_master_raster2
inCols <- ncol(par_far_master_inputRaster)
inCols
inRows <- nrow(par_far_master_inputRaster)
inRows
par_far_master_resampledRaster <- raster(ncol=(inCols/1), nrow=(inRows/resampleFactor))
extent(par_far_master_resampledRaster) <- extent(par_far_master_inputRaster)

par_far_master_resampledRaster <- resample(par_far_master_inputRaster,par_far_master_resampledRaster,method='bilinear',filename="par_far.tif",overwrite=TRUE)

print(par_far_master_resampledRaster)
plot(par_far_master_resampledRaster)

##############

##PAR Future (f) 2090 to 2100##

par_f1 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_PAR/Future/b.e11.BRCP85C5CNBDRD.f09_g16.001.pop.h.PAR_avg.208101-210012.nc")
par_f2 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_PAR/Future/b.e11.BRCP85C5CNBDRD.f09_g16.002.pop.h.PAR_avg.208101-210012.nc")
par_f3 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_PAR/Future/b.e11.BRCP85C5CNBDRD.f09_g16.009.pop.h.PAR_avg.208101-210012.nc")
par_f4 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_PAR/Future/b.e11.BRCP85C5CNBDRD.f09_g16.010.pop.h.PAR_avg.208101-210012.nc")
par_f5 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_PAR/Future/b.e11.BRCP85C5CNBDRD.f09_g16.011.pop.h.PAR_avg.208101-210012.nc")

#spotcheck 

par_f1[["filename"]]
par_f2[["filename"]]
par_f3[["filename"]]
par_f4[["filename"]]
par_f5[["filename"]]


#float PAR_avg[nlon,nlat,z_t_150m,time]   (Chunking: [160,192,8,1])  (Compression: shuffle,level 1)
#          long_name: PAR Average over Model Cell
#         units: w/m^2
#        coordinates: TLONG TLAT z_t_150m time
#       grid_loc: 3114
#      cell_methods: time: mean
#     _FillValue: 9.96920996838687e+36
#    missing_value: 9.96920996838687e+36


#pull average for location and time series of interest

parf1 <- ncvar_get(par_f1, varid = "PAR_avg", 
                   start = c( 185, 175, z_t = 1, time = 125),
                   count = c( 135, 135, z_t = 5, time = 116))


parf2 <- ncvar_get(par_f2, varid = "PAR_avg", 
                   start = c( 185, 175, z_t = 1, time = 125),
                   count = c( 135, 135, z_t = 5, time = 116))

parf3 <- ncvar_get(par_f3, varid = "PAR_avg", 
                   start = c( 185, 175, z_t = 1, time = 125),
                   count = c( 135, 135, z_t = 5, time = 116))

parf4 <- ncvar_get(par_f4, varid = "PAR_avg", 
                   start = c( 185, 175, z_t = 1, time = 125),
                   count = c( 135, 135, z_t = 5, time = 116))

parf5 <- ncvar_get(par_f5, varid = "PAR_avg", 
                   start = c( 185, 175, z_t = 1, time = 125),
                   count = c( 135, 135, z_t = 5, time = 116))

#merge

par_future_master <- abind(parf1, parf2, parf3, parf4, parf5, along = 3)

dim(par_future_master)
par_future_master_mean <- apply(par_future_master, c(1:2), mean)
dim(par_future_master_mean)


plot(par_future_master_mean)
class(par_future_master_mean)


par_future_master_raster <- par_future_master_mean %>% 
  raster() %>% 
  t() %>% 
  flip(direction = 'y')

print(par_future_master_raster)
plot(par_future_master_raster)


###SET EXTENT#####

Caribbean.extent <- extent(-179, -44, -3, 37)

extent(par_future_master_raster) <- Caribbean.extent

par_future_master_raster2 <- setExtent(par_future_master_raster, Caribbean.extent, keepres = TRUE, snap = TRUE)

print(par_future_master_raster2)
plot(par_future_master_raster2)


###RESAMPLE###

resampleFactor <- 3.375 #increase the row(cell) size by 3.375% and reduce the number of rows from 135 to 40
par_future_master_inputRaster <- par_future_master_raster2
inCols <- ncol(par_future_master_inputRaster)
inCols
inRows <- nrow(par_future_master_inputRaster)
inRows
par_future_master_resampledRaster <- raster(ncol=(inCols/1), nrow=(inRows/resampleFactor))
extent(par_future_master_resampledRaster) <- extent(par_future_master_inputRaster)


par_future_master_resampledRaster <- resample(par_future_master_inputRaster,par_future_master_resampledRaster,method='bilinear',filename="par_future.tif",overwrite=TRUE)

print(par_future_master_resampledRaster)
plot(par_future_master_resampledRaster)

###DONE WITH PAR 2090 - 2100####

#######################################################################
#                               PH                                  #
#######################################################################

##PH NEAR###

#PH near (n) 2040 - 2050 Run # 1 - 5 

#Read in ncdf4 files

ph_n1 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_PH/Near/b.e11.BRCP85C5CNBDRD.f09_g16.001.pop.h.PH.200601-208012.nc")
ph_n2 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_PH/Near/b.e11.BRCP85C5CNBDRD.f09_g16.002.pop.h.PH.200601-208012.nc")
ph_n3 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_PH/Near/b.e11.BRCP85C5CNBDRD.f09_g16.009.pop.h.PH.200601-208012.nc")
ph_n4 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_PH/Near/b.e11.BRCP85C5CNBDRD.f09_g16.010.pop.h.PH.200601-208012.nc")
ph_n5 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_PH/Near/b.e11.BRCP85C5CNBDRD.f09_g16.011.pop.h.PH.200601-208012.nc")

summary(ph_n1$var)

#spotcheck 

ph_n1[["filename"]]
ph_n2[["filename"]]
ph_n3[["filename"]]
ph_n4[["filename"]]
ph_n5[["filename"]]

#time <- ncvar_get(par_n1, varid = "time")
#as.Date(time, origin = "0000-01-01") #see time in class 'calendar'  
#424+120 i.e [424] = "2039-12-22"

#############################################################
#                          CURRENT
#############################################################

phc1 <- ncvar_get(ph_n1, varid = "PH", 
                  start = c( 185, 175, time = 40),
                  count = c( 135, 135, time = 120))

phc2 <- ncvar_get(ph_n2, varid = "PH", 
                  start = c( 185, 175, time = 40),
                  count = c( 135, 135, time = 120))

phc3 <- ncvar_get(ph_n3, varid = "PH", 
                  start = c( 185, 175,  time = 40),
                  count = c( 135, 135,  time = 120))

phc4 <- ncvar_get(ph_n4, varid = "PH", 
                  start = c( 185, 175,  time = 40),
                  count = c( 135, 135,  time = 120))

phc5 <- ncvar_get(ph_n5, varid = "PH", 
                  start = c( 185, 175, time = 40),
                  count = c( 135, 135,  time = 120))


ph_current_master <- abind(phc1, phc2, phc3, phc4, phc5, along = 3)

dim(ph_current_master)
ph_current_master_mean <- apply(ph_current_master, c(1:2), mean)
dim(ph_current_master_mean)


plot(ph_current_master_mean)
class(ph_current_master_mean)


ph_current_master_raster <- ph_current_master_mean %>% 
  raster() %>% 
  t() %>% 
  flip(direction = 'y')

print(ph_current_master_raster)
plot(ph_current_master_raster)

###SET EXTENT#####

Caribbean.extent <- extent(-180, -45, -3, 37)

extent(ph_current_master_raster) <- Caribbean.extent

ph_current_master_raster2 <- setExtent(ph_current_master_raster, Caribbean.extent, keepres = TRUE, snap = TRUE)

print(ph_current_master_raster2)
plot(ph_current_master_raster2)

###RESAMPLE###

resampleFactor <- 3.375 #increase the row(cell) size by 3.375% and reduce the number of rows from 135 to 40
ph_current_master_inputRaster <- ph_current_master_raster2
inCols <- ncol(ph_current_master_inputRaster)
inCols
inRows <- nrow(ph_current_master_inputRaster)
inRows
ph_current_master_resampledRaster <- raster(ncol=(inCols/1), nrow=(inRows/resampleFactor))
extent(ph_current_master_resampledRaster) <- extent(ph_current_master_inputRaster)


ph_current_master_resampledRaster <- resample(ph_current_master_inputRaster,ph_current_master_resampledRaster,method='bilinear',filename="ph_current.tif",overwrite=TRUE)

print(ph_current_master_resampledRaster)
plot(ph_current_master_resampledRaster)

#############################################################
#                          NEAR 2040-2050
#############################################################


phn1 <- ncvar_get(ph_n1, varid = "PH", 
                   start = c( 185, 175, time = 424),
                   count = c( 135, 135, time = 120))

phn2 <- ncvar_get(ph_n2, varid = "PH", 
                   start = c( 185, 175, time = 424),
                   count = c( 135, 135, time = 120))

phn3 <- ncvar_get(ph_n3, varid = "PH", 
                   start = c( 185, 175,  time = 424),
                   count = c( 135, 135,  time = 120))

phn4 <- ncvar_get(ph_n4, varid = "PH", 
                   start = c( 185, 175,  time = 424),
                   count = c( 135, 135,  time = 120))

phn5 <- ncvar_get(ph_n5, varid = "PH", 
                   start = c( 185, 175, time = 424),
                   count = c( 135, 135,  time = 120))


ph_near_master <- abind(phn1, phn2, phn3, phn4, phn5, along = 3)

dim(ph_near_master)
ph_near_master_mean <- apply(ph_near_master, c(1:2), mean)
dim(ph_near_master_mean)


plot(ph_near_master_mean)
class(ph_near_master_mean)


ph_near_master_raster <- ph_near_master_mean %>% 
  raster() %>% 
  t() %>% 
  flip(direction = 'y')

print(ph_near_master_raster)
plot(ph_near_master_raster)

###SET EXTENT#####

Caribbean.extent <- extent(-180, -45, -3, 37)

extent(ph_near_master_raster) <- Caribbean.extent

ph_near_master_raster2 <- setExtent(ph_near_master_raster, Caribbean.extent, keepres = TRUE, snap = TRUE)

print(ph_near_master_raster2)
plot(ph_near_master_raster2)

###RESAMPLE###

resampleFactor <- 3.375 #increase the row(cell) size by 3.375% and reduce the number of rows from 135 to 40
ph_near_master_inputRaster <- ph_near_master_raster2
inCols <- ncol(ph_near_master_inputRaster)
inCols
inRows <- nrow(ph_near_master_inputRaster)
inRows
ph_near_master_resampledRaster <- raster(ncol=(inCols/1), nrow=(inRows/resampleFactor))
extent(ph_near_master_resampledRaster) <- extent(ph_near_master_inputRaster)


ph_near_master_resampledRaster <- resample(ph_near_master_inputRaster,ph_near_master_resampledRaster,method='bilinear',filename="ph_near.tif",overwrite=TRUE)

print(ph_near_master_resampledRaster)
plot(ph_near_master_resampledRaster)

#############################################################
#                          Far 2070-2080
#############################################################


pnf1 <- ncvar_get(ph_n1, varid = "PH", 
                  start = c( 185, 175, time = 785),
                  count = c( 135, 135, time = 115))

pnf2 <- ncvar_get(ph_n2, varid = "PH", 
                  start = c( 185, 175, time = 785),
                  count = c( 135, 135, time = 115))

pnf3 <- ncvar_get(ph_n3, varid = "PH", 
                  start = c( 185, 175,  time = 785),
                  count = c( 135, 135,  time = 115))

pnf4 <- ncvar_get(ph_n4, varid = "PH", 
                  start = c( 185, 175,  time = 785),
                  count = c( 135, 135,  time = 115))

pnf5 <- ncvar_get(ph_n5, varid = "PH", 
                  start = c( 185, 175, time = 785),
                  count = c( 135, 135,  time = 115))


ph_far_master <- abind(pnf1, pnf2, pnf3, pnf4, pnf5, along = 3)

dim(ph_far_master)
ph_far_master_mean <- apply(ph_far_master, c(1:2), mean)
dim(ph_far_master_mean)


plot(ph_far_master_mean)
class(ph_far_master_mean)


ph_far_master_raster <- ph_far_master_mean %>% 
  raster() %>% 
  t() %>% 
  flip(direction = 'y')

print(ph_far_master_raster)
plot(ph_far_master_raster)

###SET EXTENT#####

Caribbean.extent <- extent(-180, -45, -3, 37)

extent(ph_far_master_raster) <- Caribbean.extent

ph_far_master_raster2 <- setExtent(ph_far_master_raster, Caribbean.extent, keepres = TRUE, snap = TRUE)

print(ph_far_master_raster2)
plot(ph_far_master_raster2)

###RESAMPLE###

resampleFactor <- 3.375 #increase the row(cell) size by 3.375% and reduce the number of rows from 135 to 40
ph_far_master_inputRaster <- ph_far_master_raster2
inCols <- ncol(ph_far_master_inputRaster)
inCols
inRows <- nrow(ph_far_master_inputRaster)
inRows
ph_far_master_resampledRaster <- raster(ncol=(inCols/1), nrow=(inRows/resampleFactor))
extent(ph_far_master_resampledRaster) <- extent(ph_far_master_inputRaster)


ph_far_master_resampledRaster <- resample(ph_far_master_inputRaster,ph_far_master_resampledRaster,method='bilinear',filename="ph_far.tif",overwrite=TRUE)

print(ph_far_master_resampledRaster)
plot(ph_far_master_resampledRaster)


##############

##PH Future (f) 2090 to 2100##
ph_f1 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_PH/Future/b.e11.BRCP85C5CNBDRD.f09_g16.001.pop.h.PH.208101-210012.nc")
ph_f2 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_PH/Future/b.e11.BRCP85C5CNBDRD.f09_g16.002.pop.h.PH.208101-210012.nc")
ph_f3 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_PH/Future/b.e11.BRCP85C5CNBDRD.f09_g16.009.pop.h.PH.208101-210012.nc")
ph_f4 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_PH/Future/b.e11.BRCP85C5CNBDRD.f09_g16.010.pop.h.PH.208101-210012.nc")
ph_f5 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_PH/Future/b.e11.BRCP85C5CNBDRD.f09_g16.011.pop.h.PH.208101-210012.nc")

#spotcheck 

ph_f1[["filename"]]
ph_f2[["filename"]]
ph_f3[["filename"]]
ph_f4[["filename"]]
ph_f5[["filename"]]


#float PAR_avg[nlon,nlat,z_t_150m,time]   (Chunking: [160,192,8,1])  (Compression: shuffle,level 1)
#          long_name: PAR Average over Model Cell
#         units: w/m^2
#        coordinates: TLONG TLAT z_t_150m time
#       grid_loc: 3114
#      cell_methods: time: mean
#     _FillValue: 9.96920996838687e+36
#    missing_value: 9.96920996838687e+36


#pull average for location and time series of interest

phf1 <- ncvar_get(ph_f1, varid = "PH", 
                   start = c( 185, 175,  time = 125),
                   count = c( 135, 135,  time = 116))


phf2 <- ncvar_get(ph_f2, varid = "PH", 
                   start = c( 185, 175,  time = 125),
                   count = c( 135, 135,  time = 116))

phf3 <- ncvar_get(ph_f3, varid = "PH", 
                   start = c( 185, 175,  time = 125),
                   count = c( 135, 135,  time = 116))

phf4 <- ncvar_get(ph_f4, varid = "PH", 
                   start = c( 185, 175,  time = 125),
                   count = c( 135, 135,  time = 116))

phf5 <- ncvar_get(ph_f5, varid = "PH", 
                   start = c( 185, 175,  time = 125),
                   count = c( 135, 135,  time = 116))

#merge

ph_future_master <- abind(phf1, phf2, phf3, phf4, phf5, along = 3)

dim(ph_future_master)
ph_future_master_mean <- apply(ph_future_master, c(1:2), mean)
dim(ph_future_master_mean)


plot(ph_future_master_mean)
class(ph_future_master_mean)


ph_future_master_raster <- ph_future_master_mean %>% 
  raster() %>% 
  t() %>% 
  flip(direction = 'y')

print(ph_future_master_raster)
plot(ph_future_master_raster)

###SET EXTENT#####

Caribbean.extent <- extent(-180, -45, -3, 37)

extent(ph_future_master_raster) <- Caribbean.extent

ph_future_master_raster2 <- setExtent(ph_future_master_raster, Caribbean.extent, keepres = TRUE, snap = TRUE)

print(ph_future_master_raster2)
plot(ph_future_master_raster2)

###RESAMPLE###

resampleFactor <- 3.375 #increase the row(cell) size by 3.375% and reduce the number of rows from 135 to 40
ph_future_master_inputRaster <- ph_future_master_raster2
inCols <- ncol(ph_future_master_inputRaster)
inCols
inRows <- nrow(ph_future_master_inputRaster)
inRows
ph_future_master_resampledRaster <- raster(ncol=(inCols/1), nrow=(inRows/resampleFactor))
extent(ph_future_master_resampledRaster) <- extent(ph_future_master_inputRaster)


ph_future_master_resampledRaster <- resample(ph_future_master_inputRaster,ph_future_master_resampledRaster,method='bilinear',filename="ph_future.tif",overwrite=TRUE)

print(ph_future_master_resampledRaster)
plot(ph_future_master_resampledRaster)

###DONE WITH PH 2090 - 2100####
#######################################################################
#                               SALT                                  #
#######################################################################

##SALT NEAR###

#SALT near (n) 2040 - 2050 Run # 1 - 5 

#Read in ncdf4 files

salt_n1 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_SALT/Near/b.e11.BRCP85C5CNBDRD.f09_g16.001.pop.h.SALT.200601-208012.nc")
salt_n2 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_SALT/Near/b.e11.BRCP85C5CNBDRD.f09_g16.002.pop.h.SALT.200601-208012.nc")
salt_n3 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_SALT/Near/b.e11.BRCP85C5CNBDRD.f09_g16.009.pop.h.SALT.200601-208012.nc")
salt_n4 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_SALT/Near/b.e11.BRCP85C5CNBDRD.f09_g16.010.pop.h.SALT.200601-208012.nc")
salt_n5 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_SALT/Near/b.e11.BRCP85C5CNBDRD.f09_g16.011.pop.h.SALT.200601-208012.nc")

print(salt_n1)

#Salinity

#spotcheck 

salt_n1[["filename"]]
salt_n1[["filename"]]
salt_n1[["filename"]]
salt_n1[["filename"]]
salt_n1[["filename"]]

#time <- ncvar_get(par_n1, varid = "time")
#as.Date(time, origin = "0000-01-01") #see time in class 'calendar'  
#424+120 i.e [424] = "2039-12-22"

#float PAR_avg[nlon,nlat,z_t_150m,time]   (Chunking: [160,192,8,1])  (Compression: shuffle,level 1)
#          long_name: PAR Average over Model Cell
#         units: w/m^2
#        coordinates: TLONG TLAT z_t_150m time
#       grid_loc: 3114
#      cell_methods: time: mean
#     _FillValue: 9.96920996838687e+36
#    missing_value: 9.96920996838687e+36

#############################################################
#                          CURRENT
#############################################################

saltc1 <- ncvar_get(salt_n1, varid = "SALT", 
                    start = c( 185, 175, z_t = 1, time = 40),
                    count = c( 135, 135, z_t = 5, time = 120))

saltc2 <- ncvar_get(salt_n2, varid = "SALT", 
                    start = c( 185, 175, z_t = 1, time = 40),
                    count = c( 135, 135, z_t = 5, time = 120))

saltc3 <- ncvar_get(salt_n3, varid = "SALT", 
                    start = c( 185, 175, z_t = 4, time = 40),
                    count = c( 135, 135, z_t = 1, time = 120))

saltc4 <- ncvar_get(salt_n4, varid = "SALT", 
                    start = c( 185, 175, z_t = 4, time = 40),
                    count = c( 135, 135, z_t = 1, time = 120))

saltc5 <- ncvar_get(salt_n5, varid = "SALT", 
                    start = c( 185, 175, z_t = 4, time = 40),
                    count = c( 135, 135, z_t = 1, time = 120))


salt_current_master <- abind(saltc1, saltc2, saltc3, saltc4, saltc5, along = 3)

dim(salt_current_master)
salt_current_master_mean <- apply(salt_current_master, c(1:2), mean)
dim(salt_current_master_mean)


plot(salt_current_master_mean)
class(salt_current_master_mean)


salt_current_master_raster <- salt_current_master_mean %>% 
  raster() %>% 
  t() %>% 
  flip(direction = 'y')

print(salt_current_master_raster)
plot(salt_current_master_raster)

###SET EXTENT#####

Caribbean.extent <- extent(-180, -45, -3, 37)

extent(salt_current_master_raster) <- Caribbean.extent

salt_current_master_raster2 <- setExtent(salt_current_master_raster, Caribbean.extent, keepres = TRUE, snap = TRUE)

print(salt_current_master_raster2)
plot(salt_current_master_raster2)

###RESAMPLE###

resampleFactor <- 3.375 #increase the row(cell) size by 3.375% and reduce the number of rows from 135 to 40
salt_current_master_inputRaster <- salt_current_master_raster2
inCols <- ncol(salt_current_master_inputRaster)
inCols
inRows <- nrow(salt_current_master_inputRaster)
inRows
salt_current_master_resampledRaster <- raster(ncol=(inCols/1), nrow=(inRows/resampleFactor))
extent(salt_current_master_resampledRaster) <- extent(salt_current_master_inputRaster)


salt_current_master_resampledRaster <- resample(salt_current_master_inputRaster,salt_current_master_resampledRaster,method='bilinear',filename="salt_current.tif",overwrite=TRUE)

print(salt_current_master_resampledRaster)
plot(salt_current_master_resampledRaster)

#############################################################
#                          NEAR 2040 - 2050
#############################################################

saltn1 <- ncvar_get(salt_n1, varid = "SALT", 
                   start = c( 185, 175, z_t = 1, time = 424),
                   count = c( 135, 135, z_t = 5, time = 120))

saltn2 <- ncvar_get(salt_n2, varid = "SALT", 
                   start = c( 185, 175, z_t = 1, time = 424),
                   count = c( 135, 135, z_t = 5, time = 120))

saltn3 <- ncvar_get(salt_n3, varid = "SALT", 
                   start = c( 185, 175, z_t = 1, time = 424),
                   count = c( 135, 135, z_t = 5, time = 120))

saltn4 <- ncvar_get(salt_n4, varid = "SALT", 
                   start = c( 185, 175, z_t = 1, time = 424),
                   count = c( 135, 135, z_t = 5, time = 120))

saltn5 <- ncvar_get(salt_n5, varid = "SALT", 
                   start = c( 185, 175, z_t = 1, time = 424),
                   count = c( 135, 135, z_t = 5, time = 120))


salt_near_master <- abind(saltn1, saltn2, saltn3, saltn4, saltn5, along = 3)

dim(salt_near_master)
salt_near_master_mean <- apply(salt_near_master, c(1:2), mean)
dim(salt_near_master_mean)


plot(salt_near_master_mean)
class(salt_near_master_mean)


salt_near_master_raster <- salt_near_master_mean %>% 
  raster() %>% 
  t() %>% 
  flip(direction = 'y')

print(salt_near_master_raster)
plot(salt_near_master_raster)

###SET EXTENT#####

Caribbean.extent <- extent(-180, -45, -3, 37)

extent(salt_near_master_raster) <- Caribbean.extent

salt_near_master_raster2 <- setExtent(salt_near_master_raster, Caribbean.extent, keepres = TRUE, snap = TRUE)

print(salt_near_master_raster2)
plot(salt_near_master_raster2)

###RESAMPLE###

resampleFactor <- 3.375 #increase the row(cell) size by 3.375% and reduce the number of rows from 135 to 40
salt_near_master_inputRaster <- salt_near_master_raster2
inCols <- ncol(salt_near_master_inputRaster)
inCols
inRows <- nrow(salt_near_master_inputRaster)
inRows
salt_near_master_resampledRaster <- raster(ncol=(inCols/1), nrow=(inRows/resampleFactor))
extent(salt_near_master_resampledRaster) <- extent(salt_near_master_inputRaster)


salt_near_master_resampledRaster <- resample(salt_near_master_inputRaster,salt_near_master_resampledRaster,method='bilinear',filename="salt_near.tif",overwrite=TRUE)

print(salt_near_master_resampledRaster)
plot(salt_near_master_resampledRaster)


#############################################################
#                          FAR 2070 - 2080
#############################################################

saltf1 <- ncvar_get(salt_n1, varid = "SALT", 
                    start = c( 185, 175, z_t = 1, time = 785),
                    count = c( 135, 135, z_t = 5, time = 115))

saltf2 <- ncvar_get(salt_n2, varid = "SALT", 
                    start = c( 185, 175, z_t = 1, time = 785),
                    count = c( 135, 135, z_t = 5, time = 115))

saltf3 <- ncvar_get(salt_n3, varid = "SALT", 
                    start = c( 185, 175, z_t = 1, time = 785),
                    count = c( 135, 135, z_t = 5, time = 115))

saltf4 <- ncvar_get(salt_n4, varid = "SALT", 
                    start = c( 185, 175, z_t = 1, time = 785),
                    count = c( 135, 135, z_t = 5, time = 115))

saltf5 <- ncvar_get(salt_n5, varid = "SALT", 
                    start = c( 185, 175, z_t = 1, time = 785),
                    count = c( 135, 135, z_t = 5, time = 115))


salt_far_master <- abind(saltf1, saltf2, saltf3, saltf4, saltf5, along = 3)

dim(salt_far_master)
salt_far_master_mean <- apply(salt_far_master, c(1:2), mean)
dim(salt_far_master_mean)


plot(salt_far_master_mean)
class(salt_far_master_mean)


salt_far_master_raster <- salt_far_master_mean %>% 
  raster() %>% 
  t() %>% 
  flip(direction = 'y')

print(salt_far_master_raster)
plot(salt_far_master_raster)

###SET EXTENT#####

Caribbean.extent <- extent(-180, -45, -3, 37)

extent(salt_far_master_raster) <- Caribbean.extent

salt_far_master_raster2 <- setExtent(salt_far_master_raster, Caribbean.extent, keepres = TRUE, snap = TRUE)

print(salt_far_master_raster2)
plot(salt_far_master_raster2)

###RESAMPLE###

resampleFactor <- 3.375 #increase the row(cell) size by 3.375% and reduce the number of rows from 135 to 40
salt_far_master_inputRaster <- salt_far_master_raster2
inCols <- ncol(salt_far_master_inputRaster)
inCols
inRows <- nrow(salt_far_master_inputRaster)
inRows
salt_far_master_resampledRaster <- raster(ncol=(inCols/1), nrow=(inRows/resampleFactor))
extent(salt_far_master_resampledRaster) <- extent(salt_far_master_inputRaster)


salt_far_master_resampledRaster <- resample(salt_far_master_inputRaster,salt_far_master_resampledRaster,method='bilinear',filename="salt_far.tif",overwrite=TRUE)

print(salt_far_master_resampledRaster)
plot(salt_far_master_resampledRaster)



##############

##SALT Future (f) 2090 to 2100##
salt_f1 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_SALT/Future/b.e11.BRCP85C5CNBDRD.f09_g16.001.pop.h.SALT.208101-210012.nc")
salt_f2 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_SALT/Future/b.e11.BRCP85C5CNBDRD.f09_g16.002.pop.h.SALT.208101-210012.nc")
salt_f3 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_SALT/Future/b.e11.BRCP85C5CNBDRD.f09_g16.009.pop.h.SALT.208101-210012.nc")
salt_f4 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_SALT/Future/b.e11.BRCP85C5CNBDRD.f09_g16.010.pop.h.SALT.208101-210012.nc")
salt_f5 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_SALT/Future/b.e11.BRCP85C5CNBDRD.f09_g16.011.pop.h.SALT.208101-210012.nc")

#spotcheck 

salt_f1[["filename"]]
salt_f2[["filename"]]
salt_f3[["filename"]]
salt_f4[["filename"]]
salt_f5[["filename"]]


#float PAR_avg[nlon,nlat,z_t_150m,time]   (Chunking: [160,192,8,1])  (Compression: shuffle,level 1)
#          long_name: PAR Average over Model Cell
#         units: w/m^2
#        coordinates: TLONG TLAT z_t_150m time
#       grid_loc: 3114
#      cell_methods: time: mean
#     _FillValue: 9.96920996838687e+36
#    missing_value: 9.96920996838687e+36


#pull average for location and time series of interest

saltf1 <- ncvar_get(salt_f1, varid = "SALT", 
                   start = c( 185, 175, z_t = 1, time = 125),
                   count = c( 135, 135, z_t = 5, time = 116))


saltf2 <- ncvar_get(salt_f2, varid = "SALT", 
                   start = c( 185, 175, z_t = 1, time = 125),
                   count = c( 135, 135, z_t = 5, time = 116))

saltf3 <- ncvar_get(salt_f3, varid = "SALT", 
                   start = c( 185, 175, z_t = 1, time = 125),
                   count = c( 135, 135, z_t = 5, time = 116))

saltf4 <- ncvar_get(salt_f4, varid = "SALT", 
                   start = c( 185, 175, z_t = 1, time = 125),
                   count = c( 135, 135, z_t = 5, time = 116))

saltf5 <- ncvar_get(salt_f5, varid = "SALT", 
                   start = c( 185, 175, z_t = 1, time = 125),
                   count = c( 135, 135, z_t = 5, time = 116))

#merge

salt_future_master <- abind(saltf1, saltf2, saltf3, saltf4, saltf5, along = 3)

dim(salt_future_master)
salt_future_master_mean <- apply(salt_future_master, c(1:2), mean)
dim(salt_future_master_mean)


plot(salt_future_master_mean)
class(salt_future_master_mean)


salt_future_master_raster <- salt_future_master_mean %>% 
  raster() %>% 
  t() %>% 
  flip(direction = 'y')

print(salt_future_master_raster)
plot(salt_future_master_raster)


###SET EXTENT#####

Caribbean.extent <- extent(-179, -44, -3, 37)

extent(salt_future_master_raster) <- Caribbean.extent

salt_future_master_raster2 <- setExtent(salt_future_master_raster, Caribbean.extent, keepres = TRUE, snap = TRUE)

print(salt_future_master_raster2)
plot(salt_future_master_raster2)


###RESAMPLE###

resampleFactor <- 3.375 #increase the row(cell) size by 3.375% and reduce the number of rows from 135 to 40
salt_future_master_inputRaster <- salt_future_master_raster2
inCols <- ncol(salt_future_master_inputRaster)
inCols
inRows <- nrow(salt_future_master_inputRaster)
inRows
salt_future_master_resampledRaster <- raster(ncol=(inCols/1), nrow=(inRows/resampleFactor))
extent(salt_future_master_resampledRaster) <- extent(salt_future_master_inputRaster)


salt_future_master_resampledRaster <- resample(salt_future_master_inputRaster,salt_future_master_resampledRaster,method='bilinear',filename="salt_future.tif",overwrite=TRUE)

print(salt_future_master_resampledRaster)
plot(salt_future_master_resampledRaster)

#######################################################################
#                               SSH                                  #
#######################################################################

##SSH NEAR###

#SSH near (n) 2040 - 2050 Run # 1 - 5 

#Read in ncdf4 files

ssh_n1 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_SSH/Near/b.e11.BRCP85C5CNBDRD.f09_g16.001.pop.h.SSH.200601-208012.nc")
ssh_n2 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_SSH/Near/b.e11.BRCP85C5CNBDRD.f09_g16.002.pop.h.SSH.200601-208012.nc")
ssh_n3 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_SSH/Near/b.e11.BRCP85C5CNBDRD.f09_g16.009.pop.h.SSH.200601-208012.nc")
ssh_n4 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_SSH/Near/b.e11.BRCP85C5CNBDRD.f09_g16.010.pop.h.SSH.200601-208012.nc")
ssh_n5 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_SSH/Near/b.e11.BRCP85C5CNBDRD.f09_g16.011.pop.h.SSH.200601-208012.nc")

#spotcheck 
print(ssh_n1)
ssh_n1[["filename"]]
ssh_n2[["filename"]]
ssh_n3[["filename"]]
ssh_n4[["filename"]]
ssh_n5[["filename"]]

#time <- ncvar_get(par_n1, varid = "time")
#as.Date(time, origin = "0000-01-01") #see time in class 'calendar'  
#424+120 i.e [424] = "2039-12-22"

#float PAR_avg[nlon,nlat,z_t_150m,time]   (Chunking: [160,192,8,1])  (Compression: shuffle,level 1)
#          long_name: PAR Average over Model Cell
#         units: w/m^2
#        coordinates: TLONG TLAT z_t_150m time
#       grid_loc: 3114
#      cell_methods: time: mean
#     _FillValue: 9.96920996838687e+36
#    missing_value: 9.96920996838687e+36


#############################################################
#                          CURRENT
#############################################################
sshc1 <- ncvar_get(ssh_n1, varid = "SSH", 
                   start = c( 185, 175, time = 40),
                   count = c( 135, 135, time = 120))

sshc2 <- ncvar_get(ssh_n2, varid = "SSH", 
                   start = c( 185, 175, time = 40),
                   count = c( 135, 135, time = 120))

sshc3 <- ncvar_get(ssh_n3, varid = "SSH", 
                   start = c( 185, 175, time = 40),
                   count = c( 135, 135, time = 120))

sshc4 <- ncvar_get(ssh_n4, varid = "SSH", 
                   start = c( 185, 175, time = 40),
                   count = c( 135, 135, time = 120))

sshc5 <- ncvar_get(ssh_n5, varid = "SSH", 
                   start = c( 185, 175, time = 40),
                   count = c( 135, 135, time = 120))

ssh_current_master <- abind(sshc1, sshc2, sshc3, sshc4, sshc5, along = 3)

dim(ssh_current_master)
ssh_current_master_mean <- apply(ssh_current_master, c(1:2), mean)
dim(ssh_current_master_mean)


plot(ssh_current_master_mean)
class(ssh_current_master_mean)


ssh_current_master_raster <- ssh_current_master_mean %>% 
  raster() %>% 
  t() %>% 
  flip(direction = 'y')

print(ssh_current_master_raster)
plot(ssh_current_master_raster)

###SET EXTENT#####

Caribbean.extent <- extent(-180, -45, -3, 37)

extent(ssh_current_master_raster) <- Caribbean.extent

ssh_current_master_raster2 <- setExtent(ssh_current_master_raster, Caribbean.extent, keepres = TRUE, snap = TRUE)

print(ssh_current_master_raster2)
plot(ssh_current_master_raster2)

###RESAMPLE###

resampleFactor <- 3.375 #increase the row(cell) size by 3.375% and reduce the number of rows from 135 to 40
ssh_current_master_inputRaster <- ssh_current_master_raster2
inCols <- ncol(ssh_current_master_inputRaster)
inCols
inRows <- nrow(ssh_current_master_inputRaster)
inRows
ssh_current_master_resampledRaster <- raster(ncol=(inCols/1), nrow=(inRows/resampleFactor))
extent(ssh_current_master_resampledRaster) <- extent(ssh_current_master_inputRaster)


ssh_current_master_resampledRaster <- resample(ssh_current_master_inputRaster,ssh_current_master_resampledRaster,method='bilinear',filename="ssh_current.tif",overwrite=TRUE)

print(ssh_current_master_resampledRaster)
plot(ssh_current_master_resampledRaster)

#############################################################
#                          NEAR 2040 - 2060
#############################################################

sshn1 <- ncvar_get(ssh_n1, varid = "SSH", 
                   start = c( 185, 175, time = 424),
                   count = c( 135, 135, time = 120))

sshn2 <- ncvar_get(ssh_n2, varid = "SSH", 
                   start = c( 185, 175, time = 424),
                   count = c( 135, 135, time = 120))

sshn3 <- ncvar_get(ssh_n3, varid = "SSH", 
                   start = c( 185, 175, time = 424),
                   count = c( 135, 135, time = 120))

sshn4 <- ncvar_get(ssh_n4, varid = "SSH", 
                   start = c( 185, 175, time = 424),
                   count = c( 135, 135, time = 120))

sshn5 <- ncvar_get(ssh_n5, varid = "SSH", 
                   start = c( 185, 175, time = 424),
                   count = c( 135, 135, time = 120))

ssh_near_master <- abind(sshn1, sshn2, sshn3, sshn4, sshn5, along = 3)

dim(ssh_near_master)
ssh_near_master_mean <- apply(ssh_near_master, c(1:2), mean)
dim(ssh_near_master_mean)


plot(ssh_near_master_mean)
class(ssh_near_master_mean)


ssh_near_master_raster <- ssh_near_master_mean %>% 
  raster() %>% 
  t() %>% 
  flip(direction = 'y')

print(ssh_near_master_raster)
plot(ssh_near_master_raster)

###SET EXTENT#####

Caribbean.extent <- extent(-180, -45, -3, 37)

extent(ssh_near_master_raster) <- Caribbean.extent

ssh_near_master_raster2 <- setExtent(ssh_near_master_raster, Caribbean.extent, keepres = TRUE, snap = TRUE)

print(ssh_near_master_raster2)
plot(ssh_near_master_raster2)

###RESAMPLE###

resampleFactor <- 3.375 #increase the row(cell) size by 3.375% and reduce the number of rows from 135 to 40
ssh_near_master_inputRaster <- ssh_near_master_raster2
inCols <- ncol(ssh_near_master_inputRaster)
inCols
inRows <- nrow(ssh_near_master_inputRaster)
inRows
ssh_near_master_resampledRaster <- raster(ncol=(inCols/1), nrow=(inRows/resampleFactor))
extent(ssh_near_master_resampledRaster) <- extent(ssh_near_master_inputRaster)


ssh_near_master_resampledRaster <- resample(ssh_near_master_inputRaster,ssh_near_master_resampledRaster,method='bilinear',filename="ssh_near.tif",overwrite=TRUE)

print(ssh_near_master_resampledRaster)
plot(ssh_near_master_resampledRaster)


#############################################################
#                          FAR 2070 - 2080
#############################################################

sshf1 <- ncvar_get(ssh_n1, varid = "SSH", 
                   start = c( 185, 175, time = 785),
                   count = c( 135, 135, time = 115))

sshf2 <- ncvar_get(ssh_n2, varid = "SSH", 
                   start = c( 185, 175, time = 785),
                   count = c( 135, 135, time = 115))

sshf3 <- ncvar_get(ssh_n3, varid = "SSH", 
                   start = c( 185, 175, time = 785),
                   count = c( 135, 135, time = 115))

sshf4 <- ncvar_get(ssh_n4, varid = "SSH", 
                   start = c( 185, 175, time = 785),
                   count = c( 135, 135, time = 115))

sshf5 <- ncvar_get(ssh_n5, varid = "SSH", 
                   start = c( 185, 175, time = 785),
                   count = c( 135, 135, time = 115))

ssh_far_master <- abind(sshf1, sshf2, sshf3, sshf4, sshf5, along = 3)

dim(ssh_far_master)
ssh_far_master_mean <- apply(ssh_far_master, c(1:2), mean)
dim(ssh_far_master_mean)


plot(ssh_far_master_mean)
class(ssh_far_master_mean)


ssh_far_master_raster <- ssh_far_master_mean %>% 
  raster() %>% 
  t() %>% 
  flip(direction = 'y')

print(ssh_far_master_raster)
plot(ssh_far_master_raster)

###SET EXTENT#####

Caribbean.extent <- extent(-180, -45, -3, 37)

extent(ssh_far_master_raster) <- Caribbean.extent

ssh_far_master_raster2 <- setExtent(ssh_far_master_raster, Caribbean.extent, keepres = TRUE, snap = TRUE)

print(ssh_far_master_raster2)
plot(ssh_far_master_raster2)

###RESAMPLE###

resampleFactor <- 3.375 #increase the row(cell) size by 3.375% and reduce the number of rows from 135 to 40
ssh_far_master_inputRaster <- ssh_far_master_raster2
inCols <- ncol(ssh_far_master_inputRaster)
inCols
inRows <- nrow(ssh_far_master_inputRaster)
inRows
ssh_far_master_resampledRaster <- raster(ncol=(inCols/1), nrow=(inRows/resampleFactor))
extent(ssh_far_master_resampledRaster) <- extent(ssh_far_master_inputRaster)


ssh_far_master_resampledRaster <- resample(ssh_far_master_inputRaster,ssh_far_master_resampledRaster,method='bilinear',filename="ssh_far.tif",overwrite=TRUE)

print(ssh_far_master_resampledRaster)
plot(ssh_far_master_resampledRaster)

##############

##SSH Future (f) 2090 to 2100##
ssh_f1 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_SSH/Future/b.e11.BRCP85C5CNBDRD.f09_g16.001.pop.h.SSH.208101-210012.nc")
ssh_f2 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_SSH/Future/b.e11.BRCP85C5CNBDRD.f09_g16.002.pop.h.SSH.208101-210012.nc")
ssh_f3 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_SSH/Future/b.e11.BRCP85C5CNBDRD.f09_g16.009.pop.h.SSH.208101-210012.nc")
ssh_f4 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_SSH/Future/b.e11.BRCP85C5CNBDRD.f09_g16.010.pop.h.SSH.208101-210012.nc")
ssh_f5 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_SSH/Future/b.e11.BRCP85C5CNBDRD.f09_g16.011.pop.h.SSH.208101-210012.nc")

#spotcheck 

ssh_f1[["filename"]]
ssh_f2[["filename"]]
ssh_f3[["filename"]]
ssh_f4[["filename"]]
ssh_f5[["filename"]]


#float PAR_avg[nlon,nlat,z_t_150m,time]   (Chunking: [160,192,8,1])  (Compression: shuffle,level 1)
#          long_name: PAR Average over Model Cell
#         units: w/m^2
#        coordinates: TLONG TLAT z_t_150m time
#       grid_loc: 3114
#      cell_methods: time: mean
#     _FillValue: 9.96920996838687e+36
#    missing_value: 9.96920996838687e+36


#pull average for location and time series of interest

sshf1 <- ncvar_get(ssh_f1, varid = "SSH", 
                   start = c( 185, 175, time = 125),
                   count = c( 135, 135, time = 116))


sshf2 <- ncvar_get(ssh_f2, varid = "SSH", 
                   start = c( 185, 175, time = 125),
                   count = c( 135, 135, time = 116))

sshf3 <- ncvar_get(ssh_f3, varid = "SSH", 
                   start = c( 185, 175, time = 125),
                   count = c( 135, 135, time = 116))

sshf4 <- ncvar_get(ssh_f4, varid = "SSH", 
                   start = c( 185, 175, time = 125),
                   count = c( 135, 135, time = 116))

sshf5 <- ncvar_get(ssh_f5, varid = "SSH", 
                   start = c( 185, 175, time = 125),
                   count = c( 135, 135, time = 116))

#merge

ssh_future_master <- abind(sshf1, sshf2, sshf3, sshf4, sshf5, along = 3)

dim(ssh_future_master)
ssh_future_master_mean <- apply(ssh_future_master, c(1:2), mean)
dim(ssh_future_master_mean)


plot(ssh_future_master_mean)
class(ssh_future_master_mean)


ssh_future_master_raster <- ssh_future_master_mean %>% 
  raster() %>% 
  t() %>% 
  flip(direction = 'y')

print(ssh_future_master_raster)
plot(ssh_future_master_raster)


###SET EXTENT#####

Caribbean.extent <- extent(-179, -44, -3, 37)

extent(ssh_future_master_raster) <- Caribbean.extent

ssh_future_master_raster2 <- setExtent(ssh_future_master_raster, Caribbean.extent, keepres = TRUE, snap = TRUE)

print(ssh_future_master_raster2)
plot(ssh_future_master_raster2)


###RESAMPLE###

resampleFactor <- 3.375 #increase the row(cell) size by 3.375% and reduce the number of rows from 135 to 40
ssh_future_master_inputRaster <- ssh_future_master_raster2
inCols <- ncol(ssh_future_master_inputRaster)
inCols
inRows <- nrow(ssh_future_master_inputRaster)
inRows
ssh_future_master_resampledRaster <- raster(ncol=(inCols/1), nrow=(inRows/resampleFactor))
extent(ssh_future_master_resampledRaster) <- extent(ssh_future_master_inputRaster)


ssh_future_master_resampledRaster <- resample(ssh_future_master_inputRaster,ssh_future_master_resampledRaster,method='bilinear',filename="ssh_future.tif",overwrite=TRUE)

print(ssh_future_master_resampledRaster)
plot(ssh_future_master_resampledRaster)

#######################################################################
#                               SST                                  #
#######################################################################

##SST NEAR###

#SST near (n) 2040 - 2050 Run # 1 - 5 

#Read in ncdf4 files

sst_n1 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_SST/Near/b.e11.BRCP85C5CNBDRD.f09_g16.001.pop.h.SST.200601-208012.nc")
sst_n2 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_SST/Near/b.e11.BRCP85C5CNBDRD.f09_g16.002.pop.h.SST.200601-208012.nc")
sst_n3 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_SST/Near/b.e11.BRCP85C5CNBDRD.f09_g16.009.pop.h.SST.200601-208012.nc")
sst_n4 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_SST/Near/b.e11.BRCP85C5CNBDRD.f09_g16.010.pop.h.SST.200601-208012.nc")
sst_n5 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_SST/Near/b.e11.BRCP85C5CNBDRD.f09_g16.011.pop.h.SST.200601-208012.nc")

print(sst_n1)
#spotcheck 

sst_n1[["filename"]]
sst_n2[["filename"]]
sst_n3[["filename"]]
sst_n4[["filename"]]
sst_n5[["filename"]]

time <- ncvar_get(sst_n1, varid = "time")
as.Date(time, origin = "0000-01-01") #see time in class 'calendar'  
#424+120 i.e [424] = "2039-12-22"

#float PAR_avg[nlon,nlat,z_t_150m,time]   (Chunking: [160,192,8,1])  (Compression: shuffle,level 1)
#          long_name: PAR Average over Model Cell
#         units: w/m^2
#        coordinates: TLONG TLAT z_t_150m time
#       grid_loc: 3114
#      cell_methods: time: mean
#     _FillValue: 9.96920996838687e+36
#    missing_value: 9.96920996838687e+36

##########################################################################################
# CURRENT
##########################################################################################

sstc1 <- ncvar_get(sst_n1, varid = "SST", 
                   start = c( 185, 175, z_t = 1, time = 40),
                   count = c( 135, 135, z_t = 1, time = 120))

sstc2 <- ncvar_get(sst_n2, varid = "SST", 
                   start = c( 185, 175, z_t = 1, time = 40),
                   count = c( 135, 135, z_t = 1, time = 120))

sstc3 <- ncvar_get(sst_n3, varid = "SST", 
                   start = c( 185, 175, z_t = 1, time = 40),
                   count = c( 135, 135, z_t = 1, time = 120))

sstc4 <- ncvar_get(sst_n4, varid = "SST", 
                   start = c( 185, 175, z_t = 1, time = 40),
                   count = c( 135, 135, z_t = 1, time = 120))

sstc5 <- ncvar_get(sst_n5, varid = "SST", 
                   start = c( 185, 175, z_t = 1, time = 40),
                   count = c( 135, 135, z_t = 1, time = 120))


sst_current_master <- abind(sstc1, sstc2, sstc3, sstc4, sstc5, along = 3)

dim(sst_current_master)
sst_current_master_mean <- apply(sst_current_master, c(1:2), mean)
dim(sst_current_master_mean)


plot(sst_current_master_mean)
class(sst_current_master_mean)


sst_current_master_raster <- sst_current_master_mean %>% 
  raster() %>% 
  t() %>% 
  flip(direction = 'y')

print(sst_current_master_raster)
plot(sst_current_master_raster)

###SET EXTENT#####

Caribbean.extent <- extent(-180, -45, -3, 37)

extent(sst_current_master_raster) <- Caribbean.extent

sst_current_master_raster2 <- setExtent(sst_current_master_raster, Caribbean.extent, keepres = TRUE, snap = TRUE)

print(sst_current_master_raster2)
plot(sst_current_master_raster2)

###RESAMPLE###

resampleFactor <- 3.375 #increase the row(cell) size by 3.375% and reduce the number of rows from 135 to 40
sst_current_master_inputRaster <- sst_current_master_raster2
inCols <- ncol(sst_current_master_inputRaster)
inCols
inRows <- nrow(sst_current_master_inputRaster)
inRows
sst_current_master_resampledRaster <- raster(ncol=(inCols/1), nrow=(inRows/resampleFactor))
extent(sst_current_master_resampledRaster) <- extent(sst_current_master_inputRaster)


sst_current_master_resampledRaster <- resample(sst_current_master_inputRaster,sst_current_master_resampledRaster,method='bilinear',filename="sst_current.tif",overwrite=TRUE)

print(sst_current_master_resampledRaster)
plot(sst_current_master_resampledRaster)

##########################################################################################
# NEAR 2040 - 2050
##########################################################################################

sstn1 <- ncvar_get(sst_n1, varid = "SST", 
                   start = c( 185, 175, z_t = 1, time = 424),
                   count = c( 135, 135, z_t = 1, time = 120))

sstn2 <- ncvar_get(sst_n2, varid = "SST", 
                   start = c( 185, 175, z_t = 1, time = 424),
                   count = c( 135, 135, z_t = 1, time = 120))

sstn3 <- ncvar_get(sst_n3, varid = "SST", 
                   start = c( 185, 175, z_t = 1, time = 424),
                   count = c( 135, 135, z_t = 1, time = 120))

sstn4 <- ncvar_get(sst_n4, varid = "SST", 
                   start = c( 185, 175, z_t = 1, time = 424),
                   count = c( 135, 135, z_t = 1, time = 120))

sstn5 <- ncvar_get(sst_n5, varid = "SST", 
                   start = c( 185, 175, z_t = 1, time = 424),
                   count = c( 135, 135, z_t = 1, time = 120))


sst_near_master <- abind(sstn1, sstn2, sstn3, sstn4, sstn5, along = 3)

dim(sst_near_master)
sst_near_master_mean <- apply(sst_near_master, c(1:2), mean)
dim(sst_near_master_mean)


plot(sst_near_master_mean)
class(sst_near_master_mean)


sst_near_master_raster <- sst_near_master_mean %>% 
  raster() %>% 
  t() %>% 
  flip(direction = 'y')

print(sst_near_master_raster)
plot(sst_near_master_raster)

###SET EXTENT#####

Caribbean.extent <- extent(-180, -45, -3, 37)

extent(sst_near_master_raster) <- Caribbean.extent

sst_near_master_raster2 <- setExtent(sst_near_master_raster, Caribbean.extent, keepres = TRUE, snap = TRUE)

print(sst_near_master_raster2)
plot(sst_near_master_raster2)

###RESAMPLE###

resampleFactor <- 3.375 #increase the row(cell) size by 3.375% and reduce the number of rows from 135 to 40
sst_near_master_inputRaster <- sst_near_master_raster2
inCols <- ncol(sst_near_master_inputRaster)
inCols
inRows <- nrow(sst_near_master_inputRaster)
inRows
sst_near_master_resampledRaster <- raster(ncol=(inCols/1), nrow=(inRows/resampleFactor))
extent(sst_near_master_resampledRaster) <- extent(sst_near_master_inputRaster)


sst_near_master_resampledRaster <- resample(sst_near_master_inputRaster,sst_near_master_resampledRaster,method='bilinear',filename="sst_near.tif",overwrite=TRUE)

print(sst_near_master_resampledRaster)
plot(sst_near_master_resampledRaster)

##########################################################################################
# FAR 2070 - 2080
##########################################################################################

sstf1 <- ncvar_get(sst_n1, varid = "SST", 
                   start = c( 185, 175, z_t = 1, time = 785),
                   count = c( 135, 135, z_t = 1, time = 115))

sstf2 <- ncvar_get(sst_n2, varid = "SST", 
                   start = c( 185, 175, z_t = 1, time = 785),
                   count = c( 135, 135, z_t = 1, time = 115))

sstf3 <- ncvar_get(sst_n3, varid = "SST", 
                   start = c( 185, 175, z_t = 1, time = 785),
                   count = c( 135, 135, z_t = 1, time = 115))

sstf4 <- ncvar_get(sst_n4, varid = "SST", 
                   start = c( 185, 175, z_t = 1, time = 785),
                   count = c( 135, 135, z_t = 1, time = 115))

sstf5 <- ncvar_get(sst_n5, varid = "SST", 
                   start = c( 185, 175, z_t = 1, time = 785),
                   count = c( 135, 135, z_t = 1, time = 115))


sst_far_master <- abind(sstf1, sstf2, sstf3, sstf4, sstf5, along = 3)

dim(sst_far_master)
sst_far_master_mean <- apply(sst_far_master, c(1:2), mean)
dim(sst_far_master_mean)


plot(sst_far_master_mean)
class(sst_far_master_mean)


sst_far_master_raster <- sst_far_master_mean %>% 
  raster() %>% 
  t() %>% 
  flip(direction = 'y')

print(sst_far_master_raster)
plot(sst_far_master_raster)

###SET EXTENT#####

Caribbean.extent <- extent(-180, -45, -3, 37)

extent(sst_far_master_raster) <- Caribbean.extent

sst_far_master_raster2 <- setExtent(sst_far_master_raster, Caribbean.extent, keepres = TRUE, snap = TRUE)

print(sst_far_master_raster2)
plot(sst_far_master_raster2)

###RESAMPLE###

resampleFactor <- 3.375 #increase the row(cell) size by 3.375% and reduce the number of rows from 135 to 40
sst_far_master_inputRaster <- sst_far_master_raster2
inCols <- ncol(sst_far_master_inputRaster)
inCols
inRows <- nrow(sst_far_master_inputRaster)
inRows
sst_far_master_resampledRaster <- raster(ncol=(inCols/1), nrow=(inRows/resampleFactor))
extent(sst_far_master_resampledRaster) <- extent(sst_far_master_inputRaster)


sst_far_master_resampledRaster <- resample(sst_far_master_inputRaster,sst_far_master_resampledRaster,method='bilinear',filename="sst_far.tif",overwrite=TRUE)

print(sst_far_master_resampledRaster)
plot(sst_far_master_resampledRaster)


##############

#SST Future (f) 2090 to 2100##
sst_f1 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_SST/Future/b.e11.BRCP85C5CNBDRD.f09_g16.001.pop.h.SST.208101-210012.nc")
sst_f2 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_SST/Future/b.e11.BRCP85C5CNBDRD.f09_g16.002.pop.h.SST.208101-210012.nc")
sst_f3 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_SST/Future/b.e11.BRCP85C5CNBDRD.f09_g16.009.pop.h.SST.208101-210012.nc")
sst_f4 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_SST/Future/b.e11.BRCP85C5CNBDRD.f09_g16.010.pop.h.SST.208101-210012.nc")
sst_f5 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_SST/Future/b.e11.BRCP85C5CNBDRD.f09_g16.011.pop.h.SST.208101-210012.nc")

#spotcheck 

sst_f1[["filename"]]
sst_f2[["filename"]]
sst_f3[["filename"]]
sst_f4[["filename"]]
sst_f5[["filename"]]


#float PAR_avg[nlon,nlat,z_t_150m,time]   (Chunking: [160,192,8,1])  (Compression: shuffle,level 1)
#          long_name: PAR Average over Model Cell
#         units: w/m^2
#        coordinates: TLONG TLAT z_t_150m time
#       grid_loc: 3114
#      cell_methods: time: mean
#     _FillValue: 9.96920996838687e+36
#    missing_value: 9.96920996838687e+36


#pull average for location and time series of interest

sstf1 <- ncvar_get(sst_f1, varid = "SST", 
                   start = c( 185, 175, z_t = 1, time = 125),
                   count = c( 135, 135, z_t = 1, time = 116))


sstf2 <- ncvar_get(sst_f2, varid = "SST", 
                   start = c( 185, 175, z_t = 1, time = 125),
                   count = c( 135, 135, z_t = 1, time = 116))

sstf3 <- ncvar_get(sst_f3, varid = "SST", 
                   start = c( 185, 175, z_t = 1, time = 125),
                   count = c( 135, 135, z_t = 1, time = 116))

sstf4 <- ncvar_get(sst_f4, varid = "SST", 
                   start = c( 185, 175, z_t = 1, time = 125),
                   count = c( 135, 135, z_t = 1, time = 116))

sstf5 <- ncvar_get(sst_f5, varid = "SST", 
                   start = c( 185, 175, z_t = 1, time = 125),
                   count = c( 135, 135, z_t = 1, time = 116))

#merge

sst_future_master <- abind(sstf1, sstf2, sstf3, sstf4, sstf5, along = 3)

dim(sst_future_master)
sst_future_master_mean <- apply(sst_future_master, c(1:2), mean)
dim(sst_future_master_mean)


plot(sst_future_master_mean)
class(sst_future_master_mean)


sst_future_master_raster <- sst_future_master_mean %>% 
  raster() %>% 
  t() %>% 
  flip(direction = 'y')

print(sst_future_master_raster)
plot(sst_future_master_raster)


###SET EXTENT#####

Caribbean.extent <- extent(-179, -44, -3, 37)

extent(sst_future_master_raster) <- Caribbean.extent

sst_future_master_raster2 <- setExtent(sst_future_master_raster, Caribbean.extent, keepres = TRUE, snap = TRUE)

print(sst_future_master_raster2)
plot(sst_future_master_raster2)


###RESAMPLE###

resampleFactor <- 3.375 #increase the row(cell) size by 3.375% and reduce the number of rows from 135 to 40
sst_future_master_inputRaster <- sst_future_master_raster2
inCols <- ncol(sst_future_master_inputRaster)
inCols
inRows <- nrow(sst_future_master_inputRaster)
inRows
sst_future_master_resampledRaster <- raster(ncol=(inCols/1), nrow=(inRows/resampleFactor))
extent(sst_future_master_resampledRaster) <- extent(sst_future_master_inputRaster)


sst_future_master_resampledRaster <- resample(sst_future_master_inputRaster,sst_future_master_resampledRaster,method='bilinear',filename="sst_future.tif",overwrite=TRUE)

print(sst_future_master_resampledRaster)
plot(sst_future_master_resampledRaster)


######################################################################
# SPEED (sqrt(UVEL^2 + VVEL^2))
######################################################################

?overlay

#######################################################################
#                               UVEL                                  #
#######################################################################

##UVEL NEAR###

#UVEL near (n) 2040 - 2050 Run # 1 - 5 

#Read in ncdf4 files

uvel_n1 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_UVEL/Near/b.e11.BRCP85C5CNBDRD.f09_g16.001.pop.h.UVEL.200601-208012.nc")
uvel_n2 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_UVEL/Near/b.e11.BRCP85C5CNBDRD.f09_g16.002.pop.h.UVEL.200601-208012.nc")
uvel_n3 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_UVEL/Near/b.e11.BRCP85C5CNBDRD.f09_g16.009.pop.h.UVEL.200601-208012.nc")
uvel_n4 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_UVEL/Near/b.e11.BRCP85C5CNBDRD.f09_g16.010.pop.h.UVEL.200601-208012.nc")
uvel_n5 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_UVEL/Near/b.e11.BRCP85C5CNBDRD.f09_g16.011.pop.h.UVEL.200601-208012.nc")

print(uvel_n1)
#spotcheck 

sst_n1[["filename"]]
sst_n2[["filename"]]
sst_n3[["filename"]]
sst_n4[["filename"]]
sst_n5[["filename"]]

time <- ncvar_get(sst_n1, varid = "time")
as.Date(time, origin = "0000-01-01") #see time in class 'calendar'  
#424+120 i.e [424] = "2039-12-22"

#float PAR_avg[nlon,nlat,z_t_150m,time]   (Chunking: [160,192,8,1])  (Compression: shuffle,level 1)
#          long_name: PAR Average over Model Cell
#         units: w/m^2
#        coordinates: TLONG TLAT z_t_150m time
#       grid_loc: 3114
#      cell_methods: time: mean
#     _FillValue: 9.96920996838687e+36
#    missing_value: 9.96920996838687e+36

##########################################################################################
# CURRENT
##########################################################################################

uvelc1 <- ncvar_get(uvel_n1, varid = "UVEL", 
                   start = c( 185, 175, z_t = 1, time = 40),
                   count = c( 135, 135, z_t = 5, time = 120))

uvelc2 <- ncvar_get(uvel_n2, varid = "UVEL", 
                   start = c( 185, 175, z_t = 1, time = 40),
                   count = c( 135, 135, z_t = 5, time = 120))

uvelc3 <- ncvar_get(uvel_n3, varid = "UVEL", 
                   start = c( 185, 175, z_t = 1, time = 40),
                   count = c( 135, 135, z_t = 5, time = 120))

uvelc4 <- ncvar_get(uvel_n4, varid = "UVEL", 
                   start = c( 185, 175, z_t = 1, time = 40),
                   count = c( 135, 135, z_t = 5, time = 120))

uvelc5 <- ncvar_get(uvel_n5, varid = "UVEL", 
                   start = c( 185, 175, z_t = 1, time = 40),
                   count = c( 135, 135, z_t = 5, time = 120))

print(uvelc1)


uvel_current_master <- abind(uvelc1, uvelc2, uvelc3, uvelc4, uvelc5, along = 3)

dim(uvel_current_master)
uvel_current_master_mean <- apply(uvel_current_master, c(1:2), mean)
dim(uvel_current_master_mean)


plot(uvel_current_master_mean)
class(uvel_current_master_mean)


uvel_current_master_raster <- uvel_current_master_mean %>% 
  raster() %>% 
  t() %>% 
  flip(direction = 'y')

print(uvel_current_master_raster)
plot(uvel_current_master_raster)

###SET EXTENT#####

Caribbean.extent <- extent(-180, -45, -3, 37)

extent(uvel_current_master_raster) <- Caribbean.extent

uvel_current_master_raster2 <- setExtent(uvel_current_master_raster, Caribbean.extent, keepres = TRUE, snap = TRUE)

print(uvel_current_master_raster2)
plot(uvel_current_master_raster2)

###RESAMPLE###

resampleFactor <- 3.375 #increase the row(cell) size by 3.375% and reduce the number of rows from 135 to 40
uvel_current_master_inputRaster <- uvel_current_master_raster2
inCols <- ncol(uvel_current_master_inputRaster)
inCols
inRows <- nrow(uvel_current_master_inputRaster)
inRows
uvel_current_master_resampledRaster <- raster(ncol=(inCols/1), nrow=(inRows/resampleFactor))
extent(uvel_current_master_resampledRaster) <- extent(uvel_current_master_inputRaster)


uvel_current_master_resampledRaster <- resample(uvel_current_master_inputRaster,uvel_current_master_resampledRaster,method='bilinear',filename="uvel_current.tif",overwrite=TRUE)

print(uvel_current_master_resampledRaster)
plot(uvel_current_master_resampledRaster)

##########################################################################################
# NEAR 2040 - 2050
##########################################################################################

uveln1 <- ncvar_get(uvel_n1, varid = "UVEL", 
                   start = c( 185, 175, z_t = 4, time = 424),
                   count = c( 135, 135, z_t = 1, time = 120))

uveln2 <- ncvar_get(uvel_n2, varid = "UVEL", 
                   start = c( 185, 175, z_t = 4, time = 424),
                   count = c( 135, 135, z_t = 1, time = 120))

uveln3 <- ncvar_get(uvel_n3, varid = "UVEL", 
                   start = c( 185, 175, z_t = 4, time = 424),
                   count = c( 135, 135, z_t = 1, time = 120))

uveln4 <- ncvar_get(uvel_n4, varid = "UVEL", 
                   start = c( 185, 175, z_t = 4, time = 424),
                   count = c( 135, 135, z_t = 1, time = 120))

uveln5 <- ncvar_get(uvel_n5, varid = "UVEL", 
                   start = c( 185, 175, z_t = 4, time = 424),
                   count = c( 135, 135, z_t = 1, time = 120))


uvel_near_master <- abind(uveln1, uveln2, uveln3, uveln4, uveln5, along = 3)

dim(uvel_near_master)
uvel_near_master_mean <- apply(uvel_near_master, c(1:2), mean)
dim(uvel_near_master_mean)


plot(uvel_near_master_mean)
class(uvel_near_master_mean)


uvel_near_master_raster <- uvel_near_master_mean %>% 
  raster() %>% 
  t() %>% 
  flip(direction = 'y')

print(uvel_near_master_raster)
plot(uvel_near_master_raster)

###SET EXTENT#####

Caribbean.extent <- extent(-180, -45, -3, 37)

extent(uvel_near_master_raster) <- Caribbean.extent

uvel_near_master_raster2 <- setExtent(uvel_near_master_raster, Caribbean.extent, keepres = TRUE, snap = TRUE)

print(uvel_near_master_raster2)
plot(uvel_near_master_raster2)

###RESAMPLE###

resampleFactor <- 3.375 #increase the row(cell) size by 3.375% and reduce the number of rows from 135 to 40
uvel_near_master_inputRaster <- uvel_near_master_raster2
inCols <- ncol(uvel_near_master_inputRaster)
inCols
inRows <- nrow(uvel_near_master_inputRaster)
inRows
uvel_near_master_resampledRaster <- raster(ncol=(inCols/1), nrow=(inRows/resampleFactor))
extent(uvel_near_master_resampledRaster) <- extent(uvel_near_master_inputRaster)


uvel_near_master_resampledRaster <- resample(uvel_near_master_inputRaster, uvel_near_master_resampledRaster,method='bilinear',filename="uvel_near.tif",overwrite=TRUE)

print(uvel_near_master_resampledRaster)
plot(uvel_near_master_resampledRaster)


##########################################################################################
#FAR 2070 - 2080
##########################################################################################

uvelf1 <- ncvar_get(uvel_n1, varid = "UVEL", 
                    start = c( 185, 175, z_t = 4, time = 785),
                    count = c( 135, 135, z_t = 1, time = 115))

uvelf2 <- ncvar_get(uvel_n2, varid = "UVEL", 
                    start = c( 185, 175, z_t = 4, time = 785),
                    count = c( 135, 135, z_t = 1, time = 115))

uvelf3 <- ncvar_get(uvel_n3, varid = "UVEL", 
                    start = c( 185, 175, z_t = 4, time = 785),
                    count = c( 135, 135, z_t = 1, time = 115))

uvelf4 <- ncvar_get(uvel_n4, varid = "UVEL", 
                    start = c( 185, 175, z_t = 4, time = 785),
                    count = c( 135, 135, z_t = 1, time = 115))

uvelf5 <- ncvar_get(uvel_n5, varid = "UVEL", 
                    start = c( 185, 175, z_t = 4, time = 785),
                    count = c( 135, 135, z_t = 1, time = 115))


uvel_far_master <- abind(uvelf1, uvelf2, uvelf3, uvelf4, uvelf5, along = 3)

dim(uvel_far_master)
uvel_far_master_mean <- apply(uvel_far_master, c(1:2), mean)
dim(uvel_far_master_mean)


plot(uvel_far_master_mean)
class(uvel_far_master_mean)


uvel_far_master_raster <- uvel_far_master_mean %>% 
  raster() %>% 
  t() %>% 
  flip(direction = 'y')

print(uvel_far_master_raster)
plot(uvel_far_master_raster)

###SET EXTENT#####

Caribbean.extent <- extent(-180, -45, -3, 37)

extent(uvel_far_master_raster) <- Caribbean.extent

uvel_far_master_raster2 <- setExtent(uvel_far_master_raster, Caribbean.extent, keepres = TRUE, snap = TRUE)

print(uvel_far_master_raster2)
plot(uvel_far_master_raster2)

###RESAMPLE###

resampleFactor <- 3.375 #increase the row(cell) size by 3.375% and reduce the number of rows from 135 to 40
uvel_far_master_inputRaster <- uvel_far_master_raster2
inCols <- ncol(uvel_far_master_inputRaster)
inCols
inRows <- nrow(uvel_far_master_inputRaster)
inRows
uvel_far_master_resampledRaster <- raster(ncol=(inCols/1), nrow=(inRows/resampleFactor))
extent(uvel_far_master_resampledRaster) <- extent(uvel_far_master_inputRaster)


uvel_far_master_resampledRaster <- resample(uvel_far_master_inputRaster, uvel_far_master_resampledRaster,method='bilinear',filename="uvel_far.tif",overwrite=TRUE)

print(uvel_far_master_resampledRaster)
plot(uvel_far_master_resampledRaster)

##############

#uvel Future (f) 2090 to 2100##
uvel_f1 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_UVEL/Future/b.e11.BRCP85C5CNBDRD.f09_g16.001.pop.h.UVEL.208101-210012.nc")
uvel_f2 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_UVEL/Future/b.e11.BRCP85C5CNBDRD.f09_g16.002.pop.h.UVEL.208101-210012.nc")
uvel_f3 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_UVEL/Future/b.e11.BRCP85C5CNBDRD.f09_g16.009.pop.h.UVEL.208101-210012.nc")
uvel_f4 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_UVEL/Future/b.e11.BRCP85C5CNBDRD.f09_g16.010.pop.h.UVEL.208101-210012.nc")
uvel_f5 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_UVEL/Future/b.e11.BRCP85C5CNBDRD.f09_g16.011.pop.h.UVEL.208101-210012.nc")

#spotcheck 


#float PAR_avg[nlon,nlat,z_t_150m,time]   (Chunking: [160,192,8,1])  (Compression: shuffle,level 1)
#          long_name: PAR Average over Model Cell
#         units: w/m^2
#        coordinates: TLONG TLAT z_t_150m time
#       grid_loc: 3114
#      cell_methods: time: mean
#     _FillValue: 9.96920996838687e+36
#    missing_value: 9.96920996838687e+36


#pull average for location and time series of interest

uvelf1 <- ncvar_get(uvel_f1, varid = "UVEL", 
                   start = c( 185, 175, z_t = 4, time = 125),
                   count = c( 135, 135, z_t = 1, time = 116))


uvelf2 <- ncvar_get(uvel_f2, varid = "UVEL", 
                   start = c( 185, 175, z_t = 4, time = 125),
                   count = c( 135, 135, z_t = 1, time = 116))

uvelf3 <- ncvar_get(uvel_f3, varid = "UVEL", 
                   start = c( 185, 175, z_t = 4, time = 125),
                   count = c( 135, 135, z_t = 1, time = 116))

uvelf4 <- ncvar_get(uvel_f4, varid = "UVEL", 
                   start = c( 185, 175, z_t = 4, time = 125),
                   count = c( 135, 135, z_t = 1, time = 116))

uvelf5 <- ncvar_get(uvel_f5, varid = "UVEL", 
                   start = c( 185, 175, z_t = 4, time = 125),
                   count = c( 135, 135, z_t = 1, time = 116))

#merge

uvel_future_master <- abind(uvelf1, uvelf2, uvelf3, uvelf4, uvelf5, along = 3)

dim(uvel_future_master)
uvel_future_master_mean <- apply(uvel_future_master, c(1:2), mean)
dim(uvel_future_master_mean)


plot(uvel_future_master_mean)
class(uvel_future_master_mean)


uvel_future_master_raster <- uvel_future_master_mean %>% 
  raster() %>% 
  t() %>% 
  flip(direction = 'y')

print(uvel_future_master_raster)
plot(uvel_future_master_raster)


###SET EXTENT#####

Caribbean.extent <- extent(-179, -44, -3, 37)

extent(uvel_future_master_raster) <- Caribbean.extent

uvel_future_master_raster2 <- setExtent(uvel_future_master_raster, Caribbean.extent, keepres = TRUE, snap = TRUE)

print(uvel_future_master_raster2)
plot(uvel_future_master_raster2)


###RESAMPLE###

resampleFactor <- 3.375 #increase the row(cell) size by 3.375% and reduce the number of rows from 135 to 40
uvel_future_master_inputRaster <- uvel_future_master_raster2
inCols <- ncol(uvel_future_master_inputRaster)
inCols
inRows <- nrow(uvel_future_master_inputRaster)
inRows
uvel_future_master_resampledRaster <- raster(ncol=(inCols/1), nrow=(inRows/resampleFactor))
extent(uvel_future_master_resampledRaster) <- extent(uvel_future_master_inputRaster)


uvel_future_master_resampledRaster <- resample(uvel_future_master_inputRaster,uvel_future_master_resampledRaster,method='bilinear',filename="uvel_future.tif",overwrite=TRUE)

print(uvel_future_master_resampledRaster)
plot(uvel_future_master_resampledRaster)


#######################################################################
#                               VVEL                                #
#######################################################################

##vVEL NEAR###

#vVEL near (n) 2040 - 2050 Run # 1 - 5 

#Read in ncdf4 files

vvel_n1 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_VVEL/Near/b.e11.BRCP85C5CNBDRD.f09_g16.001.pop.h.VVEL.200601-208012.nc")
vvel_n2 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_VVEL/Near/b.e11.BRCP85C5CNBDRD.f09_g16.002.pop.h.VVEL.200601-208012.nc")
vvel_n3 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_VVEL/Near/b.e11.BRCP85C5CNBDRD.f09_g16.009.pop.h.VVEL.200601-208012.nc")
vvel_n4 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_VVEL/Near/b.e11.BRCP85C5CNBDRD.f09_g16.010.pop.h.VVEL.200601-208012.nc")
vvel_n5 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_VVEL/Near/b.e11.BRCP85C5CNBDRD.f09_g16.011.pop.h.VVEL.200601-208012.nc")

print(vvel_n1)
#spotcheck 


time <- ncvar_get(sst_n1, varid = "time")
as.Date(time, origin = "0000-01-01") #see time in class 'calendar'  
#424+120 i.e [424] = "2039-12-22"

#float PAR_avg[nlon,nlat,z_t_150m,time]   (Chunking: [160,192,8,1])  (Compression: shuffle,level 1)
#          long_name: PAR Average over Model Cell
#         units: w/m^2
#        coordinates: TLONG TLAT z_t_150m time
#       grid_loc: 3114
#      cell_methods: time: mean
#     _FillValue: 9.96920996838687e+36
#    missing_value: 9.96920996838687e+36

##########################################################################################
# CURRENT
##########################################################################################

vvelc1 <- ncvar_get(vvel_n1, varid = "VVEL", 
                    start = c( 185, 175, z_t = 1, time = 40),
                    count = c( 135, 135, z_t = 5, time = 120))

vvelc2 <- ncvar_get(vvel_n2, varid = "VVEL", 
                    start = c( 185, 175, z_t = 1, time = 40),
                    count = c( 135, 135, z_t = 5, time = 120))

vvelc3 <- ncvar_get(vvel_n3, varid = "VVEL", 
                    start = c( 185, 175, z_t = 1, time = 40),
                    count = c( 135, 135, z_t = 5, time = 120))

vvelc4 <- ncvar_get(vvel_n4, varid = "VVEL", 
                    start = c( 185, 175, z_t = 1, time = 40),
                    count = c( 135, 135, z_t = 5, time = 120))

vvelc5 <- ncvar_get(vvel_n5, varid = "VVEL", 
                    start = c( 185, 175, z_t = 1, time = 40),
                    count = c( 135, 135, z_t = 5, time = 120))



vvel_current_master <- abind(vvelc1, vvelc2, vvelc3, vvelc4, vvelc5, along = 3)

dim(vvel_current_master)
vvel_current_master_mean <- apply(vvel_current_master, c(1:2), mean)
dim(vvel_current_master_mean)


plot(vvel_current_master_mean)
class(vvel_current_master_mean)


vvel_current_master_raster <- vvel_current_master_mean %>% 
  raster() %>% 
  t() %>% 
  flip(direction = 'y')

print(vvel_current_master_raster)
plot(vvel_current_master_raster)

###SET EXTENT#####

Caribbean.extent <- extent(-180, -45, -3, 37)

extent(vvel_current_master_raster) <- Caribbean.extent

vvel_current_master_raster2 <- setExtent(vvel_current_master_raster, Caribbean.extent, keepres = TRUE, snap = TRUE)

print(vvel_current_master_raster2)
plot(vvel_current_master_raster2)

###RESAMPLE###

resampleFactor <- 3.375 #increase the row(cell) size by 3.375% and reduce the number of rows from 135 to 40
vvel_current_master_inputRaster <- vvel_current_master_raster2
inCols <- ncol(vvel_current_master_inputRaster)
inCols
inRows <- nrow(vvel_current_master_inputRaster)
inRows
vvel_current_master_resampledRaster <- raster(ncol=(inCols/1), nrow=(inRows/resampleFactor))
extent(vvel_current_master_resampledRaster) <- extent(vvel_current_master_inputRaster)


vvel_current_master_resampledRaster <- resample(vvel_current_master_inputRaster,vvel_current_master_resampledRaster, 
                                                method='bilinear',filename="vvel_current.tif",overwrite=TRUE)

print(vvel_current_master_resampledRaster)
plot(vvel_current_master_resampledRaster)

##########################################################################################
# NEAR 2040 - 2050
##########################################################################################

vveln1 <- ncvar_get(vvel_n1, varid = "VVEL", 
                    start = c( 185, 175, z_t = 4, time = 424),
                    count = c( 135, 135, z_t = 1, time = 120))

vveln2 <- ncvar_get(vvel_n2, varid = "VVEL", 
                    start = c( 185, 175, z_t = 4, time = 424),
                    count = c( 135, 135, z_t = 1, time = 120))

vveln3 <- ncvar_get(vvel_n3, varid = "VVEL", 
                    start = c( 185, 175, z_t = 4, time = 424),
                    count = c( 135, 135, z_t = 1, time = 120))

vveln4 <- ncvar_get(vvel_n4, varid = "VVEL", 
                    start = c( 185, 175, z_t = 4, time = 424),
                    count = c( 135, 135, z_t = 1, time = 120))

vveln5 <- ncvar_get(vvel_n5, varid = "VVEL", 
                    start = c( 185, 175, z_t = 4, time = 424),
                    count = c( 135, 135, z_t = 1, time = 120))


vvel_near_master <- abind(vveln1, vveln2, vveln3, vveln4, vveln5, along = 3)

dim(vvel_near_master)
vvel_near_master_mean <- apply(vvel_near_master, c(1:2), mean)
dim(vvel_near_master_mean)


plot(vvel_near_master_mean)
class(vvel_near_master_mean)


vvel_near_master_raster <- vvel_near_master_mean %>% 
  raster() %>% 
  t() %>% 
  flip(direction = 'y')

print(vvel_near_master_raster)
plot(vvel_near_master_raster)

###SET EXTENT#####

Caribbean.extent <- extent(-180, -45, -3, 37)

extent(vvel_near_master_raster) <- Caribbean.extent

vvel_near_master_raster2 <- setExtent(vvel_near_master_raster, Caribbean.extent, keepres = TRUE, snap = TRUE)

print(vvel_near_master_raster2)
plot(vvel_near_master_raster2)

###RESAMPLE###

resampleFactor <- 3.375 #increase the row(cell) size by 3.375% and reduce the number of rows from 135 to 40
vvel_near_master_inputRaster <- vvel_near_master_raster2
inCols <- ncol(vvel_near_master_inputRaster)
inCols
inRows <- nrow(vvel_near_master_inputRaster)
inRows
vvel_near_master_resampledRaster <- raster(ncol=(inCols/1), nrow=(inRows/resampleFactor))
extent(vvel_near_master_resampledRaster) <- extent(vvel_near_master_inputRaster)


vvel_near_master_resampledRaster <- resample(vvel_near_master_inputRaster, vvel_near_master_resampledRaster,method='bilinear',filename="vvel_near.tif",overwrite=TRUE)

print(vvel_near_master_resampledRaster)
plot(vvel_near_master_resampledRaster)


##########################################################################################
# FAR 2070 - 2080
##########################################################################################

vvelf1 <- ncvar_get(vvel_n1, varid = "VVEL", 
                    start = c( 185, 175, z_t = 4, time = 785),
                    count = c( 135, 135, z_t = 1, time = 115))

vvelf2 <- ncvar_get(vvel_n2, varid = "VVEL", 
                    start = c( 185, 175, z_t = 4, time = 785),
                    count = c( 135, 135, z_t = 1, time = 115))

vvelf3 <- ncvar_get(vvel_n3, varid = "VVEL", 
                    start = c( 185, 175, z_t = 4, time = 785),
                    count = c( 135, 135, z_t = 1, time = 115))

vvelf4 <- ncvar_get(vvel_n4, varid = "VVEL", 
                    start = c( 185, 175, z_t = 4, time = 785),
                    count = c( 135, 135, z_t = 1, time = 115))

vvelf5 <- ncvar_get(vvel_n5, varid = "VVEL", 
                    start = c( 185, 175, z_t = 4, time = 785),
                    count = c( 135, 135, z_t = 1, time = 115))


vvel_far_master <- abind(vvelf1, vvelf2, vvelf3, vvelf4, vvelf5, along = 3)

dim(vvel_far_master)
vvel_far_master_mean <- apply(vvel_far_master, c(1:2), mean)
dim(vvel_far_master_mean)


plot(vvel_far_master_mean)
class(vvel_far_master_mean)


vvel_far_master_raster <- vvel_far_master_mean %>% 
  raster() %>% 
  t() %>% 
  flip(direction = 'y')

print(vvel_far_master_raster)
plot(vvel_far_master_raster)

###SET EXTENT#####

Caribbean.extent <- extent(-180, -45, -3, 37)

extent(vvel_far_master_raster) <- Caribbean.extent

vvel_far_master_raster2 <- setExtent(vvel_far_master_raster, Caribbean.extent, keepres = TRUE, snap = TRUE)

print(vvel_far_master_raster2)
plot(vvel_far_master_raster2)

###RESAMPLE###

resampleFactor <- 3.375 #increase the row(cell) size by 3.375% and reduce the number of rows from 135 to 40
vvel_far_master_inputRaster <- vvel_far_master_raster2
inCols <- ncol(vvel_far_master_inputRaster)
inCols
inRows <- nrow(vvel_far_master_inputRaster)
inRows
vvel_far_master_resampledRaster <- raster(ncol=(inCols/1), nrow=(inRows/resampleFactor))
extent(vvel_far_master_resampledRaster) <- extent(vvel_far_master_inputRaster)


vvel_far_master_resampledRaster <- resample(vvel_far_master_inputRaster, vvel_far_master_resampledRaster,method='bilinear',filename="vvel_far.tif",overwrite=TRUE)

print(vvel_far_master_resampledRaster)
plot(vvel_far_master_resampledRaster)

##############

#uvel Future (f) 2090 to 2100##
vvel_f1 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_VVEL/Future/b.e11.BRCP85C5CNBDRD.f09_g16.001.pop.h.VVEL.208101-210012.nc")
vvel_f2 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_VVEL/Future/b.e11.BRCP85C5CNBDRD.f09_g16.002.pop.h.VVEL.208101-210012.nc")
vvel_f3 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_VVEL/Future/b.e11.BRCP85C5CNBDRD.f09_g16.009.pop.h.VVEL.208101-210012.nc")
vvel_f4 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_VVEL/Future/b.e11.BRCP85C5CNBDRD.f09_g16.010.pop.h.VVEL.208101-210012.nc")
vvel_f5 <- nc_open("G:/Coral_Suitability/CESM_Variables/8.5Future_Climate_Models_CCSM/raw_data_VVEL/Future/b.e11.BRCP85C5CNBDRD.f09_g16.011.pop.h.VVEL.208101-210012.nc")

#spotcheck 



#float PAR_avg[nlon,nlat,z_t_150m,time]   (Chunking: [160,192,8,1])  (Compression: shuffle,level 1)
#          long_name: PAR Average over Model Cell
#         units: w/m^2
#        coordinates: TLONG TLAT z_t_150m time
#       grid_loc: 3114
#      cell_methods: time: mean
#     _FillValue: 9.96920996838687e+36
#    missing_value: 9.96920996838687e+36


#pull average for location and time series of interest

vvelf1 <- ncvar_get(vvel_f1, varid = "VVEL", 
                    start = c( 185, 175, z_t = 4, time = 125),
                    count = c( 135, 135, z_t = 1, time = 116))


vvelf2 <- ncvar_get(vvel_f2, varid = "VVEL", 
                    start = c( 185, 175, z_t = 4, time = 125),
                    count = c( 135, 135, z_t = 1, time = 116))

vvelf3 <- ncvar_get(vvel_f3, varid = "VVEL", 
                    start = c( 185, 175, z_t = 4, time = 125),
                    count = c( 135, 135, z_t = 1, time = 116))

vvelf4 <- ncvar_get(vvel_f4, varid = "VVEL", 
                    start = c( 185, 175, z_t = 4, time = 125),
                    count = c( 135, 135, z_t = 1, time = 116))

vvelf5 <- ncvar_get(vvel_f5, varid = "VVEL", 
                    start = c( 185, 175, z_t = 4, time = 125),
                    count = c( 135, 135, z_t = 1, time = 116))

#merge

vvel_future_master <- abind(vvelf1, vvelf2, vvelf3, vvelf4, vvelf5, along = 3)

dim(vvel_future_master)
vvel_future_master_mean <- apply(vvel_future_master, c(1:2), mean)
dim(vvel_future_master_mean)


plot(vvel_future_master_mean)
class(vvel_future_master_mean)


vvel_future_master_raster <- vvel_future_master_mean %>% 
  raster() %>% 
  t() %>% 
  flip(direction = 'y')

print(vvel_future_master_raster)
plot(vvel_future_master_raster)


###SET EXTENT#####

Caribbean.extent <- extent(-179, -44, -3, 37)

extent(vvel_future_master_raster) <- Caribbean.extent

vvel_future_master_raster2 <- setExtent(vvel_future_master_raster, Caribbean.extent, keepres = TRUE, snap = TRUE)

print(vvel_future_master_raster2)
plot(vvel_future_master_raster2)


###RESAMPLE###

resampleFactor <- 3.375 #increase the row(cell) size by 3.375% and reduce the number of rows from 135 to 40
vvel_future_master_inputRaster <- vvel_future_master_raster2
inCols <- ncol(vvel_future_master_inputRaster)
inCols
inRows <- nrow(vvel_future_master_inputRaster)
inRows
vvel_future_master_resampledRaster <- raster(ncol=(inCols/1), nrow=(inRows/resampleFactor))
extent(vvel_future_master_resampledRaster) <- extent(vvel_future_master_inputRaster)


vvel_future_master_resampledRaster <- resample(vvel_future_master_inputRaster,vvel_future_master_resampledRaster,method='bilinear',filename="vvel_future.tif",overwrite=TRUE)

print(vvel_future_master_resampledRaster)
plot(vvel_future_master_resampledRaster)

  
###SPEED####

#Current

speed_current <- overlay(uvel_current_master_resampledRaster, vvel_current_master_resampledRaster, fun = function(x,y){return(sqrt(x*x + y*y))})
plot(speed_current)
writeRaster(speed_current,'speed_current.tif', format = "GTiff", overwrite=TRUE) 

#Near

speed_near <- overlay(uvel_near_master_resampledRaster, vvel_near_master_resampledRaster, fun = function(x,y){return(sqrt(x*x + y*y))})
plot(speed_near)
writeRaster(speed_near,'speed_near.tif', format = "GTiff", overwrite=TRUE) 

#Far

speed_far <- overlay(uvel_far_master_resampledRaster, vvel_far_master_resampledRaster, fun = function(x,y){return(sqrt(x*x + y*y))})
plot(speed_far)
writeRaster(speed_far,'speed_far.tif', format = "GTiff", overwrite=TRUE)

#Future

speed_future <- overlay(uvel_future_master_resampledRaster, vvel_future_master_resampledRaster, fun = function(x,y){return(sqrt(x*x + y*y))})
plot(speed_future)
writeRaster(speed_future,'speed_future.tif', format = "GTiff", overwrite=TRUE) 

print(speed_current)
print(speed_near)
print(speed_future)