#install the required packages
#install.packages(c("SP","rgdal","raster","ggplot2","viridis","rasterVis"))
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("sf")
library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(viridis)
library(rasterVis)
library(sf)
library(dplyr)


#load the data
Image<-raster("resampled_image.tif") # raster function is use t0 import single band image
#but raster()also works
#Image<- stack("re sampled_image.tif") # use stack() to import multiple and image
#Image


#load boundary of study area
boundary<- readOGR("polygon.shp")


#creating an individual raster layers for each of the spectral bands

b1<- raster("resampled_image.tif", band=1)
plot(b1)
summary(b1)
b1/10000



b2<- raster("resampled_image.tif", band=2)
b3<- raster("resampled_image.tif", band=3)
b4<- raster("resampled_image.tif", band=4)
b5<- raster("b5_june_26.tiff.tif")# band 5 was imported directly re sampled in snap, even the resoln is same but
#the extent of b5 is different with other band, we will make it same after we crop and mask it
b5
b6<- raster("resampled_image.tif", band=6)
b7<- raster("resampled_image.tif", band=7)
b8<- raster("b8.tiff.tif")
b8a<-raster("b8a.tiff.tif")
b9<- raster("resampled_image.tif", band=9)
b10<- raster("resampled_image.tif", band=10)
b11<- raster("resampled_image.tif", band=11)
b12<- raster("resampled_image.tif", band=12)

#compare the raster
compareRaster(b2,b3)
compareRaster(b9,b2)

#####
#lets crops and mask our study area for each bands

SA_crop1<- crop(b1,boundary)
SA_b1<- mask(SA_crop1, boundary)
SA_b1_scaled<- SA_b1/10000 # lets re scale the image by scaling factor 10000
hist(SA_b1_scaled)
SA_b1_scaled

SA_crop2<- crop(b2,boundary)
SA_b2<- mask(SA_crop2, boundary)
SA_b2_scaled<-SA_b2/10000
SA_b2_scaled

SA_crop3<- crop(b3,boundary)
SA_b3<- mask(SA_crop3, boundary)
SA_b3_scaled<- SA_b3/10000

SA_crop4<- crop(b4,boundary)
SA_b4<- mask(SA_crop4, boundary)
SA_b4_scaled<-SA_b4/10000

SA_crop5<- crop(b5,boundary)
SA_b5<- mask(SA_crop5, boundary)
SA_b5_scaled<- SA_b5/10000
SA_b5_scaled

#compare Raster(SA_b6, SA_b5)

SA_crop6<- crop(b6,boundary)
SA_b6<- mask(SA_crop6, boundary)
SA_b6_scaled<- SA_b6/10000

SA_crop7<- crop(b7,boundary)
SA_b7<- mask(SA_crop7, boundary)
SA_b7_scaled<- SA_b7/10000
SA_b7_scaled

SA_crop8<- crop(b8,boundary)
SA_b8<- mask(SA_crop8, boundary)
SA_b8_scaled<- SA_b8/10000
SA_b8_scaled

SA_crop8a<- crop(b8a, boundary)
SA_b8a<- mask(SA_crop8a, boundary)
SA_b8a_scaled<- SA_b8a/10000
SA_b8a_scaled

SA_crop9<- crop(b9,boundary)
SA_b9<- mask(SA_crop9, boundary)
SA_b9_scaled<- SA_b9/10000
SA_b9_scaled

SA_crop10<- crop(b10,boundary)
SA_b10<- mask(SA_crop10, boundary)
SA_b10_scaled<- SA_b10/10000
SA_b10_scaled

SA_crop11<- crop(b11,boundary)
SA_b11<- mask(SA_crop11, boundary)
SA_b11_scaled<- SA_b11/10000
SA_b11_scaled
plot(SA_b11_scaled)

SA_crop12<- crop(b12,boundary)
SA_b12<- mask(SA_crop12, boundary)
SA_b12_scaled<- SA_b12/10000
SA_b12_scaled

###### just practice#######

##Plotting 
plot(SA_b11)
image(SA_b11) # it stretches the image

#Visualize spectral bands
png("band11.png", width=4,height= 4, units="in", res=300) # save in local device
plot(SA_b11, col=viridis_pal(option="D")(10), main="Sentinel 2 Image band 11") #show in r only
dev.off()


#create RGB Image of the study area

RGB_image<- stack(list(b4,b3,b2))
plotRGB(RGB_image, axes= TRUE, stretch="lin", main= "RGB Composite of Study Area")

# create false color composite

FCC_image<- stack(list(b8,b4,b3))
plotRGB(FCC_image, axes=TRUE, stretch="lin", main=" FCC composite of Study area")

#visualize using ggplot2

gplot(b3) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c() +
  coord_quickmap() +
  ggtitle("study area") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic() +   					   
  theme(plot.title = element_text(hjust = 0.5),             
        text = element_text(size=20),		       	   
        axis.text.x = element_text(angle = 90, hjust = 1)) 


#visualize all the bands together using facet
band_stack<- stack(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12)
gplot(band_stack) +
  geom_raster(aes(x = x, y = y, fill = value))+
  scale_fill_viridis_c() +
  facet_wrap(~variable) +
  coord_quickmap()+
  ggtitle("Sentinel 2 study area, raster plots") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic() +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))
# use brick function to display image 

imagebrick<- brick("Resampled_image.tif")
plot(imagebrick)
#manipulations of raters data

#lets create a function first

VI <- function(img, k, i) {
  bk <- img[[k]]
  bi <- img[[i]]
  Vi <- (bk - bi) / (bk + bi)
  return(Vi)
}

ndvi <- VI(imagebrick, 8, 4)
png('ndviplot.png', width = 4, height = 4, units = "in", res = 300)
plot(ndvi, col = rev(terrain.colors(10)), main = 'Sentinel 2, NDVI')
dev.off()
# create a histogram
png('ndvihist.png', width = 4, height = 4, units = "in", res = 300)
hist(ndvi,
     main = "Distribution of NDVI values",
     xlab = "NDVI",
     ylab= "Frequency",
     col = "aquamarine3",
     xlim = c(-0.5, 1),
     breaks = 30,
     xaxt = 'n')
axis(side = 1, at = seq(-0.5,1, 0.05), labels = seq(-0.5,1, 0.05))
dev.off()

# Mask cells that have NDVI of less than 0.4 (less likely to be vegetation)

png('ndvimask.png', width = 4, height = 4, units = "in", res = 300)

veg <- reclassify(ndvi, cbind(-Inf, 0.4, NA))
# We are reclassifying our object and making all values between
# negative infinity and 0.4 be NAs

plot(veg, main = 'Veg cover')
dev.off()

### lets try to import shape file of study area ( polygon, fire lines, points of location)

boundary<- readOGR("F:\\2078 and 79\\MSc\\4th sem\\manakamana cf kapilvastu\\polygon.shp")

boundary
plot(boundary, col="blue")

#importing points shapefle and adding a points in the map

location<- readOGR("F:\\2078 and 79\\MSc\\4th sem\\manakamana cf kapilvastu\\sample point.shp")
plot(boundary)
plot(location, add=TRUE, pch=19, col="purple")


# now lets crop the study area

study_area<- crop(Image,boundary)
plot(study_area, main="study area")
plot(boundary, add=TRUE)
study_area #it gave rectangular shape image

# crop only our study area by using mask function
a<-raster::mask(study_area, boundary)
plot(a)

###################Now calculate vegetation indices#############################

NDVI<-(SA_b8a_scaled-SA_b4_scaled)/(SA_b8a_scaled+SA_b4_scaled)
#plot(NDVI)
#title(" NDVI of the study area")
output_path<-"F:\\2078 and 79\\MSc\\4th sem\\my_data\\R try\\sentinel2\\vegetation_indices\\ndvi.tif"
writeRaster(NDVI, filename= output_path, format="GTiff")



#****************************************************
# Set up a multi-panel plot
par(mfrow = c(3, 3))  # 1 row, 2 columns

# Plot NDVI
plot(NDVI, main = "NDVI")

# Plot NDVI45
plot(NDVI45, main = "NDVI45")
plot(NDVI65, main= "NDVI65")
plot(NDII11, main="NDII11")
plot(NDII12, main="NDII12")
plot(RE_NDVI, main="RE_NDVI")
plot(SAVI, main="SAVI")
plot(WDRVI, main="WDRVI")
plot(GNDVI, main="GNDVI")
#***********************************************************
#****************************************************
# Set up a multi-panel plot
par(mfrow = c(3, 3))  # 1 row, 2 columns

# Plot NDVI
plot(RGR, main = "RGR")

# Plot NDVI45
plot(EVI7, main = "EVI7")
plot(EVI8, main= "EVI8")
plot(SR, main="SR")
plot(PSRI, main="PSRI")
plot(IRECI, main="IRECI")
plot(S2REP, main="S2REP")
plot(MTCI, main="MTCI")
plot(MSR, main="MSR")

# agian do it from here
# Set up a multi-panel plot
par(mfrow = c(3, 2))  # 1 row, 2 columns
plot(CIRE, min="CIRE")
plot(ARI1, main="ARI1")
plot(ARI2, main="ARI2")
plot(CRI1, main="CRI1")
plot(CRI2, main="CRI2")
plot(MCARI, main="MCARI")


#*****************************************************************

NDVI45<- (SA_b5_scaled-SA_b4_scaled)/(SA_b5_scaled+SA_b4_scaled)
NDVI45


NDVI65<- (SA_b6_scaled-SA_b5_scaled)/(SA_b6_scaled+SA_b5_scaled)
NDVI65

RGR<-(SA_b4_scaled/SA_b3_scaled)
RGR

EVI8<- 2.5*(SA_b8a_scaled-SA_b4_scaled)/(1+SA_b8a_scaled+6*SA_b4_scaled-(7.5*SA_b2_scaled))
plot(EVI8)
EVI8

EVI7<- 2.5*(SA_b7_scaled-SA_b4_scaled)/(1+SA_b7_scaled+6*SA_b4_scaled-(7.5*SA_b2_scaled))
plot(EVI7)
EVI7


SR<- SA_b8_scaled/SA_b4_scaled
SR

PSRI<- (SA_b4_scaled-SA_b3_scaled)/SA_b8_scaled
PSRI

NDII11<-(SA_b8a_scaled-SA_b11_scaled)/(SA_b8a_scaled+SA_b11_scaled)
plot(NDII11)
NDII11

NDII12<-(SA_b8a_scaled-SA_b12_scaled)/(SA_b8a_scaled+SA_b12_scaled)
plot(NDII12)
NDII12

RE_NDVI<-(SA_b8a_scaled-SA_b6_scaled)/(SA_b8a_scaled+SA_b6_scaled)
plot(RE_NDVI)
RE_NDVI

SAVI<- (SA_b8-SA_b4)/(SA_b8+SA_b4+0.428)*(1+0.428) #not scaled
SAVI

IRECI<- (SA_b7_scaled-SA_b4_scaled)/(SA_b6_scaled/SA_b5_scaled)
plot(IRECI)
IRECI

#this equation is wrong
#S2REP <- (705 + 35 * ((0.5 * (SA_b7_scaled + SA_b4_scaled)) / 2) - SA_b5_scaled) / (SA_b6_scaled - SA_b5_scaled)
#plot(S2REP)
#S2REP

# this above equation S2REP is of doubt for me
S2REP_new<- 705+35*(((SA_b7_scaled+SA_b4_scaled)/2)-SA_b5_scaled)/(SA_b6_scaled-SA_b5_scaled)
S2REP_new

S2REP<- 705+35*(((SA_b7_scaled+SA_b4_scaled)/2)-SA_b5_scaled)/(SA_b6_scaled-SA_b5_scaled)
S2REP

WDRVI<- ((0.01*SA_b7_scaled)-SA_b4_scaled)/((0.01*SA_b7_scaled+SA_b4_scaled)+((1-0.01)/(1+0.01)))
WDRVI
plot(WDRVI)

MTCI<- (SA_b6_scaled-SA_b5_scaled)/(SA_b5_scaled-SA_b4_scaled)
MTCI

MSR<- ((SA_b7_scaled/SA_b4_scaled)-1)/sqrt((SA_b7_scaled/SA_b4_scaled)+1)
MSR

GNDVI<- (SA_b8a_scaled-SA_b3_scaled)/(SA_b8a_scaled+SA_b3_scaled)
GNDVI

CIRE<- (SA_b7_scaled/SA_b5_scaled)-1
CIRE

ARI1<-(1/SA_b3_scaled)-(1/SA_b5_scaled)
ARI1

ARI2<-(SA_b8a_scaled/SA_b3_scaled)-(SA_b8a_scaled/SA_b5_scaled)
ARI2

CRI1<- (1/SA_b2_scaled)-(1/SA_b3_scaled)
CRI1

CRI2<- (1/SA_b2_scaled)-(1/SA_b5_scaled)
CRI2

MCARI<- 1-((0.2)*(SA_b5_scaled-SA_b3_scaled)/(SA_b5_scaled-SA_b4_scaled))
MCARI





# now lets calculate red edge based NDVIs

Red_edge_1<- (SA_b8-SA_b5)/(SA_b8+SA_b5)
Red_edge_1

Red_edge_2<- (SA_b8-SA_b6)/(SA_b8+SA_b6)
Red_edge_2

Red_edge_3<- (SA_b8-SA_b7)/(SA_b8+SA_b7)
Red_edge_3

Red_edge_4<- (SA_b8-SA_b8a)/(SA_b8+SA_b8a)
Red_edge_4







######## lets stack the vegetation indices together##### 

#stacked_image<- stack(NDVI, RGR, EVI, SR, PSRI, NDII, RE_NDVI, SAVI, IRECI, S2REP, Red_edge_1, Red_edge_2, Red_edge_3, Red_edge_4)
#stacked_image
#plot(stacked_image)
#you have to create stack image and save it as tif file, I think??????

######lets get sample plots####
#sample_points<- readOGR("F:\\2078 and 79\\MSc\\4th sem\\manakamana cf kapilvastu\\sample point.shp")
#sample_points@data #view the data stored in shapefile
#sp_df<-as.data.frame(sample_points)
#sp_df
#sp_ok<- sp_df[-c(22,27),] # delete 22th, 27th row forom dataframe
#sp_ok

##### import csv file####
sample_points<- read.csv("filed_data_csv.csv")
sample_points
plot(sample_points)
str(sample_points)
#############################################plot points in image#####
#myCol<- terrain.colors(6)
#plot(stacked_image, col= myCol, main="plots in study area")

#points(sample_points$X, sample_points$Y, pch=1, cex=1) # it does not work on stacked image??

# lets check the coordinate reference system of raster image 

crs_stacked_image <- st_crs(stacked_image) # st_crs() from sf package
print(crs_stacked_image)

# coordinate system of sample points that is in dataframe
#lets first assign crs to sample_points as it is in dataframe, not in spatial 

SP_crs<- st_as_sf(sample_points, coords = c("X","Y"), crs= "+proj=utm +zone=44 +datum=WGS84")
print(SP_crs)

##### now lets extract the pixels values from plot area#####
# the plot is circular with radius 12.61m

#lets try for each bands first

PV_b1<- raster::extract(SA_b1_scaled, SP_crs, buffer= 12.61, fun= mean, df=TRUE)
PV_b1
PV_b1$b1<- PV_b1$resampled_image_1
PV_b1

PV_b2<- raster::extract(SA_b2_scaled, SP_crs, buffer= 12.61, fun= mean, df=TRUE)
PV_b2
PV_b1$b2<- PV_b2$resampled_image_1
PV_b1

PV_b3<- raster::extract(SA_b3_scaled, SP_crs, buffer= 12.61, fun= mean, df=TRUE)
PV_b3
PV_b1$b3<- PV_b3$resampled_image_1
PV_b1

PV_b4<- raster::extract(SA_b4_scaled, SP_crs, buffer= 12.61, fun= mean, df=TRUE)
PV_b4
PV_b1$b4<- PV_b4$resampled_image_1
PV_b1

PV_b5<- raster::extract(SA_b5_scaled, SP_crs, buffer= 12.61, fun= mean, df=TRUE)
PV_b5
PV_b1$b5<- PV_b5$b5_june_26.tiff
PV_b1

PV_b6<- raster::extract(SA_b6_scaled, SP_crs, buffer= 12.61, fun= mean, df=TRUE)
PV_b6
PV_b1$b6<- PV_b6$resampled_image_1
PV_b1

PV_b7<- raster::extract(SA_b7_scaled, SP_crs, buffer= 12.61, fun= mean, df=TRUE)
PV_b7
PV_b1$b7<- PV_b7$resampled_image_1
PV_b1

PV_b8<- raster::extract(SA_b8_scaled, SP_crs, buffer= 12.61, fun= mean, df=TRUE)
PV_b8
PV_b1$b8<- PV_b8$b8.tiff
PV_b1

PV_b8a<- raster::extract(SA_b8a_scaled, SP_crs, buffer= 12.61, fun= mean, df=TRUE)
PV_b8a
PV_b1$b8a<- PV_b8a$b8a.tiff
PV_b1

PV_b9<- raster::extract(SA_b9_scaled, SP_crs, buffer= 12.61, fun= mean, df=TRUE)
PV_b9
PV_b1$b9<- PV_b9$resampled_image_1
PV_b1

PV_b10<- raster::extract(SA_b10_scaled, SP_crs, buffer= 12.61, fun= mean, df=TRUE)
PV_b10
PV_b1$b10<- PV_b10$resampled_image_1
PV_b1

PV_b11<- raster::extract(SA_b11_scaled, SP_crs, buffer= 12.61, fun= mean, df=TRUE)
PV_b11
PV_b1$b11<- PV_b11$resampled_image_1
PV_b1

PV_b12<- raster::extract(SA_b12_scaled, SP_crs, buffer= 12.61, fun= mean, df=TRUE)
PV_b12
PV_b1$b12<- PV_b12$resampled_image_1
PV_b1

#lets save the data frame
library(writexl)
write_xlsx(PV_b1, path = "F:\\2078 and 79\\MSc\\4th sem\\my_data\\R try\\sentinel2\\bands_pixel_8_10.xlsx")
hist(PV_b1$b2)
plot(PV_b1$b2)

# now vegetation index##############
Pixel_value<- raster::extract(NDVI, SP_crs, buffer= 12.61, fun=mean, df=TRUE)
View(Pixel_value)
# add plot number in the dataframe

Pixel_value$Plot<- SP_crs$Plot
Pixel_value

# lets fix the column names
names(Pixel_value)<- c("ID", "NDVI","Plot")
Pixel_value
hist(Pixel_value$NDVI)

Pixel_value_NDVI45<- raster::extract(NDVI45, SP_crs, buffer= 12.61, fun=mean, df=TRUE)
Pixel_value$NDVI45<-Pixel_value_NDVI45$layer

Pixel_value_NDVI65<- raster::extract(NDVI65, SP_crs, buffer= 12.61, fun=mean, df=TRUE)
Pixel_value$NDVI65<-Pixel_value_NDVI65$layer

##### now lets extract pixels value for all the vegetation indices.####

Pixel_value_RGR<- raster::extract(RGR, SP_crs, buffer= 12.61, fun= mean, na.rm=TRUE, df=TRUE)
View(Pixel_value_RGR)
hist(Pixel_value_RGR$layer)
Pixel_value$layer<-Pixel_value_RGR$layer # add column to next data fram form one df
Pixel_value
Pixel_value<- rename(Pixel_value, "rgr"="layer")
Pixel_value

Pixel_value_EVI7<- raster::extract(EVI7, SP_crs, buffer= 12.61, fun=mean, df=TRUE)
View(Pixel_value_EVI7)
hist(Pixel_value_EVI7$layer)
Pixel_value$EVI7<-Pixel_value_EVI7$layer # adding a new colume name evi that stores value from layer colmn from next df
Pixel_value

Pixel_value_EVI8<- raster::extract(EVI8, SP_crs, buffer= 12.61, fun=mean, df=TRUE)
View(Pixel_value_EVI8)
hist(Pixel_value_EVI8$layer)
Pixel_value$EVI8<-Pixel_value_EVI8$layer # adding a new colume name evi that stores value from layer colmn from next df
Pixel_value

Pixel_value_SR<- raster::extract(SR, SP_crs, buffer= 12.61, fun=mean, df=TRUE)
hist(Pixel_value_SR$layer)
Pixel_value$sr<-Pixel_value_SR$layer

Pixel_value_PSRI<- raster::extract(PSRI, SP_crs, buffer= 12.61, fun=mean, df=TRUE)
hist(Pixel_value_PSRI$layer)
Pixel_value$psri<-Pixel_value_PSRI$layer

Pixel_value_NDII11<- raster::extract(NDII11, SP_crs, buffer= 12.61, fun=mean, df=TRUE)
Pixel_value$ndii11<-Pixel_value_NDII11$layer

Pixel_value_NDII12<- raster::extract(NDII12, SP_crs, buffer= 12.61, fun=mean, df=TRUE)
Pixel_value$ndii12<-Pixel_value_NDII12$layer


Pixel_value_RE_NDVI<- raster::extract(RE_NDVI, SP_crs, buffer= 12.61, fun=mean, df=TRUE)
Pixel_value$re_ndvi<-Pixel_value_RE_NDVI$layer

Pixel_value_SAVI<- raster::extract(SAVI, SP_crs, buffer= 12.61, fun=mean, df=TRUE)
Pixel_value$savi<-Pixel_value_SAVI$layer

Pixel_value_IRECI<- raster::extract(IRECI, SP_crs, buffer= 12.61, fun=mean, df=TRUE)
hist(Pixel_value_IRECI$layer)
Pixel_value$ireci<-Pixel_value_IRECI$layer

Pixel_value_S2REP_new<- raster::extract(S2REP_new, SP_crs, buffer= 12.61, fun=mean, df=TRUE)
Pixel_value$s2rep_new<-Pixel_value_S2REP_new$layer

Pixel_value_RE_1<- raster::extract(Red_edge_1, SP_crs, buffer= 12.61, fun=mean, df=TRUE)
Pixel_value$re_1<-Pixel_value_RE_1$layer

Pixel_value_RE_2<- raster::extract(Red_edge_2, SP_crs, buffer= 12.61, fun=mean, df=TRUE)
Pixel_value$re_2<-Pixel_value_RE_2$layer

Pixel_value_RE_3<- raster::extract(Red_edge_3, SP_crs, buffer= 12.61, fun=mean, df=TRUE)
Pixel_value$re_3<-Pixel_value_RE_3$layer

Pixel_value_RE_4<- raster::extract(Red_edge_4, SP_crs, buffer= 12.61, fun=mean, df=TRUE)
Pixel_value$re_4<-Pixel_value_RE_4$layer

Pixel_value_WDRVI<- raster::extract(WDRVI, SP_crs, buffer= 12.61, fun=mean, df=TRUE)
Pixel_value$WDRVI<-Pixel_value_WDRVI$layer

Pixel_value_MTCI<- raster::extract(MTCI, SP_crs, buffer= 12.61, fun=mean, df=TRUE)
Pixel_value$MTCI<-Pixel_value_MTCI$layer

Pixel_value_MSR<- raster::extract(MSR, SP_crs, buffer= 12.61, fun=mean, df=TRUE)
Pixel_value$MSR<-Pixel_value_MSR$layer

Pixel_value_GNDVI<- raster::extract(GNDVI, SP_crs, buffer= 12.61, fun=mean, df=TRUE)
Pixel_value$GNDVI<-Pixel_value_GNDVI$layer

Pixel_value_CIRE<- raster::extract(CIRE, SP_crs, buffer= 12.61, fun=mean, df=TRUE)
Pixel_value$CIRE<-Pixel_value_CIRE$layer

Pixel_value_ARI1<- raster::extract(ARI1, SP_crs, buffer= 12.61, fun=mean, df=TRUE)
Pixel_value$ARI1<-Pixel_value_ARI1$layer

Pixel_value_ARI2<- raster::extract(ARI2, SP_crs, buffer= 12.61, fun=mean, df=TRUE)
Pixel_value$ARI2<-Pixel_value_ARI2$layer

Pixel_value_CRI1<- raster::extract(CRI1, SP_crs, buffer= 12.61, fun=mean, df=TRUE)
Pixel_value$CRI1<-Pixel_value_CRI1$layer

Pixel_value_CRI2<- raster::extract(CRI2, SP_crs, buffer= 12.61, fun=mean, df=TRUE)
Pixel_value$CRI2<-Pixel_value_CRI2$layer

Pixel_value_MCARI<- raster::extract(MCARI, SP_crs, buffer= 12.61, fun=mean, df=TRUE)
Pixel_value$MCARI<-Pixel_value_MCARI$layer



#lets save the data frame in xlsx format in local disk
#install.packages("writexl")
library(writexl)
write_xlsx(Pixel_value, path = "F:\\2078 and 79\\MSc\\4th sem\\my_data\\R try\\sentinel2\\new_pixel_data_8_10.xlsx")

head(Pixel_value)












#### lets predict the biomass in our study area using the random forest model##############################
rdata<- c(b11, s2rep_new, WDRVI)

library(caret)
library(caTools)
library(VSURF)
data<-read.csv("F:\\2078 and 79\\MSc\\4th sem\\my_data\\R try\\sentinel2\\ML_try.csv")
Ddata<-data[-c(28,27,47,39,13),]
write.csv(Ddata, file = "F:\\2078 and 79\\MSc\\4th sem\\my_data\\R try\\sentinel2\\Ddata.csv", row.names = FALSE)
Variable_selection<-VSURF(AGB~ .,
                          data = data, variable.names=TRUE)
plot(Variable_selection,var.names=TRUE)
plot(Variable_selection,step="pred",var.names=TRUE)
summary(Variable_selection)

controlparameters<-trainControl(method = "repeatedcv",
                                number=5,
                                repeats = 5,
                                savePredictions=TRUE,
                                classProbs = TRUE)

########################RF MODEL using vsurf ####################################################################################
#set.seed(123)
Final_Model<-train(AGB~b11+S2REP_new+WDRVI
                   ,
                   data = Ddata,
                   method='rf',
                   trcontrol=controlparameters)

#importance=T)                   ################Importance=T gives the importance of variables for each classes#####
Final_Model$results
Final_Model$finalModel

########################SVM MODEL using vsurf ####################################################################################
#set.seed(123)
Final_Model<-train(AGB~b11+s2rep_new +WDRVI
                   ,
                   data = Ddata,
                   method='svmRadial',
                   trcontrol=controlparameters)

#importance=T)                   ################Importance=T gives the importance of variables for each classes#####
Final_Model$results
Final_Model$finalModel

# For svmLinear

Final_Model<-train(AGB~b11+s2rep_new +WDRVI
                   ,
                   data = Ddata,
                   method='svmLinear',
                   trcontrol=controlparameters)

#importance=T)                   ################Importance=T gives the importance of variables for each classes#####
Final_Model$results
Final_Model$finalModel

#For svmPoly
Final_Model<-train(AGB~b11+s2rep_new +WDRVI
                   ,
                   data = Ddata,
                   method='svmPoly',
                   trcontrol=controlparameters)

#importance=T)                   ################Importance=T gives the importance of variables for each classes#####
Final_Model$results
Final_Model$finalModel


#######lets predict##########################################

#lets save the raster files into local device first
output_path1<-"F:\\2078 and 79\\MSc\\4th sem\\my_data\\R try\\sentinel2\\vegetation_indices\\b11.tif"
writeRaster(SA_b11_scaled, filename= output_path1, format="GTiff")

output_path2<-"F:\\2078 and 79\\MSc\\4th sem\\my_data\\R try\\sentinel2\\vegetation_indices\\S2REP_new.tif"
writeRaster(S2REP_new, filename= output_path2, format="GTiff")


output_path3<-"F:\\2078 and 79\\MSc\\4th sem\\my_data\\R try\\sentinel2\\vegetation_indices\\WDRVI.tif"
writeRaster(WDRVI, filename= output_path3, format="GTiff")


############ lets make the raster stacks first#####################
file_paths<- c(output_path1, output_path2,output_path3)

raster_stack<- stack(file_paths)

print(raster_stack)
raster_stack[[1]]
raster_stack[[2]]
raster_stack[[3]]
#merged_raster<- merge(raster_stack)
#writeRaster(merged_raster, filename = "F:\\2078 and 79\\MSc\\4th sem\\my_data\\R try\\sentinel2\\vegetation_indices\\merged_raster.tif", format = "GTiff")
#merged_raster
names(raster_stack)

predRaster <- predict(raster_stack, Final_Model)
plot(predRaster)
mean(predRaster)
plot(predRaster, main = "Aboveground Forest Biomass Map")



# Calculate mean
mean_val <- cellStats(predRaster, stat = 'mean')

# Calculate minimum
min_val <- cellStats(predRaster, stat = 'min')

# Calculate maximum
max_val <- cellStats(predRaster, stat = 'max')

# Calculate standard deviation
sd_val <- cellStats(predRaster, stat = 'sd')


print(paste("Mean: ", mean_val))
print(paste("Min: ", min_val))
print(paste("Max: ", max_val))
print(paste("Standard Deviation: ", sd_val))




# Save the raster image into a TIFF file
writeRaster(predRaster, filename = "biomassmap.tif", format = "GTiff", overwrite = TRUE)


###########for carbon map#########
# Calculate Carbon from AGB
carbonRaster <- predRaster * 0.47

# Plot Carbon Map
plot(carbonRaster, main = "Forest Carbon Map")

writeRaster(carbonRaster, filename = "carbonmap.tif", format = "GTiff", overwrite = TRUE)

# Calculate mean
mean_val <- cellStats(carbonRaster, stat = 'mean')

# Calculate minimum
min_val <- cellStats(carbonRaster, stat = 'min')

# Calculate maximum
max_val <- cellStats(carbonRaster, stat = 'max')

# Calculate standard deviation
sd_val <- cellStats(carbonRaster, stat = 'sd')

print(paste("Mean: ", mean_val))
print(paste("Min: ", min_val))
print(paste("Max: ", max_val))
print(paste("Standard Deviation: ", sd_val))

