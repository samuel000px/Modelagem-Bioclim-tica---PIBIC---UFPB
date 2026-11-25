# Projeto: Modelagem Bioclimática do Alto Curso do Rio Paraíba
# Versão: 2026

library(raster)
library(rgdal)

sourceDir <- getSrcDirectory(function(dummy) {dummy})

setwd(sourceDir)

jan<-raster('raster/TEMPERATURA/tempmeanJan.tif')
fev<-raster('raster/TEMPERATURA/tempmeanFev.tif')
mar<-raster('raster/TEMPERATURA/tempmeanMar.tif')
abr<-raster('raster/TEMPERATURA/tempmeanAbr.tif')
mai<-raster('raster/TEMPERATURA/tempmeanMai.tif')
jun<-raster('raster/TEMPERATURA/tempmeanJun.tif')
jul<-raster('raster/TEMPERATURA/tempmeanJul.tif')
ago<-raster('raster/TEMPERATURA/tempmeanAgo.tif')
set<-raster('raster/TEMPERATURA/tempmeanSet.tif')
out<-raster('raster/TEMPERATURA/tempmeanOut.tif')
nov<-raster('raster/TEMPERATURA/tempmeanNov.tif')
dez<-raster('raster/TEMPERATURA/tempmeanDez.tif')

t<-values(jan)

vec<-c(jan,fev,mar,abr,mai,jun,jul,ago,set,out,nov,dez)




I<-raster()

for(i in 1:12){
 I<-I+(vec[[i]]/5)^1.514 
}

sum = (a+b)+c
a<-2