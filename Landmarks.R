
library(Morpho)
library(rgl)
library(Arothron)
library(shapes)


rm(list=ls())
meshsh<-file2mesh("./SlidingTPS/Data/Target.obj");

Sliding<-as.matrix(read.table("./SlidingTPS/Data/Target-semilandmarks.txt"));#Sliding TPS
ICP<-as.matrix(read.table("./LS&ICP/Data/Target-semilandmarks.txt",skip=1)); #LS&ICP
NICP<-as.matrix(read.table("./TPS&NICP/Data/Target-semilandmarks.txt"));     #TPS&NICP

open3d()
shade3d(meshsh,col="white")
spheres3d(Sliding[,],col='blue',radius=1.5)
spheres3d(ICP[,],col='red',radius=1.5)
spheres3d(NICP[,],col='green',radius=1.5)

