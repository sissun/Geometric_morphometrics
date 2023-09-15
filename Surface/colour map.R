library(Arothron)
library(Morpho)
library(rgl)
library(Rvcg)


#read in reference and target meshes
rm(list=ls())

ref_mesh<-file2mesh("./Data/Sliding-Target.obj") # load mesh-1
tar_mesh<-file2mesh("./Data/NICP.obj")# load mesh-2

comp_shapes_areas<-function(mesh1,mesh2,ploton,
                            paltot=rainbow(200),from=-0.05,to=0.05,
                            n.int=200,out.rem=FALSE,fact=1.5){


#calculation of area for each triangle of the mesh
area_shape1<-vcgArea(mesh1,perface=T)$pertriangle
area_shape2<-vcgArea(mesh2,perface=T)$pertriangle
#differences and ratio of areas
diff_areas<-(area_shape1-area_shape2)/area_shape1
cat("the range of diff_areas is ",range(diff_areas),sep="\n")
#if out.rem the outliers will be set
#on the minimum or maximum value of the vector diff_areas
if(out.rem==TRUE){
  x=diff_areas
  qq <- quantile(x, c(1,3)/4, names=FALSE)
  r <- diff(qq) * fact
  tst <- x < qq[1] - r | x > qq[2] + r
  tstp<-qq[2] + r
  tstn<-qq[1] - r
  diff_areas[x>tstp]<-tstp
  diff_areas[x<tstn]<-tstn
}else{
  diff_areas=diff_areas}

if(is.null(to)==TRUE){
  to<-max(diff_areas)
}
if(is.null(from)==TRUE){
  from<-min(diff_areas)
}
#triangles with area ranged between from and to
selfromto<-which(diff_areas<to & diff_areas>=from)
#select only the values ranged between from and to
diff_areas_fromto<-diff_areas[selfromto]




if(ploton==1){
meshfromto<-mesh1
meshwhite<-mesh1
}
if(ploton==2){
  meshfromto<-mesh2
  meshwhite<-mesh2
}
#here I build two meshes the first with
#only the triangles with an area ranged from given values
#second is white (values exceeding range from-to)
meshfromto$it<-meshfromto$it[,selfromto]
meshwhite$it<-meshwhite$it[,-selfromto]
colmap_tot<-colorRampPalette(paltot)   #negative=expansion=red/orange
breaks_tot<-cut(c(from,diff_areas_fromto,to),n.int)
cols_tot<-colmap_tot(n.int)[breaks_tot]
cols_tot<-cols_tot[-c(1,length(cols_tot))]
x11()
plot(density(c(from,diff_areas,to)),main="diff_areas")
abline(v=seq(from,to,length.out = n.int),col=colmap_tot(n.int),lwd=5)
points(density(diff_areas),type="l",lwd=2)

open3d()


triangles3d(t(meshfromto$vb[,meshfromto$it]),
            col=rep(cols_tot,each=3),alpha=1,lit = T, specular = "black")
triangles3d(t(meshwhite$vb[,meshwhite$it]),
            col="grey",alpha=1,lit = T, specular = "black")

 # shade3d(tar_mesh,col="white")
 # spheres3d(Sliding,col='black',radius=1)

}

comp_shapes_areas(tar_mesh,ref_mesh,ploton=1,
                  paltot=c("darkblue","blue","green","yellow","orange","red", "darkred"),
                  n.int=200,out.rem=F,fact=1.5,from= -0.05,to= 0.05)
