library(terra)
library(RCurl)
library(RSQLite)
library(sf)



#dataset info: https://apps.nationalmap.gov/datasets/

target_area = st_read(dsn="C:\\Users\\sampc\\Dropbox\\cfc\\spatial\\WholeCFWatershed.gpkg")

areaName="test"

gpkg=paste0(getwd(),"/",areaName,".gpkg")

rastDir=file.path(getwd())
dir.create(rastDir)

#1/3 arc-second dem
getAndAddDem=function(rID,gpkg){
  
  baseURL="https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/1/TIFF/current/"
  
  #url format:
  #https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/current/n10e138/USGS_13_n44w109.tif
  demURL=paste0(baseURL,rID,"/USGS_1_",rID,".tif")
  success=T
  if(!url.exists(demURL)){
    success=F
    print(paste0("ERROR: Unable to find raster ",rID," at url:"))
    print(demURL)
  }
  
  if(success){
    if(file.exists(file.path(rastDir,"tempRast.tif"))){
      file.remove(file.path(rastDir,"tempRast.tif"))
    }
    options("timeout"=600)
    download.file(url=demURL,destfile=file.path(rastDir,"tempRast.tif"),cacheOK=FALSE,method = "curl")
    options("timeout"=60)
    
    
    tempRast=rast(file.path(rastDir,"tempRast.tif"))
    writeRaster(tempRast,filename=gpkg,
                filetype = "GPKG",
                gdal = c("APPEND_SUBDATASET=YES", paste0("RASTER_TABLE=",rID)),
    )
    
    file.remove(file.path(rastDir,"tempRast.tif"))
    
    
  }
}


target_area=st_transform(target_area,crs=4269)

st_is_valid(target_area)

area_bounds=ceiling(st_bbox(target_area))
potential_tiles=expand.grid(seq(from=area_bounds$ymin,to=area_bounds$ymax),
                            seq(from=area_bounds$xmin,to=area_bounds$xmax))
names(potential_tiles)=c("y","x")

for(i in 1:nrow(potentialTiles)){
  nw=as.numeric(potential_tiles[i,])
  #naming convention is upper left corner, eg n44w110 covers 43,109 (ll) to 44,110 (ur)
  this_tile=st_polygon(x=list(matrix(c(nw[2],nw[2],nw[2]-1,nw[2]-1,nw[2],nw[1],nw[1]-1,nw[1]-1,nw[1],nw[1]),ncol=2)),
                       dim="XY")
  this_tile=st_sfc(this_tile,crs=4269)
  
  if(st_intersects(target_area,this_tile,sparse=F)){
    
  }
  
}


#build .vrt from all layers in .gpkg

conn=dbConnect(SQLite(),gpkg)
rNames=dbGetQuery(conn,"SELECT table_name FROM gpkg_tile_matrix_set")$table_name
dbDisconnect(conn)

rNames=paste0(paste0("GPKG:",gpkg,":"),rNames)

vrt(rNames,paste0(areaName,".vrt"),overwrite=T)

