library(curl)
library(terra)
library(sf)
library(RSQLite)


#dataset info: https://apps.nationalmap.gov/datasets/

#target_area = st_read(dsn="C:\\Users\\sampc\\Dropbox\\cfc\\spatial\\WholeCFWatershed.gpkg")
target_area=st_read(dsn="/home/sam/Dropbox/cfc/spatial/WholeCFWatershed.gpkg")
areaName="cf_13deg"

gpkg=paste0(getwd(),"/",areaName,".gpkg")

rastDir=file.path(getwd())
# dir.create(rastDir)


getAndAddDem=function(rID,gpkg, resolution="1/3"){
  success=T
  
  #1 or 1/3 arc-second dem
  if(as.numeric(resolution) == 1){
    baseURL="https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/1/TIFF/current/"
    demURL=paste0(baseURL,rID,"/USGS_1_",rID,".tif")
  } else if(resolution %in% c(1/3, 13, "1/3", "13")){
    baseURL="https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/current/"
    demURL=paste0(baseURL,rID,"/USGS_13_",rID,".tif")
  } else{
    warning(paste0("unknown resolution: ",resolution))
    success=F
  }
  
  if(success){
    if(file.exists(file.path(rastDir,"tempRast.tif"))){
      file.remove(file.path(rastDir,"tempRast.tif"))
    }
    print(paste0("Downloading from: ", demURL))
    options("timeout"=600)
    curl_download(url=demURL,destfile=file.path(rastDir,"tempRast.tif"),quiet = F)
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
                            seq(from=area_bounds$xmin-1,to=area_bounds$xmax-1))
names(potential_tiles)=c("y","x")

for(i in 1:nrow(potential_tiles)){
  nw=as.numeric(potential_tiles[i,])
  #naming convention is upper left corner, eg n44w110 covers 43,109 (lr) to 44,110 (ul)
  this_tile=st_polygon(x=list(matrix(c(nw[2],nw[2],nw[2]+1,nw[2]+1,nw[2],nw[1],nw[1]-1,nw[1]-1,nw[1],nw[1]),ncol=2)),
                       dim="XY")
  this_tile=st_sfc(this_tile,crs=4269)
  
  if(st_intersects(target_area,this_tile,sparse=F)){
    #define dem tile name from nw:
    demName=paste0("n",nw[1],"w",abs(nw[2]))
    
    try( # avoid stopping the process for an out-of-region raster
      getAndAddDem(demName,gpkg, resolution=1/3)
    )
  }
  
}


#build .vrt from all layers in .gpkg

conn=dbConnect(SQLite(),gpkg)
rNames=dbGetQuery(conn,"SELECT table_name FROM gpkg_tile_matrix_set")$table_name
dbDisconnect(conn)

rNames=paste0(paste0("GPKG:",gpkg,":"),rNames)

vrt(rNames,paste0(areaName,".vrt"),overwrite=T)


#terra::writeRaster(rast(paste0(areaName,".vrt")), filename=paste0(rastDir,"/",areaName,".tif"), filetype="COG")
