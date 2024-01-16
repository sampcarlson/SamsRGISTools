library(terra)
library(RCurl)
library(RSQLite)



#dataset info: https://apps.nationalmap.gov/datasets/

areaName="test"

gpkg=paste0(getwd(),"/",areaName,".gpkg")

rastDir=file.path(tempdir(),"rasterTemp")
dir.create(rastDir)

#1/3 arc-second dem
getAndAddDem=function(rID,gpkg){
  
  
  
  
  baseURL="https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/current/"
  
  
  #url format:
  #https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/current/n10e138/USGS_13_n44w109.tif
  url=paste0(baseURL,rID,"/USGS_13_",rID,".tif")
  success=T
  if(!url.exists(url)){
    success=F
    print(paste0("ERROR: Unable to find raster ",rID," at url:"))
    print(url)
  }
  
  if(success){
    file.remove(file.path(rastDir,"tempRast.tif"))
    #getURL(url = url,)
    
    download.file(url=url,destfile=file.path(rastDir,"tempRast.tif"),extra=options(timeout=600))
    
    #options(timeout = max(300, getOption("timeout")))
    tempRast=rast(file.path(rastDir,"tempRast.tif"))
    
    writeRaster(tempRast,filename=gpkg,
                filetype = "GPKG",
                gdal = c("APPEND_SUBDATASET=YES", paste0("RASTER_TABLE=",rID)),
    )
  }
  #unlink(tempdir(),recursive=T,force=T)
}



#eg: 16 around bozeman:
getAndAddDem(rID="n44w109",gpkg)
getAndAddDem(rID="n44w110",gpkg)
getAndAddDem(rID="n44w111",gpkg)
getAndAddDem(rID="n44w112",gpkg)

getAndAddDem(rID="n45w109",gpkg)
getAndAddDem(rID="n45w110",gpkg)
getAndAddDem(rID="n45w111",gpkg)
getAndAddDem(rID="n45w112",gpkg)

getAndAddDem(rID="n46w109",gpkg)
getAndAddDem(rID="n46w110",gpkg)
getAndAddDem(rID="n46w111",gpkg)
getAndAddDem(rID="n46w112",gpkg)

getAndAddDem(rID="n47w109",gpkg)
getAndAddDem(rID="n47w110",gpkg)
getAndAddDem(rID="n47w111",gpkg)
getAndAddDem(rID="n47w112",gpkg)


#build .vrt from all layers in .gpkg

conn=dbConnect(SQLite(),gpkg)
rNames=dbGetQuery(conn,"SELECT table_name FROM gpkg_tile_matrix_set")$table_name
dbDisconnect(conn)

rNames=paste0(paste0("GPKG:",gpkg,":"),rNames)

vrt(rNames,paste0(areaName,".vrt"),overwrite=T)

