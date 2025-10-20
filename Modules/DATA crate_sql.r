

#source("/home/ivan/GIT_HUB/TLC_Markup/Modules/DATA crate_sql.r")
#source("/home/npwc/GIT/TLC_Markup/Modules/DATA crate_csv_sql.r")



basesqlpth =  "/home/ivan/GIT_HUB/TLC MarkUp System data/2022_33_predictions.db"
library(RSQLite)
sqlite    <- dbDriver("SQLite")
##########################
dir_tiles =  "/mnt/adata8tb/SSL_DB_Tiles"
#dir_tiles = "/mnt/adata8tb/SSL_DB_Tiles"
#####################################################################
if (dir.exists(dir_tiles)==F) {stop("NO dir_tiles FOUND")}
###############################################################################################
lstdirData = list.files(dir_tiles, pattern = "Data", full.names=T) 
for (sts in 1: length(lstdirData)){ 
  siteyear = lstdirData[sts]
  PredictionCSVDir = file.path(siteyear,"Predictions")
 # presentpth=file.path(PredictionCSVDir,"Present.csv")
  brandedpth=file.path(PredictionCSVDir,"Branded.csv")
  #############
  bsnm =basename(siteyear)
  year =strsplit(bsnm,"_")[[1]][1]
  site =strsplit(bsnm,"_")[[1]][2]
  
    SQLite_pth= file.path(PredictionCSVDir,paste0(year,"_",site,"_predictions.db"))
	unlink(SQLite_pth); file.copy(basesqlpth,SQLite_pth)
	
  print(site)
  print(year)
  branded=read.csv(brandedpth)
 # present=read.csv(presentpth)
  #########################################
  start1= 1#dbGetQuery(SSL, "SELECT Count(*) FROM prediction")$`Count(*)`+1
  Stop1=start1+length(branded$x1)-1
  DisplayOrder= c(start1 : Stop1)

 prediction = data.frame (
                                    id = DisplayOrder,       
									animal_name = branded$animal_name,
									site =   site,
									x1  =  branded$x1,
									x2  =  branded$x2,
								    y1  =  branded$y1,
									y2  =  branded$y2,
									tiles =  branded$file,  
									filename   =  branded$filename,
									model_name =  branded$model
									)
									
 prediction$x1[prediction$x1==0]=1
 prediction$x2[prediction$x2==0]=1
 prediction$y1[prediction$y1==0]=1
 prediction$y2[prediction$y2==0]=1
 prediction=prediction[is.na(prediction$x1)==F,]
  SSL <- dbConnect(sqlite,   SQLite_pth)
 # dbExecute(SSL, "DELETE FROM prediction")
 # dbExecute(SSL, "VACUUM")
  dbWriteTable(SSL, "prediction", prediction, append=T)
  dbDisconnect(SSL)
}
  