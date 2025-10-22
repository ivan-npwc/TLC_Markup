

#source("/home/ivan/GIT_HUB/TLC_Markup/Modules/DATA crate_sql.r")
#source("/home/npwc/GIT/TLC_Markup/Modules/DATA crate_csv_sql.r")

library(RSQLite)
library(dplyr)
##########################
dir_tiles =  "/mnt/adata8tb/SSL_DB_Tiles"
imgsdtpth = "/home/ivan/image_data.csv"
basesqlpth =  "/home/ivan/GIT_HUB/TLC MarkUp System data/base_20251020.db"
#####################################################################
sqlite    <- dbDriver("SQLite")
#############################################################
if (dir.exists(dir_tiles)==F) {stop("NO dir_tiles FOUND")}
###############################################################################################
imgs_dt = read.csv(imgsdtpth)
imgs_info=data.frame(filename = basename(imgs_dt$image_path), height = imgs_dt$height)
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
  branded = read.csv(brandedpth)
  #branded_info=left_join(branded,imgs_info,by="filename")
  #branded_info=branded_info[is.na(branded_info$height)==F,]

 #branded_info$height = as.numeric(branded_info$height)
 #branded_info$x1 =  as.numeric(branded_info$x1)
 # branded_info$x2 =  as.numeric(branded_info$x2)
  
 # branded_info$y1 =  as.numeric(branded_info$y1)
 # branded_info$y2 =   as.numeric(branded_info$y2)
  branded$tile = tools::file_path_sans_ext(branded$filename)
   branded$tile = paste0(branded$tile,"_res_",   branded$file)
  
  
  current_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  datecreated = paste0(current_time," UIA1989")
 # present=read.csv(presentpth)
  #########################################
  #start1= 1#dbGetQuery(SSL, "SELECT Count(*) FROM prediction")$`Count(*)`+1
  #Stop1=start1+length(branded$x1)-1
  #DisplayOrder= c(start1 : Stop1)

 prediction = data.frame (
                                    species="SSL" ,   
									r_year=year,
									site =   site,							
									x1  =  branded$x1,
									x2  =  branded$x2,
								    y1  = branded$y1,  # ebimage used up, sql need down  branded_info$height - branded_info$y2
									y2  =  branded$y2,
									tiles =  branded$tile,  
									file_name   =  branded$filename,
									ommited= 0,
									datecreated=datecreated,
									dateupdated=datecreated
									)
									
 prediction$x1[prediction$x1==0]=1
 prediction$x2[prediction$x2==0]=1
 prediction$y1[prediction$y1==0]=1
 prediction$y2[prediction$y2==0]=1
 prediction=prediction[is.na(prediction$x1)==F,]
  SSL <- dbConnect(sqlite,   SQLite_pth)
  
#  test=dbGetQuery(SSL, "SELECT * FROM id_prediction")
 # dbExecute(SSL, "DELETE FROM prediction")
 # dbExecute(SSL, "VACUUM")
  dbWriteTable(SSL, "id_prediction", prediction, append=T)
  dbDisconnect(SSL)
}
  