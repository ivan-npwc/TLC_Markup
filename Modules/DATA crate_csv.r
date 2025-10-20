#source("/home/ivan/GIT_HUB/TLC_Markup/Modules/DATA crate_csv_sql.r")
#source("/home/npwc/GIT/TLC_Markup/Modules/DATA crate_csv_sql.r")

library(RSQLite)
##########################
dir_tiles =  "/mnt/adata8tb/SSL_DB_Tiles"
######################################### 
#indir = "/mnt/adata8tb/SSL_DB"
#outdir =  "/mnt/adata8tb/SSL_DB_Tiles"
#########################################
if (dir.exists(dir_tiles)==F) {stop("NO dir_tiles FOUND")}
###############################################################################################
lstdirPresence = list.files(dir_tiles, pattern = "Presence", full.names=T) 

for (sts in 1: length(lstdirPresence)){ 
  siteyear = lstdirPresence[sts]
  presence=NULL
  branded=NULL
  #########
  siteyeardata = gsub("Presence","Data",siteyear)
  dir.create(siteyeardata, showWarnings=F)
  sydp = file.path(siteyeardata,"Predictions")
  dir.create(sydp, showWarnings=F)
  #############
  bsnm =basename(siteyear)
  year =strsplit(bsnm,"_")[[1]][1]
  site =strsplit(bsnm,"_")[[1]][2]
  print(site)
  print(year)
  site_days =list.files(siteyear,full.names=T)
  for (ds in 1:length(site_days)){
    day1 = site_days[ds]
	bsday =basename(day1)

	dirbrand = file.path(dir_tiles, paste0(year,"_",site,"_Branded"),bsday)
	tlspresence = list.files(day1,pattern = ".JPG")
	if (length(tlspresence) == 0) next
    ###############################################################

process_data <- function(tbl, model_name) {
  info <- strsplit(tbl$file, "#", fixed = TRUE)
  info1 <- strsplit(sapply(info, "[", 1), "_", fixed = TRUE)
  filenames <- sapply(info, "[", 2)
  animal_names <- rep("Unknown", length(tbl[,1]))

  coords_matrix <- do.call(rbind, lapply(info1, function(x) {
    c(x[4], x[6], x[8], x[10])  # x1, x2, y1, y2
  }))
  

  data.frame(
    animal_name = animal_names,
    x1 = coords_matrix[,1],
    x2 = coords_matrix[,2],
    y1 = coords_matrix[,3],
    y2 = coords_matrix[,4],
    file = tbl$file,
    filename = filenames,
    model = model_name,
    stringsAsFactors = FALSE
  )
}
tblpresenceday <- data.frame(file = tlspresence)
presenceday <- process_data(tbl = tblpresenceday,  model_name = "sealion_presence_absence_128_2025-09-29_accuracy_0.97_epoch_48")
presence = rbind(presence,presenceday)

################################################
	if (dir.exists(dirbrand)==T){
	tlstBranded = list.files(dirbrand,pattern = ".JPG")
	    if (length(tlstBranded) != 0){	
	tblbranded <- data.frame(file = tlstBranded)
	brandedday <- process_data(tblbranded,  model_name= "branded_not_branded_128_2025-10-13_accuracy_0.92_epoch_97")
	branded = rbind(branded,brandedday)
	}}
	
	}#day
	
	pthpresence=file.path(sydp,"Present.csv")
	pthbranded=file.path(sydp,"Branded.csv")
	
	write.csv(presence,pthpresence)
	write.csv(branded,pthbranded)

	}#siteyear
	
	
	
	