 #source("/home/ivan/GIT_HUB/TLC_Markup/Modules/CHECK PREDICTIONS.r")

#tcltk::tk_choose.files()
#watch -n 1 sensors

         
	 
       
        library(magick)
		library(parallel)
		library(doParallel)
	    library(foreach)

Preddir =  "/mnt/adata8tb/SSL_DB_Tiles"
SSL_DB_dir= "/mnt/adata8tb/SSL_DB"
###################################################
listsites_presence = list.files(Preddir, full.names=T,pattern="Presence")
###########################################################
for (i in 1:length(listsites_presence)){
 
 sitedir_presence =listsites_presence[i]
 
 print(paste0("START     ", sitedir_presence))
 
 site_info =strsplit(basename(sitedir_presence),"_")
 year = site_info[[1]][1]
 site = site_info[[1]][2]
# sitedir_branded  = gsub("Presence","Branded",sitedir_presence)
 SaveDir = gsub("Presence","CHECK",sitedir_presence)
 unlink(SaveDir,recursive=T)
 dir.create(SaveDir,showWarnings=F)
  daysdir_presence =list.files(sitedir_presence,full.names=T)
  
   for (y in 1:length(daysdir_presence)){
   ######################################
      if (exists("cl")==T){ stopCluster(cl);rm(cl)}
	  cl <- makePSOCKcluster(detectCores (logical=FALSE)-10) 
       clusterEvalQ(cl, {library(magick)})
       registerDoParallel(cl)
	  ##################################

     day_presence = daysdir_presence[y]
	 output_dir <- file.path(SaveDir, basename(day_presence))
	 #unlink(output_dir, recursive = TRUE)
	 print(paste0("START     ", day_presence))
	 day_branded  = gsub("Presence","Branded",day_presence)
	  
	  listImgPresence <- list.files(day_presence, full.names=T,pattern="JPG")  
	  listImgBranded <- list.files(day_branded, full.names=T,pattern="JPG")
	   
	    if (length(listImgPresence)==0) next
	    if (length(listImgBranded)==0){listImgBranded=""}
	  
	  listImgPresenceDT =data.frame(category = "Presence", tiles =listImgPresence)
	  listImgBrandedDT =data.frame(category = "Branded", tiles =listImgBranded)
	  
	  lsttils =rbind(listImgPresenceDT,listImgBrandedDT)
	  
		  
	   for (e in 1:length(lsttils$tiles)){
	   tile = basename(lsttils$tiles[e])
	   info =strsplit(tile,"#")
	   img_bsname = info[[1]][2]
	   coords = info[[1]][1]
	   info_coords = strsplit(coords,"_")
	   
	   xstart = info_coords[[1]][4]
	   xend = info_coords[[1]][6]
	   ystart = info_coords[[1]][8]
	   yend = info_coords[[1]][10]
	   
	   lsttils$img_bsname[e] = img_bsname
	   lsttils$xstart[e] = xstart
	   lsttils$xend[e] = xend
	   lsttils$ystart[e] = ystart
	   lsttils$yend[e] = yend
 }
 
  lsttils =lsttils[is.na(lsttils$img_bsname)==F,]
  lsttils$img_pth = paste0(SSL_DB_dir,"/", year ,"_", site, "_","Map", "/", basename(day_presence), "/",  lsttils$img_bsname)
  lstimgs =unique( lsttils$img_pth )

  file.exists(lstimgs)
  lstcheckpresence =list.files(output_dir)
  lstimgs =lstimgs[!basename(lstimgs) %in% lstcheckpresence]
   if (length(lstimgs)==0) next
 # for (w in 1:length(lstimgs)) {
 
 
#############################################################################################
  foreach(w = 1:length(lstimgs)) %dopar% { 
  
   img_pth = lstimgs[w]
   tiles_for_imgs = lsttils[lsttils$img_pth == img_pth,]
   presence_tiles = tiles_for_imgs[tiles_for_imgs$category=="Presence",]
   branded_tiles = tiles_for_imgs[tiles_for_imgs$category=="Branded",]
   output_path <- file.path(output_dir, basename(img_pth))
   
 if (nrow(branded_tiles) != 0) {
           dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
         if (file.exists(img_pth)==F){stop(paste0("No orig img found         ", img_pth))}
       
		 
	   if (file.exists(output_path)==F) {
		############################################
    img <- image_read(img_pth)
    img_draw <- image_draw(img)
  ################################################  
  #  if (nrow(presence_tiles) > 0) {
  #    for (i in 1:nrow(presence_tiles)) {
   #     x1 <- as.numeric(presence_tiles$xstart[i])
   #     y1 <- as.numeric(presence_tiles$ystart[i])
   #     x2 <- as.numeric(presence_tiles$xend[i])
   #     y2 <- as.numeric(presence_tiles$yend[i])
  #      rect(x1, y1, x2, y2, border = "green", lwd = 3, col = NA)
  #      
#
  #    }
   # }
 ##############################################    
      for (i in 1:nrow(branded_tiles)) {
        x1 <- as.numeric(branded_tiles$xstart[i])
        y1 <- as.numeric(branded_tiles$ystart[i])
        x2 <- as.numeric(branded_tiles$xend[i])
        y2 <- as.numeric(branded_tiles$yend[i])
      
        rect(x1, y1, x2, y2, border = "red", lwd = 3, col = NA)        
      }
    dev.off()
    image_write(img_draw, output_path, quality = 90)
    rm(img, img_draw)
    gc()
   }} # if close
  }  # dopar close
  }  # day close
  }  # site close
  