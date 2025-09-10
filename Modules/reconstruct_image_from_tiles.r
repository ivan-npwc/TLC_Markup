

      ssldbdir ="/media/ivan/2023_ HD2/SSL_DB"
      dirtiles =  "/media/ivan/USATOV_2024/SSL_DB_Tiles/2022_33_Tiles/20220811"
	  svdir =ssldbdir
      lsttiles = list.files(dirtiles, full.names=T, recursive=T)
      lsttiles =data.frame(pth=lsttiles)
	  lsttiles$bsn_tls=basename(lsttiles$pth)
	  ####################################
     border_color <- c(1, 0, 0)  # Красный цвет границы (RGB)
     border_width <- 2           # Толщина границы в пикселях
####### Функция для добавления рамки к тайлу #####  
   add_border_to_tile <- function(tile, border_width = 2, border_color = c(1, 0, 0)) {
   dims <- dim(tile)
  new_dims <- c(dims[1] + 2 * border_width, dims[2] + 2 * border_width, dims[3])
  bordered_tile <- array(border_color, dim = new_dims)
  x_start <- border_width + 1
  x_end <- border_width + dims[1]
  y_start <- border_width + 1
  y_end <- border_width + dims[2]
  bordered_tile[x_start:x_end, y_start:y_end, ] <- tile
  return(bordered_tile)
}
###########################################################
	   for (i in 1:length(lsttiles$bsn_tls)){
	   lsttiles$imgs[i] =strsplit(lsttiles$bsn_tls[i],"#")[[1]][2]
	   lsttiles$cam[i] = strsplit( lsttiles$imgs[i] ,"_")[[1]][3]
	   lsttiles$day_site[i]=gsub(basename(lsttiles$pth[i]),"",lsttiles$pth[i])
	   lsttiles$day[i]=basename( lsttiles$day_site[i])
	   lsttiles$site_year[i]=basename(gsub(lsttiles$day[i],"",lsttiles$day_site[i]))
	  lsttiles$site[i] = strsplit(lsttiles$site_year[i], "_")[[1]][2]
	  lsttiles$year[i] = strsplit(lsttiles$site_year[i], "_")[[1]][1]
	   }	 
	  #######
	  uniqcam = unique(lsttiles$cam)
       for (r in 1:length(uniqcam)){
       cam=uniqcam[r]
	   tls = lsttiles[lsttiles$cam == cam,]
	   imgs =unique(tls$imgs)
	   tlsimg =tls[tls$imgs== imgs[1],]
	   
       origimgspth = paste0(ssldbdir,"/", tlsimg$year[1],"_",tlsimg$site[1],"_Map","/", tlsimg$day[1],"/", tlsimg$imgs[1] ) 
	   savedir = paste0(ssldbdir,"/", tlsimg$year[1],"_",tlsimg$site[1],"_Tiles_check");dir.create(savedir,showWarnings = F)
	   savedirday=paste0(savedir,"/",tlsimg$day[1]);dir.create(savedirday,showWarnings = F)
	   pthsv=paste0(savedirday,"/", tlsimg$imgs[1])
	   
       img=readImage(origimgspth)

      img_dims = dim(img)
      img_width <- img_dims[1]
      img_height <- img_dims[2]
      reconstructed <- array(0, dim = c(img_width, img_height, 3))
	   for (i in 1:length(tlsimg$pth)){
	   tileP = tlsimg$pth[i]
	   tile=readImage(tileP)
	   tile=flip(tile)
	   tile_border=add_border_to_tile(tile)
	   tile_border=resize(tile_border,dim(tile)[1],dim(tile)[2])
	   info=basename(tileP)
	   info1=gsub(imgs[1],"",info)
	   info2=gsub("#","",info1)
	   info3 =strsplit(info2,"_")
	   
	   
	   x_start =as.numeric(info3[[1]][4])
	   x_end =as.numeric(info3[[1]][6])
	   
	   EBIy_start =as.numeric(info3[[1]][8])
	   EBIy_end =as.numeric(info3[[1]][10])
	      
        reconstructed[x_start:x_end, EBIy_start:EBIy_end, ] <- tile_border#[,,1:3]
		
}
##############################################

rgbreconstructed=Image(reconstructed,colormode = Color)
writeImage(rgbreconstructed,pthsv,type = "JPEG",quality = 100)

}