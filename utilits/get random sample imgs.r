
DirFrom="E:\\SSL_DB"
DirTo="E:\\SSL_DB_Sample";dir.create(DirTo)

listMaps=list.files(DirFrom, full.names=T,pattern="Map")
 for (i in 1:length(listMaps)){
 dirmap=listMaps[i]
 map1=basename(dirmap)
 map=strsplit(map1,"_")[[1]][2]
 lstimgsmap=list.files(dirmap, full.names=T, recursive=T)
 lstimgsmap=data.frame(pth=lstimgsmap)
 lstimgsmap$bsname=basename(lstimgsmap$pth)
   for(y in 1:length(lstimgsmap$bsname)){lstimgsmap$site[y]=strsplit(lstimgsmap$bsname[y],"_")[[1]][3]}
 lstimgsmap$site=gsub(".JPG","",lstimgsmap$site)   
 uniqsite=unique(lstimgsmap$site)
   for (y in 1:length(uniqsite)){
   site=uniqsite[y]
   imgsite=lstimgsmap$pth[lstimgsmap$site==site]
    index=sample(1:length(imgsite))[1:100]
    lstimgsmapsample= imgsite[index]
	mapdirto=paste0(DirTo,"\\",map1);dir.create(mapdirto, showWarnings = F)
	sitedirto=paste0(mapdirto,"\\",site);dir.create(sitedirto, showWarnings = F)
	file.copy(lstimgsmapsample,sitedirto)
 
   }

 
 }
 
 ######################
 Dir = "E:\\SSL_DB_Sample"
 imgs=list.files(Dir, full.names=T, recursive=T)
 imgsT=data.frame(pth=imgs)
 imgsT$bsname=basename(imgsT$pth)
    for(y in 1:length(imgsT$pth)){imgsT$site[y]=strsplit(imgsT$bsname[y],"_")[[1]][3]}
	 imgsT$site=gsub(".JPG","",imgsT$site)   
	length(unique(imgsT$site))