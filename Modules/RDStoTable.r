
 RDStoTable=function(rds_path){
 library(dplyr)
library(tidyr)
library(purrr)
fin=NULL
data <- readRDS(rds_path)
sites = names(data)
  for (i in 1:length(sites)){
   site = sites[i]
   sitedata= data[[site]]
   polys=names(sitedata)
     for (y in 1:length(polys)){
	 poly=polys[y]
	 polydata= sitedata[[poly]]
	 dimensions=polydata$dimensions
	 dimensions = paste(dimensions, collapse = " ")
	 points= polydata[[1]]
	 points <- paste(points, collapse = " ")
	 site1=gsub("site_","",site)
	 poly1=gsub("poly_","",poly)
    df1 = data.frame(site=site1,poly=poly1,dimensions=dimensions,points=points)
	fin=rbind(fin,df1) 
}}
return(fin)
}
##########################################################