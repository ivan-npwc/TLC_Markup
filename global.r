library(shiny)
library(purrr)
library(dplyr)
library(DT)
###############################################
 images_data = NULL
 images_df = NULL
 SSL_DBpth = ""
 images_path = "E:\\Image uniq sites folders"
  rds_path = "image_tiles.rds"
    if(file.exists(rds_path)==T){rds_path = "image_tiles.rds"}
    if(file.exists(rds_path)==F){rds_path = "Pleas select RDS system file"}

 source("Modules\\get_image_data.r")
  source("Modules\\quick_point_collector.r")
  source("Modules\\tile_optimization.r")
  source("Modules\\tiling.r")
  source("Modules\\RDStoTable.r")

 
 temp_file <- "images_data.csv"
 if (file.exists(temp_file)) {images_data<<- read.csv(temp_file)}
 if (file.exists(rds_path)){ RDSdata <<- readRDS(rds_path); RDStable <<- RDStoTable(rds_path)}
  if (file.exists(rds_path)==F){ RDSdata <<- list(); RDStable <<- data.frame(site="",poly="")}

########################################