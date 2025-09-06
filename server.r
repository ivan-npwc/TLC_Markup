
function(input, output, session) {
  
 ####################################################### 
 values <- reactiveValues(
  image_path = NULL,
  imgdata = NULL,
  pnts = NULL,
  sections = NULL,
  site = NULL,
  camId = NULL
)
 table_trigger <- reactiveVal(0)
 ##############################################
  observeEvent(input$load_rds, {
     path <- file.choose()
	 file.copy(path,"image_tiles.rds",overwrite = T)
     RDSdata <<- readRDS("image_tiles.rds") #  values$all_data
     showNotification("RDS file upload")
  })
  ####################################################################
  observeEvent(input$load_images, {
    path <- choose.dir()
	showNotification(paste("Start Geting Image Data"))
    images_data <<- get_image_data(path); write.csv(images_data,temp_file, row.names=F)
    showNotification(paste("Found", nrow(images_data), "images"))
  #  }
  })
 #############################################################
  is_marked <<- function(site, poly, RDSdata) {
    if (site %in% names(RDSdata)) {
      if (poly %in% names(RDSdata[[site]])) {
	     if(is.null(RDSdata[[site]][[poly]][[1]])==F){
        return("✓")
      }
    }
 }	
    return("✗")
  }
###############################################################
  is_masked <<- function(site, poly, RDSdata) {
    if (site %in% names(RDSdata)) {
      if (poly %in% names(RDSdata[[site]])) {
	     if(is.null(RDSdata[[site]][[poly]]$Mask)==F) {
        return("✓")
      }
    }
 }	
    return("✗")
  }
###############################################################
  output$images_table <- renderDT({
  table_trigger()
    req(images_data) 
    req(RDSdata)
   
  if (is.null(images_data)==F ){
    images_df <- images_data %>%
	
      rowwise() %>%
      mutate(
        Marked = is_marked(site, poly, RDSdata),
		Masked = is_masked(site, poly, RDSdata),
		
        Mark = paste0('<button class="btn btn-xs btn-primary" onclick="Shiny.setInputValue(\'mark_image\', \'', image_path, '\')">Mark</button>'),
		Mask = paste0('<button class="btn btn-xs btn-primary" onclick="Shiny.setInputValue(\'mask_image\', \'', image_path, '\')">Mask</button>'),
      ) %>%
      ungroup() %>%


      select(image_path, site, poly,  Masked, Marked, Mask, Mark) 
	
    }
    datatable(
      images_df,
      escape = FALSE,
      options = list(pageLength = 10, dom = 'tip'),
      selection = 'none'

    )
  })
 ################################################################################# 
  observeEvent(input$mark_image, {
  print("START MARKING")
  # Очищаем предыдущие значения
  values$image_path <- input$mark_image
  values$imgdata <- images_data[basename(images_data$image_path) == basename(values$image_path),] 
  values$pnts <- quick_point_collector(values$image_path, values$imgdata, RDSdata=RDSdata, n=4)

 #  image_path <<- input$mark_image
 #  imgdata  <<- images_data[basename(images_data$image_path) == basename(image_path),] 
 #  pnts <<- quick_point_collector(image_path,imgdata,RDSdata=RDSdata,n=4) 
 

#######
    showModal(modalDialog(
      title = "Confirm to save",
      "Save?",
      footer = tagList(
        actionButton("confirmSaveMark", "Yes", icon = icon("check")),
        modalButton("No", icon = icon("times"))
                      )
    ))
  })
########################################
  observeEvent(input$mask_image, {

   image_path <<- input$mask_image
   imgdata  <<- images_data[basename(images_data$image_path) == basename(image_path),] 
   pnts <<- quick_point_collector(image_path,imgdata,RDSdata=RDSdata,n=100) 
  #####
    showModal(modalDialog(
      title = "Confirm to save",
      "Save?",
      footer = tagList(
        actionButton("confirmSaveMask", "Yes", icon = icon("check")),
        modalButton("No", icon = icon("times"))
                      )
    ))
  })

######################################
 observeEvent(input$confirmSaveMark, {
  req(values$image_path, values$imgdata, values$pnts)
  
  values$sections <- tile_optimization(values$imgdata, values$pnts, scl=1.8)
  dimensions <- values$imgdata$dimensions
  values$site <- values$imgdata$site
  values$camId <- values$imgdata$poly
  
  unnamed_list <- list(values$sections, dimensions)
  
  if (!is.null(RDSdata[[values$site]][[values$camId]])) {
    RDSdata[[values$site]][[values$camId]][[1]] <- values$sections
    RDSdata[[values$site]][[values$camId]][[2]] <- dimensions
  } else {
    RDSdata[[values$site]][[values$camId]] <- list(
      values$sections,
      dimensions = as.numeric(strsplit(dimensions, " ")[[1]]),
      Mask = c()
    )
  }
  
  saveRDS(RDSdata, "image_tiles.rds")
  RDSdata <<- readRDS(rds_path)
  table_trigger(table_trigger() + 1)
  
  removeModal()
  print(values$image_path)
  print(RDSdata[[values$site]][[values$camId]])
  
  showNotification("Data saved", type = "message", duration = 5)
})
################################################
  observeEvent(input$confirmSaveMask, {
  image_path
  pnts
  dimensions= imgdata$dimensions
    site <<- imgdata$site
    camId <<-  imgdata$poly
     poly <- camId
  
     unnamed_list=list(c(), dimensions)
	 
	   if (is.null(RDSdata[[site]][[camId]])==F){RDSdata[[site]][[camId]][["Mask"]]=pnts} # ovewrite existing data
       if (is.null(RDSdata[[site]][[camId]])==T){
		                                      RDSdata[[site]][[camId]] <- list(
                                              unnamed_list[[1]],  
                                              dimensions = dimensions)
											  RDSdata[[site]][[camId]][["Mask"]]=pnts
												 
	   }
	   saveRDS(RDSdata, "image_tiles.rds")
       RDSdata <<- readRDS(rds_path) #  values$all_data
    


     removeModal()
     print(image_path)
	 print(pnts)

    showNotification("Data saved",  type = "message", duration = 5)
}) 
##################################################
}
