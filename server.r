
server <- function(input, output, session) {
  
 ####################################################### 
 #table_trigger <- reactiveVal(0)
 ##############################################
  # Load RDS file
  observeEvent(input$load_rds, {
     path <- file.choose()
	 file.copy(path,"image_tiles.rds")
     RDSdata <<- readRDS(path) #  values$all_data
     showNotification("RDS file upload")
  })
  ####################################################################
  # Load images folder
  observeEvent(input$load_images, {
    path <- choose.dir()
	showNotification(paste("Start Geting Image Data"))
    images_data <<- get_image_data(path); write.csv(images_data,temp_file, row.names=F)
    showNotification(paste("Found", nrow(images_data), "images"))
  #  }
  })
 #############################################################
   # Safe function to check if marked
  is_marked <<- function(site, poly, data) {

    if (site %in% RDStable$site) {
      if (poly %in% RDStable$poly) {
        return("✓")
      }
    }
    return("✗")
  }
###############################################################
  output$images_table <- renderDT({
  #table_trigger()
   
    req(images_data) 
    req(RDSdata)
   
  if (is.null(images_data)==F ){
    images_df <- images_data %>%
      rowwise() %>%
      mutate(
        Marked = is_marked(site, poly, RDSdata),
        Actions = paste0('<button class="btn btn-xs btn-primary" onclick="Shiny.setInputValue(\'mark_image\', \'', image_path, '\')">Mark</button>'),
	
      ) %>%
      ungroup() %>%
      select(image_path, site, poly, Marked, Actions)
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
  # pth=NULL
 #  pnts=NULL
  # imgdata=NULL
 #  sections=NULL
  showNotification("test",  type = "message", duration = 5)
  image_path <<- input$mark_image
   imgdata  <<- images_data[basename(images_data$image_path) == basename(image_path),] 
   pnts <<- quick_point_collector(image_path,imgdata,RDSdata=RDSdata) 
   
   #################################################################
    showModal(modalDialog(
      title = "Confirm to save",
      "Save?",
      footer = tagList(
        actionButton("confirmSave", "Yes", icon = icon("check")),
        modalButton("No", icon = icon("times"))
                      )
    ))
  })
########################################
  observeEvent(input$confirmSave, {
  
  image_path
  pnts
  imgdata
  sections <<-tile_optimization(imgdata,pnts, scl=1.8)
  dimensions= imgdata$dimensions
  site = paste0("site_",imgdata$site)
  camId = paste0("poly_",imgdata$poly)
  
       unnamed_list=list(sections, dimensions)
	   if (is.null(RDSdata[[site]][[camId]])==F){RDSdata[[site]][[camId]]=unnamed_list} # ovewrite existing data
       if (is.null(RDSdata[[site]][[camId]])==T){
		                                      RDSdata[[site]][[camId]] <- list(
                                              unnamed_list[[1]],  
                                              dimensions = as.numeric(strsplit(unnamed_list[[2]], " ")[[1]])  
                                                 )
	   }
	    
       saveRDS(RDSdata, "image_tiles.rds")

   # table_trigger(table_trigger() + 1)


     removeModal()
     print(image_path)
	 print(pnts)
     print(sections)

	# print(imgdata)
	  
	

    showNotification("Data saved",  type = "message", duration = 5)
})  
################################################
}
