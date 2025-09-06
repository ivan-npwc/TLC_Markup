options = list(display.mode='showcase')

fluidPage(



  sidebarLayout(
    sidebarPanel(
      width = 3,
      actionLink("load_rds", rds_path),
      br(),
      actionLink("load_images", images_path)
    ),
    mainPanel(
      width = 14,
      DTOutput("images_table")
    )
  )
)
#####################################################