
# paired with server_pool_tool.R

tabItem_tool_report = tabItem(tabName = "report_tab",  
  fluidRow(
    
    # box(title = "My Report", status = "primary", solidHeader = T, collapsible = T, width = 12,
    column(3,
           uiOutput("report_content_UI")),
    
    # column(3,
    #        uiOutput("report_title_UI")),
    
    column(6,
           uiOutput("report_des_UI"))
  )
  # ,
  # fluidRow(
  #   box(title = "Categorical Item Attributes", status = "primary", solidHeader = T, collapsible = T, width = 6,
  #       actionButton("show_report_btn", "Update Report"),
  #       htmlOutput("reportHtml", inline = T))
  # )
  
  
)
