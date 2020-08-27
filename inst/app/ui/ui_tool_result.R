
# paired with server_tool_result.R

tabItem_tool_result = tabItem(tabName = "tool_result_tab", 
                                                
  fluidRow(
    
    box(title = "Input2", status = "primary", solidHeader = T, collapsible = T, width = 12,
        uiOutput("pool2_from_UI"),
        actionButton(inputId = "render_all_plots_btn2", label = "Show All Plots"))
  )
)
