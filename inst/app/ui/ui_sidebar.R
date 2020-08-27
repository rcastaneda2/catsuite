
dashboardSidebar( sidebarMenu(

  id = "tabs",
  
  width = 250,
  
  # menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),

  ## Simulator widgets
  
  menuItem("Simulator", icon = icon("king", lib = "glyphicon"), tabName = "simulator_main",
           menuSubItem("Input", tabName = "simulator_input_tab"),
           menuSubItem("Create Input", tabName = "simulator_create_input_tab"),
           menuSubItem("Output", tabName = "simulator_output_tab"),
           menuSubItem("Output--Content Alignment", tabName = "simulator_content_tab")),
  
  ## ATA Form Assembly widgets

  menuItem("Form Assembly", icon = icon("queen", lib = "glyphicon"), tabName = "ATA_form_main",
           menuSubItem("Input", tabName = "form_assembly_input_tab"),
           menuSubItem("Output", tabName = "ATA_output_plot_tab")
           # menuSubItem("Table Output (not ready)", tabName = "ATA_output_table_tab")
  ),
  
  ## Module Assembly widgets
  
  ## menuItem("MST Module Assembly", icon = icon("bishop", lib = "glyphicon"), tabName = "MST_module_main",
  ##          menuSubItem("Input", tabName = "module_assembly_input_tab"),
  ##          menuSubItem("MST Create Input", tabName = "module_assembly_create_input_tab"),
  ##          menuSubItem("Output", tabName = "module_assembly_output_plot_tab")),
  
  ## Common Tools
  menuItem("Tools", icon = icon("pawn", lib = "glyphicon"), tabName = "tool_pool",
           menuSubItem("Pool Analysis", tabName = "pool_analysis_tab"))
  
  ## downloadButton('downloadReport', 'Download Report')
  
))
