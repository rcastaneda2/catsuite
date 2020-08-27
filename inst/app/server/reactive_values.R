# reactive values

# ATA data structure
#
#  1. ptr is the pointer pointing to the current ATA task
#
#  2. ata_list is a list holding ata data from different ATA tasks. e.g. ata_list[[1]] is the first task
#
#  3. add later 

# Simulator data structure
#  1. simulation is the $control, $constraints$content, $itempool and $groups input for the simulator
#  2. result is the $output, $irt, $isr and $iec output from the simulator

# for pool tool

rea_pool_tool = reactiveValues (
  pool = NULL,
  widget = NULL,
  IRTDataConf = NULL,
  categoryCols = NULL #c("TASK_TYPE", "STANDARD")
)

# x <- c(1:100)
# random_y <- rnorm(100, mean = 0)
# data <- data.frame(x, random_y)
# 
# p001 <- plot_ly(data, x = ~x, y = ~random_y, type = 'scatter', mode = 'lines')
# 
# trace_0 <- rnorm(100, mean = 5)
# trace_1 <- rnorm(100, mean = 0)
# trace_2 <- rnorm(100, mean = -5)
# x <- c(1:100)
# 
# data <- data.frame(x, trace_0, trace_1, trace_2)
# 
# p002 <- plot_ly(data, x = ~x, y = ~trace_0, name = 'trace 0', type = 'scatter', mode = 'lines') %>%
#   add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines+markers') %>%
#   add_trace(y = ~trace_2, name = 'trace 2', mode = 'markers')
# 
# # for report tool
# rea_report_tool = reactiveValues(
#   report_list = list(
#     list(content = p001,
#          title = "my pool1",
#          description = "my pool1 description"),
#     list(content = p002,
#          title = "my title 2",
#          description = "my pool2 description fdafdsafsaf"))
# )

# available content to be added to report
# MST IRT plots
# MST input tables

rea_report_tool = reactiveValues(
  report_list = list() # elements of content, title, and description
)

# for form assembly
rea_input = reactiveValues (tibbles = NULL)
rea_change_input_source <- reactiveVal(0)
rea_ata = reactiveValues(
  
  ptr = 0,
  ata_list = list(
    input = list(
      input_data_set = NULL
    ),
    output = list()
  )
)

# for module assembly
module_rea_input = reactiveValues (tibbles = NULL)
module_rea_change_input_source <- reactiveVal(0)
module_rea_ata = reactiveValues(
  
  ptr = 0,
  ata_list = list(
    input = list(
      input_data_set = NULL
    ),
    output = list()
  )
  
)

# for simiulator
rea_simulator = reactiveValues(
  # All three of these values are 'simulation' objects (history is a list of them).
  # They will each have fields like:
  #   latest$control, latest$constraints$content, latest$itempool from the example rdata file
  #   latest$result, latest$timestamp, latest$label if the simulation has been run
  latest = NULL,
  history = list(),
  selected = NULL,
  constraintControlData = NULL
)

# for create input in simulator
rea_self_input = reactiveValues(
  itempool = NULL,
  attr_to_cons_holder = NULL,
  constraints = NULL)

# for create input in MST
rea_self_MST_input = reactiveValues(
  itempool = NULL,
  attr_to_cons_holder = NULL,
  constraints = NULL,
  all_path = NULL,
  selected_path = NULL)
