
# -------------------------------
#       Load ATA Example
# -------------------------------

output$show_gs_ui <- renderUI({

  cat("### ", format(Sys.time(), "%X"), " output$show_gs_ui <- renderUI\n")

  # rea_change_input_suorce()  is for ATA example
  # datafile() is the google sheets

  if (rea_change_input_source() > 0)  editDT2Input("editTable1")

})

observeEvent(input$load_example_btn, {

  cat("### ", format(Sys.time(), "%X"), " observeEvent:input$load_example_btn\n")

  examples = dir(system.file("rdsdata", package = "myFormAssembler"))
  available_examples = grep("^Ex|^MST|^CAT|^Forms", grep("\\.rds$", examples, value = T), value = T)

  # # only limit one example for IACAT
  # available_examples = grep("^Ex03", examples, value = T)
  #

  if (!(input$example_select %in% available_examples)) return ()

  rds_path = system.file("rdsdata", package = "myFormAssembler")
  # rds_file = file.path(rds_path, "MST05.rds")
  rds_file = file.path(rds_path, input$example_select)
  data = read_rds(rds_file)
  rea_change_input_source(rea_change_input_source()+1)

  data0 = data
  data0$mst = NULL

  # print(head(data0))
  # print(class(data0))

  rea_input$tibbles <- callModule(editDT2, "editTable1", data_sets = data0)

  # update the input UI
  # update number of forms

  content = data$constraint

  # get the number of items and points from content

  n_info = get_n_lower_pt_lower(content)

  # update the input widgets for number of items/points
  updateNumericInput(session, "ata_n_form", value = n_info$n_form)
  updateTextInput(session, "ata_n_item", value = paste0(n_info$n_lower, collapse = ","))
  updateTextInput(session, "ata_n_pt", value = paste0(n_info$pt_lower, collapse = ","))


  # I might use the slider below later
  # output$ata_n_item_sliders <- renderUI({
  #
  #   lapply(seq(n_form), function(i) {
  #     #   tags$style(HTML(".irs-bar {border-top: 1px solid green}")),
  #     div(style="height: 32px;}",
  #     sliderInput(inputId = paste0("ata_form_", i, "_n_item"), label = NULL,
  #                 # label =  paste0("form_", i),
  #                 step=1, ticks=F, pre =  paste0("form ", i, ": "),
  #                 post = " items",
  #                 min = 1, max = 100, value = n_lower[i]))
  #   })
  #
  # })

  # output$ata_n_pt_sliders <- renderUI({
  #
  #   lapply(seq(n_form), function(i) {
  #     div(style="height: 32px;}",
  #         sliderInput(inputId = paste0("ata_form_", i, "_n_pt"), label = NULL,
  #                     step=1, ticks=F, pre =  paste0("form ", i, ": "),
  #                     post = " points",
  #                     min = 1, max = 200, value = pt_lower[i]))
  #   })
  #
  # })
  # # lp_obj <- myFormAssembler::load_example("MST03")
  # rea_change_input_source(rea_change_input_source()+1)

})

# get_n_lower_pt_lower = function (content) {
#
#   # print(content$FORM)
#
#   the_Form = unique(as.character(content$FORM))
#   temp = c()
#   for (i in seq(length(the_Form)))
#     temp = c(temp, eval(parse(text = paste0("c(", the_Form[i], ")"))))
#   print(unique(temp))
#   n_form = length(unique(temp))
#   # n_form = length(unique(unlist(sapply( the_Form, #content$FORM,
#   #                                      function (i) eval(parse(text = paste0("c(", i, ")")))))))
#
#   # temp_str = paste0(aa, collapse = ",")
#   # n_form = length(unique(eval(parse(text = paste0("c(", temp_str, ")")))))
#
#   if (n_form > 0 && n_form != input$ata_n_form)
#     updateNumericInput(session, "ata_n_form", value = n_form)
#
#   # update number of items for forms
#
#   n_lower = rep(0, n_form)
#   n_upper = rep(0, n_form)
#
#   if ("SCOPE" %in% names(content))
#     temp <- content %>% filter(ATTRIBUTE == "_FORM" & SLICE_UNIT == "items" & (SCOPE == "WITHIN" | is.na(SCOPE))) %>%
#     select("FORM", "SLICE_LOWER", "SLICE_UPPER") else
#       temp <- content %>% filter(ATTRIBUTE == "_FORM" & SLICE_UNIT == "items") %>%
#     select("FORM", "SLICE_LOWER", "SLICE_UPPER")
#
#   if (nrow(temp) > 0)
#     for (i in 1:nrow(temp)) {
#       n_lower[eval(parse(text = paste0("c(", temp$FORM[i], ")")))] <- as.integer(temp$SLICE_LOWER[i])
#       n_upper[eval(parse(text = paste0("c(", temp$FORM[i], ")")))] <- as.integer(temp$SLICE_UPPER[i])
#     }
#
#   # update number of points for forms
#
#   pt_lower = rep(0, n_form)
#   pt_upper = rep(0, n_form)
#
#   if ("SCOPE" %in% names(content))
#     temp <- content %>% filter(ATTRIBUTE == "_FORM" & SLICE_UNIT == "points" & (SCOPE == "WITHIN" | is.na(SCOPE))) %>%
#     select("FORM", "SLICE_LOWER", "SLICE_UPPER") else
#       temp <- content %>% filter(ATTRIBUTE == "_FORM" & SLICE_UNIT == "points") %>%
#     select("FORM", "SLICE_LOWER", "SLICE_UPPER")
#
#   if (nrow(temp) > 0)
#
#     for (i in 1:nrow(temp)) {
#       pt_lower[eval(parse(text = paste0("c(", temp$FORM[i], ")")))] <- as.integer(temp$SLICE_LOWER[i])
#       pt_upper[eval(parse(text = paste0("c(", temp$FORM[i], ")")))] <- as.integer(temp$SLICE_UPPER[i])
#
#     } else {
#       pt_lower = n_lower
#       pt_upper = n_upper
#     }
#
#   return (list(n_lower = n_lower, n_upper = n_upper, pt_lower = pt_lower, pt_upper = pt_upper))
# }


# -----------------------------------
#       Reset Form Assembly Widget
# -----------------------------------

observeEvent(input$form_assembly_reset_btn, {

  reset_reactiveValues("FORM_ASSEMBLY")

  reset_form_assembly_UI()

})

reset_form_assembly_UI = function () {

  rea_input$tibbles <- callModule(editDT2, "editTable1", data_sets = list())

  updateNumericInput(session, "ata_n_form", value = 6, min = 1, max = 20, step = 1)
  updateTextInput(session, "ata_n_item",  value = "12,12,12,12,12,12")
  updateTextInput(session, "ata_n_pt", value = "22,22,22,22,22,22")

}

# --------------------------------------
#    Use editDT to show the ATA input
# --------------------------------------

observeEvent(input$ata_n_panel, {
  if (input$ata_n_panel == 1)
    updateNumericInput(session, inputId = "ata_n_panel_to_have",
                       min = 1, max = 4, step = 1, value = 1) else
    updateNumericInput(session, inputId = "ata_n_panel_to_have",
                       min = 1, max = 1, value = 1)

})

observeEvent(rea_input$tibbles, {

  # req(rea_input$tibbles())
  cat("### ", format(Sys.time(), "%X"), " observeEvent: rea_input$tibbles\n")

  # # if (!is.null(rea_ata$ata_list$output$lp_obj$constraint$content)) {
  # if (input$ata_form_type_radio == 'mst') {
  #   output$mst_path_select_UI <- renderUI({
  #     all_path = get_mst_path_from_content (rea_input$tibbles()$constraint) %>%
  #       paste() %>% gsub("\\)||c\\(", "", .)
  #     choices = seq(length(all_path))
  #     names(choices) = paste0("path--", all_path)
  #
  #     checkboxGroupInput("mst_path_select", "MST Path:",
  #                        choices = c("None" = "none", choices), inline = T, selected = choices)
  #   })
  # } else { # input$ata_form_type_radio == 'parallel forms'
  #   output$mst_path_select_UI <- renderUI({
  #     n_form = length(unique(unlist(sapply(rea_input$tibbles$content$FORM,
  #                                          function (i) eval(parse(text = paste0("c(", i, ")")))))))
  #     all_path = as.character(1:n_form)
  #     choices = seq(length(all_path))
  #     names(choices) = paste0("path--", all_path)
  #
  #     checkboxGroupInput("mst_path_select", "MST Path:",
  #                        choices = c("None" = "none", choices), inline = T, selected = choices)
  #   })
  # }

  # try to not limit only mst here
  # if (input$ata_form_type_radio == 'mst') {

      # this function is not good since it assems there is only 1 first module for each panel
      # n_panel = get_n_panel_from_content(rea_input$tibbles()$constraint)
      n_panel = 1    # design solution later
      updateNumericInput(session, inputId = "ata_n_panel",
                         min = n_panel, max = n_panel,
                         value = n_panel)
  # }

  editDTHas = names(rea_input$tibbles())

  # for adding a row to editDT:
  #        1. render a select input
  #        2. render an action button

  output$add_row_table_select_ui <- renderUI({

    choices = c("Select one table", editDTHas[-which(editDTHas == "item")])
    selectInput(inputId = "add_row_table_select", label = NULL,
                choices = choices)
  })

  output$add_row_btn_ui <- renderUI({

    actionButton(inputId = "add_row_btn", label = "Add a New Row to ", icon = icon("plus"))

  })

  # add the objectitve checkbox based on what are in the editDT tables

  choices = c("Information (tif)" = "tif",
              "Expected Score (tcc)" = "tcc")
              # "Extra Constraints" = "slice")

  has = which(choices %in% editDTHas)

  if (length(has) > 0) { # there is tif, tcc, or slice in the editDT tables

    choices0 = c("Minimum Overlap" = "min_overlap", choices[has])
    updateCheckboxGroupInput(session, "objective_checkbox", choices = choices0, selected = NULL)
  }

  # add_input_checkbox reactive to rea_input$tibbles so the user can add tif/tcc/slice to editDT tables

  output$add_input_table_select_ui <- renderUI({

    if (length(has) < length(choices)) {
      choices0 = choices[setdiff(1:length(choices), has)]
      selectInput(inputId = "add_row_table_select", label = NULL,
                  choices = choices0)
    }

  })

  # if (length(has) < length(choices)) { # there is still tif, tcc, or slice that can be added to editDT tables
  #
  #   choices0 = choices[setdiff(1:length(choices), has)]
  #   updateCheckboxGroupInput(session, "add_input_checkbox", choices = choices0)
  #
  # }

  # nothing can be listed in input$add_input_checkbox

  if (length(input$add_input_checkbox) ==0 || input$add_input_checkbox == "nothing to add") return()

  if (length(has) == length(choices)) {
    updateCheckboxGroupInput(session, "add_input_checkbox", choices = "nothing to add")  # change this later
  }

})

# --------------------------------------------
#    MST paths checkbox changes
# --------------------------------------------

observeEvent(input$mst_path_select, {
  if (input$mst_path_select[1] == "none")
    updateCheckboxGroupInput(session, "mst_path_select",
                             selected = "none")
})

# --------------------------------------------
#    Add tcc/tif/slice to the editDT tables
# --------------------------------------------

# TCC_ID	FORM	THETA	TCC_LOWER	TCC_UPPER	MARGIN	NOTE

observeEvent(input$add_input_table_btn, {

  cat("### ", format(Sys.time(), "%X"), " observeEvent: input$add_input_table_btn\n")

  validate(
    need(rea_input$tibbles, "Need to load input tables first!"),
           need(input$add_row_table_select, 'Check at least one table first'))

  data = isolate(rea_input$tibbles())

  if ("tif" %in% input$add_row_table_select) {
      tif = tibble(TIF_ID = "1", FORM = "1", THETA = 0.0,
                        TIF_LOWER = "0.2", TIF_UPPER = "0.5",  MARGIN = 0.0, NOTE = "note")
      data[["tif"]] = tif
  }

  if ("tcc" %in% input$add_row_table_select) {
    tcc = tibble(TCC_ID = "1", FORM = "1", SCOPE = NA, THETA = 0.0,
                 TCC_LOWER = "0.2", TCC_UPPER = "0.5",  MARGIN = 0.0, NOTE = "note")
    data[["tcc"]] = tcc
  }

  rea_input$tibbles <- callModule(editDT2, "editTable1", data_sets = data)
})

# --------------------------------------------
#    Add tcc/tif/slice to the editDT tables
# --------------------------------------------


# --------------------------------------------
#    Add a new row to editDT
# --------------------------------------------

observeEvent(input$add_row_btn, {

  cat("### ", format(Sys.time(), "%X"), " observeEvent: input$add_row_btn\n")

  req(rea_input$tibbles())
  if (!(input$add_row_table_select %in% names(rea_input$tibbles())))
    return()

  data = isolate(rea_input$tibbles())
  tb = input$add_row_table_select

  if (tb == "alias") {
    data[[tb]] = data[[tb]] %>% add_row(alias = NA)
  } else {
    data[[tb]] = data[[tb]] %>% add_row(MARGIN = NA)
  }

  rea_input$tibbles <- callModule(editDT2, "editTable1", data_sets = data)

})

# observeEvent(input$add_tif_btn, {
#
#   if ("tif" %in% names(rea_input$tibbles)) return()
#
#   tif = tibble(TIF_ID = "1", FORM = "1", THETA = 0.0,
#                TIF_LOWER = "0.2", TIF_UPPER = "0.5",  MARGIN = 0.0, NOTE = "note")
#
#   data = isolate(rea_input$tibbles())
#   data[["tif"]] = tif
#   rea_input$tibbles <- callModule(editDT, "editTable1", data_sets = data)
# })
#
# observeEvent(input$add_tcc_btn, {
#
#   if ("tcc" %in% names(rea_input$tibbles)) return()
#
#   tcc = tibble(TCC_ID = "1", FORM = "1", THETA = 0.0,
#                TCC_LOWER = "0.2", TCC_UPPER = "0.5",  MARGIN = 0.0, NOTE = "note")
#
#   data = isolate(rea_input$tibbles())
#   data[["tcc"]] = tcc
#   rea_input$tibbles <- callModule(editDT, "editTable1", data_sets = data)
#
# })

# ---------------------------
#         To Solve
# ---------------------------

observeEvent(input$go_solver_btn, {
  cat("### ", format(Sys.time(), "%X"), " observeEvent: input$go_solver_btn\n")
  closeAlert(session, "catSuiteAlertId")

  msg = ""
  tryCatch({
    withProgress(value = 0.1, message = "Solver preparing...", detail = "", {
      isolate({
        req(rea_input$tibbles, input$ata_n_item, input$ata_n_pt)

        # from UI input

        n_form = input$ata_n_form

        n_item = as.integer(unlist(strsplit(input$ata_n_item, ",")))

        n_pt = as.integer(unlist(strsplit(input$ata_n_pt, ",")))

        if (length(n_item) != n_form || length(n_pt) != n_form)
          stop("error")

        # get the tibbles in the editDT

        input_obj <- isolate(rea_input$tibbles())

        lp_obj <- init_lp_obj(
          basic = list(
            job_id = "My task",
            test_name = input$ata_test_name,
            n_form = n_form,

            n_item_l  = n_item,   # rep(12, n_form),
            n_item_u  = n_item,   #  rep(12, n_form),
            n_point_l = n_pt,     #rep(22, n_form),
            n_point_u = n_pt,     #rep(22, n_form),

            form_name = paste0("FORM_", 1:n_form),
            relax = 0),

          input = list(
            files = NULL,
            input_obj = input_obj),

          objective <- if (!is.null(input$objective_checkbox))  c("content", input$objective_checkbox) else "content"

        )
        print(lp_obj$objective)
        solver = "cbc" # input$solver_type
        timeout = input$solver_timeout
        gap = 0

        lp_obj$options$solver = solver

        lp_obj$options$timeout = timeout

        lp_obj$options$gap = gap

        lp_obj$options$verbose = verbose

        if (!is.null(lp_obj$files$cat_input_file))
          lp_obj <- build_input_from_cat (lp_obj)

        if (length(setdiff(lp_obj$files, "cat_input_file")) > 0)
          lp_obj <- read_input(lp_obj)

        if (!is.null(lp_obj$input_obj))
          lp_obj <- get_input (lp_obj, lp_obj$input_obj, add_default_alias = FALSE)

        if (!is.null(lp_obj$template) && !is.null(lp_obj$use_template))
          lp_obj <- apply_template(lp_obj)

        setProgress(value = 0.2, message = "Solver running...")

        lp_obj <- to_solve (lp_obj, timeout = timeout, solver = solver, gap = gap, verbose = T)

      }) # end isolate

      if (is.null(lp_obj$error.message) & "x" %in% names(lp_obj)) {
        createAlert(session, "catSuiteAlert", "catSuiteAlertId", title = "ATA", content = paste0(msg, "Feasible solutions found and the results updated"), append = FALSE, style = "info")
      } else {
        createAlert(session, "catSuiteAlert", "catSuiteAlertId", title = "ATA", content = paste0(msg, lp_obj$error.message), append = FALSE, style = "warning")
      }

      # saveRDS(lp_obj, file = "lp_obj.rds")

      rea_ata$ata_list$output$lp_obj <- lp_obj

      if (!is.null(rea_ata$ata_list$output$lp_obj$output$form)) {

        # uniqueForms = unique(rea_ata$ata_list$output$lp_obj$output$form$form_ind)
        # if (length(uniqueForms) > 0) {
        #   updateSelectInput(session, "send_form_select", choices = c(uniqueForms), selected = uniqueForms[0])
        # }

        # MODULE_FILE = lp_obj$output$form %>%
        #   transmute(
        #     MOD_ID = paste("module-", form_ind, sep=""),
        #     ITEM_ID = lp_obj$items[[lp_obj$alias$item_id]][item_ind]
        #   ) %>%
        #   group_by(MOD_ID) %>%
        #   summarise(
        #     NUM_RES = as.integer(0),
        #     NUM_ITEMS = n(),
        #     ITEM_IDS = paste(ITEM_ID, collapse=" ")
        #   )
        #
        # output$parallel_forms_output <- renderTable(MODULE_FILE)
      }
    })
  }, error = function(e) {
    errDetail = as.character(e)
    cat("### ", format(Sys.time(), "%X"), " ATA failed: ", errDetail, "\n")
    createAlert(session, "catSuiteAlert", "catSuiteAlertId", title = "ATA", content = errDetail, append = FALSE, style = "warning")
  })
})

observeEvent(rea_ata$ata_list$output, {

  cat("### ", format(Sys.time(), "%X"), " ********  observeEvent(rea_ata$ata_list$output$lp_obj$output\n")

  # req(rea_ata$ata_list$output$lp_obj$output)

  lp_obj = rea_ata$ata_list$output$lp_obj

  req(rea_ata$ata_list$output$lp_obj$output$form)

  uniqueForms = unique(lp_obj$output$form$form_ind)

  if (length(uniqueForms) > 0) {
    updateSelectInput(session, "send_form_select", choices = c(uniqueForms), selected = uniqueForms[0])
  }


  form_summary = lp_obj$output$form_summary %>%
    mutate(mean_diff = round(mean_diff, 4),
           sd_diff   = round(sd_diff, 4),
           min_diff  = round(min_diff, 4),
           max_diff  = round(max_diff, 4))


  # add_to_report (what = datatable(form_summary), title = paste0(input$ata_test_name, "--Form Summary "),
  #                 des = "add your Form Summary description here")

  output$parallel_forms_output <- DT::renderDataTable(form_summary)

  # original
  # lp_obj = rea_ata$ata_list$output$lp_obj
  # if (length(rea_ata$ata_list$output$lp_obj$output$form) > 0) {
  #
  #   uniqueForms = unique(rea_ata$ata_list$output$lp_obj$output$form$form_ind)
  #   if (length(uniqueForms) > 0) {
  #     updateSelectInput(session, "send_form_select", choices = c(uniqueForms), selected = uniqueForms[0])
  #   }
  #
  #   uniqueFormItemInd = sort(unique(lp_obj$output$form$item_ind))
  #   countFormItemInd = vapply(uniqueFormItemInd, function(itemInd) {
  #     sum(lp_obj$output$form$item_ind == itemInd)
  #   }, as.integer(0))
  #   for (i in which(countFormItemInd > 1)) {
  #     cat("overlap count:", countFormItemInd[i], "item:", lp_obj$items[[lp_obj$alias$item_id]][uniqueFormItemInd[i]], "\n")
  #   }
  #
  #   parallel_tb = lp_obj$output$form %>%
  #            transmute(
  #              MOD_ID = paste("module-", form_ind, sep=""),
  #              ITEM_ID = lp_obj$items[[lp_obj$alias$item_id]][item_ind]
  #            ) %>%
  #            group_by(MOD_ID) %>%
  #            summarise(
  #              NUM_RES = as.integer(0),
  #              NUM_ITEMS = n(),
  #              ITEM_IDS = paste(ITEM_ID, collapse=" ")
  #            )
  #   output$parallel_forms_output <- DT::renderDataTable(parallel_tb)
  #
  # } else {
  #
  #   output$parallel_forms_output <- DT::renderDataTable({
  #     NULL
  #   })
  #
  # }


})

# output$parallel_forms_output <- DT::renderDataTable({
# # output$parallel_forms_output <- renderDT({
#
#
#   req(rea_ata$ata_list$output$lp_obj$output$form)
#
#   cat("### ", format(Sys.time(), "%X"), " output$parallel_forms_output\n")
#
#   lp_obj = rea_ata$ata_list$output$lp_obj
#
#   return(lp_obj$output$form %>%
#     transmute(
#       MOD_ID = paste("module-", form_ind, sep=""),
#       ITEM_ID = lp_obj$items[[lp_obj$alias$item_id]][item_ind]
#     ) %>%
#     group_by(MOD_ID) %>%
#     summarise(
#       NUM_RES = as.integer(0),
#       NUM_ITEMS = n(),
#       ITEM_IDS = paste(ITEM_ID, collapse=" ")
#     ))
# })


# --------------------------------
#         To Sent to Simulator
# --------------------------------

observeEvent(input$send_to_simulator_btn, {
  cat("### ", format(Sys.time(), "%X"), " observeEvent: input$send_to_simulator_btn form: ", input$send_form_select, "\n")
  req(rea_ata$ata_list$output$lp_obj$output$form)

  rea_ata$ata_list$output$lp_obj$test_name = input$ata_test_name
  # save an ATA object for debugging purpose
  # saveRDS(rea_ata$ata_list$output$lp_obj, file = "ata.rds")
  rea_simulator$latest <- formAssemblyToSimulation(rea_ata$ata_list$output$lp_obj, input$send_form_select)
  # save a simulation object for debugging purpose
  # saveRDS(rea_simulator$latest, file = "simulation.rds")

  syncTestConfigUI(rea_simulator$latest)
  updateTabItems(session, "tabs", "simulator_input_tab")
})

# ex01_simulation = readRDS(system.file("example/shallow-multistage.rds", package = "CATSimulator"))
# ex02_simulation = readRDS(system.file("example/shallow-linear.rds", package = "CATSimulator"))
# lp_obj = readRDS("ata.rds")
# selectForm = "8"
formAssemblyToSimulation <- function(lp_obj, selectForm) {
  simulation = list(
    testName = paste0(lp_obj$test_name, "-form-", selectForm)
  )

  # Handle simulation$constraints$content
  # CONS_ID WEIGHT LOWER UPPER LABEL
  # <chr>    <dbl> <dbl> <dbl> <chr>
  # CONS01     100  0.1   0.15 1A
  # simulation$constraints = list(
  #   content = read_rds(system.file("example/bp_2level.rds", package = "CATSimulator"))
  # )

  # Handle simulation$itempool
  # ITEM_ID MODEL    NC PAR_1 PAR_2 PAR_3 CONS_IDS
  # <chr>   <chr> <int> <dbl> <dbl> <dbl> <list>
  # ITEM001 3PL       2  1.7  -0.48  0.17 <chr [2]>
  # simulation$itempool = read_rds(system.file("example/pool_3pl.rds", package = "CATSimulator"))
  items = lp_obj$items[lp_obj$output$form$item_ind[lp_obj$output$form$form_ind == selectForm],]

  simulation$itempool = tibble(
    ITEM_ID = items[[lp_obj$alias$item_id]],
    MODEL = items[[lp_obj$alias$irt_model]],
    NC = items[[lp_obj$alias$point]]+1,
    PAR_1 = items[[lp_obj$alias$irt_par_a]],
    PAR_2 = items[[lp_obj$alias$irt_par_b]],
    PAR_3 = items[[lp_obj$alias$irt_par_c]]
  )

  # Handle simulation$control
  simulation$control = list()
  simulation$control$abilityEstimator = "mle"
  simulation$control$startTheta = 0.0
  simulation$control$generateGroups = TRUE
  simulation$control$itemSelectionRule = "linear"
  simulation$control$terminationRule = "asap"
  simulation$control$terminationValue = 0.4
  simulation$control$minItems = simulation$control$maxItems = nrow(simulation$itempool)

  return(simulation)
}

output$mst_structure_plot = renderPlot({
  cat("### ", format(Sys.time(), "%X"), " output$mst_structure_plot\n")

  # if (is.null(rea_simulator$latest) | is.null(rea_simulator$latest$mst$path)) return ()
  # all_path = rea_simulator$latest$mst$path

  validate(need(rea_input$tibbles, "Need to load input tables first!"))

  all_path = get_mst_path_from_content (rea_input$tibbles()$constraint)

  # if (is.null(rea_ata$ata_list$output$lp_obj) | is.null(rea_ata$ata_list$output$lp_obj$mst$path)) return ()
  # all_path = rea_ata$ata_list$output$lp_obj$mst$path
  # print(all_path)

  edgeList <- NULL
  for (i in 1:length(all_path)) {
    single_path = all_path[[i]]
    for (j in 1:(length(single_path)-1)) {
      edgeList <- c(edgeList, c(single_path[j], single_path[j+1]))
    }
  }
  edgeList = matrix(edgeList, ncol = 2, byrow = TRUE)

  graph <- graph_from_edgelist(unique(edgeList))
  plot(graph, layout = layout_as_tree, vertex.size=30, vertex.label.dist=0, edge.arrow.size=0.5,
    vertex.color = "#EFEDF5")
    # vertex.color= c("#F0F0F0", "#efedf5", "#bcbddc", "#756bb1", "#FFF5EB", "#FEE6CE", "#FDD0A2", "#FDAE6B"))
})
