
getClassificationAccuracy <- function(cutThetas, simulationResult) {
  simuleeIds = unique(simulationResult$output$SIM_ID)
  simuleeLastRows = vapply(simuleeIds, function(simuleeId) {
    max(which(simulationResult$output$SIM_ID == simuleeId))
  }, as.integer(0), USE.NAMES = FALSE)
  simuleeTrueThetas = simulationResult$output$TRUE_THETA[simuleeLastRows]
  simuleeFinalThetas = simulationResult$output$THETA[simuleeLastRows]

  breaks = c(-Inf, cutThetas, Inf)
  labels = 1:(length(cutThetas)+1)
  simuleeTrueGroups = cut(simuleeTrueThetas, breaks = breaks, labels = labels)
  simuleeFinalGroups = cut(simuleeFinalThetas, breaks = breaks, labels = labels)

  sameGroup = simuleeTrueGroups == simuleeFinalGroups
  return(sum(sameGroup) / length(sameGroup))
}

output$result_latest_table <- renderDT({

  cat("### ", format(Sys.time(), "%X"), " output$result_latest_table\n")

  req(!is.null(rea_simulator$latest), !is.null(rea_simulator$latest$result))

  df = getMeasurementResult(rea_simulator$latest$result, input$result_cut_theta)
  dt <- datatable(df, selection = "single", options = list(dom = 't'), rownames= FALSE) %>%
    formatRound(names(df), digits = DIGITS)
  dt
})

getMeasurementResult = function(result, cutThetas) {

  decisionAccuracy = getClassificationAccuracy(unique(cutThetas), result)

  df <- tibble(
    bias = result$irt$bias,
    correlation = result$irt$correlation,
    CA = decisionAccuracy,
    csem = result$irt$csem,
    mse = result$irt$mse,
    len = result$isr$testLengthMean,
    itemMaxExposure = result$iec$itemMaxExposure,
    itemsNeverUsed = result$iec$itemsNeverUsed
  ) %>% set_names(c("Bias", "Correlation", "CA", "CSEM", "MSE", "Test Length", "Item Max Exposure", "Items Never Used"))

  # add content alignment
  alignmentRate <- data.frame(t(result$isr$consViolations %>% select(CONS_ID, `0`) ), row.names = NULL) %>%
    set_names(head(.,1)) %>% slice(-1)

  df <- bind_cols(df, alignmentRate)

  return(df)

}

observeEvent(input$result_history_add_btn, {
  cat("### ", format(Sys.time(), "%X"), " observeEvent:input$result_history_add_btn\n")
  if (is.null(rea_simulator$latest) | is.null(rea_simulator$latest$result) | length(input$result_latest_label) < 1) return ()

  rea_simulator$latest$label = input$result_latest_label
  rea_simulator$history = c(rea_simulator$history, list(rea_simulator$latest))
})

observeEvent(input$result_history_delete_btn, {
  cat("### ", format(Sys.time(), "%X"), " observeEvent:input$result_history_delete_btn\n")
  if (length(input$result_history_table_rows_selected) < 1) return()

  rea_simulator$history = rea_simulator$history[-input$result_history_table_rows_selected]

  rea_simulator$selected = rea_simulator$latest
  updateSliderInput(session, "result_simulee_likelihood_slider", max = 1, value = 1)
})

observeEvent(input$result_history_clear_btn, {
  cat("### ", format(Sys.time(), "%X"), " observeEvent:input$result_history_clear_btn\n")
  rea_simulator$history = list()

  rea_simulator$selected = rea_simulator$latest
  updateSliderInput(session, "result_simulee_likelihood_slider", max = 1, value = 1)
})

output$result_history_table <- renderDT({
  req (length(rea_simulator$history) > 0)

  getResult_history_table()

})

getResult_history_table = function () {

  if (verbose) cat("### ", format(Sys.time(), "%X"), " call getResult_history_table() \n")

  dt0 <- map(rea_simulator$history, ~{
    bind_cols(label = .x$label, getMeasurementResult(.x$result, input$result_cut_theta))
  }) %>% bind_rows()

  dt <- datatable(dt0, options = list(dom = 't'), rownames= FALSE, width = "1200") %>%
    formatRound(names(dt0)[-1], digits = DIGITS)

  return(dt)

}

output$result_history_plot <- renderPlotly({
  cat("### ", format(Sys.time(), "%X"), " output$result_history_plot", input$result_history_plot_checkbox, "\n")
  if ((length(rea_simulator$history) < 1) | !input$result_history_plot_checkbox) return ()

  x = seq(length(rea_simulator$history))

  dataBias <- data.frame(
    HISTORY = x,
    BIAS = vapply(rea_simulator$history, function(history) {
      history$result$irt$bias
    }, as.numeric(0), USE.NAMES = FALSE)
  )
  pBias <- dataBias %>%
    plot_ly(name = "Bias",
            type = 'scatter',
            mode = 'lines+markers',
            x = ~HISTORY,
            y = ~BIAS)

  dataCorrelation <- data.frame(
    HISTORY = x,
    CORRELATION = vapply(rea_simulator$history, function(history) {
      history$result$irt$correlation
    }, as.numeric(0), USE.NAMES = FALSE)
  )
  pCorrelation <- dataCorrelation %>%
    plot_ly(name = "Correlation",
            type = 'scatter',
            mode = 'lines+markers',
            x = ~HISTORY,
            y = ~CORRELATION)

  dataCSEM <- data.frame(
    HISTORY = x,
    CSEM = vapply(rea_simulator$history, function(history) {
      history$result$irt$csem
    }, as.numeric(0), USE.NAMES = FALSE)
  )
  pCSEM <- dataCSEM %>%
    plot_ly(name = "CSEM",
            type = 'scatter',
            mode = 'lines+markers',
            x = ~HISTORY,
            y = ~CSEM)

  dataMSE <- data.frame(
    HISTORY = x,
    MSE = vapply(rea_simulator$history, function(history) {
      history$result$irt$mse
    }, as.numeric(0), USE.NAMES = FALSE)
  )
  pMSE <- dataMSE %>%
    plot_ly(name = "MSE",
            type = 'scatter',
            mode = 'lines+markers',
            x = ~HISTORY,
            y = ~MSE)

  dataMaxExposure <- data.frame(
    HISTORY = x,
    MAX_EXPOSURE = vapply(rea_simulator$history, function(history) {
      history$result$iec$itemMaxExposure
    }, as.numeric(0), USE.NAMES = FALSE)
  )
  pMaxExposure <- dataMaxExposure %>%
    plot_ly(name = "Max Item Exposure",
            type = 'scatter',
            mode = 'lines+markers',
            x = ~HISTORY,
            y = ~MAX_EXPOSURE)

  dataNeverUsed <- data.frame(
    HISTORY = x,
    NEVER_USED = vapply(rea_simulator$history, function(history) {
      history$result$iec$itemsNeverUsed
    }, as.numeric(0), USE.NAMES = FALSE)
  )
  pNeverUsed <- dataNeverUsed %>%
    plot_ly(name = "Items Never Used",
            type = 'scatter',
            mode = 'lines+markers',
            x = ~HISTORY,
            y = ~NEVER_USED)

  p <- subplot(pBias, pCorrelation, pCSEM, pMSE, pMaxExposure, pNeverUsed, nrows = 6, shareX = T) %>%
    layout(xaxis = list(range = c(1, length(rea_simulator$history)), tick0 = 1, dtick = 1, zeroline = FALSE))

  p$elementId <- NULL
  return (p)
})

observeEvent(input$result_latest_table_rows_selected, {
  cat("### ", format(Sys.time(), "%X"), " observeEvent:input$result_latest_table_rows_selected", input$result_latest_table_rows_selected, "\n")
  selectRows(dataTableProxy("result_history_table", session), selected = NULL)

  rea_simulator$selected = rea_simulator$latest
  updateSliderInput(session, "result_simulee_likelihood_slider", max = 1, value = 1)
})

observeEvent(input$result_history_table_rows_selected, {
  cat("### ", format(Sys.time(), "%X"), " observeEvent:input$result_history_table_rows_selected", input$result_history_table_rows_selected, "\n")
  selectRows(dataTableProxy("result_latest_table", session), selected = NULL)

  rea_simulator$selected = rea_simulator$history[[input$result_history_table_rows_selected]]
  updateSliderInput(session, "result_simulee_likelihood_slider", max = 1, value = 1)
})

output$result_scatter_plot <- renderPlotly({

  req (!is.null(rea_simulator$selected$result))

  getResult_scatter_plot(list(rea_simulator$selected))

})

getResult_scatter_plot = function(resultList) {

  if (length(resultList) < 1) return()

  p <- map(resultList, ~{
    simuleeIds = unique(.x$result$output$SIM_ID)
    simuleeLastRows = vapply(simuleeIds, function(simuleeId) {
      max(which(.x$result$output$SIM_ID == simuleeId))
    }, as.integer(0), USE.NAMES = FALSE)
    simuleeTrueThetas = .x$result$output$TRUE_THETA[simuleeLastRows]
    simuleeFinalThetas = .x$result$output$THETA[simuleeLastRows]

    data = data.frame(ID = simuleeIds, TRUE_THETA = simuleeTrueThetas, ESTIMATE = simuleeFinalThetas)
    cut_theta = unique(input$result_cut_theta)

    CATShinyModules::correlationPlot (
      plotData = data, xVar = "ESTIMATE", yVar = "TRUE_THETA", useDiffAsColor = F, cutsAt = cut_theta,
      xRange = c(THETA_LOWER, THETA_UPPER), addTitle = F, plotName = .x$label)

  })

  if (length(p) == 1) return(p[[1]])

  p %>% subplot(nrows = ceiling(length(p)/2), shareY = T, shareX = T)

}

output$CSEM_scatter_plot <- renderPlotly({

  if (is.null(rea_simulator$selected) | is.null(rea_simulator$selected$result)) return ()

  genCSEM_scatter_plot(list(rea_simulator$selected))

})

genCSEM_scatter_plot = function (resultList) {

  if (length(resultList) < 1) return()

  p <- map(resultList, ~{

    plotData <- .x$result$output %>%
      group_by(SIM_ID) %>% slice(n()) %>% ungroup() %>% select(THETA, CSEM)

    plotData %>%
      plot_ly(type = "scatter", mode = "markers", x = ~THETA, y = ~CSEM, colors = "Blues", opacity = .4, name = .x$label) %>%
      layout(xaxis = list(title = "Estimated Theta", range = c(THETA_LOWER, THETA_UPPER), zeroline = FALSE),
             yaxis = list(title = "CSEM", range = c(0, max(plotData$CSEM + 0.2)), zeroline = FALSE))
  })

  if (length(p) == 1) return(p[[1]])

  p %>% subplot(nrows = ceiling(length(p)/2), shareY = T, shareX = T)

}

output$adaptivity_plot <- renderPlotly({

  if (is.null(rea_simulator$selected) | is.null(rea_simulator$selected$result)) return ()

  genAdaptivity_plot(list(rea_simulator$selected), list(input$test_start_theta_slider))

})

# startThetaList = map(history, ~.x$control$startTheta)
genAdaptivity_plot = function (resultList, startThetaList) {

  if (length(resultList) < 1) return()

  p <- map2(resultList, startThetaList, ~{
      label = .x$label
      data0 = .x$result$output
      startTheta = .y
      data2 = data0 %>% select(SIM_ID, ITEM_DIFFICULTY, THETA) %>%
        group_by(SIM_ID) %>% group_modify(~{

          .x %>% mutate(FINAL_THETA = last(.x$THETA, 1)) %>%
            mutate(SEQ = 1:nrow(.x)) %>%
            mutate(DIFF = c(startTheta, .x$THETA[1:(nrow(.x)-1)]) - .x$ITEM_DIFFICULTY)
        }) %>% ungroup()

      n = length(unique(data2$SEQ))
      data3 = data2 %>% group_by(SEQ) %>% group_map(~{
        tibble(Item = .y,
               Average = mean(.x$DIFF, na.rm = T),
               Min = min(.x$DIFF, na.rm = T),
               Max = max(.x$DIFF, na.rm = T),
               Who = .x$SIM_ID[which(.x$DIFF == min(.x$DIFF, na.rm = T))[1]])
      }) %>% bind_rows()

      p0 <- plot_ly(data3, x = ~Item, y = ~Max, type = "scatter", mode = "lines",
                    line = list(color = 'rgba(0,100,80,1)'), showLegend = F, name = label) %>%
        add_trace(y = ~Min, type = "scatter", mode = "lines", fill = 'tonexty',
                  fillcolor='rgba(0,100,80,0.2)', line = list(color = 'rgba(0,100,80,1)'),
                  showlegend = F, name = "Mininum Difference") %>%
        add_trace(y = ~Average, type = "scatter", mode = "lines",
                  line = list(color = '#FF3344'),
                  showlegend = F, name = "Average Difference") %>%
        layout(title = "Adaptivity",
               xaxis = list(title = "Item Sequence"),
               yaxis = list(title = "Theta - Item Difficulty"))

  })

  if (length(p) == 1) return(p[[1]])

  p %>% subplot(nrows = ceiling(length(p)/2), shareY = T, shareX = T)

}

output$result_tif_plot <- renderPlotly({
  if (is.null(rea_simulator$selected) | is.null(rea_simulator$selected$result)) return ()

  itemPars = as.matrix(rea_simulator$selected$itempool[,c("PAR_1","PAR_2","PAR_3")])
  thetas = seq(THETA_LOWER, THETA_UPPER, by = 0.1)

  simuleeIds = unique(rea_simulator$selected$result$output$SIM_ID)
  testInf = sapply(simuleeIds, function (simuleeId) {
    simuleeItemIds = rea_simulator$selected$result$output$ITEM_ID[rea_simulator$selected$result$output$SIM_ID == simuleeId]
    simuleeItemPars = itemPars[which(rea_simulator$selected$itempool$ITEM_ID %in% simuleeItemIds),]
    vapply(thetas, function(theta) {
      sum(CATSimulator::itemInformation(simuleeItemPars, theta))
    }, as.numeric(0), USE.NAMES = FALSE)
  })
  testInf = data.frame(id = 1:ncol(testInf), theta = t(testInf))
  colnames(testInf) <- c("Id", thetas)
  data = reshape2::melt(testInf, id = "Id")
  colnames(data) = c("Id", "Theta", "Information")

  p <- data %>%
    group_by(Id) %>%
    plot_ly(x = ~Theta, y = ~Information) %>%
    add_lines(alpha = 0.3) %>%
    layout(xaxis = list(tickvals = seq(THETA_LOWER, THETA_UPPER, by = 0.2), zeroline = FALSE),
           yaxis = list(range = c(0, max(data$Information)), zeroline = FALSE))

  p$elementId <- NULL
  return (p)
})

output$result_all_simulees_table <- renderDT({
  if (is.null(rea_simulator$selected) | is.null(rea_simulator$selected$result)) return ()
  simuleeIds = unique(rea_simulator$selected$result$output$SIM_ID)

  simuleeLastRows = vapply(simuleeIds, function(simuleeId) {
    max(which(rea_simulator$selected$result$output$SIM_ID == simuleeId))
  }, as.integer(0), USE.NAMES = FALSE)
  data = rea_simulator$selected$result$output[simuleeLastRows, c("SIM_ID", "SEED", "TRUE_THETA", "THETA", "CSEM")]

  datatable(data, selection = "single", filter = 'top', options = list(pageLength = 10), rownames= FALSE) %>%
    formatRound(c("TRUE_THETA", "THETA", "CSEM"), digits = DIGITS)
})

observeEvent(input$result_all_simulees_table_rows_selected, {
  cat("### ", format(Sys.time(), "%X"), " observeEvent:input$result_all_simulees_table_rows_selected", input$result_all_simulees_table_rows_selected, "\n")
  if (length(input$result_all_simulees_table_rows_selected) < 1) {
    updateSliderInput(session, "result_simulee_likelihood_slider", max = 1, value = 1)
  } else {
    simuleeIds = unique(rea_simulator$selected$result$output$SIM_ID)
    selectedSimId = simuleeIds[input$result_all_simulees_table_rows_selected]
    simuleeOutputThetas = rea_simulator$selected$result$output[rea_simulator$selected$result$output$SIM_ID == selectedSimId & !is.na(rea_simulator$selected$result$output$THETA),]
    updateSliderInput(session, "result_simulee_likelihood_slider", max = nrow(simuleeOutputThetas), value = nrow(simuleeOutputThetas))
  }
})

output$result_simulee_ability_plot <- renderPlotly({
  cat("### ", format(Sys.time(), "%X"), " output$result_simulee_ability_plot", input$result_all_simulees_table_rows_selected, "\n")
  req(length(input$result_all_simulees_table_rows_selected) > 0)

  simuleeIds = unique(rea_simulator$selected$result$output$SIM_ID)
  selectedSimId = simuleeIds[input$result_all_simulees_table_rows_selected]

  data = rea_simulator$selected$result$output[rea_simulator$selected$result$output$SIM_ID == selectedSimId, c("ITEM_ID", "ITEM_DIFFICULTY", "TRUE_THETA", "THETA", "CSEM")]

  dataWithThetas = data[!is.na(data$THETA),]
  if (input$result_simulee_likelihood_slider > nrow(dataWithThetas)) return ()
  selectedRow = which(data$ITEM_ID == dataWithThetas$ITEM_ID[input$result_simulee_likelihood_slider])

  p <- data %>%
    plot_ly(name = "Difficulty",
            type = 'scatter',
            mode = 'markers',
            x = seq(nrow(data)),
            y = data$ITEM_DIFFICULTY) %>%
    layout(xaxis = list(autotick = FALSE, rangemode = "tozero", tick0 = 0),
           yaxis = list(range = c(THETA_LOWER, THETA_UPPER), zeroline = FALSE)) %>%
    add_trace(name = 'Interim Theta',
              type = 'scatter',
              mode = 'lines+markers',
              x = seq(nrow(data)),
              y = ~THETA,
              error_y = ~list(type = "data",
                              array = CSEM,
                              thickness=6,
                              width=0,
                              color='orange',
                              opacity=0.4)) %>%
    add_trace(name = "True Theta",
              mode = "lines",
              line = list(width = 2),
              x = seq(nrow(data)),
              y = ~TRUE_THETA) %>%
    add_segments(name = 'Item',
                 x = selectedRow, xend = selectedRow,
                 y = THETA_LOWER, yend = THETA_UPPER,
                 color = I(brewer.pal(3, "Dark2")[2]), opacity=0.5, showlegend = F)

  p$elementId <- NULL
  return (p)
})

output$result_simulee_likelihood_plot <- renderPlotly({
  cat("### ", format(Sys.time(), "%X"), " output$result_simulee_likelihood_plot", input$result_all_simulees_table_rows_selected, "slider", input$result_simulee_likelihood_slider, "\n")
  req (length(input$result_all_simulees_table_rows_selected) > 0)

  simuleeIds = unique(rea_simulator$selected$result$output$SIM_ID)
  selectedSimId = simuleeIds[input$result_all_simulees_table_rows_selected]
  simuleeOutput = rea_simulator$selected$result$output[rea_simulator$selected$result$output$SIM_ID == selectedSimId,]

  simuleeOutputWithThetas = simuleeOutput[!is.na(simuleeOutput$THETA),]
  if (input$result_simulee_likelihood_slider > nrow(simuleeOutputWithThetas)) return ()
  selectedRow = which(simuleeOutput$ITEM_ID == simuleeOutputWithThetas$ITEM_ID[input$result_simulee_likelihood_slider])
  interimTheta = simuleeOutput$THETA[selectedRow]

  assignedItemIndices = match(simuleeOutput$ITEM_ID[1:selectedRow], rea_simulator$selected$itempool$ITEM_ID)
  maxNC = rea_simulator$selected$itempool$NC
  PARCols = str_c("PAR_", 1:9)[str_c("PAR_", 1:9) %in% names(rea_simulator$selected$itempool)]

  item_pars = as.matrix(rea_simulator$selected$itempool[assignedItemIndices,PARCols])

  scores = simuleeOutput$SCORE[1:selectedRow]

  # Calculate the likelihood that each theta in thetaRange is the student's ability
  thetaRange = seq(from=THETA_LOWER, to=THETA_UPPER, by=0.1)
  scoreProb = CATSimulator::scoreProbability.scores(item_pars, thetaRange, scores)

  logLikelihood = apply(scoreProb, 2, function(thetaScoreProb) {
    sum(log(thetaScoreProb), na.rm = T)
  })

  data = data.frame(THETA = thetaRange, LIKELIHOOD = logLikelihood)

  p <- data %>%
    plot_ly(name = "Theta Likelihood",
            type = 'scatter',
            mode = 'lines+markers',
            x = ~THETA,
            y = ~LIKELIHOOD) %>%
    layout(xaxis = list(range = c(THETA_LOWER, THETA_UPPER), zeroline = FALSE),
           yaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = FALSE, showgrid = FALSE)) %>%
    add_segments(name = 'Interim Theta',
                 x = interimTheta, xend = interimTheta,
                 y = min(logLikelihood), yend = max(logLikelihood),
                 color = I(brewer.pal(3, "Dark2")[2]), opacity=0.5, showlegend = F)

  p$elementId <- NULL
  return (p)
})

output$result_simulee_table <- renderDT({
  cat("### ", format(Sys.time(), "%X"), " output$result_simulee_table", input$result_all_simulees_table_rows_selected, "\n")
  req (length(input$result_all_simulees_table_rows_selected) > 0)

  simuleeIds = unique(rea_simulator$selected$result$output$SIM_ID)
  selectedSimId = simuleeIds[input$result_all_simulees_table_rows_selected]

  outputRows = which(rea_simulator$selected$result$output$SIM_ID == selectedSimId)
  data = rea_simulator$selected$result$output[outputRows, !names(rea_simulator$selected$result$output) %in% c("SIM_ID", "SEED", "TRUE_THETA")]

  if (!is.null(rea_simulator$selected$constraints) & !is.null(rea_simulator$selected$constraints$content)) {
    assignedItemIndices = match(rea_simulator$selected$result$output$ITEM_ID[outputRows], rea_simulator$selected$itempool$ITEM_ID)
    itemConsIds = rea_simulator$selected$itempool$CONS_IDS[assignedItemIndices]
    data = cbind(data, CONS_IDS = vapply(itemConsIds, function(consIds) {
      paste(consIds, collapse=",")
    }, as.character(0), USE.NAMES = FALSE))
  }
  if (!("PSG_ID" %in% names(data)) & !is.null(rea_simulator$selected$constraints) & !is.null(rea_simulator$selected$constraints$passage)) {
    assignedItemIndices = match(rea_simulator$selected$result$output$ITEM_ID[outputRows], rea_simulator$selected$itempool$ITEM_ID)
    itemPsgId = rea_simulator$selected$itempool$PSG_ID[assignedItemIndices]
    data = cbind(data, PSG_ID = itemPsgId)
  }

  datatable(data, rownames= TRUE, extensions = c('Buttons','Scroller'),
            options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf'))) %>%
    formatRound(c("ITEM_DIFFICULTY", "THETA", "CSEM"), digits = DIGITS)
})

output$downloadResult2 <- downloadHandler(
  filename = "report.html",

  content = function(file) {
    req (length(rea_simulator$history) > 0)
    # need to put rmd into the library?
    tempReport <- file.path(tempdir(), "simulation-report.Rmd")
    file.copy(system.file("Rmd/simulation-report.Rmd", package = "catsuite"), tempReport, overwrite = TRUE)

    rmarkdown::render(tempReport, output_file = file,
                      params = list(n = 3)
    )
  }
)

