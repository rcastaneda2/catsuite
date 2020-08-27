
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(reshape2)
library(DT)
library(plotly)
library(data.table)
library(RColorBrewer)
library(igraph)
library(stringr)

library(myFormAssembler)
library(CATSimulator)
library(CATShinyModules)


# This format is safe for filenames, because it avoids spaces and the ':' character.
DATETIME_FORMAT = "%Y-%m-%d-%H-%M"
DIGITS = 3
verbose = FALSE

THETA_LOWER = -4
THETA_UPPER = 4

SIMULATOR_EXAMPLES = dir(system.file("example",package = "CATSimulator"), recursive = T)
ATA_EXAMPLES = dir(system.file("example",package = "myFormAssembler"), recursive = T)

EXCLUDED_CATE_COLS = c("ITEM_ID", "IRT_PAR_D", "IRT_PAR_C", "IRT_SC")
INCLUDED_CATE_COLS = c("EVIDENCE_STATEMENT")

EXCLUDED_NUME_COLS = c("ITEM_ID", "ENTITY_ID")
INCLUDED_NUME_COLS = NULL  # not used and seems not necessary

EXCLUDED_COLS = union(EXCLUDED_CATE_COLS, EXCLUDED_NUME_COLS)

# SIMULATOR_EXAMPLES = SIMULATOR_EXAMPLES[grep("group|multistage|simple", SIMULATOR_EXAMPLES, invert = TRUE)]

# options

options(
  stringsAsFactors=FALSE,
  DT.options = list(pageLength = 50, scrollX = TRUE, language = list(search = 'Filter:'))
)

# colors

RED10 = c("#FFF4F4","#FCE0E0", "#FCD1D1", "#FCB3B3", "#FC79292", "#FC7676", "#FF6060", "#FC4444", "#FF2626", "#FF0505")
WHITE_RED10 = c("#FFFFFF", RED10)
LOWER_FREQ_UPPER = c("#D6CBD3", "#ECA1A6", "#BDCEBE")
LEGENDCLR = "darkblue"
LEGENDTITLECLR = "steelblue"

# others

trimAllSpace = function (str) {gsub("\\s+", "", str) }



