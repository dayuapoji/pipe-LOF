# SETUP ###########################################################

# Library 
library(rstudioapi) # setup
library(tidyverse)  # data
library(magrittr)
library(lubridate) # time data
library(cowplot) # plot
library(caret) # ml
library(ranger)
library(egg)
library(ggrepel)

# Set current directory as working directory
setwd(dirname(getActiveDocumentContext()$path))

source('functions/get_pred.R')
source('functions/monotone_fit.R')

# list of functions
# functions <- list.files(path = 'functions/', pattern = "[.]R$", 
#                         full.names=TRUE, recursive = TRUE)
# 
# # load
# for (i in (1:length(functions))) {
#   print(functions[i])
#   source(functions[i])
# }
