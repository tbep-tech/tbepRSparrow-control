library(plyr)
library(ggplot2)
library(nlmrt)
library(numDeriv)
library(OpenMx)
library(stringr)
library(gear)
library(gplots) 
library(rgdal)
library(shiny)
library(sp)
library(spdep) 
library(data.table)
library(rstudioapi)
library(svDialogs)
library(here)

load('~/Desktop/shinyimage.RData')
for(i in 1:length(shinyimage)) 
  assign(names(shinyimage)[i], shinyimage[[i]])

path_master <- "C:/proj/tbepRSparrow/"
devtools::load_all(path_master,recompile = F)  

runApp(shinyMap2(#stream/catchment
  path_results,file_sum,path_gis,map_uncertainties,BootUncertainties,
  data_names,mapping.input.list,
  subdata,SelParmValues,
  sitedata,
  scenario_name,estimate.list,
  ConcFactor,DataMatrix.list,
  reach_decay_specification,reservoir_decay_specification,
  add_vars,csv_decimalSeparator,csv_columnSeparator,
  batch_mode,ErrorOccured))
