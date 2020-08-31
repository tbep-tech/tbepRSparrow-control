
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
library(plotly)
library(sp)
library(spdep) 
library(data.table)
library(rstudioapi)
library(svDialogs)


# shinyimage <- list(
#   file.output.list = file.output.list,
#   map_uncertainties = NA, 
#   BootUncertainties = BootUncertainties,
#   data_names = data_names, 
#   mapping.input.list = mapping.input.list,
#   subdata = subdata,
#   SelParmValues = SelParmValues,
#   sitedata = sitedata,
#   estimate.list = estimate.list, 
#   estimate.input.list = estimate.input.list,
#   ConcFactor = ConcFactor,
#   DataMatrix.list = DataMatrix.list, 
#   dlvdsgn = dlvdsgn,
#   reach_decay_specification = reach_decay_specification, 
#   reservoir_decay_specification = reservoir_decay_specification,
#   scenario.input.list = scenario.input.list, 
#   if_predict = if_predict,
#   add_vars = add_vars,
#   batch_mode = batch_mode,
#   RSPARROW_errorOption = RSPARROW_errorOption
# )
# save(shinyimage, file = here::here('shinyimage.RData'))

load(file = 'shinyimage.RData')

for(i in 1:length(shinyimage)) 
  assign(names(shinyimage)[i], shinyimage[[i]])

# reset paths to relative
path_results <- 'results/UpperMissOhio/'
path_gis <- 'gis'

path_master <- "../tbepRSparrow/"
devtools::load_all(path_master,recompile = F)  

shinyMap2(file.output.list,map_uncertainties,BootUncertainties,
          data_names,mapping.input.list,
          subdata,SelParmValues,
          sitedata,
          estimate.list,estimate.input.list,
          ConcFactor,DataMatrix.list,dlvdsgn,
          reach_decay_specification,reservoir_decay_specification,
          scenario.input.list,if_predict,
          add_vars,
          batch_mode,
          RSPARROW_errorOption)

