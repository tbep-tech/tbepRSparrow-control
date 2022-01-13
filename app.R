
path_PastResults <- "results/TampaTP_test4"
path_PastResults<-paste0(path_PastResults,"/maps/shinyArgs")
load(path_PastResults)

fls <- list.files('../tbepRSPARROW/R', pattern = '\\.R$', full.names = T)
for(fl in fls) source(fl)

unPackList(lists = list(shinyArgs = shinyArgs),
           parentObj = list(NA)) 
unPackList(lists = list(file.output.list = file.output.list),
           parentObj = list(NA)) 


#trigger shiny
shinyMap2(
  #stream/catchment
  file.output.list,map_uncertainties,BootUncertainties,
  data_names,mapping.input.list,
  #predict.list,
  subdata,SelParmValues,
  #site attr
  sitedata,
  #scenarios
  estimate.list,estimate.input.list,
  ConcFactor,DataMatrix.list,dlvdsgn,
  reach_decay_specification,reservoir_decay_specification,
  scenario.input.list,if_predict,
  #scenarios out
  add_vars,
  #batchError
  batch_mode,
  RSPARROW_errorOption)



