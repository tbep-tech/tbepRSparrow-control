
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

# shinyimage <- list(path_results = here('results/TampaBayTP'),
#                    file_sum = 'TampaBayTP',
#                    path_gis = here('gis'),
#                    map_uncertainties = NA,
#                    BootUncertainties = NA,
#                    scenario_name = 'scenario1',
#                    ConcFActor = 0.001143648,
#                    reach_decay_specification = "exp(-data[,jdecvar[i]] * beta1[,jbdecvar[i]])",
#                    reservoir_decay_specification = "(1 / (1 + data[,jresvar[i]] * beta1[,jbresvar[i]]))",
#                    add_vars = c('huc2', 'huc4'),
#                    csv_columnSeparator = ',',
#                    csv_decimalSeparator = '.',
#                    batch_mode = 'no',
#                    ErrorOccured = 'no',
# 
#                    data_names = data_names,
#                    mapping.input.list = mapping.input.list,
#                    subdata = subdata,
#                    SelParmValues = SelParmValues,
#                    sitedata = sitedata,
#                    estimate.list = estimate.list,
#                    DataMatrix.list = DataMatrix.list
# )
# save(here('shinyimage.RData'))

load(file = here::here('shinyimage.RData'))

for(i in 1:length(shinyimage)) 
  assign(names(shinyimage)[i], shinyimage[[i]])

# reset paths to relative
path_results <- here::here('results/TampaBayTP')
path_gis <- here::here('gis')

path_master <- here::here("../tbepRSparrow/")
devtools::load_all(path_master,recompile = F)  
# 
# shinyMap2(#stream/catchment
#   path_results,file_sum,path_gis,map_uncertainties,BootUncertainties,
#   data_names,mapping.input.list,
#   subdata,SelParmValues,
#   sitedata,
#   scenario_name,estimate.list,
#   ConcFactor,DataMatrix.list,
#   reach_decay_specification,reservoir_decay_specification,
#   add_vars,csv_decimalSeparator,csv_columnSeparator,
#   batch_mode,ErrorOccured)


library(shiny)
library(sp)
library(data.table)
library(maptools)
library(rgdal)
library(shinyWidgets)
library(stringr)
library(rhandsontable)
library(leaflet)
library(sf)
library(mapview)
library(magrittr)


#load predicitons if available
if (file.exists(paste(path_results,"/predict/",file_sum,"_predictList",sep=""))){
  load(paste(path_results,"/predict/",file_sum,"_predictList",sep=""))
}

#estimation objects
if (file.exists(paste(path_results,"/estimate/",file_sum,"_JacobResults",sep=""))){
  if (!exists("JacobResults")){
    load(paste(path_results,"/estimate/",file_sum,"_JacobResults",sep=""))
  }
}





#set up variable choices
choices<-createInteractiveChoices(SelParmValues,exists("predict.list"),subdata, data_names, map_uncertainties)
scenarioChoices<-createInteractiveScenarioChoices()

#map type choices
if (exists("predict.list") & exists("JacobResults")){
  mapTypeChoices<-c("","Stream","Catchment","Site Attributes","Source Reduction Scenarios")
  selectSources<-as.character(JacobResults$Parmnames[which(JacobResults$btype=="SOURCE")]) 
  #  scenarioRtables<-createRTables(selectSources,data_names,mapping.input.list)
  
}else{
  mapTypeChoices<-c("","Stream","Catchment","Site Attributes")
  selectSources<-""
}

scenarioRtables<-createRTables(selectSources,data_names,mapping.input.list)

#setup shiny ui
shinyApp(  ui=shinyUI(

  fluidPage(tags$head(
    tags$style("h5{color: red}")),
    titlePanel(
      h1(paste("Rshiny Interactive Map : ",file_sum,sep=""),h5(div(HTML("DO NOT CLICK ON ITEMS ABOVE THIS POINT!"))))),

    sidebarLayout(
      sidebarPanel(width=6,
                   #tags$head(
                   #   tags$style("h5{color: black}")),
                   h4("SPARROW Interactive Mapping                     "),
                   br(),

                   #top level user input
                   selectInput("batch","Output Mode",c("Interactive","Batch"))#,
                   # selectInput("mapFormat", "Map Format", c('Dynamic', 'Static')),
                   #
                   # selectInput("mapType","Map Type",mapTypeChoices),
                   #
                   # #Stream and Catchment arguments
                   # streamCatch("nsStreamCatch", input, choices, map_uncertainties),
                   #
                   # #site Attribute arguments
                   # shinySiteAttr("nsSiteAttr",input,choices),
                   #
                   # #scenarios arguments
                   # shinyScenarios("nsScenarios",input),
                   #
                   # #output shape file ifBatch
                   # shapeFunc("nsBatch",input),
                   #
                   # # actionButton("showInput","Show Input"),
                   # conditionalPanel(
                   #   condition = "input.batch=='Interactive'",
                   #   fluidRow(
                   #     actionButton("goPlot","Generate Plot"),
                   #     actionButton("savePDF", "SaveAs PDF"))
                   #
                   # ),
                   #
                   # conditionalPanel(
                   #   condition = "input.batch=='Batch'",
                   #   actionButton("batchPlot","Save Plot(s)")
                   # )
      ),

      mainPanel(width = 6, NULL
                # conditionalPanel(
                #   condition = "input.mapFormat=='Static'",
                #   plotOutput("plotOne", width=900,height=900)
                # ),
                # conditionalPanel(
                #   condition = "input.mapFormat=='Dynamic'",
                #   leafletOutput("plotTwo", height = 800)
                # )
      )

    ))) #end ui function
  ,

  ################################################################
  ###############################################################
  ###############################################################

  server=shinyServer(function(input, output,session) {

    # #compile all user input
    # # observeEvent(input$showInput,{
    #
    # #  compileALL<-compileALL(input, output, session, path_results, choices)
    # #  compiledInput<-compileALL$compiledInput
    #
    #
    #
    # # output$txtOut<-renderPrint({compiledInput})
    # #  })
    #
    # #select all and clear all buttons in drop downs
    # observe({
    #   if (input$batch=="Batch" & input$mapType %in% c("Stream","Catchment","Site Attributes")){
    #
    #     lapply(1:length(as.character(unique(choices$category))), function(c) {
    #       category<-as.character(unique(choices$category))[c]
    #       if (category!="Prediction Uncertainties"){
    #         nsName<-paste0("ns",tolower(str_split(category," ")[[1]][1]),"Drop")
    #       }else{
    #         nsName<-"nsuncertaintyDrop"
    #       }
    #       callModule(selectAll,nsName, category = category, choices = choices)
    #     })
    #     callModule(selectAll,"nsattrDrop", category = "Data Dictionary Variable", choices = choices)
    #
    #   }
    # })
    #
    # #update variable lists according to variable type selection in interactive mode
    # observe({
    #   if (input$batch=="Interactive" & input$mapType %in% c("Stream","Catchment")){
    #     callModule(updateVariable,"nsStreamCatch", choices= choices, mapType = input$mapType)
    #
    #   }else if (input$batch=="Interactive" & input$mapType == "Site Attributes"){
    #     callModule(updateVariable,"nsSiteAttr", choices= choices, mapType = input$mapType)
    #   }
    # })
    #
    #
    # #rTables
    # observe({
    #   if (input$mapType %in% c("Source Reduction Scenarios")){
    #     callModule(shinyScenariosMod,"nsScenarios",scenarioRtables)
    #     #callModule(handsOnMod, "nsScenarios-nsAllSources", DF = scenarioRtables$allSourcesDF )
    #   }else if (input$mapType %in% c("Stream","Catchment")){
    #     callModule(handsOnMod, "nsStreamCatch-nsCosmetic", DF = as.data.frame(scenarioRtables$cosmeticPred))
    #   }else if (input$mapType == "Site Attributes"){
    #     callModule(handsOnMod, "nsSiteAttr-nsCosmetic", DF = as.data.frame(scenarioRtables$cosmeticSite))
    #   }
    # })
    #
    # #interactive plot
    # p <- eventReactive(input$goPlot, {
    #   #run plot
    #
    #   goShinyPlot(input, output, session, choices,"goPlot",
    #               path_results,file_sum,path_gis,map_uncertainties,BootUncertainties,
    #               data_names,mapping.input.list,
    #               #predict.list,
    #               subdata,SelParmValues,
    #               #site attr
    #               sitedata,#estimate.list,Mdiagnostics.list,
    #               #scenarios
    #               scenario_name,JacobResults,
    #               ConcFactor,DataMatrix.list,
    #               reach_decay_specification,reservoir_decay_specification,
    #               #scenarios out
    #               add_vars,csv_decimalSeparator,csv_columnSeparator,
    #               #batchError
    #               batch_mode,ErrorOccured)
    #
    # })
    #
    # observe({
    #
    #   if(input$mapFormat == 'Static')
    #     output$plotOne <- renderPlot(p())
    #
    #   if(input$mapFormat == 'Dynamic')
    #     output$plotTwo <- renderLeaflet(p())
    #
    # })
    #
    # #pdf output
    # observeEvent(input$savePDF, {
    #   goShinyPlot(input, output, session, choices,"savePDF",
    #               path_results,file_sum,path_gis,map_uncertainties,BootUncertainties,
    #               data_names,mapping.input.list,
    #               #predict.list,
    #               subdata,SelParmValues,
    #               #site attr
    #               sitedata,#estimate.list,Mdiagnostics.list,
    #               #scenarios
    #               scenario_name,JacobResults,
    #               ConcFactor,DataMatrix.list,
    #               reach_decay_specification,reservoir_decay_specification,
    #               #scenarios out
    #               add_vars,csv_decimalSeparator,csv_columnSeparator,
    #               #batchError
    #               batch_mode,ErrorOccured)
    # })#end pdf output
    #
    # #batchplot
    # observeEvent(input$batchPlot, {
    #   goShinyPlot(input, output, session, choices,"batchPlot",
    #               path_results,file_sum,path_gis,map_uncertainties,BootUncertainties,
    #               data_names,mapping.input.list,
    #               #predict.list,
    #               subdata,SelParmValues,
    #               #site attr
    #               sitedata,#estimate.list,Mdiagnostics.list,
    #               #scenarios
    #               scenario_name,JacobResults,
    #               ConcFactor,DataMatrix.list,
    #               reach_decay_specification,reservoir_decay_specification,
    #               #scenarios out
    #               add_vars,csv_decimalSeparator,csv_columnSeparator,
    #               #batchError
    #               batch_mode,ErrorOccured)
    # })#end batch plot
    # session$onSessionEnded(function() {
    #   stopApp()
    # })
  })#end server function
)
