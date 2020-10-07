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
library(here)
library(shinycssloaders)
library(sf)
library(maptools)
library(rgdal)
library(shinyWidgets)
library(stringr)
library(rhandsontable)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(htmltools)
library(plotly)
library(mapview)
library(magrittr)
library(shinyjs)

# file.output.list <- lapply(file.output.list, function(x) gsub(paste0('C:/proj/tbepRSparrow-control'), '', x))
# file.output.list <- lapply(file.output.list, function(x) gsub('^/', '', x))
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
# save(shinyimage, file = 'shinyimage.RData')

# reset paths to relative
path_results <- 'results/TampaTP/'
path_gis <- 'gis'

path_master <- "../tbepRSparrow/"
devtools::load_all(path_master,recompile = F)

# load existing image
# load(file = 'shinyimage.RData')

for(i in 1:length(shinyimage)) 
  assign(names(shinyimage)[i], shinyimage[[i]])

unPackList(lists = list(file.output.list = file.output.list,
                        scenario.input.list = scenario.input.list,
                        mapping.input.list = mapping.input.list),
           parentObj = list(NA,NA, NA)) 

#load predicitons if available
if (file.exists(paste(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_predict.list",sep=""))){
  load(paste(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_predict.list",sep=""))
}

#estimation objects
if (file.exists(paste(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_JacobResults",sep=""))){
  if (!exists("JacobResults")){
    load(paste(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_JacobResults",sep=""))
  }
}

#set up variable choices
choices<-createInteractiveChoices(SelParmValues,exists("predict.list"),subdata, data_names, map_uncertainties)

#map type choices
if (exists("predict.list") & exists("JacobResults")){
  mapTypeChoices<-c("","Stream","Catchment","Site Attributes","Source Change Scenarios")
  selectSources<-as.character(JacobResults$Parmnames[which(JacobResults$btype=="SOURCE")])
  
}else{
  mapTypeChoices<-c("","Stream","Catchment","Site Attributes")
  selectSources<-""
}

scenarioRtables<-createRTables(selectSources,data_names,mapping.input.list)

#setup shiny ui
shinyApp(  ui=shinyUI(
  
  fluidPage(
    tags$head(tags$style("h5{color: red}")),
    
    shinyjs::useShinyjs(),
    
    titlePanel(
      h1(paste("RShiny Decision Support System : ",run_id,sep=""))),

         sidebarLayout(
           sidebarPanel(width=6,
                        h4("SPARROW Interactive Mapping                     "),
                        br(),
                        
                        #top level user input
                        selectInput("batch","Output Mode",c("Interactive","Batch")),
                        selectInput("enablePlotly","Output Map Format",c("static","plotly","leaflet"),selected = ifelse(mapping.input.list$enable_plotlyMaps=="yes","plotly","static")),
                        textOutput("plotlyExplanation"),
                        br(),
                        selectInput("mapType","Map Type",mapTypeChoices),
                        
                        #Stream and Catchment arguments
                        streamCatch("nsStreamCatch", input, choices, map_uncertainties,sitedata,add_plotlyVars),
                        
                        #site Attribute arguments
                        shinySiteAttr("nsSiteAttr",input,choices,sitedata,add_plotlyVars),
                        
                        #scenarios arguments
                        shinyScenarios("nsScenarios",input,choices,sitedata,add_plotlyVars, scenario.input.list),
                        
                        #output shape file ifBatch
                        shapeFunc("nsBatch",input),
                        
                        # actionButton("showInput","Show Input"),
                        conditionalPanel(
                          condition = "input.batch=='Interactive'",
                          fluidRow(
                            actionButton("goPlot","Generate Map"),
                            actionButton("savePDF", "Save Map"))       
                          
                        ),
                        
                        conditionalPanel(
                          condition = "input.batch=='Batch'",
                          actionButton("batchPlot","Save Map(s)")      
                        )
           ),
           mainPanel(width = 6,
                     uiOutput("plot")
                     
           )
         )
      
    )),
  
  server=shinyServer(function(input, output,session) {
    
    # shiny reactive values for image
    shinyrct <- reactiveValues(
      file.output.list = NULL,
      map_uncertainties = NULL,
      BootUncertainties = NULL,
      data_names = NULL,
      mapping.input.list = NULL,
      subdata = NULL,
      SelParmValues = NULL,
      sitedata = NULL,
      estimate.list = NULL,
      estimate.input.list = NULL,
      ConcFactor = NULL,
      DataMatrix.list = NULL,
      dlvdsgn = NULL,
      reach_decay_specification = NULL,
      reservoir_decay_specification = NULL,
      scenario.input.list = NULL,
      if_predict = NULL,
      add_vars = NULL,
      batch_mode = NULL,
      RSPARROW_errorOption = NULL
    )
    
    
    
    #update red labels
    observe({
      if (input$mapType!=""){
        updateSelectInput(session, "mapType",
                          label = "Map Type"
        )
      }
      
      #no plotly catchment maps in interactive mode
      currentSelect<-isolate(input$enablePlotly)
      if (input$mapType=="Catchment" & input$batch=="Interactive"){
        updateSelectInput(session,"enablePlotly","Output Map Format",c("static","leaflet"),selected = "static")
        output$plotlyExplanation<-renderText({"Plotly not available for catchment maps in Interactive mode due to long processing time\n to get interactive catchment maps select Batch mode and enable plotly"
        })
      }else if (input$mapType=="Source Change Scenarios"){
        if (length(input$`nsScenarios-outType`)==0){
          updateSelectInput(session,"enablePlotly","Output Map Format",c("static","plotly","leaflet"),selected = currentSelect)
          output$plotlyExplanation<-renderText({"Plotly Maps will take longer to render in Interactive mode"})
        }else{
          if (input$`nsScenarios-outType`=="Catchment" & input$batch=="Interactive"){
            updateSelectInput(session,"enablePlotly","Output Map Format",c("static","leaflet"),selected = "static")
            output$plotlyExplanation<-renderText({"Plotly not available for catchment maps in Interactive mode due to long processing time\n to get interactive catchment maps select Batch mode and enable plotly"})
          }else{
            updateSelectInput(session,"enablePlotly","Output Map Format",c("static","plotly","leaflet"),selected = currentSelect)
            output$plotlyExplanation<-renderText({"Plotly Maps will take longer to render in Interactive mode"})
          }
        }
      }else{
        updateSelectInput(session,"enablePlotly","Output Map Format",c("static","plotly","leaflet"),selected = currentSelect)
        output$plotlyExplanation<-renderText({"Plotly Maps will take longer to render in Interactive mode"})
      }
      
      
    })  
    
    #select all and clear all buttons in drop downs 
    observe({        
      if (input$batch=="Batch"){
        if (input$mapType %in% c("Stream","Catchment")){
          lapply(1:length(as.character(unique(choices$category))), function(c) {
            category<-as.character(unique(choices$category))[c]
            if (category!="Prediction Uncertainties"){
              nsName<-paste0("ns",tolower(str_split(category," ")[[1]][1]),"Drop")
            }else{
              nsName<-"nsuncertaintyDrop"
            }
            callModule(selectAll,nsName, category = category, choices = choices)
          })
        }else{
          choicesScen<-choices[which(!choices$category %in% c("Data Dictionary Variable","Prediction Uncertainties") & regexpr("Monitoring-adjusted",choices$definition)<0),]
          ratioChoices<-data.frame(category = c("Relative Change in Load","Relative Change in Load"),
                                   variable = c("ratio_total","ratio_inc","percent_total","percent_inc"),
                                   definition = c("Ratio of the changed total load to the baseline (unchanged) total load",
                                                  "Ratio of the changed incremental load to the baseline (unchanged) incremental load"))
          choices$category<-ifelse(choices$category=="Load Predictions","Load Predictions for Changed Sources",
                                   ifelse(choices$category=="Yield Predictions","Yield Predictions for Changed Sources",choices$category))
          choicesScen<-rbind(choicesScen,ratioChoices)
          
          lapply(1:length(as.character(unique(choicesScen$category))), function(c) {
            category<-as.character(unique(choicesScen$category))[c]
            nsName<-paste0("nsScen",tolower(str_split(category," ")[[1]][1]),"Drop")
            callModule(selectAll,nsName, category = category, choices = choicesScen)
          }) 
        }
        if (input$mapType %in% c("Stream","Catchment","Site Attributes")){
          callModule(selectAll,"nsattrDrop", category = "Data Dictionary Variable", choices = choices)
        }
        
      }
    })
    
    #update variable lists according to variable type selection in interactive mode
    observe({
      if (input$batch=="Interactive" & input$mapType %in% c("Stream","Catchment")){     
        callModule(updateVariable,"nsStreamCatch", choices= choices, mapType = input$mapType)
        
      }else if (input$batch=="Interactive" & input$mapType == "Site Attributes"){
        callModule(updateVariable,"nsSiteAttr", choices= choices, mapType = input$mapType)
      }else{
        choicesScen<-choices[which(!choices$category %in% c("Data Dictionary Variable","Prediction Uncertainties") & regexpr("Monitoring-adjusted",choices$definition)<0),]
        ratioChoices<-data.frame(category = c("Relative Change in Load","Relative Change in Load"),
                                 variable = c("ratio_total","ratio_inc","percent_total","percent_inc"),
                                 definition = c("Ratio of the changed total load to the baseline (unchanged) total load",
                                                "Ratio of the changed incremental load to the baseline (unchanged) incremental load"))
        choices$category<-ifelse(choices$category=="Load Predictions","Load Predictions for Changed Sources",
                                 ifelse(choices$category=="Yield Predictions","Yield Predictions for Changed Sources",choices$category))
        choicesScen<-rbind(choicesScen,ratioChoices)
        callModule(updateVariable,"nsScenarios", choices= choicesScen, mapType = input$mapType)
      }
    })
    
    observe({
      
      # shiny reactives from model run if present
      if(!is.null(unlist(reactiveValuesToList(shinyrct)))){
        scenario.input.list <- shinyrct$scenario.input.list
        mapping.input.list <- shinyrct$mapping.input.list
      }
      
      if (input$mapType %in% c("Source Change Scenarios")){
        callModule(shinyScenariosMod,"nsScenarios",scenarioRtables,path_results,scenario.input.list, mapping.input.list)
        
      }else if (input$mapType %in% c("Stream","Catchment")){
        testRow<-testCosmetic(input, output, session, 
                              DF = as.data.frame(scenarioRtables$cosmeticPred),mapType = input$mapType,
                              scenario.input.list, mapping.input.list)$rowNums
        callModule(validCosmetic,"nsStreamCatch-nsCosmetic", 
                   DF = as.data.frame(scenarioRtables$cosmeticPred),rowNum = testRow)
        
      }else if (input$mapType == "Site Attributes"){
        testRow<-testCosmetic(input, output, session, 
                              DF = as.data.frame(scenarioRtables$cosmeticSite),mapType = input$mapType,
                              scenario.input.list, mapping.input.list)$rowNums
        callModule(validCosmetic,"nsSiteAttr-nsCosmetic", 
                   DF = as.data.frame(scenarioRtables$cosmeticSite),rowNum = testRow)
        
      }
    })
    
    #interactive plot
    p1<-eventReactive(input$goPlot, {
      
      gc()
      output$plotOne<-NULL
      output$plotlyPlot<-NULL
      output$leafPlot<-NULL
      gc()
      
      suppressWarnings(remove(p,envir = .GlobalEnv))
      suppressWarnings(remove(currentP))
      
      # shiny reactives from model run if present
      if(!is.null(unlist(reactiveValuesToList(shinyrct)))){
        for(i in 1:length(shinyrct)) 
          assign(names(shinyrct)[i], shinyrct[[i]])      
      }
      
      #test bad Settings
      badSettings<-as.data.frame(matrix(0,ncol=4,nrow=0))
      names(badSettings)<-c("Setting","CurrentValue","Type","Test")
      errMsg<-NA
      if (input$mapType %in% c("Stream","Catchment")){
        badSettings<-testCosmetic(input, output, session, DF = as.data.frame(scenarioRtables$cosmeticPred),mapType =input$mapType,
                                  scenario.input.list, mapping.input.list)$badSettings
      }else if (input$mapType == "Site Attributes"){
        badSettings<-testCosmetic(input, output, session, DF = as.data.frame(scenarioRtables$cosmeticSite),mapType =input$mapType,
                                  scenario.input.list, mapping.input.list)$badSettings
      }else{
        errMsg1<-testRedTbl(input, output, session, DF = as.data.frame(scenarioRtables$sourceRed))$errMsg
        errMsg2<-testRedTbl(input, output, session, DF = as.data.frame(scenarioRtables$allSourcesDF))$errMsg
        errMsg3<-testRedTbl(input, output, session, DF = as.data.frame(scenarioRtables$allSourcesDFno))$errMsg
        
        
        errMsg<-na.omit(c(errMsg1,errMsg2, errMsg3))
        if (length(errMsg)==0){
          errMsg<-NA
        }else{
          errMsg<-errMsg[1]
        }
        
        badSettings<-testCosmetic(input, output, session, DF = as.data.frame(scenarioRtables$cosmeticScen), "Source Change Scenarios",
                                  scenario.input.list, mapping.input.list)$badSettings
        
      }
      
      
      currentP<-goShinyPlot(input, output, session, choices,"goPlot", badSettings,errMsg,
                            file.output.list,map_uncertainties,BootUncertainties,
                            data_names,mapping.input.list,
                            #predict.list,
                            subdata,SelParmValues,
                            #site attr
                            sitedata,estimate.list,estimate.input.list,#Mdiagnostics.list,
                            #scenarios
                            JacobResults,
                            ConcFactor,DataMatrix.list,dlvdsgn,
                            reach_decay_specification,reservoir_decay_specification,
                            scenario.input.list,if_predict,
                            #scenarios out
                            add_vars,
                            #batchError
                            batch_mode,
                            RSPARROW_errorOption)
      
      #print plot size
      env <- environment()
      objs<-data.frame(
        object = ls(env),
        size = unlist(lapply(ls(env), function(x) {
          object.size(get(x, envir = env, inherits = FALSE))})))
      
      print(paste0("Plot size : ",objs[which(objs$object=="currentP"),]$size))
      
      
      if (input$enablePlotly=="static"){ 
        output$plot<-renderUI({
          plotOutput("plotOne", width=900,height=900) %>% withSpinner(color="#0dc5c1")
        })
        #time plot render
        output$plotOne  <- renderPlot({
          isolate(currentP)
        })
        
        
      }else if (input$enablePlotly=="plotly"){
        
        output$plot<-renderUI({
          plotlyOutput("plotlyPlot", width=900,height=900) %>% withSpinner(color="#0dc5c1")
        })
        
        output$plotlyPlot <- renderPlotly({
          isolate(currentP)
        })
        
        
      }else if (input$enablePlotly=="leaflet"){
        output$plot<-renderUI({
          leafletOutput("leafPlot", width=900,height=900) %>% withSpinner(color="#0dc5c1")
        })
        
        output$leafPlot<-renderLeaflet({
          isolate(currentP)
        })
      }
      
      
    })
    
    
    observe({
      p1()
    })
    
    #pdf output
    #observeEvent(input$savePDF, {
    p2<-eventReactive(input$savePDF, {
      
      # shiny reactives from model run if present
      if(!is.null(unlist(reactiveValuesToList(shinyrct)))){
        for(i in 1:length(shinyrct)) 
          assign(names(shinyrct)[i], shinyrct[[i]])      
      }
      
      #test bad Settings
      badSettings<-as.data.frame(matrix(0,ncol=4,nrow=0))
      names(badSettings)<-c("Setting","CurrentValue","Type","Test")
      errMsg<-NA
      if (input$mapType %in% c("Stream","Catchment")){
        badSettings<-testCosmetic(input, output, session, DF = as.data.frame(scenarioRtables$cosmeticPred),mapType =input$mapType,
                                  scenario.input.list, mapping.input.list)$badSettings
      }else if (input$mapType == "Site Attributes"){
        badSettings<-testCosmetic(input, output, session, DF = as.data.frame(scenarioRtables$cosmeticSite),mapType =input$mapType,
                                  scenario.input.list, mapping.input.list)$badSettings
      }else{
        errMsg1<-testRedTbl(input, output, session, DF = as.data.frame(scenarioRtables$sourceRed))$errMsg
        errMsg2<-testRedTbl(input, output, session, DF = as.data.frame(scenarioRtables$allSourcesDF))$errMsg
        errMsg3<-testRedTbl(input, output, session, DF = as.data.frame(scenarioRtables$allSourcesDFno))$errMsg
        
        
        errMsg<-na.omit(c(errMsg1,errMsg2, errMsg3))
        if (length(errMsg)==0){
          errMsg<-NA
        }else{
          errMsg<-errMsg[1]
        }
        
        badSettings<-testCosmetic(input, output, session, DF = as.data.frame(scenarioRtables$cosmeticScen), "Source Change Scenarios",
                                  scenario.input.list, mapping.input.list)$badSettings
        
      }
      
      goShinyPlot(input, output, session, choices,"savePDF",badSettings, errMsg,
                  file.output.list, map_uncertainties,BootUncertainties,
                  data_names,mapping.input.list,
                  #predict.list,
                  subdata,SelParmValues,
                  #site attr
                  sitedata,estimate.list,estimate.input.list,#Mdiagnostics.list,
                  #scenarios
                  JacobResults,
                  ConcFactor,DataMatrix.list,dlvdsgn,
                  reach_decay_specification,reservoir_decay_specification,
                  scenario.input.list,if_predict,
                  #scenarios out
                  add_vars,
                  #batchError
                  batch_mode,
                  RSPARROW_errorOption)
    })#end save plot p2
    
    observe({
      
      p2()
      
      #try(dev.off(), silent = TRUE)
    })
    
    
    #batchplot
    p3<-eventReactive(input$batchPlot, {
      
      # shiny reactives from model run if present
      if(!is.null(unlist(reactiveValuesToList(shinyrct)))){
        for(i in 1:length(shinyrct)) 
          assign(names(shinyrct)[i], shinyrct[[i]])      
      }
      
      # observeEvent(input$batchPlot, {
      #test bad Settings
      badSettings<-as.data.frame(matrix(0,ncol=4,nrow=0))
      names(badSettings)<-c("Setting","CurrentValue","Type","Test")
      errMsg<-NA
      if (input$mapType %in% c("Stream","Catchment")){
        badSettings<-testCosmetic(input, output, session, DF = as.data.frame(scenarioRtables$cosmeticPred),mapType =input$mapType,
                                  scenario.input.list, mapping.input.list)$badSettings
      }else if (input$mapType == "Site Attributes"){
        badSettings<-testCosmetic(input, output, session, DF = as.data.frame(scenarioRtables$cosmeticSite),mapType =input$mapType,
                                  scenario.input.list, mapping.input.list)$badSettings
      }else{
        errMsg1<-testRedTbl(input, output, session, DF = as.data.frame(scenarioRtables$sourceRed))$errMsg
        errMsg2<-testRedTbl(input, output, session, DF = as.data.frame(scenarioRtables$allSourcesDF))$errMsg
        errMsg3<-testRedTbl(input, output, session, DF = as.data.frame(scenarioRtables$allSourcesDFno))$errMsg
        
        
        errMsg<-na.omit(c(errMsg1,errMsg2, errMsg3))
        if (length(errMsg)==0){
          errMsg<-NA
        }else{
          errMsg<-errMsg[1]
        }
        
        badSettings<-testCosmetic(input, output, session, DF = as.data.frame(scenarioRtables$cosmeticScen), "Source Change Scenarios",
                                  scenario.input.list, mapping.input.list)$badSettings
        
      }
      
      goShinyPlot(input, output, session, choices,"batchPlot",badSettings,errMsg,
                  file.output.list, map_uncertainties,BootUncertainties,
                  data_names,mapping.input.list,
                  #predict.list,
                  subdata,SelParmValues,
                  #site attr
                  sitedata,estimate.list,estimate.input.list,#Mdiagnostics.list,
                  #scenarios
                  JacobResults,
                  ConcFactor,DataMatrix.list,dlvdsgn,
                  reach_decay_specification,reservoir_decay_specification,
                  scenario.input.list,if_predict,
                  #scenarios out
                  add_vars,
                  #batchError
                  batch_mode,
                  RSPARROW_errorOption)
    })#end batch plot p3
    
    observe({
      p3()
    })
    
    session$onSessionEnded(function() {
      stopApp()
    }) 
  })#end server function
)#end shinyApp function