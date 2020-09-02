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
path_results <- 'results/ModRun/'
path_gis <- 'gis'

path_master <- "../tbepRSparrow/"
devtools::load_all(path_master,recompile = F)

# load existing image
load(file = 'shinyimage.RData')

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
    
    tabsetPanel(
      tabPanel('Sparrow control',
               br(),
               column(12,
                      column(3, selectInput('upload', label = NULL, choices = list('Manual entry' = FALSE, 'Control file upload' = TRUE), selected = TRUE)),
                      column(3, actionButton("run", "Run RSparrow!"))
               ),
               
               # for control file upload or input       
               uiOutput('entry'),
               # textOutput('modeltext')
               
      ),
      tabPanel('Plots', 
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
      )
    ))),
  
  server=shinyServer(function(input, output,session) {
    
    # control file upload or form fill
    output$entry <- renderUI({
      
      # input
      upload <- as.logical(input$upload)
      
      if(upload)
        out <- wellPanel(style = "overflow-x:scroll; background-color:#e3fbff",
                         fileInput('controlupload', NULL, accept = '.R')
        )
      
      if(!upload)
        out <- wellPanel(id = "inputs", style = "overflow-y:scroll; max-height: 700px; background-color:#e3fbff",
                         
                         h4('1. DATA IMPORT SETTINGS'),
                         
                         p('Set csv read/write options'),
                         textInput('csv_decimalSeparator', 'csv_decimalSeparator', value = ','),
                         textInput('csv_columnSeparator', 'csv_columnSeparator', value = ','),
                         p('Create an initial Data Dictionary file from the data1 column names. This file will have to be edited prior to executing RSPARROW'),
                         selectInput('create_initial_dataDictionary', 'create_initial_dataDictionary', choices = c('yes', 'no'), selected = 'no'),
                         p('Create an initial parameter and design_matrix files from names in the Data Dictionary file. The parameter names must be listed for both the sparrowNames and data1UserNames and the. varType should be defined as SOURCE, DELIVF, STRM, or RESV to populate parmType in the (run_id)_parameters.CSV. The initial file will have to be edited prior to executing RSPARROW'),
                         selectInput('create_initial_parameterControlFiles', 'create_initial_parameterControlFiles',  choices = c('yes', 'no'), selected = 'no'),
                         p('Select input data file (accepted file types ".csv" or binary file with no extension created in previous RSPARROW run. Binary file will be automatically created if file type is not binaryfor fast import in subsequent runs.'),
                         textInput('input_data_fileName', 'input_data_fileName', value = 'data1.csv'),
                         p('Loads previous binary data input file "(input_data_fileName)_priorImport" from results directory. This setting will override run_dataImport. NOTE: The OPTIONS FOR NETWORK ATTRIBUTES AND AREA VERIFICATION (section 2) will ONLY be executed if load_previousDataImport<-"no"'),
                         selectInput('load_previousDataImport', 'load_previousDataImport',  choices = c('yes', 'no'), selected = 'no'),
                         p('Indicate whether or not to run _userModifyData.R script'), 
                         selectInput('if_userModifyData', 'if_userModifyData', choices = c('yes', 'no'), selected = 'no'),
                         
                         h4('2. STREAM NETWORK ATTRIBUTES AND VERIFICATION'),
                         p('NOTE: This section is only executed if data import of the csv file is run. To run data import, set load_previousDataImport<-"no"'),
                         p('Verify drainage area accumulation in the network. NOTE: This requires that precalculated values of "demtarea" are present in DATA1 Area totals will be affected by "frac"'),
                         selectInput('if_verify_demtarea', 'if_verify_demtarea', choices = c('yes', 'no'), selected = 'no'),
                         p('Indicate whether maps are to be generated for if_verify_demtarea<-"yes", NOTE: Generating maps can significantly slow processing time for larger models'),
                         selectInput('if_verify_demtarea_maps', 'if_verify_demtarea_maps', choices = c('yes', 'no'), selected = 'no'),
                         p('Request the calculation of selected reach attributes: Identify the attributes for calculation and placement in DATA1'),
                         selectInput('calculate_reach_attribute_list', 'calculate_reach_attribute_list', selected = NULL, multiple = T, selectize = T, 
                                     choices = c('hydseq', 'headflag', 'demtarea')),
                         p('Specify any additional DATA1 variables (and conditions) to be used to filter reaches. Default conditions are FNODE > 0 and TNODE > 0. The filter is used to create "subdata" object from "data1". Example: c("data1$drainden > 0 & !is.na(data1$drainden)"'),
                         textInput('filter_data1_conditions', 'filter_data1_conditions', value = NA),
                         p('Indicate whether hydseq in the DATA1 file needs to be reversed'), 
                         selectInput('if_reverse_hydseq', 'if_reverse_hydseq', choices = c('yes', 'no'), selected = 'no'),
                         
                         h4('3. MONITORING SITE FILTERING OPTIONS'),
                         p("The index 'calsites' requires setting in the 'userModifyData.R' script to exclude unwanted sites, (calsites:  1=site selected for calibration; 0 or NA = site not used). Default setting = 1 for sites with positive loads 'depvar>0'. Minimum drainage area size for monitoring sites located on headwater reaches"),
                         numericInput('minimum_headwater_site_area', 'minimum_headwater_site_area', value = 0),
                         p('Minimum number of reaches between monitoring sites'),
                         numericInput('minimum_reaches_separating_sites', 'minimum_reaches_separating_sites', value = 1),
                         p('Minimum allowable incremental drainage area between monitoring sites'),
                         numericInput('minimum_site_incremental_area', 'minimum_site_incremental_area', value = 0),
                         
                         h4('4. MODEL ESTIMATION'),
                         p('Specify the land-to-water delivery function. The exponential computation yields an NxS matrix, where N is the number of reaches and S is the number of sources (see Chapter 4.4.4.1 for details).'), 
                         textInput('incr_delivery_specification', 'incr_delivery_specification', value = "exp(ddliv1 %*% t(dlvdsgn))"),
                         p('Specify if the delivery variables are to be mean adjusted (recommended). This improves the interpretability of the source coefficients'),
                         textInput('if_mean_adjust_delivery_vars', 'if_mean_adjust_delivery_vars', value = "yes"),
                         p('Specify the R reach decay function code.'),
                         textInput('reach_decay_specification', 'reach_decay_specification', value ="exp(-data[,jdecvar[i]] * beta1[,jbdecvar[i]])"), 
                         p('Specify the R reservoir decay function code.'),
                         p('An alternative simplified Arrhenius temperature dependent function can also be used as described in equation 1.35 in the SPARROW documentation (Schwarz et al. 2006). This requires the specification of an OTHER class variable for temperature.'),
                         p('OTHER variable (e.g., temp):  reservoir_decay_specification <- "(1 / (1 + data[,jresvar[i]] * beta1[,jbresvar[i]])) * (beta1[,jbothervar[1]]+1)**data[,jothervar[1]]", where the OTHER index references the parameter vector sequence, corresponding to the order of the OTHER variables listed in the parameters.csv file'),
                         p('NOTE: if using parameter scaling with this equation, the beta1[,jbothervar[1]] coefficient must be scaled to be consistent with the +1 adjustment'),
                         p('OTHER variable (e.g., temp)'),
                         selectInput('reservoir_decay_specification', 'reservoir_decay_specification', selected = '(1 / (1 + data[,jresvar[i]] * beta1[,jbresvar[i]]))', choices = c('(1 / (1 + data[,jresvar[i]] * beta1[,jbresvar[i]]))', 'exp(-data[,jresvar[i]] * beta1[,jbresvar[i]])', '(1 / (1 + data[,jresvar[i]] * beta1[,jbresvar[i]])) * (beta1[,jbothervar[1]]+1)**data[,jothervar[1]]')),
                         p('Specify if model estimation is to be performed ("no" obtains coefficient estimates from a previous estimation; if no previous estimates available, then the initial coefficient values in the beta file are used, "yes" indicates that all estimation, prediction, maps, and scenario files from the subdirectory with the name = run_id will be deleted and only regenerated if settings are turned on.'),
                         selectInput('if_estimate', 'if_estimate', choices = c('yes', 'no'), selected = "yes"),
                         p('Specify if model simulation is to be performed using the initial parameter values. "yes" indicates that all estimation, prediction, maps, and scenario files from the subdirectory with the name = run_id will be deleted and only regenerated if settings are turned on. A "yes" setting will over-ride a selection of if_estimate<-"yes".'),
                         selectInput('if_estimate_simulation', 'if_estimate_simulation', choices = c('yes', 'no'), selected = 'no'),
                         p('Specify if the more accurate Hessian coefficient SEs are to be estimated.  Note: set to "no" for Jacobian estimates and to reduce run times'),
                         selectInput('ifHess', 'ifHess', choices = c('yes', 'no'), selected = 'yes'),
                         p('NLMRT optimization shift setting that tests floating-point equality. Initially select values between 1.0e+12 and 1.0e+14. Lower magnitude offset values will execute more NLLS iterations'),
                         numericInput('s_offset', 's_offset', value = 1.0e+14), 
                         p('Select regression weights: "default": weights = 1.0  (unweighted NLLS), "lnload":  weights expressed as reciprocal of variance proportional to log of predicted load, "user": weights assigned by user in userModifyData.R script, expressed as the reciprocal of the variance proportional to user-selected variables'),
                         selectInput('NLLS_weights', 'NLLS_weights', choices = c('default', 'lnload', 'user'), selected = "default"),
                         
                         h4('5. MODEL SPATIAL DIAGNOSTICS '),
                         p("Specify if the spatial autocorrelation diagnostic graphics for Moran's I test are to be output"),
                         selectInput('if_spatialAutoCorr', 'if_spatialAutoCorr', choices = c('yes', 'no'), selected = "no"),
                         p("Specify the R statement for the Moran's I distance weighting function:"),
                         selectInput('MoranDistanceWeightFunc', 'MoranDistanceWeightFunc', choices = c('1/sqrt(distance)', '1/(distance)^2', '1/distance'), selected = '1/distance'),
                         p('Specify spatially contiguous discrete classification variables (e.g., HUC-x) for producing site diagnostics. Diagnostics include: (1) observed-predicted plots, (2) spatial autocorrelation (only the first variable is used), (3) sensitivities (only the first variable is used)'),
                         selectInput('classvar', 'classvar', choices = c('huc2', 'huc4'), selectize = T, selected = NULL, multiple = T),
                         p('Specify non-contiguous land use classification variables for boxplots of observed-predicted ratios by decile class. Note that the land use variables listed for "class_landuse" should be defined in areal units (e.g., sq. km) in the data1.csv file or by defining the variables in the userModifyData.R subroutine. In the RSPARROW diagnostic output plots, land use is expressed as a percentage of the incremental area between monitoring sites'),
                         selectInput('class_landuse', 'class_landuse', choices = c("forest","agric","crops","pasture","urban","shrubgrass"), selected = c("forest","agric","crops","pasture","urban","shrubgrass"), multiple = T, selectize = T),
                         p('Produces a summary table of reach predictions of the total yield for watersheds with relatively uniform land use. Specify the minimum land-use percentages of the total drainage area above a reach to select reaches with uniform land use for the land uses listed in the "class_landuse" setting.'),
                         selectInput('class_landuse_percent', 'class_landuse_percent', choices = c(90,50,75,75,80,10), selected = c(90,50,75,75,80,10), multiple = T, selectize = T),
                         p('Specify whether bivariate correlations among explanatory variables are to be executed by specifying "parmCorrGroup" in the parameters.csv file as "1" (yes) or "0" (no)'),
                         selectInput('if_corrExplanVars', 'if_corrExplanVars', choices = c('yes', 'no'), selected = "yes"),
                         
                         h4('6. SELECTION OF VALIDATION SITES'),
                         p('Split the monitoring sites into calibration and validation set'), 
                         selectInput('if_validate', 'if_validate', choices = c('yes', 'no'), selected = "no"),
                         p('Indicate the decimal fraction of the monitoring sites as validation sites. Two methods are available for selecting validation sites (see documentation)'),
                         numericInput('pvalidate', 'pvalidate', value = 0.25, min = 0, max = 1),
                         
                         h4('7. MODEL PREDICTIONS'),
                         p('Specify if standard predictions are to be produced. Note: Bias retransformation correction is applied to all predictions, except "deliv_frac", based on the Smearing estimator method'),
                         selectInput('if_predict', 'if_predict', choices = c('yes', 'no'), selected = "yes"),
                         p('Load predictions:'),
                         HTML(
                          '<ul>
                            <li>pload_total: Total load (fully decayed)</li>
                            <li>pload_(sources): Source load (fully decayed)</li>
                            <li>mpload_total: Monitoring-adjusted total load (fully decayed)</li>
                            <li>mpload_(sources): Monitoring-adjusted source load (fully decayed)</li>
                            <li>pload_nd_total: Total load delivered to streams (no stream decay)</li>
                            <li>pload_nd_(sources): Source load delivered to streams (no stream decay)</li>
                            <li>pload_inc: Total incremental load delivered to streams</li>
                            <li>pload_inc_(sources): Source incremental load delivered to streams</li>
                            <li>deliv_frac: Fraction of total load delivered to terminal reach</li>
                            <li>pload_inc_deliv: Total incremental load delivered to terminal reach</li>
                            <li>pload_inc_(sources)_deliv: Source incremental load delivered to terminal reach</li>
                            <li>share_total_(sources): Source shares for total load (percent)</li>
                            <li>share_inc_(sources): Source share for incremental load (percent)</li>
                          </ul>'),
                         p('Yield predictions:'),
                         HTML('
                          <ul>
                            <li>Concentration: Concentration based on decayed total load and discharge</li>
                            <li>yield_total: Total yield (fully decayed)</li>
                            <li>yield_(sources): Source yield (fully decayed)</li>
                            <li>myield_total: Monitoring-adjusted total yield (fully decayed)</li>
                            <li>myield_(sources): Monitoring-adjusted source yield (fully decayed)</li>
                            <li>yield_inc: Total incremental yield delivered to streams</li>
                            <li>yield_inc_(sources): Source incremental yield delivered to streams</li>
                            <li>yield_inc_deliv: Total incremental yield delivered to terminal reach</li>
                            <li>yield_inc_(sources)_deliv: Source incremental yield delivered to terminal reach</li>
                          </ul>'),
                         p('Uncertainty predictions (requires prior execution of bootstrap predictions; section 10):'),
                         HTML('
                          <ul>
                            <li>se_pload_total: Standard error of the total load (percent of mean)</li>
                            <li>ci_pload_total: 95% prediction interval of the total load (percent of mean)</li>
                          </ul>'),
                         p('Specify the load units for predictions and for diagnostic plots'),
                         textInput('loadUnits', 'loadUnits', value = "kg/year"),
                         p('Specify the concentration conversion factor, computed as Concentration = load / discharge * ConcFactor (for predictions and diagnostic plots). Use 3.170979e-05 kg/yr and m3/s to mg/L, use 0.001143648 for kg/yr and ft3/s to mg/L.'),
                         selectInput('ConcFactor', 'ConcFactor', choices = c('3.170979e-05', '0.001143648'), selected = '0.001143648'),
                         textInput('ConcUnits', 'ConcUnits', value = "mg/L"),
                         p('Specify the yield conversion factor, computed as Yield = load / demtarea * yieldFactor (for predictions and diagnostic plots). For example, use 1/100 to convert kg/km2/yr to kg/ha/year.'),
                         numericInput('yieldFactor', 'yieldFactor', value = 1/100),
                         textInput('yieldUnits', 'yieldUnits', value = "kg/ha/year"),
                         p('Specify additional variables to include in prediction, yield, and residuals csv files'),
                         selectInput('add_vars', 'add_vars', choices = c('huc2', 'huc4'), selectize = T, selected = NULL, multiple = T),
                         
                         h4('8. DIAGNOSTIC PLOTS AND MAPS'), 
                         p('Shape file input/output and geographic coordinates'),
                         p('Identify the stream reach shape file and "waterid" common variable in the shape file'),
                         textInput('lineShapeName', 'lineShapeName', value = "ccLinesMRB3"),
                         textInput('lineWaterid', 'lineWaterid', "MRB_ID"),
                         p('Identify the stream catchment polygon shape file and "waterid" common variable in the shape file'),
                         textInput('polyShapeName', 'polyShapeName', value ="mrb3_cats_usonly_withdata_tn"),
                         textInput('polyWaterid', 'polyWaterid', value = "MRB_ID"),
                         p('Identify optional geospatial shape file for overlay of lines on stream/catchment maps'),
                         textInput('LineShapeGeo', 'LineShapeGeo', value = "states"),
                         p('Identify the desired Coordinate Reference System (CRS) mapping transform for geographic coordinates (latitude-longitude)'),
                         textInput('CRStext', 'CRStext', value = "+proj=longlat +datum=NAD83"),
                         p('Indicate whether shape files are to be converted to binary to reduce execution times'),
                         selectInput('if_create_binary_maps', 'if_create_binary_maps', choices = c('yes', 'no'), selected = "no"),
                         p('Convert shape files to binary'),
                         selectInput('convertShapeToBinary.list', 'convertShapeToBinary.list', selected = c("lineShapeName","polyShapeName","LineShapeGeo"), choices = c("lineShapeName","polyShapeName","LineShapeGeo"), multiple = T, selectize = T),
                         p('Select ERSI shape file output for streams, catchments, residuals, site attributes'),
                         selectizeInput('outputESRImaps', 'outputESRImaps', selected = 'no, no, no, no', choices = c('yes, yes, yes, yes', 'no, no, no, no'), multiple = F, options = list(create = TRUE)),
                         p('Specify the geographic units minimum/maximum limits for mapping and prediction maps. If set to NA (missing), limits will be automatically determined from the monitoring site values.'),
                         textInput('lat_limit', 'lat_limit', value = '35,50'),
                         textInput('lon_limit', 'lon_limit', value = '-105,-70'),
                         p('Maps of model predictions and dataDictionary variables: Identify list of load and yield predictions for mapping to output PDF file (enter NA for none). Any variables listed in the data-dictionary are available for mapping by streams or catchments. Note: To map model predictions, then "if_predict" must = "yes" or predictions must have been previously executed.'),
                         selectInput('master_map_list', 'master_map_list', selected = 'pload_total', selectize = T, multiple = T, choices =  c("pload_total","se_pload_total","ci_pload_total", "deliv_frac","demtarea","hydseq", "yield_total","yield_inc","share_total_ndep","share_inc_ndep")),
                         p('Identify type of map(s) to output to PDF file from "stream","catchment", or "both"'), 
                         selectInput('output_map_type', 'output_map_type', selected = "stream", choices = c('stream', 'catchment', 'both')),
                         p('Map display settings for model predictions or dataDictionary variables'),
                         numericInput('predictionTitleSize', 'predictionTitleSize', value = 16),
                         numericInput('predictionLegendSize', 'predictionLegendSize', value = 0.5), 
                         textInput('predictionLegendBackground', 'predictionLegendBackground', value = "white"), 
                         textInput('predictionMapColors', 'predictionMapColors', value = "blue, dark green, gold, red, dark red"), 
                         numericInput('predictionClassRounding', 'predictionClassRounding', value = 3), 
                         textInput('predictionMapBackground', 'predictionMapBackground', value = "white"), 
                         numericInput('lineWidth' , 'lineWidth', value = 0.5),
                         p('Model diagnostics:  Station attribute maps, model plots, residual maps'), 
                         p('SiteAttribute maps - Identify site attributes to map. Note that any variables in the dataDictionary.csv can be mapped.'),
                         p('Example site attribute R statements in the "userModifyData.R" subroutine:'), 
                         HTML('
                           <ul>
                              <li>meanload <- depvar</li>
                              <li>meanyield <- depvar / demtarea</li>
                              <li>meanconc <- depvar/meanq*ConcFactor</li>
                              <li>meanloadSE <- depvar_se/depvar*100</li>
                            </ul>'),
                         selectInput('map_siteAttributes.list', 'map_siteAttributes.list', choices = c("meanload","meanyield","meanconc","meanloadSE"), 
                                     selected = c("meanload","meanyield","meanconc","meanloadSE"), selectize = T, multiple = T), 
                         numericInput('siteAttrTitleSize', 'siteAttrTitleSize', value = 16), 
                         numericInput('siteAttrLegendSize', 'siteAttrLegendSize', value = 0.5), 
                         selectInput('siteAttrColors', 'siteAttrColors', choices = c("blue","green4","yellow","orange","red"), 
                                     selected = c("blue","green4","yellow","orange","red"), multiple = T, selectize = T), 
                         numericInput('siteAttrClassRounding', 'siteAttrClassRounding', value = 2), 
                         numericInput('siteAttr_mapPointStyle', 'siteAttr_mapPointStyle', value = 16),
                         numericInput('siteAttr_mapPointSize', 'siteAttr_mapPointSize', value = 2),
                         textInput('siteAttrMapBackground', 'siteAttrMapBackground', value = "white"), 
                         p('Diagnostic plots and residual maps from the model estimation'), 
                         p('Diagnostic plot settings'), 
                         numericInput('diagnosticPlotPointSize', 'diagnosticPlotPointSize', value = 0.4),
                         numericInput('diagnosticPlotPointStyle', 'diagnosticPlotPointStyle', value = 1), 
                         p('Residual maps'), 
                         p('specify breakpoints for mapping of residuals, if residual_map_breakpoints set to NA, then breakpoint defaults will be applied. Breakpoint defaults are c(-2.5,-0.75,-0.25,0,0.25,0.75,2.5) and must have a length of 7 and be centered around 0.'), 
                         textInput('residual_map_breakpoints', 'residual_map_breakpoints', value = '-2.5,-1.0,-0.5,0,0.5,1.0,2.5'),
                         textInput('ratio_map_breakpoints', 'ratio_map_breakpoints', value = '0.3,0.5,0.8,1,1.25,2,3.3'), 
                         numericInput('residualTitleSize', 'residualTitleSize', value = 1), 
                         numericInput('residualLegendSize', 'residualLegendSize', value = 1), 
                         p('ResidualColors must be length=8 corresponding to residual_map_breakpoints'), 
                         textInput('residualColors', 'residualColors', value = 'red, red, gold, gold, dark green, dark green, blue, blue'), 
                         p('residualPointStyle must be length=8 corresponding to residual_map_breakpoints. 2 = open upward triangle, 6 = open downward triangle, 1 = open circle'), 
                         textInput('residualPointStyle', 'residualPointStyle', value = '2, 2, 1, 1, 1, 1, 6, 6'),    
                         p('residualPointSize_breakpoints must be length=8 corresponding to breakpoints'),
                         textInput('residualPointSize_breakpoints', 'residualPointSize_breakpoints', value = '0.75, 0.5, 0.4, 0.25, 0.25, 0.4, 0.5, 0.75'), 
                         p('residualPointSize_factor causes symbol size to increase or decrease, this scales all point sizes in residualPointSize_breakpoints.'), 
                         numericInput('residualPointSize_factor', 'residualPointSize_factor', value = 1), 
                         textInput('residualMapBackground', 'residualMapBackground', value = "white"), 
                         p('Enable plotly interactive displays for maps (interactive plots are automatic)'), 
                         selectInput('enable_plotlyMaps', 'enable_plotlyMaps', choices = c('yes', 'no'), selected = "yes"),
                         selectInput('add_plotlyVars', 'add_plotlyVars', choices = c("waterid","rchname","staid"), 
                                     selected = c("waterid","rchname","staid"), multiple = T, selectize = T), 
                         selectInput('showPlotGrid', 'showPlotGrid', choices = c('yes', 'no'), selected = "no"), 

                         h4('9. RSHINY INTERACTIVE DECISION SUPPORT SYSTEM (DSS) MAPPER'),
                         
                         # #Enable the interactive RShiny mapper
                         # enable_ShinyApp<-"no"
                         # 
                         # #Specify preferred Web browser for the RShiny display
                         # path_shinyBrowser<-"C:/Program Files/Mozilla Firefox/firefox.exe"
                         # path_shinyBrowser<-"C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"
                         # path_shinyBrowser<-"C:/Windows/SystemApps/Microsoft.MicrosoftEdge_8wekyb3d8bbwe/MicrosoftEdge.exe"
                         # path_shinyBrowser <- NA      # user's default browser
                         # path_shinyBrowser<-"C:/Program Files (x86)/Google/Chrome/Application/chrome.exe"
                         # 
                         # # R Shiny can be enabled from RStudio in a specified browser (e.g., Chrome) for a previously 
                         # #   executed model by running the following function in the Console window:
                         # # runBatchShiny("C:/UserTutorial/results/Model6",
                         # #              path_shinyBrowser ="C:/Program Files (x86)/Google/Chrome/Application/chrome.exe" )
                         # 
                         # ############################################################
                         # 
                         # # Simulation of source-change management scenarios in the RShiny mapper
                         # 
                         # #  NOTE: Requires prior execution of the model estimation ('if_estimate')
                         # #        and standard predictions ('if_predict'). 
                         # 
                         # #  Scenarios can be executed using the R Shiny interactive mapper using the
                         # #    control setting 'enable_ShinyApp <-"yes"'
                         # 
                         # ###############################################
                         # 
                         # # Identify the locations for applying scenarios
                         # #    "none", "all reaches", "selected reaches"
                         # select_scenarioReachAreas <- "all reaches"
                         # select_scenarioReachAreas <- "selected reaches"
                         # select_scenarioReachAreas <- "none"     # do not execute scenarios
                         # 
                         # # Indicate the watershed locations where the scenarios will be applied
                         # #   to either "all reaches" or "selected reaches".
                         # select_targetReachWatersheds <- NA      # Execute the scenarios for all reaches in the
                         # # modeled spatial domain (i.e., above the user-defined
                         # # terminal reaches)
                         # select_targetReachWatersheds <- 15531   # Execute for a single watershed inclusive of 
                         # # this watershed outlet reach ('waterid' system
                         # # variable) and all upstream reaches
                         # select_targetReachWatersheds <- c(15531,14899,1332)   # Execute for multiple watersheds 
                         # # inclusive of these watershed outlet reaches
                         # # ('waterid' system variable) and all upstreams
                         # # reaches
                         # 
                         # ###############################################
                         # 
                         # # Set scenario source conditions for "all reaches" in user-defined watersheds
                         # 
                         # # Settings applicable to select_scenarioReachAreas<-"all reaches" option.
                         # # Source changes are applied to "all reaches" in the user-defined watersheds
                         # 
                         # # List the source variables evaluated in the change scenarios.
                         # scenario_sources <- NA
                         # scenario_sources <- c("point","ndep","crops")  
                         # 
                         # # For land-use 'scenario_sources' with areal units, specify a land-use source in the 
                         # # model to which a complimentary area conversion is applied that is equal to the 
                         # # change in area of the `scenario_source` variable. Note that the converted source 
                         # # variable must differ from those that appear in 'scenario_sources' setting. 
                         # landuseConversion<-c(NA, NA,"forest") # convert crop area to forested area
                         # landuseConversion<-NA    # option if no land-use variables appear in 
                         # # 'scenario_source' setting
                         # 
                         # # Source-adjustment factors (increase or decrease) applied to "all reaches" 
                         # #  in the user-specified watersheds. Enter a factor of 0.1 or 1.1 to obtain a 10% 
                         # #  reduction or increase in a source, respectively.
                         # scenario_factors <- NA
                         # scenario_factors <- c(0.20,1.1,0.25)   # order consistent with order of 
                         # #  the 'scenario_sources'
                         # 
                         # ###############################################
                         # 
                         # # Set scenario source conditions for "selected reaches" in user-defined watersheds
                         # 
                         # # Settings applicable to select_scenarioReachAreas<-"selected reaches" option.
                         # # Source changes applied to "selected reaches" in the user-defined watersheds.
                         # 
                         # # (A) Specify the source-adjustment factors in the 'userModifyData.R' 
                         # #      script for each of the "scenario_source" variables. 
                         # #      The variable names for the factors are defined by adding the 
                         # #      prefix "S_" to the *sparrowNames* variable name for each source.
                         # #      In the example, point sources are reduced by 20% and atmospheric
                         # #      deposition is increased by 10% in Ohio Basin (huc2=5) and cropland
                         # #      area is reduced by 25% in the Upper Mississippi Basin: 
                         # #         S_point <- ifelse(huc2 == 5,0.2,1)
                         # #         S_atmdep <- ifelse(huc2 == 5,1.1,1)
                         # #         S_crops <- ifelse(huc2 == 7,0.25,1)
                         # 
                         # # (B) Specify the land-use types used for land conversion in the 'userModifyData.R' 
                         # #      script, in cases where the scenario sources are land use/cover variables, 
                         # #      expressed in areal units. The variable names for the conversion types are 
                         # #      defined by adding the suffix "_LC" to the *sparrowNames* variable name for
                         # #      each land-use area source.   
                         # #      In the example, cropland area is converted to pasture area in reaches of
                         # #      the Upper Mississippi River Basin (huc2=7), and "NA" is assumed for the 
                         # #      land conversion for the mass-based point sources and atmospheric sources:
                         # #         S_crops_LC <- ifelse(huc2==7,"pasture",NA)
                         # 
                         # # (C) Add the variable names for the source-adjustment factors (e.g., "S_crops")
                         # #      and the conversion land-use type (e.g., "S_crops_LC") as 'sparrowNames' 
                         # #      in the dataDictionary.csv file, with an OPEN 'varType'.
                         # 
                         # #   dataDictionary.csv file:
                         # #        varType   sparrowNames      data1UserNames        varunits    explanation
                         # #         OPEN     S_crops                NA
                         # #         OPEN     S_crops_LC             NA
                         # 
                         # 
                         # ###############################################
                         # 
                         # # Specify the scenario output settings
                         # 
                         # # Set scenario name; this becomes the directory and file name for all scenario output
                         # #  NOTE: only one scenario can be run at a time; avoid using "/" or "\" for name
                         # scenario_name<-"scenario1"
                         # 
                         # # specify the colors for six classes for mapped predictions
                         # scenarioMapColors<-c("light blue","blue","dark green","gold","red","dark red")
                         # 
                         # 
                         # # Identify prediction variables to display a stream map of the effects of the 
                         # #  scenario on water-quality loads. Options include:
                         # #
                         # # RELATIVE METRICS:
                         # # Ratio of the changed load (resulting from the scenario) to the baseline load 
                         # # associated with the original (unchanged) mass or area of the model sources. 
                         # #  Metric names and explanations:
                         # #   ratio_total                Ratio for the total load (a measure of the watershed-
                         # #                               scale effect of the change scenario)
                         # #   ratio_inc                  Ratio for the total incremental load delivered to 
                         # #                               the reach (a measure of the "local" effect of the
                         # #                               change scenario)
                         # # ABSOLUTE METRICS:
                         # # Load prediction names and explanations
                         # #   pload_total                Total load (fully decayed)
                         # #   pload_(sources)            Total source load (fully decayed)
                         # #   pload_nd_total             Total load delivered to streams (no stream decay)
                         # #   pload_nd_(sources)         Total source load delivered to streams (no stream decay)
                         # #   pload_inc                  Total incremental load delivered to reach 
                         # #                                 (with 1/2 of reach decay)
                         # #   pload_inc_(sources)        Source incremental load delivered to reach 
                         # #                                 (with 1/2 of reach decay)
                         # #   pload_inc_deliv            Total incremental load delivered to terminal reach
                         # #   pload_inc_(sources)_deliv  Total incremental source load delivered to terminal reach
                         # #   share_total_(sources)      Source shares for total load (percent)
                         # #   share_inc_(sources)        Source shares for incremental load (percent)
                         # 
                         # # Yield prediction names and explanations
                         # #   Concentration              Flow-weighted concentration based on decayed total load 
                         # #                                 and mean discharge
                         # #   yield_total                Total yield (fully decayed)
                         # #   yield_(sources)            Total source yield (fully decayed)
                         # #   yield_inc                  Total incremental yield delivered to reach 
                         # #                                 (with 1/2 of reach decay)
                         # #   yield_inc_(sources)        Total incremental source yield delivered to reach 
                         # #                                 (with 1/2 of reach decay)
                         # #   yield_inc_deliv            Total incremental yield delivered to terminal reach
                         # #   yield_inc_(sources)_deliv  Total incremental source yield delivered to 
                         # #                                 terminal reach
                         # #
                         # scenario_map_list <- c("ratio_total","ratio_inc","pload_total","concentration")
                         
                         h4('10. MODEL PREDICTION UNCERTAINTIES'), 
                         
                         h4('11. DIRECTORY AND MODEL IDENTIFICATION AND CONTROL SCRIPT OPERATIONS'),
                         
                         h4('12. INSTALLATION AND UPDATING OF R LIBRARIES'),
                         
                         h4('Run Model')
                         
        )
      
      return(out)
      
    })
    
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
    
    # run model
    observeEvent(
      input$run, 
      withProgress(
        message='Please wait',
        detail='Running...',
        value = 0, {
          
          # input
          controlupload <- input$controlupload
          file.copy(from = controlupload$datapath, to = 'results/sparrow_control.R', overwrite = T)
          
          # run model
          source('results/sparrow_control.R')
          
          shinyimage <- list(
            file.output.list = file.output.list,
            map_uncertainties = NA,
            BootUncertainties = BootUncertainties,
            data_names = data_names,
            mapping.input.list = mapping.input.list,
            subdata = subdata,
            SelParmValues = SelParmValues,
            sitedata = sitedata,
            estimate.list = estimate.list,
            estimate.input.list = estimate.input.list,
            ConcFactor = ConcFactor,
            DataMatrix.list = DataMatrix.list,
            dlvdsgn = dlvdsgn,
            reach_decay_specification = reach_decay_specification,
            reservoir_decay_specification = reservoir_decay_specification,
            scenario.input.list = scenario.input.list,
            if_predict = if_predict,
            add_vars = add_vars,
            batch_mode = batch_mode,
            RSPARROW_errorOption = RSPARROW_errorOption
          )
          save(shinyimage, file = 'shinyimage.RData')
          
          # add relevant shiny data to shiny reactive
          shinyrct$file.output.list <- file.output.list
          shinyrct$map_uncertainties <- NA
          shinyrct$BootUncertainties <- BootUncertainties
          shinyrct$data_names <- data_names
          shinyrct$mapping.input.list <- mapping.input.list
          shinyrct$subdata <- subdata
          shinyrct$SelParmValues <- SelParmValues
          shinyrct$sitedata <- sitedata
          shinyrct$estimate.list <- estimate.list
          shinyrct$estimate.input.list <- estimate.input.list
          shinyrct$ConcFactor <- ConcFactor
          shinyrct$DataMatrix.list <- DataMatrix.list
          shinyrct$dlvdsgn <- dlvdsgn
          shinyrct$reach_decay_specification <- reach_decay_specification
          shinyrct$reservoir_decay_specification <- reservoir_decay_specification
          shinyrct$scenario.input.list <- scenario.input.list
          shinyrct$if_predict <- if_predict
          shinyrct$add_vars <- add_vars
          shinyrct$batch_mode <- batch_mode
          shinyrct$RSPARROW_errorOption <- RSPARROW_errorOption
          
        })
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