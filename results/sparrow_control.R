###############################
## RSPARROW CONTROL SCRIPT ####
###############################

# Authors:  R. Alexander, USGS, Reston, VA (ralex@usgs.gov)
#           L. Gorman-Sanisaca, USGS, Baltimore, MD (lgormansanisaca@usgs.gov)
# Modified: 06-04-2018
# SPARROW URL:  http://water.usgs.gov/nawqa/sparrow/

 ####################################################################
 ### SPECIFY USER SETTINGS AND EXECUTE THE ENTIRE CONTROL SCRIPT ####
 ####################################################################
 # NOTE: Users are required to select and execute all statements in 
 # the control script for proper operation of RSPARROW. Enter "CTRL A" 
 # on the keyboard to highlight/select all of the text, followed by 
 # "CTRL R" or "Run" on the tab above the control script.
 # Alternatively, select the "Source" button in the upper right of the 
 # control script window in RStudio (located next to the "Run" button). 

  ################################
  ### 1. DATA IMPORT SETTINGS ####
  ################################
   #Set csv read/write options
   csv_decimalSeparator <- "."
   csv_columnSeparator <- ","     
     
   #Create an initial Data Dictionary file from the data1 column names
   #This file will have to be edited prior to executing RSPARROW
   create_initial_dataDictionary<-"no"
     
   #Create an initial parameter and design_matrix files from names in the Data Dictionary file
   #The parameter names must be listed for both the sparrowNames and data1UserNames and the 
   #  varType should be defined as SOURCE, DELIVF, STRM, or RESV to populate parmType in the 
   #  (run_id)_parameters.CSV
   #The initial file will have to be edited prior to executing RSPARROW
   create_initial_parameterControlFiles<-"no"     
     
   #Select input data file (accepted file types ".csv" or binary file 
   #  with no extension created in previous RSPARROW run).
   #Binary file will be automatically created if file type is not binary 
   #  for fast import in subsequent runs.
   input_data_fileName <- "tampa_data1.csv"
     
   # Loads previous binary data input file "(input_data_fileName)_priorImport"      
   #  from results directory. This setting will override run_dataImport.
   # NOTE: The OPTIONS FOR NETWORK ATTRIBUTES AND AREA VERIFICATION (section 2)
   #     will ONLY be executed if load_previousDataImport<-"no" 
   load_previousDataImport<-"no" 
     
   #Indicate whether or not to run _userModifyData.R
   if_userModifyData<-"yes"

  ######################################################
  ### 2. STREAM NETWORK ATTRIBUTES AND VERIFICATION ####
  ######################################################
  #   NOTE: This section is only executed if data import of the csv file is run.
  #         To run data import, set load_previousDataImport<-"no"

  # Verify drainage area accumlation in the network
  #   NOTE: This requires that precalculated values of 'demtarea' are present in DATA1
  #         Area totals will be affected by 'frac'
  if_verify_demtarea <- "no"
   
  #Indicate whether maps are to be generated for if_verify_demtarea<-"yes"
  #   NOTE: Generating maps can significantly slow processing time for larger models
  if_verify_demtarea_maps<-"no"
  
  # Request the calculation of selected reach attributes:
  #  Identify the attributes for calculation and placement in DATA1 
  #  Select from following: 'hydseq', 'headflag', and/or 'demtarea'

  #calculate_reach_attribute_list <- c("hydseq","headflag","demtarea")  # calculate for these variables
  #calculate_reach_attribute_list <- c("hydseq")  # calculate for these variables
  #calculate_reach_attribute_list <- NA    # no calculation is requested
  calculate_reach_attribute_list<- c("headflag")
  
  # Specify any additional DATA1 variables (and conditions) to be used to filter reaches:
  #  Default conditions are FNODE > 0 and TNODE > 0
  #  The filter is used to create 'subdata' object from 'data1'
  #filter_data1_conditions <- c("data1$drainden > 0 & !is.na(data1$drainden)")
  filter_data1_conditions <- NA

  # Indicate whether hydseq in the DATA1 file needs to be reversed
  if_reverse_hydseq <- "no"
 

  #############################################
  ### 3. MONITORING SITE FILTERING OPTIONS ####                       
  #############################################
 
  # The index 'calsites' requires setting in the modifications function to exclude unwanted sites
  # (calsites:  1=site selected for calibration; 0 or NA = site not used)
  # Default setting = 1 for sites with positive loads 'depvar>0'
  
  # Minimum drainage area size for monitoring sites located on headwater reaches
  minimum_headwater_site_area <- 0
  
  # Minimum number of reaches between monitoring sites
  minimum_reaches_separating_sites <- 1
 
  # Minimum allowable incremental drainage area between monitoring sites
  minimum_site_incremental_area <- 0

  ############################
  ### 4. MODEL ESTIMATION ####
  ############################
 
  # Specify if the delivery variables are to be mean adjusted (recommended). This
  #  improves the interpretability of the source coefficients and gives meaning to 
  #  the land-to-water delivery factor. */
  if_mean_adjust_delivery_vars <- "yes"

  # Specify the R reach decay function code.  
  reach_decay_specification <- "exp(-data[,jdecvar[i]] * beta1[,jbdecvar[i]])"

  # Specify the R reservoir decay function code. 
  #reservoir_decay_specification <- "exp(-data[,jresvar[i]] * beta1[,jbresvar[i]])"
  reservoir_decay_specification <- "(1 / (1 + data[,jresvar[i]] * beta1[,jbresvar[i]]))"

  # An alternative simplified Arrhenius temperature dependent function can also be used
  #  as described in equation 1.35 in the SPARROW documentation (Schwarz et al. 2006).
  #  This requires the specification of an OTHER class variable for temperature.
  # OTHER variable (e.g., temp):  reservoir_decay_specification <- "(1 / (1 + data[,jresvar[i]] * beta1[,jbresvar[i]])) * (beta1[,jbothervar[1]]+1)**data[,jothervar[1]]"
  # where the OTHER index references the parameter vector sequence, corresponding to the order of the OTHER variables listed in the parameters.csv file
  # NOTE: if using parameter scaling with this equation, the beta1[,jbothervar[1]] coefficient must be scaled to be consistent with the +1 adjustment
  # OTHER variable (e.g., temp)
  # reservoir_decay_specification <- "(1 / (1 + data[,jresvar[i]] * beta1[,jbresvar[i]])) * (beta1[,jbothervar[1]]+1)**data[,jothervar[1]]"

  # Specify if estimation is to be performed 
  #  ("no" obtains coefficient estimates from a previous estimation; 
  #    if no previous estimates available, then the initial coefficient values in the beta file are used)
  # "yes" indicates that all estimation, prediction, maps, and scenario files 
  #       from the subdirectory with the name = run_id will be deleted 
  #       and only regenerated if settings are turned on
  if_estimate <- "yes"
  
 
  #Specify if simulation is to be performed using the initial parameter values
  # "yes" indicates that all estimation, prediction, maps, and scenario files 
  #       from the subdirectory with the name = run_id will be deleted 
  #       and only regenerated if settings are turned on
  # A "yes" setting will over-ride a selection of if_estimate<-"yes".
  if_estimate_simulation<-"no"
  
  # Specify if more accurate Hessian coefficient SEs are to be estimated
  #  Note: set to "no" for Jacobian estimates and to reduce run times
  ifHess <- "yes"

  # NLMRT optimization shift setting that tests floating-point equality
  # Initially select values between 1.0e+12 and 1.0e+14
  # Lower magnitude offset values will execute more NLLS iterations
  s_offset <- 1.0e+14
  
  # Select regression weights:  
  #  "default": weights = 1.0
  #  "lnload":  weights expressed as reciprocal of variance proportional to log of predicted load
  #  "area":    weights inversely proportional to incremental drainage size
  #  "user":    weights assigned by user in userModifyData.R subroutine, expressed as the 
  #               reciprocal of the variance proportional to user-selected variables
  NLLS_weights <- "default"
  
  # Specify if auto-scaling of parameters for optimization is to be used
  # Recommended default setting for models:  if_auto_scaling="no"
  # if "yes" then set 'parmScale' in parameters.CSV to 1.0 for fully automated scaling
  # (i.e., the scaling value is automatically derived so that the initial value 'parmInit' falls between 1 and 10)
  # or allow use of the user-specified 'parmScale' value to scale the initial value
  if_auto_scaling <- "no"

  #############################
  ### 5. MODEL DIAGNOSTICS ####
  #############################

  # Specifiy if the spatial autocorrelation diagnostic graphics for Moran's I test are to be output 
  if_diagnostics <- "yes"
  if_diagnostics <- "no"

  # Specify the R statement for the Moran's I distance weighting function:
  # MoranDistanceWeightFunc <- "1/(distance)^2"    # inverse squared distance
  MoranDistanceWeightFunc <- "1/distance"        # inverse distance


  # Specify spatially contiguous discrete classification variables (e.g., HUC-x) for producing site diagnostics
  # Diagnostics include: (1) observed-predicted plots
  #                      (2) spatial autocorrelation (only the first variable is used)
  #                      (3) sensitivities (only the first variable is used)
  classvar<-NA  # for NA, total drainage area is used as the default classification variable
  #classvar <- c("huc2","huc4")
  #classvar<-c("huc4")

  # Specify non-contiguous land use classification variables for boxplots of observed-predicted ratios by decile class
  #  Note that the land use variables listed for "class_landuse" should be defined in areal units (e.g., sq. km) 
  #  in the data1.csv file or by defining the variables in the userModifyData.R subroutine
  #  In the RSPARROW diagnostic output plots, land use is expressed as a percentage of the incremental area between monitoring sites
  class_landuse<-NA   # for NA, total drainage area is used as the default classification variable
  #class_landuse <- c("forest","agric","urban","shrubgrass")

  # Produces a summary table of reach predictions of the total yield for watersheds 
  #  with relatively uniform land use. 
  # Specify the minimum land-use percentages of the total drainage area above a 
  #  reach to select reaches with uniform land use for the land uses listed in the 
  #  "class_landuse" setting.
  class_landuse_percent<-c(90,50,80,10)

  # Specify whether bivariate correlations among explanatory variables are to be executed by
  #   specifying 'parmCorrGroup' in the parameters.csv file as "1" (yes) or "0" (no)
  select_corr <- "yes"

  #########################################
  ### 6. SELECTION OF VALIDATION SITES ####
  #########################################
 
  # Split the monitoring sites into calibration and validation set
  if_validate <- "no"

  # Indicate the decimal fraction of the monitoring sites as validation sites
  #  Two methods are avilable for selecting validation sites (see documentation)
  pvalidate <- 0.25

  #############################
  ### 7. MODEL PREDICTIONS ####
  #############################
 
  # Specify if standard predictions are to be produced.
  #  Note: Bias retransformation correction is applied to all predictions, 
  #    except "deliv_frac", based on the Smearing estimator method
  if_predict <- "yes"
  
  # Load predictions:
  #   pload_total                Total load (fully decayed)
  #   pload_(sources)            Source load (fully decayed)
  #   mpload_total               Monitoring-adjusted total load (fully decayed)
  #   mpload_(sources)           Monitoring-adjusted source load (fully decayed)
  #   pload_nd_total             Total load delivered to streams (no stream decay)
  #   pload_nd_(sources)         Source load delivered to streams (no stream decay)
  #   pload_inc                  Total incremental load delivered to streams
  #   pload_inc_(sources)        Source incremental load delivered to streams
  #   deliv_frac                 Fraction of total load delivered to terminal reach
  #   pload_inc_deliv            Total incremental load delivered to terminal reach
  #   pload_inc_(sources)_deliv  Source incremental load delivered to terminal reach
  #   share_total_(sources)      Source shares for total load (percent)
  #   share_inc_(sources)        Source share for incremental load (percent)
  
  # Yield predictions:
  #   Concentration              Concentration based on decayed total load and discharge
  #   yield_total                Total yield (fully decayed)
  #   yield_(sources)            Source yield (fully decayed)
  #   myield_total               Monitoring-adjusted total yield (fully decayed)
  #   myield_(sources)           Monitoring-adjusted source yield (fully decayed)
  #   yield_inc                  Total incremental yield delivered to streams
  #   yield_inc_(sources)        Source incremental yield delivered to streams
  #   yield_inc_deliv            Total incremental yield delivered to terminal reach
  #   yield_inc_(sources)_deliv  Source incremental yield delivered to terminal reach
  
  # Uncertainty predictions (requires prior execution of bootstrap predictions; section 10):
  #   se_pload_total             Standard error of the total load (percent of mean)
  #   ci_pload_total             95% prediction interval of the total load (percent of mean)


  # Specify the load units for predictions and for diagnostic plots
  loadUnits <- "kg/year"
  
  # Specify the concentration conversion factor, computed as Concentration = load / discharge * ConcFactor
  #  (for predictions and diagnostic plots)
  ConcFactor <- 3.170979e-05   # kg/yr and m3/s to mg/L
  ConcFactor <- 0.001143648    # kg/yr and ft3/s to mg/L

  ConcUnits <- "mg/L"
  
  # Specify the yield conversion factor, computed as Yield = load / demtarea * yieldFactor
  #  (for predictions and diagnostic plots)
  yieldFactor <- 1/100     # example, to convert kg/km2/yr to kg/ha/year use 1/100 factor

  yieldUnits <- "kg/ha/year"
  
  #Specify additional variables to include in prediction, yield, and residuals csv files
  add_vars<-NA
  add_vars<-c("huc2","huc4")
  #add_vars<-c("huc4","huc8","comid")

  #####################################
  ### 8. DIAGNOSTIC PLOTS AND MAPS ####
  #####################################
  
  # Shape file input/output and geographic coordinates
  
  # Identify the stream reach shape file and 'waterid' common variable in the shape file
  lineShapeName <- "ccLinesMRB3_TampaBay____" 
  lineWaterid <- "waterid"
 
   
  # Identify the stream catchment polygon shape file and 'waterid' common variable in the shape file
  polyShapeName <- "mrb3_cats_usonly_withdata_tn_TampaBay__"
  polyWaterid <- "waterid"
  
  
  # Identify optional geospatial shape file for overlay of lines on stream/catchment maps
  LineShapeGeo <- NA
  LineShapeGeo <- "states"
 
  # Identify the desired Coordinate Reference System (CRS) mapping transform for 
  #   geographic coordinates (latitude-longitude)
  CRStext <- "+proj=longlat +datum=NAD83"
  
  # Indicate whether shape files are to be converted to binary to reduce execution times
  if_create_binary_maps<-"yes"

  # Convert shape files to binary 
  convertShapeToBinary.list <- c("lineShapeName","polyShapeName","LineShapeGeo")
  #convertShapeToBinary.list <- c("polyShapeName","LineShapeGeo")


  # Select ERSI shape file output for streams, catchments, residuals, site attributes
  outputERSImaps <-  c("no","no","no","no")   
  #outputERSImaps <- c("yes","yes","yes","yes") 


  # Specify the geographic units minimum/maximum limits for mapping and prediction maps
  # If set to NA (missing), limits will be automatically determined from the monitoring site values
  lat_limit <- c(35,50)
  lon_limit <- c(-105,-70)
  lat_limit <- c(31,37)
  lon_limit <- c(-85,-74)
  lat_limit <- c(26,29)
  lon_limit <- c(-83,-81)
  

  ####################################################
  # Maps of model predictions and dataDictionary variables

  # Identify list of load and yield predictions for mapping to output PDF file (enter NA for none)
  # Any variables listed in the data-dictionary are available for mapping by streams or catchments
  # Note: To map model predictions, then 'if_predict' must = "yes" or predictions must have been 
  #       previouly executed

  master_map_list <- c("pload_total","se_pload_total","ci_pload_total",
      "deliv_frac","demtarea","hydseq",
      "yield_total","yield_inc","share_total_ndep","share_inc_ndep")
  master_map_list <- NA
  
  #Identify type of map(s) to output to PDF file from "stream","catchment", or "both"
  output_map_type<-c("both")
  

  #map display settings for model predictions or dataDictionary variables
  predictionTitleSize<-1
  predictionLegendSize<-0.75
  predictionLegendBackground<-"grey"
  predictionMapColors<-c("blue","dark green","gold","red","dark red") #length sets number of breakpoints
  predictionClassRounding<-1
  predictionMapBackground<-"white"
  lineWidth<-0.8 #for stream maps #0.8

  ####################################################
  # Model diagnostics:  Station attribute maps, model plots, residual maps

  ####################
  # SiteAttribute maps - Identify site attributes to map
  #  Note that any variables in the dataDictionary.csv can be mapped
  map_siteAttributes.list<-c("meanload","meanyield","meanconc","meanloadSE")  #Identify site attributes to map
  # Example site attribute R statements in the 'userModifyData.R' subroutine:
    # meanload <- depvar
    # meanyield <- depvar / demtarea
    # meanconc <- depvar/meanq*ConcFactor
    # meanloadSE <- depvar_se/depvar*100
  
  siteAttrTitleSize<-1
  siteAttrLegendSize<-1
  #siteAttrColors<-c("blue","green4","yellow","orange","red","darkred") #length sets number of breakpoints
  siteAttrColors<-c("blue","green4","yellow","orange","red")
  siteAttrClassRounding<-2
  siteAttr_mapPointStyle<-16 #pch=16
  siteAttr_mapPointSize<-1 # sets point size scaling 
  siteAttrMapBackground<-"grey"


  ####################
  # Diagnostic plots and residual maps from the model estimation

  # Diagnostic plot settings
  diagnosticPlotPointSize <- 0.4
  diagnosticPlotPointStyle <- 1
  
  #Residual maps
  #specify breakpoints for mapping of residuals
  #if residual_map_breakpoints set to NA, then breakpoint defaults will be applied
  #   breakpoint defaults are c(-2.5,-0.75,-0.25,0,0.25,0.75,2.5)
  #   breakpoints must have a length of 7 and be centered around 0
  residual_map_breakpoints<-c(-2.5,-1.0,-0.5,0,0.5,1.0,2.5)
  # For obs/pred ratio maps must have length=7
  ratio_map_breakpoints<-c(0.3,0.5,0.8,1,1.25,2,3.3) 
  residualTitleSize<-1
  residualLegendSize<-1
  #residualColors must be length=8 corresponding to residual_map_breakpoints
  residualColors<-c("red","red","gold","gold","dark green","dark green","blue","blue") 
  residualPointStyle<- c(2,2,1,1,1,1,6,6)    #must be length=8 corresponding to residual_map_breakpoints
                                             # 2 = open upward triangle
                                             # 6 = open downward triangle
                                             # 1 = open circle
  #residualPointSize_breakpoints must be length=8 corresponding to breakpoints
  residualPointSize_breakpoints<-c(0.75,0.5,0.4,0.25,0.25,0.4,0.5,0.75) 

  #residualPointSize_factor causes symbol size to increase or decrease 
  #  This scales all point sizes in residualPointSize_breakpoints
  residualPointSize_factor<-1 
  residualMapBackground<-"grey"
 

  ####################################################
  #Trigger interactive Rshiny Maps upon completing run
  enable_interactiveMaps<-"yes"



  #################################################
  ### 9. PREDICTION SOURCE-REDUCTION SCENARIOS ####
  #################################################
  
  # Indicate the spatial domain to apply scenario predictions:  "none", "all reaches", "selected reaches"
  #  NOTE: (1) output includes CSV files for the changed absolute values of loads, 
  #            concentration, and yields and CSV for the changes in all values
  #            (expressed as a ratio of new to baseline condition)
  #        (2) requires prior or active execution of standard predictions to support baseline comparison
  if_predict_scenarios <- "all reaches"
  #if_predict_scenarios <- "selected reaches"
  #if_predict_scenarios <- "none"     # do not execute scenarios
  
  #Set scenario name; this becomes the directory and file name for all scenario output
  #  NOTE: only one scenario can be run at a time; avoid using "/" or "\" for name
  scenario_name<-"scenario1"

  # Identify prediction variables to map change from baseline loads, expressed as ratio of new to baseline load
  scenario_map_list <- c("pload_total","pload_inc")
  

  scenarioMapColors<-c("light blue","blue","dark green","gold","red","dark red")

  # Identify source variables for evaluation of downstream effects of source reductions (increases)
  scenario_sources <- NA
  scenario_sources <- c("point","ndep","FARM_N","MANC_N","urban")
  scenario_sources <- c("FacWW_P","FacPmine","Pmines_v17","urban_Seas","FertP_Seas","ManureP_Seas","GeolP")

  # OPTION 1: Settings for application of source reductions to "all reaches":
    # Source adjustment reduction (or increase) factors to apply to "all reaches"
    #  (for example, enter 0.1 or 1.1 for a 10% reduction or increase in sources, respectively)
    scenario_all_factors <- NA
    #scenario_all_factors <- c(0.25,1.15,0.1,0.1,0.1)
    scenario_all_factors <- c(0.2,0.5,1.15,1.0,0.2,0.3,0.4)

  # OPTION 2: Settings for application of source reductions to user-specified "selected reaches":
    # (A) Specify the adjustment factors in the 'userModifyData.R' subroutine, such as  
    #      (e.g., S_point <- ifelse(huc2 == 5,0.5,1)  # point sources are reduced by 0.5 in Ohio Basin)
    # (B) Add the variable names for the factors to the 'sparrowNames' in the dataDictionary 
    #       (e.g, "S_point","S_ndep","S_FARM_N")

  ###########################################
  ### 10. MODEL PREDICTION UNCERTAINTIES ####
  ###########################################
  
  # Number of parametric bootstrap iterations
  biters <- 0

  # Confidence interval setting
  confInterval <- 0.90
    
  # Specify the initial seed for the boot_estimate, boot_predict, 
  #   and if_validate<-"yes" and pvalidate>0
  iseed <- 139933493

  # Specify if parametric bootstrap estimation (Monte Carlo) is to be executed
  if_boot_estimate <- "no"

  # Specify if bootstrap predictions (mean, SE, confidence intervals) are to be executed
  #  Note: Bias retransformation correction based on parametric bootstrap estimation
  #        Requires completion of bootstrap estimation 
  if_boot_predict <- "no"

  # Bootstrap load prediction names and explanations
  #   mean_pload_total                Bias-adjusted total load (fully decayed)
  #   mean_pload_(sources)            Bias-adjusted source load (fully decayed)
  #   mean_mpload_total               Bias-adjusted conditional (monitoring-adjusted)
  #                                      total load (fully decayed)
  #   mean_mpload_(sources)           Bias-adjusted conditional (monitoring-adjusted)
  #                                      source load (fully decayed)
  #   mean_pload_nd_total             Bias-adjusted total load delivered to streams 
  #                                      (no stream decay)
  #   mean_pload_nd_(sources)         Bias-adjusted source load delivered to streams 
  #                                      (no stream decay)
  #   mean_pload_inc                  Bias-adjusted total incremental load delivered 
  #                                      to streams
  #   mean_pload_inc_(sources)        Bias-adjusted source incremental load delivered 
  #                                      to streams
  #   mean_deliv_frac                 Fraction of total load delivered to terminal reach
  #   mean_pload_inc_deliv            Bias-adjusted total incremental load delivered to 
  #                                      terminal reach
  #   mean_pload_inc_(sources)_deliv  Bias-adjusted source incremental load delivered to 
  #                                      terminal reach
  #   mean_share_total_(sources)      Bias-adjusted percent source shares for total load
  #   mean_share_inc_(sources)        Bias-adjusted percent source shares for 
  #                                      incremental load

  # Bootstrap yield prediction names and explanations
  #   mean_conc_total                 Bias-adjusted concentration based on decayed total 
  #                                      load and mean annual discharge
  #   mean_yield_total                Bias-adjusted total yield (fully decayed)
  #   mean_yield_(sources)            Bias-adjusted source yield (fully decayed)
  #   mean_myield_total               Bias-adjusted conditional (monitoring-adjusted) total 
  #                                      yield (fully decayed)
  #   mean_myield_(sources)           Bias-adjusted conditional (monitoring-adjusted) 
  #                                      source yield (fully decayed)
  #   mean_yield_inc                  Bias-adjusted total incremental yield delivered to 
  #                                      streams
  #   mean_yield_inc_(sources)        Bias-adjusted source incremental yield delivered to 
  #                                      streams
  #   mean_yield_inc_deliv            Bias-adjusted total incremental yield delivered to 
  #                                      terminal reach
  #   mean_yield_inc_(sources)_deliv  Bias-adjusted source incremental yield delivered to 
  #                                      terminal reach
  
  #############################################################################
  ### 11. DIRECTORY AND MODEL IDENTIFICATION AND CONTROL SCRIPT OPERATIONS ####
  #############################################################################
  
  path_master <- "C:/proj/RSPARROW04302020/RSPARROW_master"

  #results, data, and gis directories should be in Users Directory
  results_directoryName<-"results"
  data_directoryName<-"data"
  gis_directoryName<-"gis"

  #Select current run_id for the model
  run_id<-"TampaBayTP"
  
  # Load previous model settings into active control script
  #  Specify name of old model to copy into results directory for edit and run
  #  Set copy_PriorModelFiles<-NA to run current control files located in results directory
  #   copy_PriorModelFiles<-"run_id"
  copy_PriorModelFiles<-NA
  

  # Run model comparison summary
  #  Select models to compare, use ONLY previous run_ids found in the results directory
  #  Select NA for no comparision
  #compare_models<-c("Model1","Model2","Model3")
  compare_models<-NA

  #Specify model comparison name, subdirectory name for the comparision results
  modelComparison_name<-"Compare1"
  modelComparison_name<-NA
    

  # Option to open CSV files from R-session for editing
  edit_Parameters<-"yes"
  edit_DesignMatrix<-"no"
  edit_dataDictionary<-"no"
    
  batch_mode<-"no"

  #########################################################
  ### 12. INSTALLATION AND VERIFICATION OF R LIBRARIES ####
  #########################################################
    
  # Install required packages
  #   this is a one time process unless a new version of R is installed
  #   packages previously installed by user will be skipped
    
  if_install_packages<-"no"
  source(paste(path_master,"/R/installPackages.R",sep=""))
  installPackages(if_install_packages,path_master)
    
  # Load RSPARROW functions (These 2 lines should ALWAYS be run together)
  suppressWarnings(remove(list=c("saved","runScript","run2","runRsparrow")))
  devtools::load_all(path_master,recompile = FALSE)  
    
  #############################################
  ## Start Model Run
  ## DO NOT MAKE EDITS TO THIS SECTION
  #############################################
  activeFile<-findScriptName() #get activeFile Name
  runRsparrow<-"yes"
  rstudioapi::setCursorPosition(c(1,1,427,1))
  source(paste(path_master,"/R/runRsparrow.R",sep=""))
 

