# README

Control files for running tbepRSparrow

The file `app.R` is a version of the Shiny app for RSparrow that is hosted on https://shiny.tbeptech.org/tbepRSparrow-control/. It is similar to code in the file `debugShiny.R`.  The file loads the required libraries that were installed manually on the shiny server.  A shiny "image" file is then loaded and assigned to objects in the environment.  Objects in this image file were created by running the file `results/sparrow_control.R`, debugging through a browser at some point, and saving all objects in the workspace that were passed to `ShinyMap2()`.  Each of the objects in the image file are then used as arguments to the Shiny functions in RSparrow.  

This repository and [tbepRSparrow](https://github.com/tbep-tech/tbepRSparrow) are loaded on the tbep-tech server at `/srv/shiny-server/`.  The `app.R` file loads the tbepRSparrow package on initation using `devtools::load_all()` (i.e., it is not an installed package).  The tbepRSparrow package includes all functions for running the model and plotting the results. A results, gis, and data folder are also included in this repository and all are used by the Shiny application.  In particular, the results folder includes pre-processed results from the RSparrow model after running `results/sparrow_control.R`.  As is, the current application includes results for a fixed run of RSparrow.  

Updates to this application will include: 

* Adding a new release of RSparrow that includes interactive visualizations.  The current version includes some old functions I created that aren't that great.  The new version of RSparrow should replace the contents of [tbeRSparrow](https://github.com/tbep-tech/tbepRSparrow).  Take special note of modifications that were made to the original repo to allow the Shiny app to work (e.g., fixing relative paths, adding a `scenario_name` argument to `shinyScenarios` and `shinymap2`)

* Update the `gis`, `data`, and `results` folders with new content for the latest version of RSparrow.  In particular, the `results` folders is populated with files after running the model.  Take note of the file sizes for upload to GitHub. 

* Figure out how to integrate RSparrow directly with the shiny app.  This could be done the simple way (upload a zipped file for the results folder) or the hard way by running RSparrow directly on the server.  The latter could be a separate Shiny app, but I'll have to figure out how to allow Shiny to modify files on the server. 