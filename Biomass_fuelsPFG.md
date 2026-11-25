---
title: "Biomass_fuelsPFG"
author: ""
date: "18 March 2020"
output:
  html_document:
    keep_md: yes
---

# Overview

Provide an overview of what the module does / how to use the module.

Module documentation should be written so that others can use your module.
This is a template for module documentation, and should be changed to reflect your module.

## R Markdown

R Markdown syntax allows R code, outputs, and figures to be rendered in the documentation.

For help writing in R Markdown, see http://rmarkdown.rstudio.com/.

# Usage


``` r
library(SpaDES.core)

setPaths(modulePath = file.path("E:/GitHub/LandscapesInMotion/R/SpaDES/m"))
getPaths() # shows where the 4 relevant paths are

times <- list(start = 0, end = 10)

parameters <- list(
  #.progress = list(type = "text", interval = 1), # for a progress bar
  ## If there are further modules, each can have its own set of parameters:
  #module1 = list(param1 = value1, param2 = value2),
  #module2 = list(param1 = value1, param2 = value2)
)
modules <- list("Biomass_fuelsPFG")
objects <- list()
inputs <- list()
outputs <- list()

mySim <- simInit(times = times, params = parameters, modules = modules,
                 objects = objects)

mySimOut <- spades(mySim)
```

# Parameters

Provide a summary of user-visible parameters.


``` r
df_params <- SpaDES.core::moduleParams("Biomass_fuelsPFG", "..")
knitr::kable(df_params)
```



|paramName       |paramClass |default |min |max |paramDesc                                                                                                                                                                                                                 |cacheKey |
|:---------------|:----------|:-------|:---|:---|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:--------|
|fireInitialTime |numeric    |1       |NA  |NA  |The event time that the first fire disturbance event occurs                                                                                                                                                               |TRUE     |
|fireTimestep    |numeric    |NA      |NA  |NA  |The number of time units between successive fire events in a fire module                                                                                                                                                  |TRUE     |
|nonForestFire   |logical    |FALSE   |NA  |NA  |Determines whether fire fuels will be calculated for non-forest pixels. If TRUE, the user should provide fuels table ('nonForestFuelsTable') for non forested land cover classes in accordance to the classes in 'rstLCC' |TRUE     |
|.plotMaps       |logical    |TRUE    |NA  |NA  |Controls whether maps should be plotted or not                                                                                                                                                                            |TRUE     |
|sppEquivCol     |character  |Boreal  |NA  |NA  |The column in sim$specieEquivalency data.table to use as a naming convention                                                                                                                                              |TRUE     |
|.studyAreaName  |character  |NA      |NA  |NA  |Human-readable name for the study area used. If `NA`, a hash of `studyArea` will be used.                                                                                                                                 |TRUE     |
|.useCache       |logical    |init    |NA  |NA  |use caching for the spinup simulation?                                                                                                                                                                                    |TRUE     |

# Events

Describe what happens for each event type.

## Plotting

Write what is plotted.

## Saving

Write what is saved.

# Data dependencies

## Input data

How to obtain input data, and a description of the data required by the module.
If `sourceURL` is specified, `downloadData("Biomass_fuelsPFG", "path/to/modules/dir")` may be sufficient.


``` r
df_inputs <- SpaDES.core::moduleInputs("Biomass_fuelsPFG", "..")
knitr::kable(df_inputs)
```



|objectName          |objectClass              |desc                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |sourceURL                                                                          |
|:-------------------|:------------------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:----------------------------------------------------------------------------------|
|cohortData          |data.table               |age cohort-biomass table hooked to pixel group map by pixelGroupIndex at succession time step                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |NA                                                                                 |
|FirePFGs            |data.table               |Table of fire-related plant functional groups, with species and age composition per group.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |https://drive.google.com/file/d/1xsNfPLy91D3VxMASAH4_-PUWs3HQWqBx/view?usp=sharing |
|FirePFGs2Fuels      |data.table               |Table of correspondence between fire plant functional groups and FBP fuel types (using fuel types from CF Fire Behaviour Prediction System (2nd Ed.)). The table includes (minimum) thresholds of abundance that each group must have in order to apply a particular fuel type to a stand.                                                                                                                                                                                                                                                                                                                                                                                                                             |https://drive.google.com/file/d/1nkKTsxPIpIvjwLsiMu6NrWv52c498UKx/view?usp=sharing |
|nonForestFuelsTable |data.table               |Table of correspondence between non-forested land-cover classes and fire fuels. Fuel types come from CF Fire Behaviour Prediction System (2nd Ed.). Default values use the LCC2005 land-cover product, and consider only grasslands and shurblands.                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |NA                                                                                 |
|rasterToMatch       |RasterLayer              |a raster of the studyArea in the same resolution and projection as biomassMap                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |NA                                                                                 |
|rstLCCRTM           |RasterLayer              |A land classification map in study area, masked to rasterToMatch It must be 'corrected', in the sense that: 1) Every class must not conflict with any other map in this module (e.g., speciesLayers should not have data in LCC classes that are non-treed); 2) It can have treed and non-treed classes. The non-treed will be removed within this module if P(sim)$omitNonTreedPixels is TRUE; 3) It can have transient pixels, such as 'young fire'. These will be converted to a the nearest non-transient class, probabilistically if there is more than 1 nearest neighbour class, based on P(sim)$LCCClassesToReplaceNN. The default layer used, if not supplied, is Canada national land classification in 2005 |https://drive.google.com/file/d/1g9jr0VrQxqxGjZ4ckF6ZkSMP-zuYzHQC/view?usp=sharing |
|studyArea           |SpatialPolygonsDataFrame |Polygon of the study area. Required to make `rstLCCRTM` when it is not supplied; its hash is the default `.studyAreaName`.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |NA                                                                                 |
|sppEquiv            |data.table               |table of species equivalencies. See LandR::sppEquivalencies_CA.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |NA                                                                                 |

## Output data

Description of the module outputs.


``` r
df_outputs <- SpaDES.core::moduleOutputs("Biomass_fuelsPFG", "..")
knitr::kable(df_outputs)
```



|objectName          |objectClass |desc                                                                                                                                           |
|:-------------------|:-----------|:----------------------------------------------------------------------------------------------------------------------------------------------|
|fuelTypesMaps       |list        |List of RasterLayers of fuel types and coniferDominance per pixel.                                                                             |
|pixelNonForestFuels |data.table  |Table of non forest fuel attributes (pixel ID, land cover, fuel type name and code, and degree of curing) for each pixel with non-forest fuels |

# Links to other modules

Describe any anticipated linkages to other modules.
