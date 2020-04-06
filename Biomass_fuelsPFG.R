# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "Biomass_fuelsPFG",
  description = "LandR Biomass model fire fuels calculator module, based on plant functional group composition", #"insert module description here",
  keywords = c("fire fuels", "fuel type", "plant functional groups", "LandR"),
  authors = c(person("Ceres", "Barros", email = "cbarros@mail.ubc.ca", role = c("aut", "cre")),
              person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("ctb")),
              person("Steven", "Cumming", email = "stevec@sbf.ulaval.ca", role = c("ctb"))),
  childModules = character(0),
  version = list(Biomass_fuelsPFG = numeric_version("0.0.9000"),
                 LandR = "0.0.3.9000", SpaDES.core = "0.2.7"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "Biomass_fuelsPFG.Rmd"),
  reqdPkgs = list("data.table", "dplyr", "sn", "RColorBrewer",
                  "PredictiveEcology/LandR@development",
                  "PredictiveEcology/SpaDES.core@development",
                  "PredictiveEcology/SpaDES.tools@development",
                  "PredictiveEcology/reproducible@development"),
  parameters = rbind(
    defineParameter("fireInitialTime", "numeric", 1,
                    desc = "The event time that the first fire disturbance event occurs"),
    defineParameter("fireTimestep", "numeric", NA,
                    desc = "The number of time units between successive fire events in a fire module"),
    defineParameter(name = "nonForestFire", class = "logical", default = FALSE,
                    desc = paste("Determines whether fire fuels will be calculated for non-forest pixels.",
                                 "If TRUE, the user should provide fuels table ('nonForestFuelsTable') for non forested",
                                 "land cover classes in accordance to the classes in 'rstLCC'")),
    defineParameter(".plotMaps", "logical", TRUE, NA, NA,
                    desc = "Controls whether maps should be plotted or not"),
    defineParameter("sppEquivCol", "character", "Boreal", NA, NA,
                    "The column in sim$specieEquivalency data.table to use as a naming convention"),
    defineParameter(".useCache", "logical", "init", NA, NA,
                    desc = "use caching for the spinup simulation?")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "cohortData", objectClass = "data.table",
                 desc = "age cohort-biomass table hooked to pixel group map by pixelGroupIndex at succession time step",
                 sourceURL = NA),
    expectsInput(objectName = "FirePFGs", objectClass = "data.table",
                 desc = paste("Table of fire-related plant functional groups, with species and age composition",
                              "per group."),
                 source = "https://drive.google.com/file/d/1xsNfPLy91D3VxMASAH4_-PUWs3HQWqBx/view?usp=sharing"),
    expectsInput(objectName = "FirePFGs2Fuels", objectClass = "data.table",
                 desc = paste("Table of correspondence between fire plant functional groups and FBP fuel types",
                              "(using fuel types from CF Fire Behaviour Prediction System (2nd Ed.)).",
                              "The table includes (minimum) thresholds of abundance that each group must have in",
                              "order to apply a particular fuel type to a stand."),
                 source = "https://drive.google.com/file/d/1nkKTsxPIpIvjwLsiMu6NrWv52c498UKx/view?usp=sharing"),
    expectsInput(objectName = "nonForestFuelsTable", objectClass = "data.table",
                 desc = paste("Table of correspondence between non-forested land-cover classes and fire fuels.",
                              "Fuel types come from CF Fire Behaviour Prediction System (2nd Ed.). Default values",
                              "use the LCC2005 land-cover product, and consider only grasslands and shurblands.")),
    expectsInput("rasterToMatch", "RasterLayer",
                 desc = "a raster of the studyArea in the same resolution and projection as biomassMap",
                 sourceURL = NA),
    expectsInput("rstLCCRTM", "RasterLayer",
                 desc = paste("A land classification map in study area, masked to rasterToMatch It must be 'corrected',",
                              "in the sense that:\n",
                              "1) Every class must not conflict with any other map in this module\n",
                              "    (e.g., speciesLayers should not have data in LCC classes that are non-treed);\n",
                              "2) It can have treed and non-treed classes. The non-treed will be removed within this\n",
                              "    module if P(sim)$omitNonTreedPixels is TRUE;\n",
                              "3) It can have transient pixels, such as 'young fire'. These will be converted to a\n",
                              "    the nearest non-transient class, probabilistically if there is more than 1 nearest\n",
                              "    neighbour class, based on P(sim)$LCCClassesToReplaceNN.\n",
                              "The default layer used, if not supplied, is Canada national land classification in 2005"),
                 sourceURL = "https://drive.google.com/file/d/1g9jr0VrQxqxGjZ4ckF6ZkSMP-zuYzHQC/view?usp=sharing"),
    expectsInput(objectName = "sppEquiv", objectClass = "data.table",
                 desc = "table of species equivalencies. See LandR::sppEquivalencies_CA.",
                 sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "fuelTypesMaps", objectClass = "list",
                  desc = "List of RasterLayers of fuel types and coniferDominance per pixel."),
    createsOutput(objectName = "pixelNonForestFuels", objectClass = "data.table",
                  desc = paste("Table of non forest fuel attributes (pixel ID, land cover, fuel type",
                               "name and code, and degree of curing) for each pixel with non-forest fuels"))
  )
))

doEvent.Biomass_fuelsPFG = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      # do stuff for this event
      sim <- fuelsInit(sim)

      sim <- scheduleEvent(sim, P(sim)$fireInitialTime,
                           "Biomass_fuelsPFG", "doFuelTypes", eventPriority = 1)
    },

    doFuelTypes = {
      # do stuff for this event
      sim <- calcFuelTypes(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$fireTimestep,
                           "Biomass_fuelsPFG", "doFuelTypes",  eventPriority = 1)
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

### initialization
fuelsInit <- function(sim) {
  ## checks
  if (start(sim) == P(sim)$fireInitialTime)
    warning(red("start(sim) and P(sim)$fireInitialTime are the same.\nThis may create bad scheduling with init events"))

  return(invisible(sim))
}

calcFuelTypes <- function(sim) {
  ## PIXEL FUEL TYPES TABLE ------------------------
  ## create pixelGroupFuelTypes table from cohorData
  pixelGroupFuelTypes <- sim$cohortData[, .(speciesCode, pixelGroup, ecoregionGroup, age, B)]

  ## calculate stand B before expanding
  pixelGroupFuelTypes[, sumB := sum(B), by = pixelGroup]

  ## add potential PFGs, and remove those that do not match the ages
  pixelGroupFuelTypes <- sim$FirePFGs[pixelGroupFuelTypes, on = .(speciesCode), allow.cartesian = TRUE]
  pixelGroupFuelTypes <- pixelGroupFuelTypes[age >= ageMin & age < ageMax,]

  ## add biomass per candidate PFG
  pixelGroupFuelTypes[, PFGsumB := sum(B), by = .(pixelGroup, PFGno)]
  ## calculate relative abundance of each group
  pixelGroupFuelTypes[, PFGrelAbund := PFGsumB/sumB]

  colsKeep <- grep("PFG|pixelGroup", names(pixelGroupFuelTypes), value = TRUE)
  pixelGroupFuelTypes <- sim$FirePFGs2Fuels[unique(pixelGroupFuelTypes[, ..colsKeep]),
                                            on = .(PFGno), allow.cartesian = TRUE]

  ## exclude candidate functional groups/fuel types that don't meet the minimum abundance threshold
  pixelGroupFuelTypes <- pixelGroupFuelTypes[PFGrelAbund >= Threshold]

  ## exclude fuel types that don't meet the minimum composition criteria
  ## e.g.  mixed stands: need enough cover from deciduous AND coniferous species.
  ## e.g. doug-fir/ponderosa stands need enough cover of doug-fir/ponderosa and a minimum amount of conifer cover.
  specialFTs <- sim$FirePFGs2Fuels$FT[duplicated(sim$FirePFGs2Fuels$FT)]
  if (length(specialFTs)) {
    for (spFT in specialFTs) {
      needPFGs <- sim$FirePFGs2Fuels[FT == spFT, PFGno]  ## list required PFGs for this fuel type (FT)
      PGsWFT <- unique(pixelGroupFuelTypes[grepl(spFT, FT), pixelGroup])  ## list all pixelGroups where at least one PFG for the FT was found
      PGsWFT2 <- pixelGroupFuelTypes[pixelGroup %in% PGsWFT, all(needPFGs %in% PFGno),
                                     by = pixelGroup][which(V1), pixelGroup]    ## list pixelGroups that have all required PFGs
      PGsWFT3 <- setdiff(PGsWFT, PGsWFT2)  ## pixelGroups that did not have all PFGs

      pixelGroupFuelTypes <- pixelGroupFuelTypes[!(pixelGroup %in% PGsWFT3 & FT == spFT)]  ## exclude candidate FTs that didn't meet the requirementsin the
    }
  }

  ## resolve pixelGroups that still have several candidate fuel types.
  ## rules of thumb:
  ## 1. if there are >= 2 fuel types, the one with more candidate PFGs wins (more support from spp composition)
  ## 2. if there they have equal number of candidate PFGs, the one with highest threshold wins
  dupPGs <- pixelGroupFuelTypes$pixelGroup[duplicated(pixelGroupFuelTypes$pixelGroup)]
  pixelGroupFTDups <- pixelGroupFuelTypes[pixelGroup %in% dupPGs]
  pixelGroupFTNotDups <- pixelGroupFuelTypes[!pixelGroup %in% dupPGs]

  ## resolve FTs with more candidate PFGs
  pixelGroupFTDups[, sumPFGs := length(PFGno), by = .(pixelGroup, FT)]   ## how many PFGs per candidate FT?
  pixelGroupFTDups[, sumPFGs := sumPFGs == max(sumPFGs), by = pixelGroup]  ## keep winners maximum?
  pixelGroupFTDups <- pixelGroupFTDups[as.logical(sumPFGs)]   ## remove losers

  ## resolve FTs with higher PFG thresholds - this will also "resolve" pixelGroups that still have the same FT with its several PFGs
  pixelGroupFTDups[, hiPFGs := as.logical(Threshold == max(Threshold)), by = pixelGroup]
  pixelGroupFTDups <- pixelGroupFTDups[(hiPFGs)]

  ## checks
  ## any more duplicates? for now stop, as they'll need to be dealt with as they appear
  if (any(duplicated(pixelGroupFTDups$pixelGroup)))
    stop("There are still pixelGroups with several possible fuel types.")

  if (any(pixelGroupFTDups$pixelGroup %in% pixelGroupFTNotDups$pixelGroup))
    stop("Something went wrong in checking for pixelGroups with several candidate fuel types.
         Please debug calcFuelTypes event function in Biomass_fuelsPFGs.")

  ## re-bind tables after filtering unnecessary columns
  pixelGroupFuelTypes <- rbind(pixelGroupFTDups[, .(FT, pixelGroup)], pixelGroupFTNotDups[, .(FT, pixelGroup)])

  ## calculate % conifers in mixed fuel types.
  notMixedPixelGroup <- pixelGroupFuelTypes[!grepl("M", FT)]
  mixedPixelGroup <- pixelGroupFuelTypes[grepl("M", FT)]

  noPGs <- length(unique(mixedPixelGroup$pixelGroup))  ## to check losses of PFGs later

  cols <- c("speciesCode", "B", "pixelGroup")
  mixedPixelGroup <- sim$cohortData[, ..cols][mixedPixelGroup, on = .(pixelGroup)]
  mixedPixelGroup[, Type := equivalentName(speciesCode, sim$sppEquiv, "Type")]

  mixedPixelGroup[, sumB := sum(B), by = pixelGroup]
  mixedPixelGroup[Type == "Conifer", coniferDom := sum(B)/sumB, by = pixelGroup]

  ## remove unnecessary columns and NAs that came from deciduous stands
  mixedPixelGroup <- unique(mixedPixelGroup[, .(pixelGroup, FT, coniferDom)])
  mixedPixelGroup <- mixedPixelGroup[!is.na(coniferDom)]

  ## checks
  if (getOption("LandR.verbose", TRUE) > 0) {
    if (any(mixedPixelGroup$coniferDom == 1, na.rm = TRUE) |
        any(mixedPixelGroup$coniferDom == 0, na.rm = TRUE))
      stop("Something went wrong some pixelGroups of mixed fuel type have 0 or 100% conifer cover.
            Please debug calcFuelTypes event function in Biomass_fuelsPFGs.")

    if (length(unique(mixedPixelGroup$pixelGroup)) != noPGs)
      stop("Something went wrong and some pixelGroups were lost when calculating conifer dominance.
            Please debug calcFuelTypes event function in Biomass_fuelsPFGs.")
  }

  ## bind again
  pixelGroupFuelTypes <- rbind(mixedPixelGroup, notMixedPixelGroup, use.names = TRUE, fill = TRUE)

  ## make a numeric column of fuel type
  pixelGroupFuelTypes[, finalFuelType := as.numeric(as.factor(FT))]
  ## change names
  setnames(pixelGroupFuelTypes, old = "FT", new = "FuelTypeFBP")

  ## rasterize forests fuel types table
  fuelTypesMaps <- rasterizeReduced(pixelGroupFuelTypes, sim$pixelGroupMap,
                                    newRasterCols = c("finalFuelType" , "coniferDom"),
                                    mapcode = "pixelGroup")

  ## ADD FUEL TYPES IN NON-FORESTED PIXELS ---------------------------
  if (P(sim)$nonForestFire) {
    nonForestPix <- which(sim$rstLCCRTM[] %in% sim$nonForestFuelsTable$LC)

    if (any(!is.na(fuelTypesMaps$finalFuelType[nonForestPix])))
      stop(paste("Either some pixelGroups correspond to non forest classes",
                 "in 'nonForestFuelsTable', or some of the clases in this table are forested"))

    if (!compareRaster(sim$rstLCCRTM, fuelTypesMaps$finalFuelType,
                       values = FALSE, stopiffalse = FALSE))
      stop("'rstLCCRTM' does not match 'fuelTypesMaps' rasters")

    # fuelTypesMaps$finalFuefuelTypesMaps[nonForestPix] <- sim$rstLCCRTM[nonForestPix]
    LC2FuelsTable <- data.table(LC = getValues(sim$rstLCCRTM)[nonForestPix],
                                pixID = nonForestPix)
    LC2FuelsTable <- sim$nonForestFuelsTable[LC2FuelsTable, on = "LC"]

    ## for fuel types with varying curing degree, using the a skewned normal with mean defined by
    ## the parameters in the table. SD/omega and alpha (i.e. skewnness) fixed to 10
    ## plot(0:100, dsn(c(0:100), xi = 60, omega = 10, alpha = 10), type = "l")
    ## then bound the values to defined min/max values
    LC2FuelsTable[which(!fixedCuring),
                  finalCuring := rsn(1, xi = curingMean, omega = 10, alpha = 10),
                  by = seq_len(nrow(LC2FuelsTable[which(!fixedCuring)]))]
    LC2FuelsTable[which(!fixedCuring), finalCuring := min(finalCuring, curingMax),
                  by = seq_len(nrow(LC2FuelsTable[which(!fixedCuring)]))]
    LC2FuelsTable[which(!fixedCuring), finalCuring := max(finalCuring, curingMin),
                  by = seq_len(nrow(LC2FuelsTable[which(!fixedCuring)]))]

    LC2FuelsTable[which(fixedCuring), finalCuring := curingMean]

    ## convert fuel type numbers to existing ones, add if necessary
    LC2FuelsTable <- unique(pixelGroupFuelTypes[, .(FuelTypeFBP, finalFuelType)])[LC2FuelsTable, on = .(FuelTypeFBP)]
    LC2FuelsTableNAs <- LC2FuelsTable[is.na(finalFuelType)]
    LC2FuelsTableNAs <- LC2FuelsTableNAs[, finalFuelType := NULL]

    newFTs <- unique(LC2FuelsTableNAs$FuelTypeFBP)
    newFTTable <- data.table(FuelTypeFBP = newFTs,
                             finalFuelType = max(pixelGroupFuelTypes$finalFuelType) + 1:length(newFTs))
    LC2FuelsTableNAs <- newFTTable[LC2FuelsTableNAs, on = .(FuelTypeFBP)]

    ## bind
    LC2FuelsTable <- rbind(LC2FuelsTable[!is.na(finalFuelType)], LC2FuelsTableNAs, use.names = TRUE)

    ## add non-forest fuels to map and attribute 0 conifer dominance values
    fuelTypesMaps$finalFuelType[LC2FuelsTable$pixID] <- as.numeric(LC2FuelsTable$finalFuelType)
    fuelTypesMaps$coniferDom[LC2FuelsTable$pixID] <- 0

    ## add make rasters of curing, etc due to projections (pixIds are not trackable)
    fuelTypesMaps$curing <- sim$rstLCCRTM
    fuelTypesMaps$curing[] <- NA
    fuelTypesMaps$curing[LC2FuelsTable$pixID] <-  LC2FuelsTable$finalCuring

    ## export to sim
    sim$pixelNonForestFuels <- LC2FuelsTable[, .(pixID, FuelTypeFBP, finalFuelType, finalCuring)]
  } else {
    ## if not running fires in non-forested pixel export an empty object
    sim$pixelNonForestFuels <- NULL
    fuelTypesMaps$curing <- fuelTypesMaps$finalFuelType
    fuelTypesMaps$curing[] <- NA
  }

  ##  add levels to fuel types raster
  fuelTypesMaps$finalFuelType <- ratify(fuelTypesMaps$finalFuelType)
  levs <- as.data.table(raster::levels(fuelTypesMaps$finalFuelType)[[1]])
  levs2 <- rbind(unique(pixelGroupFuelTypes[, .(finalFuelType, FuelTypeFBP)]),
                 unique(sim$pixelNonForestFuels[, .(finalFuelType, FuelTypeFBP)]),
                 use.names = TRUE) %>%
    unique(.)
  setnames(levs2, "finalFuelType", "ID")
  levs <- levs2[levs, on = "ID"]
  levels(fuelTypesMaps$finalFuelType) <- as.data.frame(levs)
  setColors(fuelTypesMaps$finalFuelType, n = nrow(levs)) <- brewer.pal(n = nrow(levs), "Accent")

  ## export to sim
  sim$fuelTypesMaps <- fuelTypesMaps

  ## TODO: add plotting options, here on in a separate event
  ## how: have Biomass_core export the map/stats plot windows
  ## to sim, so they can be used here.
  if (P(sim)$.plotMaps) {
    browser()
  }

  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  if (getOption("LandR.verbose", TRUE) > 0)
    message(currentModule(sim), ": using dataPath '", dPath, "'.")

  #######################################################

  ## SPECIES EQUIVALENCY TABLE ---------------------------
  if (!suppliedElsewhere("sppEquiv", sim)) {
    if (!is.null(sim$sppColorVect))
      stop("If you provide sppColorVect, you MUST also provide sppEquiv")

    data("sppEquivalencies_CA", package = "LandR", envir = environment())
    sim$sppEquiv <- as.data.table(sppEquivalencies_CA)

    ## By default, Abies_las is renamed to Abies_sp
    sim$sppEquiv[KNN == "Abie_Las", LandR := "Abie_sp"]
  }

  ## FIRE PFGS AND FUEL TYPES ----------------------
  if (!suppliedElsewhere("FirePFGs", sim)) {
    sim$FirePFGs <- prepInputs(targetFile = "FirePFGs.csv",
                               url = extractURL("FirePFGs"),
                               destinationPath = dPath,
                               fun = "data.table::fread",
                               header = TRUE)

    ## convert species names and exclude spp that are not in the simulation (if they have been excluded from sppEquiv already)
    sim$FirePFGs$speciesCodeLong <- sub("_", " ", sim$FirePFGs$speciesCodeLong)
    sim$FirePFGs$speciesCode <- equivalentName(sim$FirePFGs$speciesCodeLong, sim$sppEquiv, column = P(sim)$sppEquivCol)
    sim$FirePFGs <- sim$FirePFGs[!is.na(speciesCode), .(PFGno, PFGName, speciesCode, ageMin, ageMax)]
    sim$FirePFGs <- unique(sim$FirePFGs)
  }

  if (!suppliedElsewhere("FirePFGs2Fuels", sim)) {
    sim$FirePFGs2Fuels <- prepInputs(targetFile = "FirePFGs2Fuels.csv",
                                     url = extractURL("FirePFGs2Fuels"),
                                     destinationPath = dPath,
                                     fun = "data.table::fread",
                                     header = TRUE)
  }

  ## FUEL TYPES FOR NON-FOREST LAND-COVER CLASSES ------
  if (P(sim)$nonForestFire) {
    if (!suppliedElsewhere("nonForestFuelsTable", sim)) {
      ## for non forest fuels classified as open vegetation/grassland (O1a, O1b)
      ## the decree of curing needs to be defined, and whether it is fixed (only mean necessary)
      ## or drawn from a distribution (mean, min and max required)
      ## if drawn from a distribution, a normal distribution with right-side fat tail will be used (Perrakis, pers. comm.)
      ## mean, min and max values from Perrakis (pers. comm.)
      sim$nonForestFuelsTable <- data.table(LC = c(16, 17, 21, 22, 23, 24, 25),
                                            FuelTypeFBP = c("O1b", "O1b", "O1b", "O1b", "O1b", "O1b", "NF"),
                                            FuelType = c(15, 15, 15, 15, 15, 15, 19),
                                            fixedCuring = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, NA),
                                            curingMean = c(60, 60, 60, 60, 35, 30, NA),
                                            curingMin = c(50, 50, 50, 50, 0, 30, NA),
                                            curingMax = c(80, 80, 80, 80, 60, 30, NA))
    }
  }

  ## LAND COVER RASTERS ----------------------------------
  if (!suppliedElsewhere("rstLCCRTM", sim)) {
    if (!suppliedElsewhere("studyArea", sim)) {
      message("'studyArea' was not provided by user. Using a polygon (6250000 m^2) in southwestern Alberta, Canada")
      sim$studyArea <- randomStudyArea(seed = 1234, size = (250^2)*100)
    }

    if (!suppliedElsewhere("studyAreaLarge", sim)) {
      message("'studyAreaLarge' was not provided by user. Using the same as 'studyArea'")
      sim <- objectSynonyms(sim, list(c("studyAreaLarge", "studyArea")))
    }

    if (!identical(crs(sim$studyArea), crs(sim$studyAreaLarge))) {
      warning("studyArea and studyAreaLarge have different projections.\n
            studyAreaLarge will be projected to match crs(studyArea)")
      sim$studyAreaLarge <- spTransform(sim$studyAreaLarge, crs(sim$studyArea))
    }

    ## check whether SA is within SALarge
    ## convert to temp sf objects
    studyArea <- st_as_sf(sim$studyArea)
    studyAreaLarge <- st_as_sf(sim$studyAreaLarge)

    if (!st_within(studyArea, studyAreaLarge)[[1]])
      stop("studyArea is not fully within studyAreaLarge.
           Please check the aligment, projection and shapes of these polygons")
    rm(studyArea, studyAreaLarge)

    ## Raster(s) to match ------------------------------------------------
    needRTM <- FALSE
    if (is.null(sim$rasterToMatch) || is.null(sim$rasterToMatchLarge)) {
      if (!suppliedElsewhere("rasterToMatch", sim) ||
          !suppliedElsewhere("rasterToMatchLarge", sim)) {      ## if one is not provided, re do both (safer?)
        needRTM <- TRUE
        message("There is no rasterToMatch/rasterToMatchLarge supplied; will attempt to use rawBiomassMap")
      } else {
        stop("rasterToMatch/rasterToMatchLarge is going to be supplied, but ", currentModule(sim), " requires it ",
             "as part of its .inputObjects. Please make it accessible to ", currentModule(sim),
             " in the .inputObjects by passing it in as an object in simInit(objects = list(rasterToMatch = aRaster)",
             " or in a module that gets loaded prior to ", currentModule(sim))
      }
    }

    if (needRTM) {
      if (!suppliedElsewhere("rawBiomassMap", sim)) {
        sim$rawBiomassMap <- Cache(prepInputs,
                                   targetFile = asPath(basename(rawBiomassMapFilename)),
                                   archive = asPath(c("kNN-StructureBiomass.tar",
                                                      "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.zip")),
                                   url = extractURL("rawBiomassMap"),
                                   destinationPath = dPath,
                                   studyArea = sim$studyAreaLarge,   ## Ceres: makePixel table needs same no. pixels for this, RTM rawBiomassMap, LCC.. etc
                                   # studyArea = sim$studyArea,
                                   rasterToMatch = if (!needRTM) sim$rasterToMatchLarge else NULL,
                                   # maskWithRTM = TRUE,    ## if RTM not supplied no masking happens (is this intended?)
                                   maskWithRTM = if (!needRTM) TRUE else FALSE,
                                   ## TODO: if RTM is not needed use SA CRS? -> this is not correct
                                   # useSAcrs = if (!needRTM) TRUE else FALSE,
                                   useSAcrs = FALSE,     ## never use SA CRS
                                   method = "bilinear",
                                   datatype = "INT2U",
                                   filename2 = TRUE, overwrite = TRUE,
                                   userTags = cacheTags,
                                   omitArgs = c("destinationPath", "targetFile", "userTags", "stable"))
      }
      ## if we need rasterToMatch/rasterToMatchLarge, that means a) we don't have it, but b) we will have rawBiomassMap
      ## even if one of the rasterToMatch is present re-do both.

      if (is.null(sim$rasterToMatch) != is.null(sim$rasterToMatchLarge))
        warning(paste0("One of rasterToMatch/rasterToMatchLarge is missing. Both will be created \n",
                       "from rawBiomassMap and studyArea/studyAreaLarge.\n
              If this is wrong, provide both rasters"))

      sim$rasterToMatchLarge <- sim$rawBiomassMap
      RTMvals <- getValues(sim$rasterToMatchLarge)
      sim$rasterToMatchLarge[!is.na(RTMvals)] <- 1

      sim$rasterToMatchLarge <- Cache(writeOutputs, sim$rasterToMatchLarge,
                                      filename2 = file.path(cachePath(sim), "rasters", "rasterToMatchLarge.tif"),
                                      datatype = "INT2U", overwrite = TRUE,
                                      userTags = cacheTags,
                                      omitArgs = c("userTags"))

      sim$rasterToMatch <- Cache(postProcess,
                                 x = sim$rawBiomassMap,
                                 studyArea = sim$studyArea,
                                 rasterToMatch = sim$rasterToMatchLarge,
                                 useSAcrs = FALSE,
                                 maskWithRTM = FALSE,   ## mask with SA
                                 method = "bilinear",
                                 datatype = "INT2U",
                                 filename2 = file.path(cachePath(sim), "rasterToMatch.tif"),
                                 overwrite = TRUE,
                                 userTags = cacheTags,
                                 omitArgs = c("destinationPath", "targetFile", "userTags", "stable"))

      ## covert to 'mask'
      RTMvals <- getValues(sim$rasterToMatch)
      sim$rasterToMatch[!is.na(RTMvals)] <- 1
    }

    if (!identical(crs(sim$studyArea), crs(sim$rasterToMatch))) {
      warning(paste0("studyArea and rasterToMatch projections differ.\n",
                     "studyArea will be projected to match rasterToMatch"))
      sim$studyArea <- spTransform(sim$studyArea, crs(sim$rasterToMatch))
      sim$studyArea <- fixErrors(sim$studyArea)
    }

    if (!identical(crs(sim$studyAreaLarge), crs(sim$rasterToMatchLarge))) {
      warning(paste0("studyAreaLarge and rasterToMatchLarge projections differ.\n",
                     "studyAreaLarge will be projected to match rasterToMatchLarge"))
      sim$studyAreaLarge <- spTransform(sim$studyAreaLarge, crs(sim$rasterToMatchLarge))
      sim$studyAreaLarge <- fixErrors(sim$studyAreaLarge)
    }

    if (!suppliedElsewhere("rstLCC", sim)) {
      sim$rstLCCRTM <- Cache(prepInputs,
                             targetFile = lcc2005Filename,
                             archive = asPath("LandCoverOfCanada2005_V1_4.zip"),
                             url = extractURL("rstLCC"),
                             destinationPath = dPath,
                             studyArea = sim$studyArea,
                             rasterToMatch = sim$rasterToMatch,
                             maskWithRTM = TRUE,
                             method = "ngb",
                             datatype = "INT2U",
                             filename2 = FALSE, overwrite = TRUE,
                             userTags = c("prepInputsrstLCCRTM", cacheTags), # use at least 1 unique userTag
                             omitArgs = c("destinationPath", "targetFile", "userTags"))
    } else {
      sim$rstLCCRTM <- Cache(postProcess,
                             x = sim$rstLCC,
                             rasterToMatch = sim$rasterToMatch,
                             method = "ngb",
                             maskWithRTM = TRUE,
                             filename2 = NULL,
                             userTags = c("prepInputsrstLCCRTM", cacheTags),
                             omitArgs = "userTags")
    }

  }

  return(invisible(sim))
}
