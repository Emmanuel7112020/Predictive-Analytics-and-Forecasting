#Data 
#library(apollo)
#database <-  apollo_modeChoiceData
#help(apollo_modeChoiceData)
#str(database)
 #database <-  subset(database,SP==1)
 
###############
# Q1
#################
 
 ### Clear memory
 #rm(list = ls())
 
 ### Load Apollo library
 library(apollo)
 
 ### Initialise code
 apollo_initialise()
 
 ### Set core controls
 apollo_control <- list(modelName= "MNL_baseline",modelDescr= "Baselinemodel on mode choice SP data",indivID= "ID", outputDirectory = "output")
 
 ### Load mode choice data from the apollo library
 database <- apollo_modeChoiceData
 
 help(apollo_modeChoiceData)
 str(database)
 
 database <- subset(database,SP==1)
 
 
 apollo_beta <- c(
   asc_bus               = 0,
   asc_air               = 0,
   asc_rail              = 0,
   b_cost                = 0,
   b_female_bus          = 0,
   b_female_air          = 0,
   b_female_rail         = 0,
   b_tt_car              = 0,
   b_tt_bus              = 0,
   b_tt_air              = 0,
   b_tt_rail             = 0
   #lambda_PT             = 1
   )
 
 
 apollo_fixed <- NULL
 
 apollo_inputs <- apollo_validateInputs()
 
 apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality="estimate"){
   
   ### Attach inputs and detach after function exit
   apollo_attach(apollo_beta, apollo_inputs)
   on.exit(apollo_detach(apollo_beta, apollo_inputs))
   
   ### Create list of probabilities P
   P <- list()
   
   ### List of utilities
   V <- list()
   V[["car"]]  =                                     b_cost * cost_car  + b_tt_car  * time_car
   V[["bus"]]  = asc_bus  + b_female_bus  * female + b_cost * cost_bus  + b_tt_bus  * time_bus
   V[["air"]]  = asc_air  + b_female_air  * female + b_cost * cost_air  + b_tt_air  * time_air
   V[["rail"]] = asc_rail + b_female_rail * female + b_cost * cost_rail + b_tt_rail * time_rail
   
   ### Define settings for MNL model
   mnl_settings = list(
     alternatives = c(car=1, bus=2, air=3, rail=4),
     avail        = list(car=av_car, bus=av_bus, air=av_air, rail=av_rail),
     choiceVar    = choice,
     utilities    = V
   )
   
   
   ### Compute probabilities using MNL model
   P[["model"]] <- apollo_mnl(mnl_settings, functionality)
   
   ### Take product across observation for same individual
   P <- apollo_panelProd(P, apollo_inputs, functionality)
   
   ### Prepare and return outputs of function
   P <- apollo_prepareProb(P, apollo_inputs, functionality)
   return(P)
   
 }
 
 baseModel <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
 
 apollo_modelOutput(baseModel)
 
 apollo_saveOutput(baseModel)
 
 
####################
 # Q.2
####################
 
 ### two-level Nested Logit specification Q2
 
 apollo_control <- list(
   modelName       = "NL_two_levels",
   modelDescr      = "Two-level NL model on mode choice SP data",
   indivID         = "ID", 
   outputDirectory = "output")
 
 apollo_beta <- c(
   asc_bus               = 0,
   asc_air               = 0,
   asc_rail              = 0,
   b_cost                = 0,
   b_female_bus          = 0,
   b_female_air          = 0,
   b_female_rail         = 0,
   b_tt_car              = 0,
   b_tt_bus              = 0,
   b_tt_air              = 0,
   b_tt_rail             = 0,
   lambda_PT             = 1)
 
 
 apollo_fixed <- NULL
 
 apollo_inputs <- apollo_validateInputs()
 
 apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality="estimate"){
   
   ### Attach inputs and detach after function exit
   apollo_attach(apollo_beta, apollo_inputs)
   on.exit(apollo_detach(apollo_beta, apollo_inputs))
   
   ### Create list of probabilities P
   P <- list()
   
   ### List of utilities
   V <- list()
   V[["car"]]  =                                     b_cost * cost_car  + b_tt_car  * time_car
   V[["bus"]]  = asc_bus  + b_female_bus  * female + b_cost * cost_bus  + b_tt_bus  * time_bus
   V[["air"]]  = asc_air  + b_female_air  * female + b_cost * cost_air  + b_tt_air  * time_air
   V[["rail"]] = asc_rail + b_female_rail * female + b_cost * cost_rail + b_tt_rail * time_rail
   
   nlNests <- list(root=1, PT=lambda_PT)
   nlStructure <- list()
   nlStructure[["root"]] <- c("car","PT")
   nlStructure[["PT"]] <- c("bus","air","rail")
   
   ### Define settings for MNL model
   nl_settings = list(
     alternatives = c(car=1, bus=2, air=3, rail=4),
     avail        = list(car=av_car, bus=av_bus, air=av_air, rail=av_rail),
     choiceVar    = choice,
     utilities    = V,
     nlNests      = nlNests,
     nlStructure  = nlStructure
   )
   
   
   ### Compute probabilities using MNL model
   P[["model"]] <- apollo_nl(nl_settings, functionality)
   
   ### Take product across observation for same individual
   P <- apollo_panelProd(P, apollo_inputs, functionality)
   
   ### Prepare and return outputs of function
   P <- apollo_prepareProb(P, apollo_inputs, functionality)
   return(P)
   
 }
 
 nlModel <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
 
 apollo_modelOutput(nlModel)
 
 apollo_saveOutput(nlModel)
 
 ### LR test against MNL model - write result to an additional file
 
 apollo_sink()
 apollo_lrTest(baseModel,nlModel)
 apollo_sink()
 
 
 
 ####################
 # Q.3
 ####################
 
 ### three-level Nested Logit specification Q3
 
 apollo_control <- list(
   modelName       = "NL_three_levels",
   modelDescr      = "Three-level NL model on mode choice SP data",
   indivID         = "ID", 
   outputDirectory = "output")
 
 apollo_beta <- c(
   asc_bus               = 0,
   asc_air               = 0,
   asc_rail              = 0,
   b_cost                = 0,
   b_female_bus          = 0,
   b_female_air          = 0,
   b_female_rail         = 0,
   b_tt_car              = 0,
   b_tt_bus              = 0,
   b_tt_air              = 0,
   b_tt_rail             = 0,
   lambda_ground             = 1,
   lambda_tires          = 1
   )
 
 
 apollo_fixed <- NULL
 
 apollo_inputs <- apollo_validateInputs()
 
 apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality="estimate"){
   
   ### Attach inputs and detach after function exit
   apollo_attach(apollo_beta, apollo_inputs)
   on.exit(apollo_detach(apollo_beta, apollo_inputs))
   
   ### Create list of probabilities P
   P <- list()
   
   ### List of utilities
   V <- list()
   V[["car"]]  =                                     b_cost * cost_car  + b_tt_car  * time_car
   V[["bus"]]  = asc_bus  + b_female_bus  * female + b_cost * cost_bus  + b_tt_bus  * time_bus
   V[["air"]]  = asc_air  + b_female_air  * female + b_cost * cost_air  + b_tt_air  * time_air
   V[["rail"]] = asc_rail + b_female_rail * female + b_cost * cost_rail + b_tt_rail * time_rail
   
   nlNests <- list(root=1, ground=lambda_ground,tires=lambda_tires)
   nlStructure <- list()
   nlStructure[["root"]] <- c("air","ground")
   nlStructure[["ground"]] <- c("rail","tires")
   nlStructure[["tires"]] <- c("car","bus")
   
   ### Define settings for MNL model
   nl_settings = list(
     alternatives = c(car=1, bus=2, air=3, rail=4),
     avail        = list(car=av_car, bus=av_bus, air=av_air, rail=av_rail),
     choiceVar    = choice,
     utilities    = V,
     nlNests      = nlNests,
     nlStructure  = nlStructure
   )
   
   
   ### Compute probabilities using MNL model
   P[["model"]] <- apollo_nl(nl_settings, functionality)
   
   ### Take product across observation for same individual
   P <- apollo_panelProd(P, apollo_inputs, functionality)
   
   ### Prepare and return outputs of function
   P <- apollo_prepareProb(P, apollo_inputs, functionality)
   return(P)
   
 }
 
 nlModel_3lvls<- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
 
 apollo_modelOutput(nlModel_3lvls)
 
 apollo_saveOutput(nlModel_3lvls)
 
 
 apollo_sink()
 apollo_lrTest(baseModel,nlModel_3lvls)
 apollo_lrTest(nlModel,nlModel_3lvls)
 apollo_sink()