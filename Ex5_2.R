rm(list=ls())
library(apollo)

# set your working directory to where the data is stored (or save the data to your current working directory)
#getwd()
#setwd("/Users/lucawebe/Documents/Lehre/PAF_WS2122/Joint SP RP Data")

# read in data
data <- read.delim("netherlands.dat")
# alternatively:
# data <- read.table("netherlands.dat",header=TRUE,sep="\t",dec=".")
str(data)
View(data)

# change the choice variables of SP data from 10/11 to 0/1 
summary(factor(data$choice))
data$choice[which(data$choice==10)] <- 0
data$choice[which(data$choice==11)] <- 1

# build subsets on RP and SP data
rp_data <- subset(data,rp==1)
sp_data <- subset(data,sp==1)

apollo_initialise()

#---------------------------#
# Question 1: RP-data only  #
#---------------------------#
database <- rp_data

apollo_control <- list(modelName="Dutch RP",
                       modelDescr="Model for RP observations only",indivID="id")

apollo_beta <- c(asc_car  = 0,
                 asc_rail = 0,
                 b_cost   = 0,
                 b_tt     = 0)

apollo_fixed <- c("asc_car")

apollo_inputs <- apollo_validateInputs()

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  # Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  # create list of probabilities P
  P = list()
  
  # List of utilities
  V <- list()
  V[["car"]] <-  asc_car  + b_cost * cost_car  + b_tt * (rpovt_car + ivtt_car)
  V[["rail"]] <- asc_rail + b_cost * cost_rail + b_tt * (rpovt_rail + ivtt_rail)
  
  # compute probabilities using MNL-model
  mnl_settings <- list(
    alternatives = c(car=0, rail=1),
    avail        = 1,
    choiceVar    = choice,
    V            = V
  )
  
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)	 ### Compute probabilities using model
  
  
  #P <- apollo_panelProd(P, apollo_inputs, functionality)	 ### Take product across observations
  ### for same ID
  
  P <- apollo_prepareProb(P, apollo_inputs, functionality)	 ### Prepare and return outputs of function
  
  
  return(P) 
}

model_rp <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

output_rp <- apollo_modelOutput(model_rp)

#---------------------------#
# Question 2: SP-data only  #
#---------------------------#
database <- sp_data

apollo_control <- list(modelName="Dutch SP",
                       modelDescr="Model for SP observations only",indivID="id")

apollo_beta <- c(asc_car  = 0,
                 asc_rail = 0,
                 b_cost   = 0,
                 b_tt     = 0,
                 b_inertia  = 0)

apollo_fixed <- c("asc_car")

apollo_inputs <- apollo_validateInputs()

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  # Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  # create list of probabilities P
  P <- list()
  
  # List of utilities
  V <- list()
  V[["car"]] <-  asc_car  + b_cost * cost_car  + b_tt * (rpovt_car + ivtt_car)
  V[["rail"]] <- asc_rail + b_cost * cost_rail + b_tt * (rpovt_rail + ivtt_rail) + b_inertia * (rp_choice==1)
  
  # compute probabilities using MNL-model
  mnl_settings <- list(
    alternatives = c(car=0, rail=1),
    avail        = 1,
    choiceVar    = choice,
    V            = V
  )
  
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)	  ### Compute probabilities using model
  
  
  P <- apollo_panelProd(P, apollo_inputs, functionality)	  ### Take product across observations
  ### for same ID
  
  P <- apollo_prepareProb(P, apollo_inputs, functionality)  ### Prepare and return outputs of function
  
  
  return(P) 
}

model_sp <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

output_sp <- apollo_modelOutput(model_sp)

#--------------------------------------#
# Question 3 - ratios of coefficients  #
#--------------------------------------#

beta_rp <- output_rp[,1]
beta_sp <- output_sp[,1]

beta_rp/beta_sp


#------------------------------#
# Question 4 - combined Model  #
#------------------------------#
database <- data

apollo_control <- list(modelName="Dutch RP-SP",
                       modelDescr="Model for joint RP&SP data",indivID="id")


apollo_beta <- c(asc_car     = 0,
                 asc_rail_sp = 0,
                 asc_rail_rp = 0,
                 b_cost      = 0,
                 b_tt        = 0,
                 b_inertia   = 0,
                 mu_rp       = 1,
                 mu_sp       = 1
)

apollo_fixed <- c("asc_car","mu_rp")

apollo_inputs <- apollo_validateInputs()

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  # Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  # create list of probabilities P
  P <- list()
  
  # List of utilities without scaling
  V <- list()
  V[["car"]] <-  asc_car  + b_cost * cost_car  + b_tt * (rpovt_car + ivtt_car)
  V[["rail"]] <- asc_rail_rp * (rp==1) + asc_rail_sp * (sp==1) + b_cost * cost_rail + b_tt * (rpovt_rail + ivtt_rail) + b_inertia * (rp_choice==1) * (sp==1)
  
  # compute probabilities for the RP part using MNL-model, apply RP scale to V
  mnl_settings_rp <- list(
    alternatives = c(car=0, rail=1),
    avail        = 1,
    choiceVar    = choice,
    V            = list(car  = mu_rp * V[["car"]],
                        rail = mu_rp * V[["rail"]]),
    rows         = (rp==1)
  )
  
  P[["RP"]] <- apollo_mnl(mnl_settings_rp, functionality)	 
  
  # compute probabilities for the SP part using MNL-model, apply SP scale to V
  mnl_settings_sp <- list(
    alternatives = c(car=0, rail=1),
    avail        = 1,
    choiceVar    = choice,
    V            = list(car  = mu_sp * V[["car"]],
                        rail = mu_sp * V[["rail"]]),
    rows         = (sp==1)
  )
  
  P[["SP"]] <- apollo_mnl(mnl_settings_sp, functionality)	 
  
  # combined model
  P <- apollo_combineModels(P,apollo_inputs, functionality)
  
  P <- apollo_panelProd(P, apollo_inputs, functionality)	 ### Take product across observations
  ### for same ID
  
  P <- apollo_prepareProb(P, apollo_inputs, functionality)	 ### Prepare and return outputs of function
  
  return(P) 
}

model_combined <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

output_combined <- apollo_modelOutput(model_combined)
output_combined

#------------------------------#
# Question 5 - fix mu_sp to 1  #
#------------------------------#

apollo_fixed <- c("asc_car","mu_sp")
apollo_inputs <- apollo_validateInputs()
model_combined2 <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

output_combined2 <- apollo_modelOutput(model_combined2)
output_combined2

mu_rp1 <- output_combined['mu_rp',1]
mu_sp1 <- output_combined['mu_sp',1]
mu_rp2 <- output_combined2['mu_rp',1]
mu_sp2 <- output_combined2['mu_sp',1]

mu_rp1/mu_sp1
mu_rp2/mu_sp2
mu_sp1/mu_rp1
mu_sp2/mu_rp2

#-----------------------#
# Values of Traveltime  #
#-----------------------#

# compute b_tt/b_cost for each model
# how much Guilders is a customer willing to pay for a 1 minute reduction of travel time?

VTT_rp <- output_rp['b_tt',1]/output_rp['b_cost',1]
VTT_sp <- output_sp['b_tt',1]/output_sp['b_cost',1]
VTT_combined <- output_combined['b_tt',1]/output_combined['b_cost',1]

VTT_rp/60
VTT_sp/60
VTT_combined/60


