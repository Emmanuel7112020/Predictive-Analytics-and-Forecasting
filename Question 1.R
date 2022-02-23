#Exercise 4

#Question 1

database <- read.delim("marketing.dat")
database<-database[order(database$ID),]

#or

database=read.csv(file="ChoiceLab.csv",header=TRUE)
database<-database[order(database$ID),]



library(apollo)								# run apollo package
apollo_initialise()


#set some controls

#indicate the name (in quotes) of the column in the data which contains the identifer variable
#for individual decision makers. For our data set that is "ID"





apollo_control=list(modelName="Fashion 1",
                    modelDescr="Some Description",indivID="ID")


#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_leave              = 0,
              asc_age                = 0,
              asc_nbempl		         = 0,
              asc_negprofit	       	 = 0		
)


#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary  

apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()								 ### List of utilities
  V[['leave']]  =  asc_leave + asc_age*Age+asc_nbempl*NbEmpl+asc_negprofit*NegProfit
  V[['stay']] = 0  
  
  mnl_settings = list(						             ### Define settings for model 
    alternatives = c(leave=1, stay=0),				 ### component	
    avail        =  1,
    choiceVar    = Choice,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 ### Compute probabilities using model
  
  P = apollo_panelProd(P, apollo_inputs, functionality)	 ### Take product across observation
  ### for same ID
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 ### Prepare and return outputs of function
  
  return(P)
}


Fashion1 = apollo_estimate(apollo_beta,
                           apollo_fixed,
                           apollo_probabilities,
                           apollo_inputs)

apollo_modelOutput(Fashion1)

