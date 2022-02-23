library(apollo)								# run apollo package
apollo_initialise()

apollo_control=list(modelName="Model 2",
modelDescr="Some Description",indivID="obs")


#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_2              = 0,
              asc_3              = 0,
		  asc_4		   = 0,
		  asc_5		   = 0,
		  asc_6		   = 0,
		  asc_7		   = 0,
		  asc_8		   = 0,
		  asc_9		   = 0,
		  asc_i2		   = 0,
		  asc_i3		   = 0,
		  asc_i4		   = 0,
		  asc_i5		   = 0,
		  asc_i6		   = 0,
		  asc_i7		   = 0,
		  asc_i8		   = 0,
		  asc_i9		   = 0,
		  beta_oc		   = 0,
		  beta_fc		   = 0,
		  beta_fc_i		   = 0		
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
  V[['gg']]		= 					+beta_oc * op_cost.1 + beta_fc * fix_cost.1 + beta_fc_i*fix_cost.1*income 
  V[['ge']] 	=   asc_2	+ asc_i2*income	+beta_oc * op_cost.2 + beta_fc * fix_cost.2 + beta_fc_i*fix_cost.2*income 
  V[['deo']] 	=   asc_3	+ asc_i3*income	+beta_oc * op_cost.3 + beta_fc * fix_cost.3 + beta_fc_i*fix_cost.3*income 
  V[['dee']] 	=   asc_4	+ asc_i4*income	+beta_oc * op_cost.4 + beta_fc * fix_cost.4 + beta_fc_i*fix_cost.4*income 
  V[['oo']] 	=   asc_5	+ asc_i5*income	+beta_oc * op_cost.5 + beta_fc * fix_cost.5 + beta_fc_i*fix_cost.5*income 
  V[['oe']]  	=   asc_6	+ asc_i6*income	+beta_oc * op_cost.6 + beta_fc * fix_cost.6 + beta_fc_i*fix_cost.6*income 
  V[['ee']]  	=   asc_7	+ asc_i7*income	+beta_oc * op_cost.7 + beta_fc * fix_cost.7 + beta_fc_i*fix_cost.7*income 
  V[['we']]  	=   asc_8	+ asc_i8*income	+beta_oc * op_cost.8 + beta_fc * fix_cost.8 + beta_fc_i*fix_cost.8*income 
  V[['wee']] 	=   asc_9	+ asc_i9*income	+beta_oc * op_cost.9 + beta_fc * fix_cost.9 + beta_fc_i*fix_cost.9*income 
 
   mnl_settings = list(						      							### Define settings for model 
   alternatives = c(gg=1, ge=2, deo=3, dee=4, oo=5, oe=6, ee=7, we=8, wee=9),					### component	
   avail        =  list(gg=avail.1, ge=avail.2, deo=avail.3, dee=avail.4, oo=avail.5,
				 oe=avail.6, ee=avail.7, we=avail.8, wee=avail.9),
   choiceVar    = choice,
   V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 ### Compute probabilities using model
  
  #P = apollo_panelProd(P, apollo_inputs, functionality)	 ### Take product across observation
										 ### for same ID
 
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 ### Prepare and return outputs of function

  return(P)
}


Model2 = apollo_estimate(apollo_beta,
				apollo_fixed,
				apollo_probabilities,
				apollo_inputs)

apollo_modelOutput(Model2)


