# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

setwd("C:\\Users\\Ami")

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "ASC all cost coeff all time coeff",
  modelDescr      = "ASC +travel cost+travel time",
  indivID         = "ID", # Ensure your dataset includes an individual ID column
  outputDirectory = "output_Aumy"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

### Loading data from package
### if data is to be loaded from a file (e.g. called data.csv), 
database = read.csv("thesissheet123.csv",header=TRUE)

database = subset(database, choice %in% c(1,2,3))

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(
  
  ASC_bus = 0,             # Base mode ASC fixed to zero
  ASC_train = 0,
  ASC_ac_bus = 0,
  #ASC_personal_car = 0,
  #ASC_rental_car = 0,
  #B_cost = 0,
  B_cost_bus = 0,
  B_cost_acbus = 0,
  B_cost_train = 0,
  #B_cost_carp = 0,
  #B_cost_carr = 0,
  #B_traveltime =0
  B_traveltime_bus=0,
  B_traveltime_acbus=0,
  B_traveltime_train=0
  #B_traveltime_carp=0,
  #B_traveltime_carr=0
  
)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("ASC_bus")

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate") {
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  
  V[["bus"]] = ASC_bus + B_cost_bus * (tc_bus/100) + B_traveltime_bus * (tt_bus/10)
  
  
  V[["ac_bus"]] = ASC_ac_bus +B_cost_acbus * (tc_acbus/100) + B_traveltime_acbus * (tt_acbus/10)
  
  V[["train"]] = ASC_train +B_cost_train * (tc_train/100) + B_traveltime_train * (tt_train/10)
  
  #V[["personal_car"]] = ASC_personal_car + B_cost_carp * (tc_carpersonal_n / 100) + B_traveltime_carp * (tt_car_personal/10)
  
  #V[["rental_car"]] = ASC_rental_car + B_cost_carr * (tc_carrental_n / 100) + B_traveltime_carr * (tt_car_rental/10)
  
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(bus = 1,ac_bus=2,train=3), 
    avail         = list(bus=bus_avail,ac_bus=ac_bus_avail,train=train_avail), 
    choiceVar     = choice,
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  # P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

#apollo_saveOutput(model)