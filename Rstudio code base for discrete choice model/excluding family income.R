
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
  modelName       = "ASC_seperate_cost_income separate_sep time",
  modelDescr      = "ASC with single cost, time,sepfamily income, single income, and distance",
  indivID         = "ID", # Ensure your dataset includes an individual ID column
  outputDirectory = "output_Aumy"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

### Loading data from package
### if data is to be loaded from a file (e.g. called data.csv), 
database = read.csv("thesissheetTT.csv",header=TRUE)

#database = subset(database, choice %in% c(1, 2, 3))

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(
  
  ASC_bus = 0,             # Base mode ASC fixed to zero
  ASC_train = 0,
  ASC_ac_bus = 0,
  ASC_personal_car = 0,
  ASC_rental_car = 0,
  
  B_cost_bus = 0,
  B_cost_train = 0,
  B_cost_acbus  = 0,
  B_cost_personal_car = 0,
  B_cost_rental_car = 0,
  # B_cost =0,
  
  B_TravelTime_bus = 0,
  B_TravelTime_train = 0,
  B_TravelTime_ac_bus = 0,
  B_TravelTime_personal_car = 0,
  B_TravelTime_rental_car = 0,
  
  #B_distance = 0,
  #B_IncomeSelf_bus = 0,
  B_IncomeSelf_acbus = 0,
  B_IncomeSelf_train = 0,
  B_IncomeSelf_carp = 0,
  B_IncomeSelf_carr = 0
  #B_IncomeFam_bus = 0,
  #B_IncomeFam_acbus = 0,
  #B_IncomeFam_train = 0,
  #B_IncomeFam_carp = 0,
  #B_IncomeFam_carr = 0
  
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
  
  V[["bus"]] = ASC_bus + B_cost_bus * (tc_bus/100) + B_TravelTime_bus * tt_bus  #+ B_IncomeSelf_bus * income_self #+ B_distance * (distance_km) 
  V[["ac_bus"]] = ASC_ac_bus + B_cost_acbus * (tc_acbus/100) + B_IncomeSelf_acbus * income_self + B_TravelTime_ac_bus * tt_acbus
    #+ B_distance * (distance_km/100) + B_IncomeFam_acbus * income_fam  
  V[["train"]] = ASC_train + B_cost_train * (tc_train / 100) + B_IncomeSelf_train * income_self + B_TravelTime_train * tt_train
  
  V[["personal_car"]] = ASC_personal_car + B_cost_personal_car * (tc_carpersonal_n/100)  + B_IncomeSelf_carp * income_self+ + B_TravelTime_personal_car * tt_car_personal
                        #+B_IncomeFam_carp * income_fam + B_distance * (distance_km/100)
  V[["rental_car"]] = ASC_rental_car + B_cost_rental_car* (tc_carrental_n/100) + B_IncomeSelf_carr * income_self+  B_TravelTime_rental_car * tt_car_rental
                     #+ B_distance * (distance_km/100) + B_IncomeFam_carr * income_fam +
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(bus = 1, ac_bus = 2, train = 3,personal_car=4,rental_car=5), 
    avail         = list(bus=bus_avail, ac_bus=ac_bus_avail, train=train_avail,personal_car = personal_car_avail,rental_car= rental_car_avail), 
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
  modelName       = "ASC_seperate_cost_separate_sep time",
  modelDescr      = "ASC with single cost, time",
  indivID         = "ID", # Ensure your dataset includes an individual ID column
  outputDirectory = "output_Aumy"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

### Loading data from package
### if data is to be loaded from a file (e.g. called data.csv), 
database = read.csv("thesissheet123.csv",header=TRUE)

#database = subset(database, choice %in% c(1, 2, 3))

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(
  
  ASC_bus = 0,             # Base mode ASC fixed to zero
  ASC_train = 0,
  ASC_ac_bus = 0,
  ASC_personal_car = 0,
  ASC_rental_car = 0,
  
  B_cost_bus = 0,
  B_cost_train = 0,
  B_cost_acbus  = 0,
  B_cost_carp = 0,
 # B_cost_carr = 0
#B_cost =0,
  
 B_TravelTime_bus = 0,
 B_TravelTime_train = 0,
B_TravelTime_ac_bus = 0,
 B_TravelTime_personal_car = 0
  #B_TravelTime_rental_car = 0
  #B_income_interaction =0,
 # B_distance = 0,
  #B_IncomeSelf_bus = 0,
  #B_IncomeSelf_acbus = 0,
  #B_IncomeSelf_train = 0,
 # B_IncomeSelf_carp = 0,
 # B_IncomeSelf_carr = 0
  #B_IncomeFam_bus = 0,
  #B_IncomeFam_acbus = 0,
  #B_IncomeFam_train = 0,
  #B_IncomeFam_carp = 0,
  #B_IncomeFam_carr = 0
  
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
  
  V[["bus"]] = ASC_bus + B_cost_bus * (tc_bus/100) + B_TravelTime_bus * tt_bus  #+ B_IncomeSelf_bus * income_self #+ B_distance * (distance_km) 
  V[["ac_bus"]] = ASC_ac_bus + B_cost_acbus * (tc_acbus/100) + B_TravelTime_ac_bus * tt_acbus #+ B_IncomeSelf_acbus * income_self + B_distance * (distance_km/100) 
  #+ B_IncomeFam_acbus * income_fam  +  B_income_interaction * (income_self * income_fam)
  V[["train"]] = ASC_train + B_cost_train * (tc_train / 100) + B_TravelTime_train * tt_train #+ B_IncomeSelf_train * income_self+ B_distance * (distance_km/100)  #+  B_income_interaction * (income_self * income_fam)
  
  V[["personal_car"]] = ASC_personal_car + B_cost_carp * (tc_carpersonal_n/100) + B_TravelTime_personal_car * tt_car_personal # + B_IncomeSelf_carp * income_self+ B_distance * (distance_km/100) 
  #+B_IncomeFam_carp * income_fam +  B_income_interaction * (income_self * income_fam)
 # V[["rental_car"]] = ASC_rental_car + B_cost_carr* (tc_carrental_n/100) + B_TravelTime_rental_car * tt_car_rental #+ B_IncomeSelf_carr * income_self+ B_distance * (distance_km/100) 
  #+ B_IncomeFam_carr * income_fam +  B_income_interaction * (income_self * income_fam)
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(bus = 1, ac_bus = 2, train = 3,personal_car=4,rental_car=5), 
    avail         = list(bus=bus_avail, ac_bus=ac_bus_avail, train=train_avail,personal_car = personal_car_avail,rental_car= rental_car_avail), 
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