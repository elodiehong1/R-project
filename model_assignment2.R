## set up package
library(heemod)
########################################################################################
## Question 2 sub-question 1
## Only defined treatment A
#######################################################################################
tm_drugA = define_transition(
  state_names = c("mild", "moderate", "severe", "dead"),
  0.85, 0.10, 0.04, 0.01,
  0,    0.85, 0.14, 0.01,
  0,    0,    0.6,  0.4,
  0,    0,    0,    1
)
tm_drugA

# setting up some initial cost variables
cost_drugA = 15000

state_mild= define_state(
  
  # cost of healthcare doesn't depend on strategy (drug)
  cost_health = 2000,     
  # cost of drug depends on strategy
  cost_drugs = dispatch_strategy(
    drugA = cost_drugA
  ), 
  
  cost_total = cost_health + cost_drugs,
  QALYs = 0.95  # QALYs is our choice of effect here. 
)
## 'effect' argument in run_model.

state_moderate = define_state(
  cost_health = 3400,
  cost_drugs = dispatch_strategy(
    drugA = cost_drugA 
  ),
  cost_total = cost_health + cost_drugs,
  QALYs = 0.8
)

state_severe = define_state(
  cost_health = 10000,
  cost_drugs = dispatch_strategy(
    drugA = cost_drugA
  ),
  cost_total = cost_health + cost_drugs,
  QALYs = 0.4
)

state_dead = define_state(
  cost_health = 0,
  cost_drugs = 0,
  cost_total = 0,
  QALYs = 0
)

strat_drugA = define_strategy(
  transition = tm_drugA,
  mild       = state_mild,        # the state definitions distinguish between strategies
  moderate = state_moderate,    # via `dispatch_strategy`
  severe = state_severe,
  dead = state_dead
)

res_drugsA <- run_model(
  drugA = strat_drugA,    # the name here is what's used in `dispatch_strategy`
  cycles = 200,            # How many cycles do we want to run for
  cost = cost_total,
  effect = QALYs,
  init = c(1000, 0, 0, 0)
)

## Find 'dead' counts so that we can go back and change our number of cycles
## in 'run_model'

countsA = get_counts(res_drugsA)
countsA[countsA$state_names == "dead",]

########################################################################################
## Question 2 sub-question 2
##Add treatment B, rr=0.6, discount=0.04
#######################################################################################
tm_drugA = define_transition(
  state_names = c("mild", "moderate", "severe", "dead"),
  0.85, 0.10, 0.04, 0.01,
  0,    0.85, 0.14, 0.01,
  0,    0,    0.6,  0.4,
  0,    0,    0,    1
)
tm_drugA

rr = 0.6  ## Our relative risk

tm_drugB = define_transition(
  state_names = c("mild", "moderate", "severe", "dead"),
  C, 0.10*rr, 0.04*rr,0.01*rr,
  0, C,       0.14*rr,0.01*rr,
  0, 0,       C      ,0.4*rr,
  0, 0,       0      ,1
)

#tm_drugB

# setting up some initial cost variables
cost_drugA = 15000
cost_drugB = 19500

state_mild= define_state(
  
  # cost of healthcare doesn't depend on strategy (drug)
  cost_health = discount(2000, 0.04),     
  # cost of drug depends on strategy
  cost_drugs = discount(dispatch_strategy(
    drugA = cost_drugA,
    drugB = cost_drugB),
  0.04),
  
  cost_total = cost_health + cost_drugs,
  QALYs = 0.95  # QALYs is our choice of effect here. 
)
## 'effect' argument in run_model.

state_moderate = define_state(
  cost_health = discount(3400, 0.04), 
  cost_drugs = discount(dispatch_strategy(
    drugA = cost_drugA,
    drugB = cost_drugB),
    0.04),
  cost_total = cost_health + cost_drugs,
  QALYs = 0.8
)

state_severe = define_state(
  cost_health = discount(10000, 0.04), 
  cost_drugs = discount(dispatch_strategy(
    drugA = cost_drugA,
    drugB = cost_drugB),
    0.04),
  cost_total = cost_health + cost_drugs,
  QALYs = 0.4
)

state_dead = define_state(
  cost_health = 0,
  cost_drugs = 0,
  cost_total = 0,
  QALYs = 0
)

strat_drugA = define_strategy(
  transition = tm_drugA,
  mild       = state_mild,        # the state definitions distinguish between strategies
  moderate = state_moderate,    # via `dispatch_strategy`
  severe = state_severe,
  dead = state_dead
)

strat_drugB = define_strategy(
  transition = tm_drugB,
  mild = state_mild,        # the state definitions distinguish between strategies
  moderate = state_moderate,    # via `dispatch_strategy`
  severe = state_severe,
  dead = state_dead
)

res_drugsAB <- run_model(
  drugA = strat_drugA,    # the name here is what's used in `dispatch_strategy`
  drugB = strat_drugB,
  cycles = 200,            # How many cycles do we want to run for
  cost = cost_total,
  effect = QALYs
)

summary(res_drugsAB)

########################################################################################
## Question 2 sub-question 3
##Add treatment B, rr=0.6 change to 0.9 when severe to dead, 
#discount=0.04
#######################################################################################
tm_drugA = define_transition(
  state_names = c("mild", "moderate", "severe", "dead"),
  0.85, 0.10, 0.04, 0.01,
  0,    0.85, 0.14, 0.01,
  0,    0,    0.6,  0.4,
  0,    0,    0,    1
)
tm_drugA

rr = 0.6  ## Our relative risk

tm_drugB = define_transition(
  state_names = c("mild", "moderate", "severe", "dead"),
  C, 0.10*rr, 0.04*rr,0.01*rr,
  0, C,       0.14*rr,0.01*rr,
  0, 0,       C      ,0.4*0.9,
  0, 0,       0      ,1
)

#tm_drugB

# setting up some initial cost variables
cost_drugA = 15000
cost_drugB = 19500

state_mild= define_state(
  
  # cost of healthcare doesn't depend on strategy (drug)
  cost_health = discount(2000, 0.04),     
  # cost of drug depends on strategy
  cost_drugs = discount(dispatch_strategy(
    drugA = cost_drugA,
    drugB = cost_drugB),
    0.04),
  
  cost_total = cost_health + cost_drugs,
  QALYs = 0.95  # QALYs is our choice of effect here. 
)
## 'effect' argument in run_model.

state_moderate = define_state(
  cost_health = discount(3400, 0.04), 
  cost_drugs = discount(dispatch_strategy(
    drugA = cost_drugA,
    drugB = cost_drugB),
    0.04),
  cost_total = cost_health + cost_drugs,
  QALYs = 0.8
)

state_severe = define_state(
  cost_health = discount(10000, 0.04), 
  cost_drugs = discount(dispatch_strategy(
    drugA = cost_drugA,
    drugB = cost_drugB),
    0.04),
  cost_total = cost_health + cost_drugs,
  QALYs = 0.4
)

state_dead = define_state(
  cost_health = 0,
  cost_drugs = 0,
  cost_total = 0,
  QALYs = 0
)

strat_drugA = define_strategy(
  transition = tm_drugA,
  mild       = state_mild,        # the state definitions distinguish between strategies
  moderate = state_moderate,    # via `dispatch_strategy`
  severe = state_severe,
  dead = state_dead
)

strat_drugB = define_strategy(
  transition = tm_drugB,
  mild = state_mild,        # the state definitions distinguish between strategies
  moderate = state_moderate,    # via `dispatch_strategy`
  severe = state_severe,
  dead = state_dead
)

res_drugsAB <- run_model(
  drugA = strat_drugA,    # the name here is what's used in `dispatch_strategy`
  drugB = strat_drugB,
  cycles = 200,            # How many cycles do we want to run for
  cost = cost_total,
  effect = QALYs
)

summary(res_drugsAB)