pacman::p_load(lme4,optimx,dfoptim,dplyr)
source('/vol/milkunB/jbosmans/SolarWindPotentials_Global/allFit.R')

# model with all predictors, all scaled, created in Script_glmer.R: 
load('glmer_model_all.rda')
#summary(model_all_scaled)

print('Apply allFit to check whether all optimizers converge to practically equivalent values')
allFit(show.meth.tab=TRUE)  # show available methods
model_all <- allFit(model_all)
ss        <- summary(model_all)

ss$which.OK            ## logical vector: which optimizers worked?
					   ## the other components only contain values for the optimizers that worked
ss$llik                ## vector of log-likelihoods
ss$fixef               ## table of fixed effects
ss$sdcor               ## table of random effect SDs and correlations
ss$theta               ## table of random effects parameters, Cholesky scale

print('Conclusion: different optimizers yield the same results. So ignore warnings.')
