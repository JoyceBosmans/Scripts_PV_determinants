
# load packages
pacman::p_load(combinat)

# load dataframe
load(file = '../data/rda/df_PA_Eur_country_060622_proprocessed.Rda')

# get number of different variables and their indices (first two columns and last two columns in data frame are not predictive variables)
n_vars <- ncol(df_PA) - 4
vars <- seq(1 + 2,n_vars + 2)

# create a counter for variable combinations
counter <- 1

# empty data frame
combs <- data.frame(matrix(NA, nrow = 2^n_vars - 1, ncol = n_vars))

# loop over different set sizes (leave out trivial set with no variables
for (i in 1:(n_vars-1)){
    
    # get all possible combinations of variables
    temp <- combinat::combn(vars, i)
    
    for (j in 1:nrow(temp)){
        combs[counter:(counter+ncol(temp)-1), j] <- temp[j, ]
    }
    
    # increase counter
    counter <- counter + ncol(temp)
}

combs[counter,] <- vars

save(combs, file = '../data/rda/var_combs/combinations_Eur.Rda')
