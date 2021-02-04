### Building fake data for chill freeze experiment
# Started 1 April 2019 - Cat
## Based off of Lizzie's testdata code for OSPREE


##################################################################
## This version has only simple linear model with 2-way interactions:
# dvr ~ tx + chill1 + chill2 + txchill1 + txchill2
# species varies by slopes and intercepts (aka random slopes and intercepts)
# non-centered data version
##################################################################

# Code designed with normal distribution for intercepts for a and sigma_y

nsp = 10 # number of species

ntot = 48 # numbers of individuals per species. 

#  with species  (note to self: This is not the best, better to draw from a distribution)
intermean <- 2.5 # mean for selecting intercept (duration of vegetative risk) across all species
intersd <- 3 # SD for selecting species intercepts
spint <- rnorm(nsp, intermean, intersd)  # different intercepts by species

# now start building ...
testdat <- vector()

# assumptions:
# (a) predictors are NOT centered
# (b) predictors are not correlated
# (c) 2-way interactions only

# and some important points ...
# (z) the below draws treatments from distribution in such a way that there is a lot more variation than we have


for(i in 1:nsp){ # loop over species. i = 1
  
  tx = rnorm(ntot, 1, 0.5) # for centered: force = rnorm(ntot, 0, 2)
  chill1 = rnorm(ntot, 1, 0.5) 
  chill2 = rnorm(ntot, 1, 0.5) 
  
  # set up effect sizes
  txcoef = -1 # steeper slope for false spring treatment
  chill1coef = 0.5 # less steep for forcing
  chill2coef = 1
  
  # SD for each treatment
  txcoef.sd = 1
  chill1coef.sd = 0.5 
  chill2coef.sd = 0.8
  
  # set interaction effects. 3 two-way interactions
  txchill1coef = -0.5
  txchill2coef = -1
  
  # SD for interaction effects. 3 two-way interactions
  txchill1coef.sd = 0.2
  txchill2coef.sd = 0.8
  
  # build model matrix 
  mm <- model.matrix(~(tx+chill1+chill2+tx:chill1+tx:chill2), data.frame(tx, chill1, chill2))
  
  # coefficients need to match the order of the colums in the model matrix (mm)
  # so here, that's intercept, chill, force, photo
  coeff <- c(spint[i], 
             rnorm(1, txcoef, txcoef.sd),
             rnorm(1, chill1coef, chill1coef.sd),
             rnorm(1, chill2coef, chill2coef.sd),
             rnorm(1, txchill1coef, txchill1coef.sd),
             rnorm(1, txchill2coef, txchill2coef.sd)
  )
  
  ht.rgr <- rnorm(n = ntot, mean = mm %*% coeff, sd = 0.1)
  
  testdatx <- data.frame(ht.rgr, species = i, 
                          tx, chill1, chill2)
  
  testdat <- rbind(testdat, testdatx)  
}


#write.csv(testdat, file="~/Documents/git/chillfreeze/analyses/output/fakedata_height.csv", row.names = FALSE)

