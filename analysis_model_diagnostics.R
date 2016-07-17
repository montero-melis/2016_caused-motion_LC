## Model diagnostics and summaries reported in the paper

library(lme4)
library(arm)

###############################################################
## Load the fitted models
###############################################################

# The models can be fitted from scratch by sourcing the following script:
# source("analysis_fit_models.R")
# However, this takes a long time (probably several days for all 4 models)

# For convenience the models have been saved as R-Data files. 
# Load these instead:
load("fitted_models_fm_exp1.RData")
load("fitted_models_fm_exp2.RData")
load("fitted_models_fm_exp3.RData")
load("fitted_models_fm_compound.RData")


###############################################################
## Convenience functions to obtain significance values for model coefficients
###############################################################

# Function to obtain significance values for model coefficients
# based on t-statistic (at 5 different significance levels):
mysignif <- function(fm, mymethod = "Wald", mynsim = 10000){
  # p < .1?
  v <- c()
  for (mylevel in c(.9, .95, .99, .999)) {
    ci <- confint.merMod(fm, parm = "beta_", method = mymethod, level = mylevel,
                         nsim = mynsim)
    # now check if 0 is contained in the interval to test for significance
    # (i.e. if next line evaluates to TRUE, the coef is significantly different
    # from 0)
    sig <- apply(ci, 1, function(x) findInterval(0, x)) != 1
    # into vector
    v <- c(v, sig)
  }
  m <- matrix(v, nrow = length(fixef(fm)))
  # col1: signif at .10, col2 signif at .05, etc
  x <- apply(m, 1, sum)
  sigmarks <- ifelse(
    x == 0, "", ifelse(
      x == 1, ".", ifelse(
        x == 2, "*", ifelse(
          x == 3, "**", ifelse(
            x == 4, "***", "fish")))))
  out <- data.frame(predictor = names(fixef(fm)), significance = sigmarks)
  out
}


## Model diagnostics:

# Retrieved from
# https://github.com/aufrank/R-hacks/blob/master/mer-utils.R
# through HLP blog entry
# https://hlplab.wordpress.com/2011/02/24/diagnosing-collinearity-in-lme4/

# Variance inflation factor
vif.mer <- function (fit) {
  ## adapted from rms::vif
  
  v <- vcov(fit)
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}

# Kappa's k
kappa.mer <- function (fit,
                       scale = TRUE, center = FALSE,
                       add.intercept = TRUE,
                       exact = FALSE) {
  X <- fit@pp$X
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  nrp <- sum(1 * (nam == "(Intercept)"))
  if (nrp > 0) {
    X <- X[, -(1:nrp), drop = FALSE]
    nam <- nam[-(1:nrp)]
  }
  
  if (add.intercept) {
    X <- cbind(rep(1), scale(X, scale = scale, center = center))
    kappa(X, exact = exact)
  } else {
    kappa(scale(X, scale = scale, center = scale), exact = exact)
  }
}


###############################################################
## Model summaries and diagnostics, Exp. 1-3
###############################################################

summary(fm_exp1)
summary(fm_exp2)
summary(fm_exp3)

# significance of fixef coefficients
mysignif(fm_exp1)
mysignif(fm_exp2)
mysignif(fm_exp3)

## Collinearity in the models:
kappa.mer(fm_exp1)
sort(vif.mer(fm_exp1))

kappa.mer(fm_exp2)
sort(vif.mer(fm_exp2))

kappa.mer(fm_exp3)
sort(vif.mer(fm_exp3))

# Create dataframes with model estimates; use write_to_disk argument to
# write them to disk as csv tables that can be imported into Excel/Word
model_csv <- function(fm_name = NULL, write_to_disk = FALSE) {
  fm <- get(fm_name)
  m <- summary(fm)$coefficients
  m <- round(m, 2)
  d <- data.frame(Predictor = rownames(m))
  d <- cbind(d, m)
  row.names(d) <- NULL
  d$Sig <- mysignif(fm)[, 2]
  if (write_to_disk) {
    write.csv(format(d, digits = 2), quote = FALSE,
              file = paste("fixed-effects_", fm_name, ".csv", sep = ""),
              row.names = FALSE, fileEncoding = "UTF-8")
  }
  d
}

model_csv("fm_exp1")  
model_csv("fm_exp2")
model_csv("fm_exp3")
# to write to file call
# model_csv("fm_exp1", write_to_disk = TRUE)  


###############################################################
## Model summaries and diagnostics Compound analysis
###############################################################

# The model has Ob&Gr as random effects, but not 2nd order interactions between
# event components as random effects.
summary(fm_compound)

# Obtain p-values based on t-statistic (at 4 different significance levels):
mysignif(fm_compound)

# Collinearity in the model:
kappa.mer(fm_compound)
sort(vif.mer(fm_compound))

# model output as csv file
model_csv("fm_compound")

