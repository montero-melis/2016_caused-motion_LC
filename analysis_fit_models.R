## Script to fit the models

# The many mixed models fitted in the script take a very long computation time.
# Instead of running them, you can load the stored data (.Rdata format), as is
# done in the script "analysis_figures_results.R"

library("plyr")
library("lme4")


###############################################################
## Import data
###############################################################

# This data frame contains the similarity data for all three experiments.
# See "LC_process-data.R"
exp123 <- read.csv("data_experiments_similarity.csv", encoding = "UTF-8")
# change order of factor levels to mirror experiments 1--3
exp123$Encoding <- factor(exp123$Encoding, levels = c("linguistic", "free",
                                                      "interference"))

head(exp123)
str(exp123)
summary(exp123)

# create one separate data frame with the data for each experiment
e1 <- exp123[exp123$Encoding == "linguistic", ]
e2 <- exp123[exp123$Encoding == "free", ]
e3 <- exp123[exp123$Encoding == "interference", ]

# participant info
participants <- read.csv("data_participants.csv", encoding = "UTF-8")


###############################################################
## Functions for model fitting
###############################################################

# Instead of repeating code related to aspects of model fitting which are the
# same across experiments, create convenience functions and parameters that 
# can be reused. This ensures consistency of the analyses across experiments.

# F Jaegers function to center categorical predictors
# http://hlplab.wordpress.com/2009/04/27/centering-several-variables/
myCenter= function(x) {
  if (is.numeric(x)) { return(x - mean(x, na.rm=T)) }
  if (is.factor(x)) {
    x= as.numeric(x)
    return(x - mean(x, na.rm=T))
  }
  if (is.data.frame(x) || is.matrix(x)) {
    m= matrix(nrow=nrow(x), ncol=ncol(x))
    colnames(m)= paste("c", colnames(x), sep="")
    for (i in 1:ncol(x)) {
      m[,i]= myCenter(x[,i])
    }
    return(as.data.frame(m))
  }
}

## Function to center predictor variables
center_my_data <- function(df = NULL) {
  vars <- c("Language", "P", "MC", "MO", "Di", "Ob", "Gr")
  df[, vars] <- myCenter(df[, vars])
  df
}
## Interpretation of coefficients:
# Swedish +, Spanish -
# P, MC, MO, Di, Ob, Gr: same +, different -


## Model formula for each of Exp 1 through 3
# full model (control variables as fixed effects and random effects)
formula_exp_full_re <- 
  "Sim ~ 1 + (P + MC + MO + Di) ^ 2 * Language + Gr * Language + Ob * Language + 
(1 + (P + MC + MO + Di) ^ 2 + Gr + Ob | Subject) + (1 | Item)"
formula_exp_full_re <- gsub("\n", "", formula_exp_full_re)

# show formula
formula_exp_full_re


###############################################################
## Experiment 1: linguistic encoding
###############################################################

# participant info
with(participants[participants$encoding == "linguistic", ],
     table(language))
with(participants[participants$encoding == "linguistic", ],
     table(language, gender))
ddply(participants[participants$encoding == "linguistic", ],
      .(language), summarise, 
      Mage = mean(age), SDage = sd(age))

## fit full model ("full" in terms of random effects)

# center data
e1c <- center_my_data(e1)
head(e1)
head(e1c)

# fit full model
Sys.time()  # keep track of the time this takes
ptm <- proc.time()

fm_exp1 <- lmer(
  formula = as.formula(formula_exp_full_re),
  data = e1c, verbose=2,
  control = lmerControl(optCtrl = list(maxfun = 100000)))

t.fm_exp1 <- proc.time() - ptm
print(t.fm_exp1)
Sys.time()

summary(fm_exp1)


###############################################################
## Experiment 2: free encoding
###############################################################

# participant info
with(participants[participants$encoding == "free", ],
     table(language))
with(participants[participants$encoding == "free", ],
     table(language, gender))
ddply(participants[participants$encoding == "free", ],
      .(language), summarise, 
      Mage = mean(age), SDage = sd(age))


## fit model

# center data
e2c <- center_my_data(e2)
head(e2)
head(e2c)

# full model (in terms of random effects)
Sys.time()
ptm <- proc.time()

fm_exp2 <- lmer(
  formula = as.formula(formula_exp_full_re),
  data = e2c, verbose=2,
  control = lmerControl(optCtrl = list(maxfun = 100000)))

t.fm_exp2 <- proc.time() - ptm
print(t.fm_exp2)
Sys.time()

summary(fm_exp2)


###############################################################
## Experiment 3: verbal interference
###############################################################

# participant info
with(participants[participants$encoding == "interference", ],
     table(language))
with(participants[participants$encoding == "interference", ],
     table(language, gender))
ddply(participants[participants$encoding == "interference", ],
      .(language), summarise, 
      Mage = mean(age), SDage = sd(age))


## fit model

# center data
e3c <- center_my_data(e3)
head(e3)
head(e3c)

# full model (in terms of random effects)
Sys.time()
ptm <- proc.time()

fm_exp3 <- lmer(
  formula = as.formula(formula_exp_full_re),
  data = e3c, verbose=2,
  control = lmerControl(optCtrl = list(maxfun = 100000)))

t.fm_exp3 <- proc.time() - ptm
print(t.fm_exp3)
Sys.time()

summary(fm_exp3)


###############################################################
## Compound analysis
###############################################################

# Use forward coding for Encoding variable, so that the two comparisons are:
# linguistic vs free, free vs interference.
# (For reference see Serlin & Levin, 1985, in J. Educational Statistics)
e123c <- center_my_data(exp123)
# contrast matrix
cm <- matrix(c(1/3, 1/3, 1/3,
               1, -1, 0,
               0, 1, -1), 3, byrow=T)
myforward <- solve(cm)[, 2:3]
# Encoding condition
contrasts(e123c$Encoding) <- myforward
colnames(contrasts(e123c$Encoding)) <- c("ling_vs_free", "free_vs_interf")
contrasts(e123c$Encoding)
rm(cm, myforward)

# fit model
# Controls for random effects of Gr + Ob, but not 2-way interactions
# (more complex models failed to converge)
Sys.time()
ptm <- proc.time()

fm_compound <- lmer(
  formula = Sim ~ 1 + (P + MC + MO + Di) ^ 2 * Language * Encoding +
    Gr * Language * Encoding + Ob * Language * Encoding + 
    (1 + P + MC + MO + Di + Gr + Ob | Subject) + (1 | Item),
  data = e123c, verbose=2, REML = FALSE,
  control = lmerControl(optCtrl = list(maxfun = 100000)))

t.fm_compound <- proc.time() - ptm
print(t.fm_compound)
Sys.time()

summary(fm_compound)


###############################################################
## Save models to disk (as R-objects)
###############################################################

# Once saved they can be loaded from the other analysis scripts
save(fm_exp1, file = "fitted_models_fm_exp1.RData")
save(fm_exp2, file = "fitted_models_fm_exp2.RData")
save(fm_exp3, file = "fitted_models_fm_exp3.RData")
save(fm_compound, file = "fitted_models_fm_compound.RData")
