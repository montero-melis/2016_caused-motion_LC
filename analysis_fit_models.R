## Script to fit the models

# The many mixed models fitted in the script take a very long computation time.
# Instead of running them, you can load the stored data (.Rdata format), as is
# done in the script "analysis_figures_results.R"


library("plyr")
# library("reshape2")
# library("tidyr")
# library("ggplot2")
library("lme4")
# library("arm")
# # library("GGally")
# library("knitr")
# # library("lattice")


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

fm_e1_full <- lmer(
  formula = as.formula(formula_exp_full_re),
  data = e1c, verbose=2,
  control = lmerControl(optCtrl = list(maxfun = 100000)))

t.fm_e1_full <- proc.time() - ptm
print(t.fm_e1_full)
Sys.time()

summary(fm_e1_full)



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

fm_e2_full <- lmer(
  formula = as.formula(formula_exp_full_re),
  data = e2c, verbose=2,
  control = lmerControl(optCtrl = list(maxfun = 100000)))

t.fm_e2_full <- proc.time() - ptm
print(t.fm_e2_full)
Sys.time()

summary(fm_e2_full)



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

fm_e3_full <- lmer(
  formula = as.formula(formula_exp_full_re),
  data = e3c, verbose=2,
  control = lmerControl(optCtrl = list(maxfun = 100000)))

t.fm_e3_full <- proc.time() - ptm
print(t.fm_e3_full)
Sys.time()

summary(fm_e3_full)



###############################################################
## Model summaries and diagnostics, Exp. 1-3
###############################################################


# Obtain p-values based on t-statistic (at 5 different significance levels):

# convenience function to compute p-values at  different significance levels
mysignif <- function(fm, mymethod = "Wald", mynsim = 10000){
  # p < .1?
  v <- c()
  for (mylevel in c(.9, .95, .99, .999)) {
    ci <- confint.merMod(fm, method = mymethod, level = mylevel, nsim = mynsim)
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

mysignif(fm_e1_full)
mysignif(fm_e2_full)
mysignif(fm_e3_full)

## Collinearity in the model:

# load the necessary functions
source("mer-utils_fjaeger.R")
rm(colldiag.mer, maxcorr.mer)  # can't get these to work anyway

kappa.mer(fm_e1_full)
sort(vif.mer(fm_e1_full))

kappa.mer(fm_e2_full)
sort(vif.mer(fm_e2_full))

kappa.mer(fm_e3_full)
sort(vif.mer(fm_e3_full))

# create csv tables with model estimates to be imported in Excel/Word
model_csv <- function(fm_name = NULL) {
  fm <- get(fm_name)
  m <- summary(fm)$coefficients
  m <- round(m, 2)
  d <- data.frame(Predictor = rownames(m))
  d <- cbind(d, m)
  row.names(d) <- NULL
  d$Sig <- mysignif(fm)[, 2]
  print(d)
  write.csv(format(d, digits = 2), quote = FALSE,
            file = paste("models/final_models/", fm_name, ".csv", sep = ""),
            row.names = FALSE, fileEncoding = "UTF-8")
}

model_csv("fm_e1_full")
model_csv("fm_e2_full")
model_csv("fm_e3_full")



###############################################################
## Compound analysis
###############################################################

# Now forward coding for Encoding, so that the two comparisons are: 
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
# simple model (in terms of random effects); the full model failed to converge
# after 3.8 days fitting on my Dell laptop.
Sys.time()
ptm <- proc.time()

fm_e123_simple_ML <- lmer(
  formula = Sim ~ 1 + (P + MC + MO + Di) ^ 2 * Language * Encoding +
    Gr * Language * Encoding + Ob * Language * Encoding + 
    (1 + P + MC + MO + Di | Subject) + (1 | Item),
  data = e123c, verbose=2, REML = FALSE,
  control = lmerControl(optCtrl = list(maxfun = 100000)))

t.fm_e123_simple_ML <- proc.time() - ptm
print(t.fm_e123_simple_ML)
Sys.time()

summary(fm_e123_simple_ML)


# fit model
# medium model (in terms of random effects)
# Controls for random effects of Gr + Ob, but not 2-way interactions
Sys.time()
ptm <- proc.time()

fm_e123_medium_ML <- lmer(
  formula = Sim ~ 1 + (P + MC + MO + Di) ^ 2 * Language * Encoding +
    Gr * Language * Encoding + Ob * Language * Encoding + 
    (1 + P + MC + MO + Di + Gr + Ob | Subject) + (1 | Item),
  data = e123c, verbose=2, REML = FALSE,
  control = lmerControl(optCtrl = list(maxfun = 100000)))

t.fm_e123_medium_ML <- proc.time() - ptm
print(t.fm_e123_medium_ML)
Sys.time()

summary(fm_e123_medium_ML)


# fit model
# medium2 model (in terms of random effects). Controls for random effects of
# 2-way interactions, but not Gr + Ob.
Sys.time()
ptm <- proc.time()

fm_e123_medium2_ML <- lmer(
  formula = Sim ~ 1 + (P + MC + MO + Di) ^ 2 * Language * Encoding +
    Gr * Language * Encoding + Ob * Language * Encoding + 
    (1 + (P + MC + MO + Di) ^ 2 | Subject) + (1 | Item),
  data = e123c, verbose=2, REML = FALSE,
  control = lmerControl(optCtrl = list(maxfun = 100000)))

t.fm_e123_medium2_ML <- proc.time() - ptm
print(t.fm_e123_medium2_ML)
Sys.time()

summary(fm_e123_medium2_ML)


###############################################################
## Model summaries and diagnostics, Exp. 1-3
###############################################################

# model to be used (simpler name)
fm_comp <- fm_e123_medium_ML  # Ob&Gr as ranef, but not 2nd order interactions

# Obtain p-values based on t-statistic (at 4 different significance levels):
mysignif(fm_comp)

# Collinearity in the model:
kappa.mer(fm_comp)
sort(vif.mer(fm_comp))

# model output as csv file
model_csv("fm_comp")


###############################################################
## Visualization Compound analysis
###############################################################

# 1) function analogous to coefCI used in exp 1 through 3
coefCI_comp <- function(fm_sim, var = NULL, pr = c(.025, .5, .975)) {
  # fm_sim is an object created by the arm::sim function
  # var specifies the semantic component for which effects are to be estimated
  # pr specifies the probability quantiles to be obtained, by default 95% CI
  
  mysim <- fixef(fm_sim)
  main <- mysim[, var]
  if (! var %in% c("Ob", "Gr")) {
    inter_ling_free <- mysim[, paste(var, "Encodingling_vs_free", sep = ":")]
    inter_free_interf <- mysim[, paste(var, "Encodingfree_vs_interf", sep = ":")]
  } else {
    inter_ling_free <- mysim[, paste("Encodingling_vs_free", var, sep = ":")]
    inter_free_interf <- mysim[, paste("Encodingfree_vs_interf", var, sep = ":")]
  }
  # I use the weights from forward coding for Encoding factor, see
  # contrasts(fm_comp@frame$Encoding)  
  ling   <- main + inter_ling_free * 2 / 3 + inter_free_interf * 1 / 3
  free   <- main - inter_ling_free * 1 / 3 + inter_free_interf * 1 / 3
  interf <- main - inter_ling_free * 1 / 3 - inter_free_interf * 2 / 3
  # obtain quantiles from coefficient simulations, i.e. confidence intervals
  myquant <- data.frame(rbind(quantile(ling, probs = pr),
                              quantile(free, probs = pr),
                              quantile(interf, probs = pr)))
  names(myquant) <- c("lower95", "median", "upper95")
  myquant$Component <- var
  myquant$Encoding <- c("linguistic", "free", "interference")
  myquant
}


# 2) This function calls coefCI to compute coefficients for each component
modelCI_comp <- function(fm, nb.sims = 2000, 
                         components = c("P", "MC", "MO", "Di", "Gr", "Ob")) {
  # fm: the model for which 95% CI have to be estimated
  # nb.sims: number of simulations used in the sim function (>= 2000)
  # components: the event components for which to compute CI
  
  fm_sim <- sim(fm, n.sims = nb.sims)
  out <- data.frame()
  for (component in components) {
    d <- coefCI_comp(fm_sim, var = component)
    out <- rbind(out, d)
  }
  # order Component factor levels as entered in function call
  out$Component <- factor(out$Component, levels = components)
  # and give less cryptic names to Component levels
  out$Component <- mapvalues(out$Component, from = levels(out$Component),
                             to = c("same Path", "same MannerCause",
                                    "same MannerObject", "same Direction",
                                    "same Ground", "same Object"))
  out$Encoding <- factor(out$Encoding, levels = c("linguistic", "free",
                                                  "interference"))
  out
}


# obtain data frame
fm_comp_CI <- modelCI_comp(fm_comp)

# specify some ggplot variables
exp_comp_theme <- theme(
  text = element_text(size=12),
  axis.title.x = element_text(vjust=0),
  axis.title.y = element_text(vjust=1),
  legend.position = "top")
y_limits <- ylim(c(-.025, .275))


# Now plot it -- all six factors (including Di, Gr & Ob)
exp_comp_plot <- ggplot(fm_comp_CI, aes(x = Encoding, y = median, ymax = upper95,
                                        ymin = lower95, fill = Encoding)) +
  geom_bar(stat = "identity") +
  geom_errorbar(width = .2) +
  facet_wrap(~ Component) +
  xlab("Encoding condition") + ylab("Increase in similarity") +
  theme_bw() +
  exp_comp_theme +
  y_limits +
  scale_fill_brewer(palette = "Accent")

print(exp_comp_plot)

# add significance labels
coords <- data.frame(x = c(1, 2, rep(NA, 4), 1, 2, rep(NA, 4)),
                     y = c(.25, .25, rep(NA, 4), .2, .2, rep(NA, 4)),
                     Component = rep(levels(fm_comp_CI$Component), each = 2))
mytext <- data.frame(x = 1.5,
                     y = c(rep(.255,3), rep(.205, 3)),
                     text = c("**","","", "***", "", ""),
                     Component = levels(fm_comp_CI$Component))


exp_comp_plot <- exp_comp_plot + 
  geom_path(data = coords, aes(x, y, ymin=NULL, ymax=NULL, fill=NULL)) +
  geom_text(data = mytext, aes(x, y, label=text, ymin=NULL, ymax=NULL, fill=NULL))

print(exp_comp_plot)

ggsave("myfigures/LC_compound.png", height = 5.5)


# plot used for talks (Barcelona SPB and Biling Centre)
exp_comp_plot_talk <- ggplot(
  fm_comp_CI, aes(x = Encoding, y = median, ymax = upper95, ymin = lower95,
                  fill = Encoding)) +
  geom_bar(stat = "identity") +
  geom_errorbar(width = .2) +
  facet_wrap(~ Component) +
  xlab("Encoding condition") + ylab("Increase in similarity") +
  gg_talktheme + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  y_limits +
  scale_fill_brewer(palette = "Accent")

print(exp_comp_plot_talk)

# add significance labels to plot
exp_comp_plot_talk <- exp_comp_plot_talk + 
  geom_path(data = coords, aes(x, y, ymin=NULL, ymax=NULL, fill=NULL), size = .5) +
  geom_text(data = mytext, aes(x, y, label=text, ymin=NULL, ymax=NULL, fill=NULL),
            size = 8)
print(exp_comp_plot_talk)

ggsave("myfigures/talk_exp_comp.png", width = 7.5, height = 6)


# and simpler version, only significant differences
pathdir <- levels(fm_comp_CI$Component)[c(1, 4)]
exp_comp_plot_talk2 <- ggplot(
  fm_comp_CI[fm_comp_CI$Component %in% pathdir, ],
  aes(x = Encoding, y = median, ymax = upper95, ymin = lower95, 
      fill = Encoding)) +
  geom_bar(stat = "identity") +
  geom_errorbar(width = .2) +
  facet_wrap(~ Component) +
  xlab("Encoding condition") + ylab("Increase in similarity") +
  gg_talktheme + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  y_limits +
  scale_fill_brewer(palette = "Accent")

print(exp_comp_plot_talk2)

# add significance labels to plot
coords <- data.frame(x = rep(c(1, 2), 2),
                     y = rep(c(.25, .2), each = 2),
                     Component = rep(pathdir, each = 2))
mytext <- data.frame(x = 1.5,
                     y = c(.255, .205),
                     text = c("**", "***"),
                     Component = pathdir)

exp_comp_plot_talk2 <- exp_comp_plot_talk2 + 
  geom_path(data = coords, aes(x, y, ymin=NULL, ymax=NULL, fill=NULL), size = .5) +
  geom_text(data = mytext, aes(x, y, label=text, ymin=NULL, ymax=NULL, fill=NULL),
            size = 8)
print(exp_comp_plot_talk2)

ggsave("myfigures/talk_exp_comp_path-dir.png", width = 7.5, height = 6)



# Now plot it -- only three critical components
mycomps <- fm_comp_CI$Component %in% levels(fm_comp_CI$Component)[1:3]
exp_comp_plot_main <- ggplot(
  data = fm_comp_CI[mycomps, ],
  aes(x = Encoding, y = median, ymax = upper95, ymin = lower95, 
      fill = Encoding)) +
  geom_bar(stat = "identity") +
  geom_errorbar(width = .2) +
  facet_wrap(~ Component) +
  xlab("Encoding condition") + ylab("Increase in similarity") +
  theme_bw() +
  exp_comp_theme +
  y_limits +
  scale_fill_brewer(palette = "Accent")

print(exp_comp_plot_main)
ggsave("myfigures/LC_compound_P-MC-MO.png")

rm(mycomps)


###############################################################
## Trade-off between components
###############################################################

# How was reliance on the different components related to each other?
# Did some of the components compete within subjects, such that more reliance
# on one component was associated with less reliance on another?

# convenience function to extract coefficients by experiment and arrange
# into data frame.
extract_coef <- function(fm, encoding = NULL) {
  mycoef <- coef(fm)$Subject
  mycoef$Encoding <- encoding
  mycoef$Subject <- paste(row.names(mycoef),substring(encoding, 1, 2),
                          sep = "_")
  columns <- c("Subject", "Encoding", "P", "MC", "MO", "Di", "Gr", "Ob")
  out <- mycoef[, columns]
  out
}

subj_coefs <- rbind(
  extract_coef(fm_e1_full, encoding = "linguistic"),
  extract_coef(fm_e2_full, encoding = "free"),
  extract_coef(fm_e3_full, encoding = "interference"))

# Path, MC, MO
ggpairs(subj_coefs,
        columns = 3:5,
        lower=list(continuous="smooth"),
        colour = "Encoding")
# Path, MC, MO & Direction
ggpairs(subj_coefs,
        columns = 3:6,
        lower=list(continuous="smooth"),
        colour = "Encoding")
# Path, MC, MO & Direction
ggpairs(subj_coefs,
        columns = 3:8,
        lower=list(continuous="smooth"),
        colour = "Encoding")
