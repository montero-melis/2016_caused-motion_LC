## Script to fit the models


# Experiment 1
fm_exp1 <- lmer(
  formula = as.formula(formula_exp_full_re),
  data = e1c,
  control = lmerControl(optCtrl = list(maxfun = 100000)),
  verbose=2)
summary(fm_exp1)




# The many mixed models fitted in the script take a very long computation time.
# Instead of running them, you can load the stored data (.Rdata format), as is
# done in the script "analysis_figures_results.R"


# library("plyr")
# library("reshape2")
# library("tidyr")
# library("ggplot2")
# library("lme4")
# library("arm")
# # library("GGally")
# library("knitr")
# # library("lattice")


###############################################################
## Import (and process) data
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

# # participant info
# participants <- read.csv("s_data/participants_exp123.csv", encoding = "UTF-8")


# ###############################################################
# ## Convenience functions
# ###############################################################
# 
# # compute standard error (takes care of vectors that contain NA's)
# se <- function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))



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


## Model formulae for each of Exp 1 through 3

# model without any 2nd order interactions between semantic components
formula_exp_nointer <- 
  "Sim ~ 1 + (P + MC + MO + Di + Gr + Ob) * Language +
(1 + P + MC + MO + Di + Gr + Ob | Subject) + (1 | Item)"

# simple model (control variables only as fixed effects)
formula_exp_simple_re <- 
  "Sim ~ 1 + (P + MC + MO + Di) ^ 2 * Language + Gr * Language + Ob * Language + 
(1 + P + MC + MO + Di | Subject) + (1 | Item)"

# full model (control variables as fixed effects and random effects)
formula_exp_full_re <- 
  "Sim ~ 1 + (P + MC + MO + Di) ^ 2 * Language + Gr * Language + Ob * Language + 
(1 + (P + MC + MO + Di) ^ 2 + Gr + Ob | Subject) + (1 | Item)"

formula_exp_nointer <- gsub("\n", "", formula_exp_nointer)
formula_exp_simple_re <- gsub("\n", "", formula_exp_simple_re)
formula_exp_full_re <- gsub("\n", "", formula_exp_full_re)

# show formulae
formula_exp_nointer
formula_exp_simple_re
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

## fit models

# center data
e1c <- center_my_data(e1)
head(e1)
head(e1c)


# no higher interactions model (REML=TRUE, default)
Sys.time()
ptm <- proc.time()

fm_e1_nointer <- lmer(
  formula = as.formula(formula_exp_nointer),
  data = e1c, verbose=2,
  control = lmerControl(optCtrl = list(maxfun = 100000)))

t.fm_e1_nointer <- proc.time() - ptm
print(t.fm_e1_nointer)
Sys.time()

summary(fm_e1_nointer)


# simple model (in terms of random effects)
Sys.time()
ptm <- proc.time()

fm_e1_simple_ML <- lmer(
  formula = as.formula(formula_exp_simple_re),
  data = e1c, verbose=2, REML=FALSE, 
  control = lmerControl(optCtrl = list(maxfun = 100000)))

t.fm_e1_simple_ML <- proc.time() - ptm
print(t.fm_e1_simple_ML)
Sys.time()

summary(fm_e1_simple_ML)

# simple model (in terms of random effects)
# REML=TRUE
Sys.time()
ptm <- proc.time()

fm_e1_simple <- lmer(
  formula = as.formula(formula_exp_simple_re),
  data = e1c, verbose=2, REML=TRUE, 
  control = lmerControl(optCtrl = list(maxfun = 100000)))

t.fm_e1_simple <- proc.time() - ptm
print(t.fm_e1_simple)
Sys.time()

summary(fm_e1_simple)


# full model (in terms of random effects)
Sys.time()
ptm <- proc.time()

fm_e1_full_ML <- lmer(
  formula = as.formula(formula_exp_full_re),
  data = e1c, verbose=2, REML=FALSE, 
  control = lmerControl(optCtrl = list(maxfun = 100000)))

t.fm_e1_full_ML <- proc.time() - ptm
print(t.fm_e1_full_ML)
Sys.time()

summary(fm_e1_full_ML)


# full model (in terms of random effects)
# REML=TRUE
Sys.time()
ptm <- proc.time()

fm_e1_full <- lmer(
  formula = as.formula(formula_exp_full_re),
  data = e1c, verbose=2, REML=TRUE, 
  control = lmerControl(optCtrl = list(maxfun = 100000)))

t.fm_e1_full <- proc.time() - ptm
print(t.fm_e1_full)
Sys.time()

summary(fm_e1_full)



###############################################################
## Experiment 1: by-speaker correlations
###############################################################


## categorization data

# load fitted model for Exp1 (linguistic encoding condition),
# i.e. full model fitted in previous section (can be loaded as R image from
# "models/final_models/LC_analysis.RData")
fm_ling <- fm_e1_full

summary(fm_ling)



# Extract by subject coefficients
# For how to extract by-subject coefficients (adjusted with random effects):
# http://stats.stackexchange.com/questions/122009/extracting-slopes-for-cases-from-a-mixed-effects-model-lme4
bs_coef <- coef(fm_ling)$Subject
# Add column with subject ID
bs_coef$Subject <- row.names(bs_coef)
head(bs_coef)

# remove all interaction terms and other predictors not needed:
bs_coef <- bs_coef[, c(27, 2:5, 7, 8)]
head(bs_coef)

## participant data

# import participant data
ppts <- read.csv("../data_shared/participant_data_hopi_140708.csv")
lookup <- ppts[, c("ID", "Group")]
lookup <- rename(lookup, c("ID" = "Subject", "Group" = "Language"))
lookup$Language <- revalue(lookup$Language, c("SpAD" = "Spanish",
                                              "SwAD" = "Swedish"))
head(lookup)

bs_coef <- join(bs_coef, lookup)
rm(ppts, lookup)

head(bs_coef)
# to long format
bs_coef_l <- gather(bs_coef, Component, Similarity, P:Ob)
# rename Components
bs_coef_l$Component <- revalue(bs_coef_l$Component,
                               c("P" = "Path", "MC" = "MannerCause",
                                 "MO" = "MannerObject", "Di" = "Direction",
                                 "Gr" = "Ground", "Ob" = "Object"))
head(bs_coef_l)

## linguistic data

# we already import the coded linguistic data in wide format for the norming  
# study above (df: lingw)
# we don't need all the columns
lingw <- lingw[, c(2:3, 10:15)]
# convert to long format analogous to bs_coef_l
lingl <- gather(lingw, Component, Descriptions, Path:Object)
# turn value column into binary (either component mentioned or not in given
# description)
lingl$Descriptions <- ifelse(lingl$Descriptions == 0, 0, 1)
lingl <- rename(lingl, c("subject" = "Subject"))
head(lingl)

# by-subject averages
ling_bs <- ddply(lingl, .(Subject, Component), summarise,
                 Description = sum(Descriptions)/length(Descriptions))
head(ling_bs)

# now join
bs <- join(bs_coef_l, ling_bs)
# reorder factor levels
bs$Component <- factor(bs$Component, levels = c("Path", "MannerCause", 
                                                "MannerObject", "Direction",
                                                "Ground", "Object"))
head(bs)

rm(lingl, lingw, ling_bs, bs_coef, bs_coef_l)

## plot correlations

# focus on critical components only
ggplot(data = bs[bs$Component %in% c("Path", "MannerCause", "MannerObject"), ],
       aes(x = Description, y = Similarity, colour = Language, 
           shape = Language, linetype = Language)) +
  geom_point() + geom_smooth(method = "lm") +
  facet_wrap(~ Component) +
  ylim(-.1, .5) + theme_bw() +
  ylab("Increase in similarity") +
  xlab("Proportion of descriptions") +
  theme(legend.position=c(.9, .8),
        text = element_text(size=12),
        axis.title.x = element_text(vjust=0),
        axis.title.y = element_text(vjust=1)) 

ggsave("myfigures/LC_ling-similarity_correl.png", height = 3.5)

## Pearson correlations
for(comp in c("Path", "MannerCause", "MannerObject")) {
  print(paste("Component:", comp))
  for(lang in c("Spanish", "Swedish")) {
    mysubset <- bs$Component == comp & bs$Language == lang
    d <- bs[mysubset, ]
    print(paste("Language:", lang))
    print(with(d, cor.test(Similarity, Description)))
    
  }
}

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


## fit models

# center data
e2c <- center_my_data(e2)
head(e2)
head(e2c)


# no higher interactions model (REML=TRUE, default)
Sys.time()
ptm <- proc.time()

fm_e2_nointer <- lmer(
  formula = as.formula(formula_exp_nointer),
  data = e2c, verbose=2,
  control = lmerControl(optCtrl = list(maxfun = 100000)))

t.fm_e2_nointer <- proc.time() - ptm
print(t.fm_e2_nointer)
Sys.time()

summary(fm_e2_nointer)


# simple model (in terms of random effects)
Sys.time()
ptm <- proc.time()

fm_e2_simple <- lmer(
  formula = as.formula(formula_exp_simple_re),
  data = e2c, verbose=2, 
  control = lmerControl(optCtrl = list(maxfun = 100000)))

t.fm_e2_simple <- proc.time() - ptm
print(t.fm_e2_simple)
Sys.time()

summary(fm_e2_simple)


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


## fit models

# center data
e3c <- center_my_data(e3)
head(e3)
head(e3c)


# no higher interactions model (REML=TRUE, default)
Sys.time()
ptm <- proc.time()

fm_e3_nointer <- lmer(
  formula = as.formula(formula_exp_nointer),
  data = e3c, verbose=2,
  control = lmerControl(optCtrl = list(maxfun = 100000)))

t.fm_e3_nointer <- proc.time() - ptm
print(t.fm_e3_nointer)
Sys.time()

summary(fm_e3_nointer)


# simple model (in terms of random effects)
Sys.time()
ptm <- proc.time()

fm_e3_simple <- lmer(
  formula = as.formula(formula_exp_simple_re),
  data = e3c, verbose=2, 
  control = lmerControl(optCtrl = list(maxfun = 100000)))

t.fm_e3_simple <- proc.time() - ptm
print(t.fm_e3_simple)
Sys.time()

summary(fm_e3_simple)


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
## Visualization Experiments 1-3 (functions)
###############################################################

# For each experiment, we want a figure that shows the effect on similarity 
# judgements of each of the three critical event components by language.
# To generate the data needed for this figure we write a couple of functions
# that will be nested (one calls the other). The necessary data is obtained 
# by simulating model coefficients from the fitted model using arm::sim.
# (As I did for example for the Language Learning paper, see the gitrepos 
# "caused-motion_l2")

# 1) Function to compute 95% confidence intervals (CI) for a semantic component
# by language, based on simulations from the model's fixef parameters.
# The result is put into a dataframe.
coefCI <- function(fm_sim, var = NULL, spa_we = NULL, swe_we = NULL,
                   pr = c(.025, .5, .975)) {
  # fm_sim is an object created by the arm::sim function
  # var specifies the semantic component for which effects are to be estimated
  # spa_we & swe_we specify the weights of each language when centering the
  # Language variable (needs to be obtained from data frame used to fit model)
  # pr specifies the probability quantiles to be obtained, by default 95% CI
  
  mysim <- fixef(fm_sim)
  main <- mysim[, var]
  interaction <- mysim[, paste(var, "Language", sep = ":")]
  spa <- main + spa_we * interaction
  swe <- main + swe_we * interaction
  myquant <- data.frame(rbind(quantile(spa, probs = pr),
                              quantile(swe, probs = pr)))
  names(myquant) <- c("lower95", "median", "upper95")
  myquant$Component <- var
  myquant$Language <- c("Spanish", "Swedish")
  myquant
}

# 2) This function calls coefCI to compute coefficients for each component
modelCI <- function(fm, nb.sims = 2000, components = c("P", "MC", "MO")) {
  # fm: the model for which 95% CI have to be estimated
  # nb.sims: number of simulations used in the sim function (>= 2000)
  # components: the event components for which to compute CI
  
  fm_sim <- sim(fm, n.sims = nb.sims)
  # extract language weights after centering the Language variable
  lang_weights <- sort(unique(fm@frame$Language))
  spa_wei <- lang_weights[1]  # the negative value in our coding scheme
  swe_wei <- lang_weights[2]  # the positive value
  out <- data.frame()
  for (component in components) {
    d <- coefCI(fm_sim, var = component, spa_we = spa_wei, swe_we = swe_wei)
    out <- rbind(out, d)
  }
  # order Component factor levels as entered in function call
  out$Component <- factor(out$Component, levels = components)
  # and give less cryptic names to Component levels
  out$Component <- mapvalues(out$Component, from = levels(out$Component),
                             to = c("same Path", "same MannerCause",
                                    "same MannerObject"))
  out
}

# specify some global plotting variables used for figures in all experiments
exp123_theme <- theme(text = element_text(size=12),
                      axis.title.y=element_text(vjust=1),
                      plot.title = element_text(hjust = 0, vjust=1),
                      legend.position = c(.85, .75))
y_limits <- ylim(c(-.025, .275))



###############################################################
## Visualization Experiments 1-3 (plots)
###############################################################

## Experiment 1

# Obtain data frame
fm_exp1_CI <- modelCI(fm_e1_full)

# Now plot it
exp1_plot <- ggplot(fm_exp1_CI, aes(x = Language, y = median, fill = Language,
                                    ymax = upper95, ymin = lower95)) +
  geom_bar(stat = "identity") +
  geom_errorbar(width = .2) +
  facet_wrap(~ Component) +
  xlab("") + ylab("Increase in similarity") +
  theme_bw() +
  exp123_theme +
  y_limits +
  ggtitle("A. Linguistic encoding (Exp. 1)")
print(exp1_plot)

# add significance labels
coords <- data.frame(x = c(rep(NA, 4), 1, 2),
                     y = c(rep(NA, 4), .10, .10),
                     Component = fm_exp1_CI$Component)
mytext <- data.frame(x = 1.5, y = 0.105, text = c("","","*"),
                     Component = levels(fm_exp1_CI$Component))
# add to plot
exp1_plot <- exp1_plot + 
  geom_path(data = coords, aes(x, y, ymin=NULL, ymax=NULL, fill=NULL)) +
  geom_text(data = mytext, aes(x, y, label=text, ymin=NULL, ymax=NULL, fill=NULL))

print(exp1_plot)

ggsave("myfigures/LC_exp1.png")

# plot used for talks (Barcelona SPB and Biling Centre)
exp1_plot_talk <- ggplot(
  fm_exp1_CI, aes(x = Language, y = median, fill = Language, ymax = upper95,
                  ymin = lower95)) +
  geom_bar(stat = "identity") +
  geom_errorbar(width = .2) +
  facet_wrap(~ Component) +
  xlab("") + ylab("Increase in similarity") +
  gg_talktheme +
  y_limits
print(exp1_plot_talk)

# add significance labels to plot
exp1_plot_talk <- exp1_plot_talk + 
  geom_path(data = coords, aes(x, y, ymin=NULL, ymax=NULL, fill=NULL), size = 1) +
  geom_text(data = mytext, aes(x, y, label=text, ymin=NULL, ymax=NULL, fill=NULL),
            size = 12)
print(exp1_plot_talk)

ggsave("myfigures/talk_exp1.png")


## Experiment 2

# Obtain data frame
fm_exp2_CI <- modelCI(fm_e2_full)

# Now plot it
exp2_plot <- ggplot(fm_exp2_CI, aes(x = Language, y = median, fill = Language,
                                    ymax = upper95, ymin = lower95)) +
  geom_bar(stat = "identity") +
  geom_errorbar(width = .2) +
  facet_wrap(~ Component) +
  xlab("") + ylab("Increase in similarity") +
  theme_bw() +
  exp123_theme +
  y_limits +
  ggtitle("B. Free encoding (Exp. 2)")
print(exp2_plot)

# add significance labels
coords <- data.frame(x = c(rep(NA, 4), 1, 2),
                     y = c(rep(NA, 4), .11, .11),
                     Component = fm_exp1_CI$Component)
mytext <- data.frame(x = 1.5, y = 0.115, text = c("","","*"),
                     Component = levels(fm_exp1_CI$Component))
# add to plot
exp2_plot <- exp2_plot + 
  geom_path(data = coords, aes(x, y, ymin=NULL, ymax=NULL, fill=NULL)) +
  geom_text(data = mytext, aes(x, y, label=text, ymin=NULL, ymax=NULL, fill=NULL))

print(exp2_plot)

ggsave("myfigures/LC_exp2.png")


# plot used for talks (Barcelona SPB and Biling Centre)
exp2_plot_talk <- ggplot(
  fm_exp2_CI, aes(x = Language, y = median, fill = Language, ymax = upper95,
                  ymin = lower95)) +
  geom_bar(stat = "identity") +
  geom_errorbar(width = .2) +
  facet_wrap(~ Component) +
  xlab("") + ylab("Increase in similarity") +
  gg_talktheme +
  y_limits
print(exp2_plot_talk)

# add significance labels to plot
exp2_plot_talk <- exp2_plot_talk + 
  geom_path(data = coords, aes(x, y, ymin=NULL, ymax=NULL, fill=NULL), size = 1) +
  geom_text(data = mytext, aes(x, y, label=text, ymin=NULL, ymax=NULL, fill=NULL),
            size = 12)
print(exp2_plot_talk)

ggsave("myfigures/talk_exp2.png")



## Experiment 3

# Obtain data frame
fm_exp3_CI <- modelCI(fm_e3_full)

# Now plot it
exp3_plot <- ggplot(fm_exp3_CI, aes(x = Language, y = median, fill = Language,
                                    ymax = upper95, ymin = lower95)) +
  geom_bar(stat = "identity") +
  geom_errorbar(width = .2) +
  facet_wrap(~ Component) +
  xlab("") + ylab("Increase in similarity") +
  theme_bw() +
  exp123_theme +
  y_limits +
  ggtitle("C. Verbal interference (Exp. 3)")

print(exp3_plot)
ggsave("myfigures/LC_exp3.png")


# plot used for talks (Barcelona SPB and Biling Centre)
exp3_plot_talk <- ggplot(
  fm_exp3_CI, aes(x = Language, y = median, fill = Language, ymax = upper95,
                  ymin = lower95)) +
  geom_bar(stat = "identity") +
  geom_errorbar(width = .2) +
  facet_wrap(~ Component) +
  xlab("") + ylab("Increase in similarity") +
  gg_talktheme +
  y_limits
print(exp3_plot_talk)

ggsave("myfigures/talk_exp3.png")



###############################################################
## Multiplot Experiments 1-3
###############################################################

# multiplot function for ggplot retrieved at
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

png(filename = "myfigures/LC_exp123.png", units = "in", width = 7, height = 9,
    res = 400)
multiplot(exp1_plot, exp2_plot, exp3_plot, cols=1)
dev.off()



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
