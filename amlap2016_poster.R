# At AMLAP2016 I present a poster version of this study. This script creates
# the figures from the paper adapted to the poster.
# It mostly replicates the code from "analysis_figures_results.R", but I prefer
# to keep the two scripts apart and incur the cost of code duplication.


# Reproduce the figures from the results sections of the paper.

library(ggplot2)
library(arm)
library(plyr)
library(tidyr)

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
## Visualization Experiments 1-3 (functions)
###############################################################

# For each experiment, we want a figure that shows the effect on similarity 
# judgements of each of the three critical event components by language.
# To generate the data needed for this figure we write a couple of functions
# that will be nested (one calls the other). The necessary data is obtained 
# by simulating model coefficients from the fitted model using arm::sim.
# (See Gelman & Hill, 2007 for examples of how to use the sim function.)

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

### Set nb.sims back to 10000!!!

# 2) This function calls coefCI to compute coefficients for each component
modelCI <- function(fm, nb.sims = 5000, components = c("P", "MC", "MO")) {
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
exp123_theme <- theme(text = element_text(size = 24),
                      axis.title.y = element_text(vjust = 1),
                      plot.title = element_text(hjust = 0, vjust = 1),
                      legend.position = c(.85, .75),
                      legend.title = element_text(size = 20),
                      legend.text = element_text(size = 18),
                      legend.key.size = unit(1, "cm"))
y_limits <- ylim(c(-.025, .275))

signif_line_size <- 1
signif_label_size <- 18

mywidth <- 22
myheight <- 12
mydpi <- 800


###############################################################
## Visualization Experiments 1-3 (plots)
###############################################################

# First obtain data frames for each experiment; set seed so plots look as in
# article
set.seed(6949)
fm_exp1_CI <- modelCI(fm_exp1)  # might take some time for large nb.sims
set.seed(6139)
fm_exp2_CI <- modelCI(fm_exp2)
set.seed(8049)
fm_exp3_CI <- modelCI(fm_exp3)


## Experiment 1

# Basic plot
exp1_plot <- ggplot(data = fm_exp1_CI, 
                    aes(x = Language, y = median, fill = Language, 
                        ymax = upper95, ymin = lower95)) +
  geom_bar(stat = "identity") +
  geom_errorbar(width = .2) +
  facet_wrap(~ Component) +
  xlab("") + ylab("Increase in similarity") +
  theme_bw() +
  exp123_theme +
  y_limits
exp1_plot

# prepare significance labels
coords <- data.frame(x = c(rep(NA, 4), 1, 2),
                     y = c(rep(NA, 4), .10, .10),
                     Component = fm_exp1_CI$Component)
mytext <- data.frame(x = 1.5, y = 0.105, text = c("","","*"),
                     Component = levels(fm_exp1_CI$Component))

# add significance labels and title
exp1_plot_complete <- exp1_plot +
  ggtitle("Exp 1: Linguistic encoding") +
  geom_path(data = coords, aes(x, y, ymin=NULL, ymax=NULL, fill=NULL), size = signif_line_size) +
  geom_text(data = mytext, aes(x, y, label=text, ymin=NULL, ymax=NULL, fill=NULL), size = signif_label_size)
exp1_plot_complete

ggsave("myfigures/FIG_amlap_exp1.png", units = "cm", dpi = mydpi,
       width = mywidth, height = myheight)


## Experiment 2

# Simply replace data frame, see syntax in
# http://docs.ggplot2.org/current/gg-add.html
# Plot
exp2_plot <- exp1_plot %+% fm_exp2_CI
exp2_plot

# add significance labels
coords <- data.frame(x = c(rep(NA, 4), 1, 2),
                     y = c(rep(NA, 4), .11, .11),
                     Component = fm_exp1_CI$Component)
mytext <- data.frame(x = 1.5, y = 0.115, text = c("","","*"),
                     Component = levels(fm_exp1_CI$Component))

# add to plot
exp2_plot_complete <- exp2_plot + 
  ggtitle("Exp 2: Free encoding") +
  geom_path(data = coords, aes(x, y, ymin=NULL, ymax=NULL, fill=NULL), size = signif_line_size) +
  geom_text(data = mytext, aes(x, y, label=text, ymin=NULL, ymax=NULL, fill=NULL), size = signif_label_size)
exp2_plot_complete

ggsave("myfigures/FIG_amlap_exp2.png", units = "cm", dpi = mydpi,
       width = mywidth, height = myheight)


## Experiment 3

# Simply replace data frame, see syntax in
# http://docs.ggplot2.org/current/gg-add.html
# Plot
exp3_plot <- exp1_plot %+% fm_exp3_CI
exp3_plot
# add title
exp3_plot_complete <- exp3_plot +
  ggtitle("Exp 3: Verbal interference")
exp3_plot_complete

ggsave("myfigures/FIG_amlap_exp3.png", units = "cm", dpi = mydpi,
       width = mywidth, height = myheight)


# clean up workspace
rm(coords, fm_exp1_CI, fm_exp2_CI, fm_exp3_CI, mytext,
   exp1_plot, exp1_plot_complete, exp2_plot, exp2_plot_complete,
   exp3_plot, exp3_plot_complete, multiplot, coefCI, modelCI,
   exp123_theme, y_limits)


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
  # Use the weights from forward coding for Encoding factor, see
  # contrasts(fm_compound@frame$Encoding)  
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
modelCI_comp <- function(fm, nb.sims = 5000, 
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
set.seed(1807)
fm_compound_CI <- modelCI_comp(fm_compound)

# specify some ggplot variables
exp_comp_theme <- theme(
  text = element_text(size = 24),
  axis.text.x = element_text(angle = 45, hjust = 1),
  legend.position = "top")
y_limits <- ylim(c(-.025, .275))

# specify some global plotting variables used for figures in all experiments
exp123_theme <- theme(text = element_text(size = 24),
                      axis.title.y = element_text(vjust = 1),
                      plot.title = element_text(hjust = 0, vjust = 1),
                      legend.position = c(.85, .75),
                      legend.title = element_text(size = 20),
                      legend.text = element_text(size = 18),
                      legend.key.size = unit(1, "cm"))



# Now plot it -- only two variables that show effects (Path and Direction)
exp_comp_plot <- 
  ggplot(fm_compound_CI[fm_compound_CI$Component %in% c("same Path", "same Direction"),],
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
exp_comp_plot

# significance labels
coords <- data.frame(x = c(1, 2),
                     y = c(.25, .25, .2, .2),
                     Component = rep(c("same Path", "same Direction"), each = 2))
mytext <- data.frame(x = 1.5,
                     y = c(.255, .205),
                     text = c("**", "***"),
                     Component = c("same Path", "same Direction"))
# add to plot
exp_comp_plot_complete <- exp_comp_plot + 
  geom_path(data = coords, aes(x, y, ymin=NULL, ymax=NULL, fill=NULL),
            size = signif_line_size) +
  geom_text(data = mytext, aes(x, y, label=text, ymin=NULL, ymax=NULL, fill=NULL),
            size = 12)
exp_comp_plot_complete


# png version AMLAP (ggsave() call is equivalent to the png() call used above)
ggsave("myfigures/FIG_amlap_compound.png", width = mywidth, height = myheight, units = "cm", dpi = 800)


# clean up
rm(coords, fm_compound_CI, mytext, exp_comp_plot, exp_comp_plot_complete,
   exp_comp_theme, y_limits, coefCI_comp, modelCI_comp)

