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

# 2) This function calls coefCI to compute coefficients for each component
modelCI <- function(fm, nb.sims = 10000, components = c("P", "MC", "MO")) {
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
exp123_theme <- theme(text = element_text(size = 12),
                      axis.title.y = element_text(vjust = 1),
                      plot.title = element_text(hjust = 0, vjust = 1),
                      legend.position = c(.85, .75),
                      legend.title = element_text(size = 10),
                      legend.text = element_text(size = 8),
                      legend.key.size = unit(0.25, "cm"))
y_limits <- ylim(c(-.025, .275))



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
  ggtitle("A. Linguistic encoding (Exp. 1)") +
  geom_path(data = coords, aes(x, y, ymin=NULL, ymax=NULL, fill=NULL)) +
  geom_text(data = mytext, aes(x, y, label=text, ymin=NULL, ymax=NULL, fill=NULL))
exp1_plot_complete


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
  ggtitle("B. Free encoding (Exp. 2)") +
  geom_path(data = coords, aes(x, y, ymin=NULL, ymax=NULL, fill=NULL)) +
  geom_text(data = mytext, aes(x, y, label=text, ymin=NULL, ymax=NULL, fill=NULL))
exp2_plot_complete


## Experiment 3

# Simply replace data frame, see syntax in
# http://docs.ggplot2.org/current/gg-add.html
# Plot
exp3_plot <- exp1_plot %+% fm_exp3_CI
exp3_plot
# add title
exp3_plot_complete <- exp3_plot +
  ggtitle("C. Verbal interference (Exp. 3)")
exp3_plot_complete


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

# multiplot
multiplot(exp1_plot_complete, exp2_plot_complete, exp3_plot_complete, cols=1)

# pdf version submitted to L&C
# pdf("../myfigures/LC_exp123.pdf", width = 5, height = 7)
multiplot(exp1_plot_complete, exp2_plot_complete, exp3_plot_complete, cols=1)
# dev.off()

# clean up workspace
rm(coords, fm_exp1_CI, fm_exp2_CI, fm_exp3_CI, mytext,
   exp1_plot, exp1_plot_complete, exp2_plot, exp2_plot_complete,
   exp3_plot, exp3_plot_complete, multiplot, coefCI, modelCI,
   exp123_theme, y_limits)
   

###############################################################
## Experiment 1: by-speaker correlations
###############################################################

## Plot and compute correlations between similarity (nonverbal) data
## and tedency to encode a given component in verbal descriptions

## Similarity data

# Extract by subject coefficients
# For how to extract by-subject coefficients (adjusted with random effects):
# http://stats.stackexchange.com/questions/122009/extracting-slopes-for-cases-from-a-mixed-effects-model-lme4
bs_coef <- coef(fm_exp1)$Subject
# Add column with subject ID
bs_coef$subject <- row.names(bs_coef)
head(bs_coef)
# remove all interaction terms and other predictors not needed:
bs_coef <- bs_coef[, c(27, 2:5, 7, 8)]
head(bs_coef)
# to long format
bs_coef_l <- gather(bs_coef, component, Similarity, P:Ob)
# rename components
bs_coef_l$component <- revalue(bs_coef_l$component,
                               c("P" = "Path", "MC" = "MannerCause",
                                 "MO" = "MannerObject", "Di" = "Direction",
                                 "Gr" = "Ground", "Ob" = "Object"))
head(bs_coef_l)

## linguistic data

# load from file
exp1_ling <- read.csv("data_exp1_ling.csv")
head(exp1_ling)

## now join the two
bs <- join(exp1_ling, bs_coef_l)
# reorder factor levels
bs$component <- factor(bs$component, levels = c("Path", "MannerCause", 
                                                "MannerObject", "Direction",
                                                "Ground", "Object"))
head(bs)


## plot correlations

# specify global plotting theme
exp1_correl_theme <- theme(text = element_text(size = 12),
                           legend.position = c(.85, .8),
                           legend.title = element_text(size = 10),
                           legend.text = element_text(size = 10),
                           axis.text.x = element_text(angle = 45, hjust = 1))
y_limits <- ylim(-.1, .5)


# focus on critical components only
exp1_correl_plot <- 
  ggplot(data = bs[bs$component %in% c("Path", "MannerCause", "MannerObject"), ],
         aes(x = Proportion, y = Similarity, colour = language, 
             shape = language, linetype = language)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ component) +
  y_limits +
  theme_bw() +
  ylab("Increase in similarity") +
  xlab("Proportion of descriptions") +
  exp1_correl_theme
exp1_correl_plot

# # pdf version submitted to L&C
# ggsave("../myfigures/LC_ling-similarity_correl.pdf", 
#        width = 5, height = 3.5)

## Pearson correlations
for(comp in c("Path", "MannerCause", "MannerObject")) {
  print(paste("component:", comp))
  for(lang in c("Spanish", "Swedish")) {
    mysubset <- bs$component == comp & bs$language == lang
    d <- bs[mysubset, ]
    print(paste("Language:", lang))
    print(with(d, cor.test(Similarity, Proportion)))
  }
}

rm(bs, bs_coef, bs_coef_l, d, exp1_ling, comp, exp1_correl_plot,
   exp1_correl_theme, lang, mysubset, y_limits)


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
modelCI_comp <- function(fm, nb.sims = 10000, 
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
  text = element_text(size = 12),
  axis.text.x = element_text(angle = 45, hjust = 1),
  legend.position = "top")
y_limits <- ylim(c(-.025, .275))

# Now plot it -- all six factors (including Di, Gr & Ob)
exp_comp_plot <- ggplot(fm_compound_CI,
                        aes(x = Encoding, y = median, ymax = upper95,
                            ymin = lower95, fill = Encoding)) +
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
coords <- data.frame(x = c(1, 2, rep(NA, 4), 1, 2, rep(NA, 4)),
                     y = c(.25, .25, rep(NA, 4), .2, .2, rep(NA, 4)),
                     Component = rep(levels(fm_compound_CI$Component), each = 2))
mytext <- data.frame(x = 1.5,
                     y = c(rep(.255,3), rep(.205, 3)),
                     text = c("**","","", "***", "", ""),
                     Component = levels(fm_compound_CI$Component))
# add to plot
exp_comp_plot_complete <- exp_comp_plot + 
  geom_path(data = coords, aes(x, y, ymin=NULL, ymax=NULL, fill=NULL)) +
  geom_text(data = mytext, aes(x, y, label=text, ymin=NULL, ymax=NULL, fill=NULL))
exp_comp_plot_complete

# # pdf version submitted to L&C
# ggsave("../myfigures/LC_compound.pdf", height = 4.5, width = 5)

# clean up
rm(coords, fm_compound_CI, mytext, exp_comp_plot, exp_comp_plot_complete,
   exp_comp_theme, y_limits, coefCI_comp, modelCI_comp)

