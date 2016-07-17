# Reproduce analyses for norming study

library(plyr)
library(reshape2)  # e.g. melt()
library(ggplot2)
library(knitr)  # e.g. kable() function
library(lme4)  # fit mixed models (version used: lme4_1.1-7)
library(reshape2)  # converting data from wide to long format and viceversa


###############################################################
## Convenience functions
###############################################################

# compute standard error (takes care of vectors that contain NA's)
se <- function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))


###############################################################
## Import norming data and column renaming
###############################################################

# import coded linguistic data in wide format
norms <- read.csv("data_norms.csv")
head(norms)
# Each row stands for a description of one item by a participant
# Columns Path, ..., Object are binary and indicate whether the given semantic
# information was expressed or not. Similarly, "Verb_P" indicates whether the
# main verb expressed Path information, ..., "OutsideV_MO" indicates whether
# Manner of Object was expressed outside of the main verb.

# rename columns for ease of reference
norms <- rename(norms, c("Path"="P", "MannerCause"="MC", "MannerObject"="MO",
                         "Direction" = "Di", "Ground" = "Gr", "Object" = "Ob"))
head(norms)

# number of subjects per language
table(unique(norms[, c("language", "subject")])$language)


###############################################################
## Proportion of descriptions mentioning each component
###############################################################

# wide to long format
norms_l <- melt(norms, id = c("language", "subject"),
                measure.vars = c("P", "MC", "MO", "Di", "Gr", "Ob"),
                variable.name = "ConceptComp")

# by-subject averages
norms_bs <- ddply(norms_l, .(language, subject, ConceptComp), summarise,
                  prop = sum(value) / length(value))

# plot (not included in the paper)
plot_norms <- ggplot(norms_bs, aes(x = language, y = prop, fill = language)) +
  stat_summary(fun.y = mean, geom = "bar", aes(width = .7)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_wrap(~ ConceptComp)
plot_norms

# by language: average and StandardError (reported in Supporting Information)
norms_bylang <- ddply(
  norms_bs, .(language, ConceptComp), summarise,
  MeanProp = round(mean(prop),2), SE = round(se(prop), 2))
kable(norms_bylang)
# Note that SE assumes normality in probability space, which is a wrong
# assumption. We use it here only for quick reference and because it's easily
# understood. The mixed models below handle this in the correct way.

# clean up
rm(norms_bs, norms_bylang, plot_norms, norms_l)


###############################################################
## Logistic mixed models to test for differences in mentioning components
###############################################################

## fit models  to test whether the likelihood of expressing each component
## differs between languages?

# effect coding for language variable
contrasts(norms$language) <- contr.treatment(2)
colnames(contrasts(norms$language)) <- "Swe_vs_Spa"
contrasts(norms$language)

# model formula
norms_form <- "~ 1 + language + (1 | subject) + (1 + language | videoname)"

# Path
fm_norms_P <- glmer(  # fails to converge
  formula = as.formula(paste("P", norms_form)),
  data = norms, family = binomial(link="logit")
)
# ... fails to converge, so simplify:
# Path (remove random effect interaction, it's the maximal that converges)
fm_norms_P <- glmer(
  P ~ 1 + language + (1 | subject) + (1 | videoname) + (0+language|videoname),
  data = norms, family = binomial(link="logit"))
summary(fm_norms_P)

# MannerCause
fm_norms_MC <- glmer(
  formula = as.formula(paste("MC", norms_form)),
  data = norms, family = binomial(link="logit")
)
summary(fm_norms_MC)

# MannerObject
fm_norms_MO <- glmer( 
  formula = as.formula(paste("MO", norms_form)),
  data = norms, family = binomial(link="logit")
)
summary(fm_norms_MO)


## summary of Norming study (put it all into a single dataframe): 
# the three models reported in paper and Supporting Information
norms_summ <- rbind(
  summary(fm_norms_P)$coefficients,
  summary(fm_norms_MC)$coefficients,
  summary(fm_norms_MO)$coefficients)
# add the outcome variable in each model
norms_summary <- data.frame(
  ConceptComp = rep(c("P", "MC", "MO"), each=2),
  predictor = row.names(norms_summ),
  norms_summ,
  row.names=NULL)

# bonferroni adjustments (multiply p-values by 3 to stay at nominal alpha)
norms_summary$p_bonferr <- 3 * norms_summary$Pr...z..

# round off to 3 decimals
norms_summary[, 3:7] <- lapply(norms_summary[, 3:7], function(x) round(x, 3))

# Table reported in Supporting information (only corrected p-values shown)
norms_summary[, -6]
# NB: in the main text only the coefficient for comparison Swe vs Spa is
# reported.


# An analogous model is also reported for Ground (Supporting information). This
# is to check whether Spanish speakers' greater reliance on Ground (compound
# analysis) can be explained by greater tendency to mention this component in 
# Spanish than in Swedish.
fm_norms_Gr <- glmer( 
  formula = as.formula(paste("Gr", norms_form)),
  data = norms, family = binomial(link="logit")
)
summary(fm_norms_Gr)
# This is not the case, no difference in Ground mentions between languages!

# remove clutter
rm(norms_summ, norms_summary, fm_norms_MC, fm_norms_MO, fm_norms_P,
   fm_norms_Gr, norms_form)


###############################################################
## Semantics-to-syntax mapping
###############################################################

# Where was the information expressed, in main Verb or outside of main verb?

# Rearrange information in dataframe (convenience function)
widetolong <- function(d = norms) {
  cols <- names(d)
  # intialize dataframe that will be output
  out <- data.frame()
  # loop through each semantic component
  for (i in 0:2) {
    # currcols are the columns containing information about a given semantic
    # component (P, MC, MO in each of the loops)
    currcols <- names(d)[c(4, 10, 13) + i]
    # current component in loop (P, MC, MO)
    currcomp <- names(d)[4 + i]
    # first to long for component in loop
    tmp_l <- melt(d, id.vars = c("language", "subject", "videoname"),
                  measure.vars = currcols)
    # then to wide
    tmp_w <- dcast(tmp_l, language + subject + videoname ~ variable,
                   value.var = "value")
    # Rename columns with a general name
    names(tmp_w)[4:6] <- c("Expressed", "in_Verb", "outside_Verb")
    # Add a column about the current component
    tmp_w$component <- currcomp
    # append to final data frame
    out <- rbind(out, tmp_w)
  }
  out
}
# convert data to suitable long format
norms_l <- widetolong()
head(norms_l)
# factor levels in order used in the paper
norms_l$component <- factor(norms_l$component, levels = c("P", "MC", "MO"))
# arrange as table
tb_mapping <- ddply(
  norms_l, .(language, component), summarise,
  V = sum(in_Verb / length(in_Verb)),
  Outside_V = sum(outside_Verb / length(outside_Verb)),
  Total = sum(Expressed / length(Expressed))
  )
# express as percentage and round to 1 decimal place
tb_mapping[, 3:5] <- lapply(tb_mapping[, 3:5], function(x) round(100 * x, 1))
# This is Table 1 in the paper (from norming study)
tb_mapping
