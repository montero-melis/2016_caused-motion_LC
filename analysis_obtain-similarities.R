## Raw data from the similarity arrangement task consist of X-/Y-coordinates
## for each placed item. This script transforms the data to the similarity
## scores used for the analyses using mixed models reported in the paper.

library(dplyr)
# library("reshape2")


###############################################################
## Import necessary data
###############################################################

# "Raw" data as obtained from E-prime (after some variable tidying)
rawd <- read.csv("data_experiments_raw.csv")

# # participant data
# ppts <- read.csv("data_participants.csv")
# 
# # videoclip information
# videoinfo <- read.csv("data_videoclip-info.csv")


###############################################################
## Compute distances by participant and block
###############################################################

# First order dataframe by subject, block and scene. Ordering by scene is 
# important so that pairs of scenes always appear in the same (alphabetic)
# order. E.g. "trd_bronei:trt_pourue" should never appear as 
# "trt_pourue:trd_bronei" (it makes sense when you look at mypairs() function).
rawd <- rawd[with(rawd, order(Subject, Block, Scene)), ]

# This function computes distances between video pairs...
mydist <- function(df){
  mymatrix <- as.matrix(df[, c("X", "Y")])
  d <- dist(mymatrix)
  as.vector(d)
}
# and this one the names of the pairs of scenes being compared:
mypairs <- function(df) {
  x <- df[, "Scene"]
  le <- length(x)
  out <- c()
  for (i in 1:(le-1)){
    curr <- paste(x[i], x[(i+1):le], sep = ":")
    out <- c(out, curr)
  }
  out
}

# Compute scene pairs and distances as vectors
# (note that the INDICES argument of by() ensures the two match)
pairs_v <- unlist(by(rawd, rawd[, c("Block", "Subject")], mypairs))
dist_v <- unlist(by(rawd, rawd[, c("Block", "Subject")], mydist))
subj_unique <- unique(rawd$Subject)

# put into data frame (following the order of the by() output)
dist <- data.frame(
  Subject = rep(subj_unique, each = 3 * 22 * 21 / 2),
  Block = rep(1:3, each = 22 * 21 / 2),
  Item = pairs_v,
  Distance = dist_v
)
head(dist)
tail(dist)
str(dist)


###############################################################
## Transform distances to normalized similarity
###############################################################

# Transform distance to a similarity measure between 0 and 1 by dividing each
# participant-block combination by the maximum distance for that participant-
# block. Hence, 0 is minimum, 1 is maximum similarity.
mysim <- function(v){
  maxdist <- max(v)
  sim <- 1 - (v / maxdist)
  sim
}

# add to data frame
dist <- dist %>%
  group_by(Subject, Block) %>%
  mutate(Similarity = mysim(Distance))

# clean up
rm(mydist, mypairs, pairs_v, dist_v, subj_unique, mysim)


###############################################################
## Average across blocks for each participant
###############################################################

sim <- dist %>%
  group_by(Subject, Item) %>%
  summarise(Similarity = mean(Similarity))

# sort the data frame
sim <- sim[with(sim, order(Subject, Item)), ]


###############################################################
## Add predictors
###############################################################

# add info about components shared or not in stimuli, using file from study 1
vpairs <- read.csv("../data_shared/videopairs.csv", stringsAsFactors = FALSE)
names(vpairs)[1] <- "Item"
dist <- join(dist, vpairs)
rm(vpairs)

# now add participant language
subj_lang <- unique(rawd[, c("subject", "language")])
names(subj_lang) <- c("Subject", "Language")  # capital S for join to work and consistency
# join
dist <- join(dist, subj_lang)
rm(subj_lang)

head(dist)

# # save to disk
# write.csv(vi_sim, file = "models/data/sim_data_verbal-interference.csv",
#           fileEncoding = "UTF-8", row.names = FALSE)




## Another convenience function to add metainformation to data frame about 
## whether videos 1 and 2 take on same values on semantic variables 
## -- essentially my predictors in regression analyses

same.value.fnc <- function(mydf, videoinformation = videoinfo, select.vars =
                             c("videoname", "Path", "MannerCause",
                               "MannerObject", "Direction", "Object", "Ground"))
{
  unique.pairs <- mydf[unique(mydf$videopair_id), c("videopair_id", "video1", "video2")]
  lookup1 <- videoinfo[, select.vars] # lookup for first video in comparison
  names(lookup1) <- paste("v1", select.vars, sep=".")
  names(lookup1)[1] <- "video1"
  unique.pairs <- join(unique.pairs, lookup1)
  lookup2 <- videoinfo[, select.vars] # lookup for second video in comparison
  names(lookup2) <- paste("v2", select.vars, sep=".")
  names(lookup2)[1] <- "video2"
  unique.pairs <- join(unique.pairs, lookup2)
  # we only need to know whether video pairs have the same value on each 
  # variable or not, recode then:
  same.value <- with(unique.pairs, 
                     data.frame(
                       videopair_id = videopair_id,
                       same_Path = as.numeric(v1.Path==v2.Path),
                       same_MannerCause = as.numeric(v1.MannerCause==v2.MannerCause),
                       same_MannerObject = as.numeric(v1.MannerObject==v2.MannerObject),
                       same_Direction = as.numeric(v1.Direction==v2.Direction),
                       same_Object = as.numeric(v1.Object==v2.Object),
                       same_Ground = as.numeric(v1.Ground==v2.Ground)
                     )
  )
  # join with dist.df
  mydf <- join(mydf, same.value)
  return(mydf)
}

# lookup data frame to add language and condition information for each subject:
lookup <- ppts[,c("subject", "gender", "language", "condition")]


###############################################################
## Now compute dataframes and save them to disk
###############################################################

# Without imputed values
simdata <- same.value.fnc(dist.df.fnc(dist.df.eachblock, drop.imputed=TRUE))
simdata <- join(simdata, lookup)  # add participant data
write.csv(simdata, "s_data/sim_data.csv", row.names=FALSE)




###############################################################
## Compute distances from coordinates -- the slow way
###############################################################

# Below is a more transparent way of computing the distances from coordinates.
# But this way is also much less efficient. It serves only the purpose of
# clarifying how it was done above, but without using built-in functions,
# which are faster but less transparent.

# Given two observations (corresponding to two arranged scenes), this function
# computes the distance in pixels between them.
dist_pixel <- function(d1, d2) {
  obs1 <- d1[, c("X", "Y")]
  obs2 <- d2[, c("X", "Y")]
  diff <- obs2 - obs1
  dist <- sqrt(diff[1] ^ 2 + diff[2] ^ 2)
  dist
}

# This function pastes the name of the two scenes being compared
paste_scenes <- function(d1, d2) {
  paste(d1[, "Scene"], d2[, "Scene"], sep = ":")
}

# Last one is a wrapper function that applies the former by participant-block
compute_distance <- function(d = rawd) {
  # initialize the output dataframe
  out <- data.frame(
    Subject = rep(unique(d$Subject), each = 3 * 22 * 21 / 2),
    Block = rep(unique(d$Block), each = 22 * 21 / 2),
    Item = NA, 
    Distance = NA)
  rowcounter <- 0
  for (sbj in unique(d$Subject)) {
    for (bl in unique(d$Block)) {
      print(paste("Subject:", sbj, "and block:", bl))
      # data frame for one subject--block combination
      df <- d[d$Subject == sbj & d$Block == bl, ]
      # Now loop through each of the pairwise combinations of rows
      for (i in 1 : (nrow(df) - 1)) {
        for (j in (i + 1) : nrow(df)) {
          # update counter
          rowcounter <- rowcounter + 1
          # compute corresponding distance for scene pair
          out$Item[rowcounter] <- paste_scenes(df[i,], df[j,])
          out$Distance[rowcounter] <- dist_pixel(df[i,], df[j,])
        }
      }
    }
  }
  out
}

## Now compute distances

# First order dataframe by subject, block and scene. Ordering by scene is 
# important so that pairs of scenes always appear in the same (alphabetic)
# order. E.g. "trd_bronei:trt_pourue" should never appear as 
# "trt_pourue:trd_bronei"
rawd <- rawd[with(rawd, order(Subject, Block, Scene)), ]

# Now compute distances between videopairs by subject and block
# (takes very long because of the many for loops and the inefficient function).
mydist <- compute_distance()
