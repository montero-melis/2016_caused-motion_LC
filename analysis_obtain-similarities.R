## Raw data from the similarity arrangement task consist of X-/Y-coordinates
## for each placed item. This script transforms the data to the similarity
## scores used for the analyses using mixed models reported in the paper.

library(dplyr)


###############################################################
## Import necessary data
###############################################################

# "Raw" data as obtained from E-prime (after some variable tidying)
rawd <- read.csv("data_experiments_raw.csv")

# videoclip information
vpairs <- read.csv("data_videoclip-info.csv")


###############################################################
## Compute distances by participant and block
###############################################################

# First, order dataframe by subject, block and scene. Ordering by scene is 
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

# add Similarity measure to data frame
dist <- dist %>%
  group_by(Subject, Block) %>%
  mutate(Similarity = mysim(Distance))

head(dist)

# clean up
rm(mydist, mypairs, pairs_v, dist_v, subj_unique, mysim)


###############################################################
## Average across blocks for each participant
###############################################################

# Put into new dataframe with only Similarity measure
sim <- dist %>%
  group_by(Subject, Item) %>%
  summarise(Similarity = mean(Similarity))

# sort the data frame by Subject ID and Item
sim <- sim[with(sim, order(Subject, Item)), ]

head(sim)


###############################################################
## Add predictors and indexing information
###############################################################

## Add info about whether components are shared or not between pairs of items.
## These are our predictors to see what event components participants rely on.

# Function to achieve this:
scene_comparison <- function(d = vpairs) {
  # Make sure Scenes are ordered alphabetically
  d <- d[order(d$Scene), ]
  # Components as matrix
  m <- as.matrix(d[, 2:7])  # select just columns with components
  rownames(m) <- d$Scene
  # initialize output dataframe
  out <- data.frame()
  # compare each pair of Scenes: which components are same/different?
  for (i in 1 : (nrow(m) - 1)) {
    # scene against which the others are compared
    s1 <- m[i, , drop = FALSE]
    # scenes compared against s1
    s2 <- m[(i + 1) : nrow(m), , drop = FALSE]
    # Row by row, compare components. T if same, F if different.
    comparison <- t(apply(s2, 1, function(row) row == s1))
    # put into dataframe
    curr_df <- data.frame(
      Item = paste(rownames(s1), rownames(s2), sep = ":"),
      comparison,
      row.names = NULL)
    out <- rbind(out, curr_df)
  }
  out[, 2:7] <- lapply(out[, 2:7], as.numeric)  # T -> 1, F -> 0
  names(out)[2:7] <- names(d)[2:7]  # components as column names
  out
}
scene_pairs <- scene_comparison()
head(scene_pairs)

# join with sim
sim <- left_join(sim, scene_pairs)


## Add language and encoding condition (Experiment)

subj_info <- unique(rawd[, c("Subject", "Language", "Encoding")])

# join
sim <- left_join(sim, subj_info)
rm(subj_info)

# rearrange and rename columns
sim <- sim %>%
  select(Subject, Language, Encoding, Item, P = Path, MC = MannerCause,
         MO = MannerObject, Di = Direction, Ob = Object, Gr = Ground, 
         Sim = Similarity)

# reorder rows
sim <- sim[with(sim, order(Encoding, Language, Subject, Item)), ]

head(sim)
tail(sim)


###############################################################
## Write to disk if you want
###############################################################

# # save to disk (this is the same data as "data_experiments_similarity.csv",
# # except for occasional rounding differences at the 16th decimal)
# write.csv(sim, file = "data_experiments_similarity2.csv",
#           fileEncoding = "UTF-8", row.names = FALSE)


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
