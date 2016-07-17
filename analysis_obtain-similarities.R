## Raw data from the similarity arrangement task consist of X-/Y-coordinates
## for each placed item. This script transforms the data to the similarity
## scores used for the analyses using mixed models reported in the paper.

# library("plyr")
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
## Functions to compute distances from coordinaes (by participant and block)
###############################################################

# NB: All these functions are specifically tailored to the form of our data;
# they need to be adapted if they are to be used to other data. Furthermore,
# I have preferred transparency of computations to efficiency.

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


###############################################################
## Compute similarities by participant and block
###############################################################


# order by subject, block and videoname
rawd <- rawd[with(rawd, order(subject, block, video)), ]

## Compute distances between videopairs

# this function computes the distances between video pairs...
mydist <- function(df){
  mymatrix <- as.matrix(df[, c("X", "Y")])
  d <- dist(mymatrix)
  as.vector(d)
}
# and this one the vector of the corresponding names of video pairs
mypairs <- function(x) {
  le <- length(x)
  out <- c()
  for (i in 1:(le-1)){
    curr <- paste(x[i], x[(i+1):le], sep = ":")
    out <- c(out, curr)
  }
  out
}
# compute as vectors
pairs_v <- unlist(by(rawd[, "Scene"], rawd[, c("Subject", "Block")], mypairs))
dist_v <- unlist(by(rawd, rawd[, c("Subject", "Block")], mydist))
subj_uni <- unique(rawd$subject)

# put into data frame (note the order of the by() output)
dist <- data.frame(
  Subject = rep(subj_uni, each = 231),
  Block = rep(1:3, each = length(subj_uni) * 231),
  Item = pairs_v,
  Dist = dist_v
)

# Transform distance to similarity measure between 0 and 1 by dividing each
# participant-block combination by the maximum distance in that participant-block
# 0 is minimum, 1 is maximum similarity
mysim <- function(df){
  di <- df[["Dist"]]
  maxdist <- max(di)
  sim <- 1 - (di / maxdist)
  sim
}

# compute as vector and add to data frame
sim_v <- unlist(by(dist, dist[, c("Subject", "Block")], mysim))
dist$Sim <- sim_v

# clean up
rm(mydist, mypairs, pairs_v, dist_v, subj_uni, mysim, sim_v)

# sort the data frame
dist <- dist[with(dist, order(Subject, Block, Item)), ]

## Add predictors

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

# average across blocks
vi_sim <- ddply(dist, .(Subject, Language, Item, Path, MannerCause, 
                        MannerObject, Direction, Object, Ground),
                summarise, Sim = mean(Sim))

# # save to disk
# write.csv(vi_sim, file = "models/data/sim_data_verbal-interference.csv",
#           fileEncoding = "UTF-8", row.names = FALSE)




###############################################################
## Compute similarities per participant and block
###############################################################


# convenience function to compute distances from a subset of the nonling
# dataframe
mydist <- function(df){
  mymatrix <- as.matrix(df[, c("x_coord", "y_coord")])
  rownames(mymatrix) <- df[, "video"]
  d <- dist(mymatrix) 
  return(d)
}

## steps 1 and 2: create three similarity matrices for each participant,
## one for each block

# intialize list that will contain 3d arrays, one per participant
dist.list <- list() 
video.names <- sort(videoinfo$videoname)  # videonames in alphab. order
for (subj in unique(nonling$subject)){
  array.3d <- 
    array(data=NA, dim=c(32, 32, 3),
          dimnames=list(
            v1 = video.names,
            v2 = video.names,
            block = 1:3)
    )
  for (block in 1:3){
    df <- nonling[nonling$subject==subj & nonling$block==block,]
    raw.curr.dist <- mydist(df)
    curr.dist.matrix <- as.matrix(raw.curr.dist)
    # Fit distances for each block into 32x32 matrix
    for (i in rownames(curr.dist.matrix)){
      for (j in colnames(curr.dist.matrix)){
        array.3d[i,j,block] <- curr.dist.matrix[i,j]
      }
    }
  }
  dist.list[[paste(subj)]] <- array.3d
}
# clean up
rm(curr.dist.matrix, raw.curr.dist, df, block,i,j,subj, array.3d, video.names, mydist)

## Arrange pairwise distances between videoclips in data frame

# number of comparisons, i.e. pairwise videoclip comparisons:
nb.compar <- 33*32/2 
# number rows in final data frame:
nb.rows <- nb.compar * 3 * length(dist.list) 

## Distances (iterate through similarity matrices in dist.list)
dist.vector <- c() # initialize vector
# arrange distances as single vector to be put in data frame
for (curr.ppt in names(dist.list)){
  for (block in 1:3){
    curr.matrix <- dist.list[[curr.ppt]][,,block]
    curr.dist <- as.vector(as.dist(curr.matrix))
    dist.vector <- c(dist.vector, curr.dist)
    # impute zero distances for comparison of same videoclip:
    dist.vector <- c(dist.vector, rep(0,32)) 
  }
}
## video names following structure in distance objects when converted 
## to vector
v.names <- rownames(dist.list[[1]][,,1])
# Create two variables containing the names of the two videos compared
# in each pairwise comparison.
v1.id <- c()
for (i in 1:32){
  curr <- rep(i, 32-i)
  v1.id <- c(v1.id, curr)
}
v2.id <- c()
for (i in 2:32){
  curr <- i:32
  v2.id <- c(v2.id, curr)
}
v1 <- v.names[v1.id]
v2 <- v.names[v2.id]
# now add the comparison of same videoclips, i.e. same videoclip name in
# v1 and v2. This is needed only if one imputes missing values (!)
v1 <- c(v1, v.names)
v2 <- c(v2, v.names)

## put everything into data frame, reproducing the structure in the distance
## iteration
dist.df.eachblock <- data.frame(
  subject = rep(names(dist.list), each = nb.compar*3),
  block = rep(1:3, each=nb.compar),
  videopair_id = 1:nb.compar,
  video1 = v1,
  video2 = v2,
  distance = dist.vector
)
# clean up
rm(curr.matrix, block, curr, curr.dist, curr.ppt, dist.vector, i, nb.compar,
   nb.rows, v.names, v1, v1.id, v2, v2.id)
# show sample data
head(dist.df.eachblock)
tail(dist.df.eachblock)


###############################################################
## Functions to average over participant-blocks and add other relevant info
###############################################################

## Step 3: average pairwise similarity across blocks for each participant
# This will be done with two data sets:
# 1) keep imputed values
# 2) drop imputed values

# For each of these two data sets, we will compute from the raw distances

# a) normalized distances (NB: nromalizing over subject-block!):
# divide each raw distance in a subject-block slot by the max(distance)
# in that slot.
# b) standardized distances (NB: standardizing over subject-block!):
# for each distance in a subject-block slot, substract by mean(distance)
# for that slot, and divide by sd(distance) for that slot.

# function to average in the right way (raw, normalizing, standardizing)

dist.df.fnc <- function(mydf, drop.imputed=TRUE){
  # reorder data frame rows by subject and block
  # (needed to combine with ddply-output in 'stand' and 'norm' options)
  mydf <- mydf[order(mydf$subject, mydf$block), ]
  
  # use imputed values? Default, no
  if (drop.imputed) {
    my.imputed <- with(mydf, video1 == video2)
    mydf <- mydf[!my.imputed,]
  }
  
  # Compute values needed for different standardizations:
  SubjBlockSummary <- ddply(mydf, .(subject, block), summarise, 
                            meanDist = mean(distance, na.rm=TRUE),
                            sdDist = sd(distance, na.rm=TRUE),
                            maxDist = max(distance, na.rm=TRUE))
  # repeat each row in SubjBlockSummary as many times as there are items
  # so its structure matches that of mydf
  itemsPerSubjBlock <- nrow(mydf) / 
    (length(unique(mydf$subject)) * length(unique(mydf$block)))
  # Each row in MatchedSummary should match that of mydf
  MatchedSummary <- SubjBlockSummary[rep(1:nrow(SubjBlockSummary), each=itemsPerSubjBlock),]
  
  # normDist: Divide raw distance by max-distance for each data point
  mydf$normDist <- mydf$distance / MatchedSummary$maxDist
  # standDist: substract mean(distance) and divide by sdDist
  mydf$standDist <- (mydf$distance - MatchedSummary$meanDist) / MatchedSummary$sdDist
  
  # now average over blocks for each participant
  mydf <- ddply(mydf, .(subject, videopair_id, video1, video2), summarise,
                RawDist = mean(distance, na.rm=TRUE),
                NormDist = mean(normDist, na.rm=TRUE),
                StandDist = mean(standDist, na.rm=TRUE))
  return(mydf)
}


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

# With imputed values
simdata_imputed <- same.value.fnc(dist.df.fnc(dist.df.eachblock, drop.imputed=FALSE))
simdata_imputed <- join(simdata_imputed, lookup)  # add participant data
write.csv(simdata_imputed, "s_data/sim_data_imputed.csv", row.names=FALSE)
