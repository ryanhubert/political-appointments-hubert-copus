# Ryan Hübert and Ryan Copus
# Political Appointments and Outcomes in Federal District Courts
# Journal of Politics
# January 2021

## Data and supporting materials necessary to reproduce the numerical results
## in the article are available in the Journal of Politics Dataverse
## (https://dataverse.harvard.edu/dataverse/jop) and at the website of the
## corresponding author (https://ryanhubert.com/). An online appendix with
## supplementary material is available at the Journal of Politics website
## and at the website of the corresponding author.

require(tidyverse)
require(estimatr)

## Set working directory
## Note: you should paste the full path to your local working directory
root <- ""

## Root directory must have a trailing slash
if(!grepl("/$", root)){
  root <- paste0(root,"/")
}

setwd(root)

## Import dataset and some extra tools we need
df <- read.csv(paste0(root, "Analysis/USDC-DATASET-CLEANED.csv"),
               stringsAsFactors = FALSE)
load(paste0(root, "Analysis/MODEL.RData"))

## Define a list of outcomes
outcomes <- c("pro_defendant", "settlement", "dism_invol", "dism_vol",
              "jud_def", "jud_oth", "jud_pla", "other")

## Define a dataframe to collect the estimates or import one that already exists
if(!file.exists(paste0(root, "Analysis/effects_main.RData"))){
  ef <- NULL
} else {
  load(paste0(root, "Analysis/effects_main.RData"))
}


## Define a vector of objects in environment to keep when clearing workspace
kobj <- c("outcomes", "root", "df", "ef", "Clean", "Reg", "kobj")

# Note to reader: for each analysis, we define a `mask` which indicates
# which subset of the dataset we use for the analysis. For example, for our
# main analyses, we drop all cases where the initial assigned judge was
# eventually replaced by a new judge via a reassignment. See the manuscript
# and the Online Appendix for more information about this.


################################################################################
## Main ARA Effects and Robustness Checks Reported in the Online Appendix
################################################################################

## For each outcome, estimate main effects and several robustness checks
## reported in Table C1 of the Online Appendix and Figure 4 of the manuscript.

## Define subsets of data and estimate columns 1-5 of Table C1 in the Online
## Appendix. Note: effects from column 1 are plotted in Figure 4 in manuscript.
masks <- list("main" = (df$judge_replaced == 0), # column 1
              "non_senior" = (df["judge_replaced"]==0) & (df["jsenior"] == 0), # column 2
              "first_guess" = (df["judge_replaced"] == 0 | df["judge_replaced"] == 1), # column 3
              "truncated" = (df["judge_replaced"]==0) & (df$YEAR > 1995 & df$YEAR < 2013), # column 4
              "judge_variation" = (df["judge_replaced"]==0))  # column 5

treats <- c("jrepublican.jdemocrat", "jrepublican.jdemocrat.jyears")

## Loop over the masks, treatments and outcomes to estimate effects
for(m in names(masks)){
  for(t in treats){
    if(m != "main" & t == "jrepublican.jdemocrat.jyears"){ next }
    for(y in outcomes){
      cat("\nEstimating main effects and robustness checks...\n")
      cat(paste0(" Mask: ", m, "\n"))
      cat(paste0("  Treatment: ", t, "\n"))
      cat(paste0("   Outcome: ", y, "\n"))
      ## Check if this effect has already been estimated, and skip if so
      if(length(which(ef$outcome == y & ef$mask == m & ef$treatment == t)) == 0){
        mask <- list(masks[[m]])
        names(mask) <- m
        if(m == "judge_variation"){
          ## For this model, we drop all cases from blocks with no variation in
          ## judges. See online appendix A.3 for a discussion.
          row <- Clean(datafr = df, mask = mask,
                       out = y,
                       treat = strsplit(t, "[.]")[[1]][1],
                       control = strsplit(t, "[.]")[[1]][2],
                       require_cluster_var = TRUE)
        } else if(t == "jrepublican.jdemocrat.jyears"){
          ## For this model, we control for a judge"s number of years on
          ## the bench.
          row <- Clean(datafr = df, mask = list("main" = masks[["main"]]),
                       out = y,
                       treat = strsplit(t, "[.]")[[1]][1],
                       control = strsplit(t, "[.]")[[1]][2],
                       extra.regressors = c("jyears"))
        } else {
          ## This is our main specification.
          row <- Clean(datafr = df, mask = mask,
                       out = y,
                       treat = strsplit(t, "[.]")[[1]][1],
                       control = strsplit(t, "[.]")[[1]][2])
        }
        row <- Reg(clean_object = row)
        if(is.list(row)){
          ef <- rbind(ef, row[["row"]])
        }
      }
    }
  }
}

## Save the effects
save(ef, file = paste0(root, "Analysis/effects_main.RData"))
write.csv(ef, file = paste0(root, "Analysis/effects_main.csv"))
rm(list = ls()[which(!ls() %in% kobj)])


################################################################################
## Per-judge effects to rule out outliers (see footnote 21)
################################################################################

m <- "main"
y <- "pro_defendant"
treats <- unique(as.vector(df$jid_anon[(df$judge_replaced == 0)]))

for(t in treats){
  outparty <- ifelse(mean(df$jrepublican[(df$judge_replaced == 0) &
                                           (df$jid_anon==t)]) > 0.5,
                     "jdemocrat",
                     "jrepublican")

  cat("\nEstimating judge-specific effects...\n")
  cat(paste0(" Mask: main\n"))
  cat(paste0("  Treatment: ", paste0("j", t, ".", outparty), "\n"))
  cat(paste0("   Outcome: ", y, "\n"))

  ## Check if this effect has already been estimated, and skip if so
  if(length(which(ef$outcome == y & ef$mask == m & ef$treatment ==  paste0("j", t, ".", outparty))) == 0){
    df[paste0("j",t)] <- ifelse(df$jid_anon == t, 1, 0)
    row <- Clean(datafr = df, mask = list("main" = (df$judge_replaced == 0)),
                 out = "pro_defendant", treat = paste0("j",t), control = outparty)
    row <- Reg(clean_object = row, cluster = FALSE)
    if(is.list(row)){
      ef <- rbind(ef, row[["row"]])
    }
    df[paste0("j",t)] <- NULL
  }
}

save(ef, file = paste0(root, "Analysis/effects_main.RData"))
write.csv(ef, file = paste0(root, "Analysis/effects_main.csv"))
rm(list = ls()[which(!ls() %in% kobj)])


################################################################################
## Post-Treatment Bias in ARA Effects when Selecting on Case Outcomes
################################################################################

t <- "jrepublican.jdemocrat"
y <- "pro_defendant"

## Define subsets of data used to estimate biased effects & then estimate
masks <- list("biased_not_settled" = (df$judge_replaced == 0) & (df$settlement == 0),
              "biased_not_withdrawn" = (df$judge_replaced == 0) & (df$settlement == 0) & (df$dism_vol == 0),
              "biased_judgments" = (df$judge_replaced == 0) & (rowSums(df[,c("jud_pla","jud_def","jud_oth")]) == 1),
              "biased_appealed" = (df$judge_replaced == 0) & (df$APPEAL == 1))

for(m in names(masks)){
  cat("\nEstimating biased effects...\n")
  cat(paste0(" Mask: ", m, "\n"))
  cat(paste0("  Treatment: ", t, "\n"))
  cat(paste0("   Outcome: ",y , "\n"))
  ## Check if this effect has already been estimated, and skip if so
  if(length(which(ef$outcome == y & ef$mask == m & ef$treatment == t)) == 0){
    mask <- list(masks[[m]])
    names(mask) <- m
    row <- Clean(datafr = df, mask = mask,
                 out = "pro_defendant", treat = "jrepublican", control = "jdemocrat")
    row <- Reg(clean_object = row)
    if(is.list(row)){
      ef <- rbind(ef, row[["row"]])
    }
  }
}

save(ef, file = paste0(root, "Analysis/effects_main.RData"))
write.csv(ef, file = paste0(root, "Analysis/effects_main.csv"))
rm(list = ls()[which(!ls() %in% kobj)])


################################################################################
## ARA Effects for Specific Presidents" Appointees and Over Time
################################################################################

masks <- list("main" = (df$judge_replaced==0),
              "clinton" = (df$judge_replaced==0) & grepl("(199[56789]|200[0])", df$block),
              "bush43" = (df$judge_replaced==0) & grepl("(200[1-8])", df$block),
              "obama" = (df$judge_replaced==0) & grepl("(200[9]|201[0-9])",df$block))

treats <- c("jrepublican.jdemocrat", "jobama.jbush43", "jclinton.jbush41",
           "jbush41.jreagan", "jreagan.jcarter", "jobama.jclinton",
           "jbush43.jclinton", "jbush41.jclinton", "jreagan.jclinton")

y <- "pro_defendant"

for(m in names(masks)){
  for(t in treats){
    ## Do not estimate president effects on cases heard before president elected
    cond1 <- (grepl("obama", t) & m %in% c("clinton", "bush43"))
    cond2 <- (grepl("bush43", t) & m %in% c("clinton"))
    if(cond1 | cond2){ next }
    ## Check if this effect has already been estimated, and skip if so
    if(length(which(ef$outcome == y & ef$mask == m & ef$treatment ==  t)) == 0){
      cat("\nEstimating adjacent presidents and over-time effects...\n")
      cat(paste0(" Mask: ", m, "\n"))
      cat(paste0("  Treatment: ", t,"\n"))
      cat(paste0("   Outcome: ", y, "\n"))
      mask <- list(masks[[m]])
      names(mask) <- m
      row <- Clean(datafr = df, mask = mask,
                   out = y,
                   treat = strsplit(t, "[.]")[[1]][1],
                   control = strsplit(t, "[.]")[[1]][2])
      row <- Reg(clean_object = row)
      if(is.list(row)){
        ef <- rbind(ef, row[["row"]])
      }
    }
  }
}

save(ef, file = paste0(root, "Analysis/effects_main.RData"))
write.csv(ef, file = paste0(root, "Analysis/effects_main.csv"))
rm(list = ls()[which(!ls() %in% kobj)])
