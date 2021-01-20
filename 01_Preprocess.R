require(tidyverse)

## Define working directory
## Note: you should paste the full path to your local working directory
root <- ""

## Root directory must have a trailing slash
if(!grepl("/$", root)){
  root <- paste0(root,"/")
}

## Create all required directories if not yet available
dir.create(paste0(root, "Analysis"), showWarnings = FALSE)
dir.create(paste0(root, "Plots"), showWarnings = FALSE)
dir.create(paste0(root, "Tables"), showWarnings = FALSE)

## Import raw data
df <- read.csv(paste0(root, "USDC-DATASET-JOP.csv"), stringsAsFactors = FALSE)

################################################################################
## Drop some observations (see article and online appendix for details)
################################################################################

## How many cases in dataset?
origcount <- length(unique(df$file))
cat(paste0("Total civil rights cases: ", origcount, "\n"))

## How many cases in dataset by court?
desc <- summarize(group_by(df, court), total = n_distinct(file), 
                  .groups = "drop")
for(i in 1:nrow(desc)){
  cat(paste0(" --> Total cases in ",paste0(desc[i,],collapse = ": "), "\n"))
}
  
## Remove cases with no judge listed
toremove <- is.na(df$jid_anon)
df <- distinct(df[!toremove,])
cat(paste0(" --> Removed ",origcount-length(unique(df$file))," (no judge info)\n"))
count <- length(unique(df$file))
  
## Remove cases that are IFP denials
toremove <- df$ifp_denial == 1
df <- distinct(df[!toremove,])
cat(paste0(" --> Removed ",count-length(unique(df$file))," (ifp denial)\n"))
count <- length(unique(df$file))

################################################################################
## Add some useful variables and drop some others
################################################################################

## Code dummy variables for the outcomes of interest
df$settlement <- ifelse(df$OUTCOME == "settlement", 1, 0)
df$dism_invol <- ifelse(grepl("dism_(oth|juris|pros)",df$OUTCOME), 1, 0)
df$dism_vol <- ifelse(df$OUTCOME == "dism_vol", 1, 0)
df$jud_def <- ifelse(grepl("jud",df$OUTCOME) & grepl("defendant",df$OUTCOME), 1, 0)
df$jud_pla <- ifelse(grepl("jud",df$OUTCOME) & grepl("plaintiff",df$OUTCOME), 1, 0)
df$jud_oth <- ifelse(grepl("jud",df$OUTCOME) & grepl("(NA|both)",df$OUTCOME), 1, 0)
df$other <- ifelse(grepl("(transfer|remand)",df$OUTCOME), 1, 0)
df$pro_defendant <- rowSums(df[,c("dism_invol", "dism_vol", "jud_def")])

## Define the "control" group for main analysis
df$jdemocrat <- 1 - df$jrepublican

## Create blocks for blocked randomization design
df$block <- paste0(df$court, df$division, "_", df$YEAR)

## Add dummies for blocks
for(b in unique(df$block)){
  df[b] <- ifelse(df$block==b, 1, 0)
}

## Create dummy variable for judges' appointing presidents
df$jpresident <- paste0("j",tolower(df$jpresident))
for(i in unique(df$jpresident)){
  if(i %in% c("jcarter", "jreagan", "jbush41", "jclinton", "jbush43", "jobama")){
    df[i] <- ifelse(df$jpresident==i,1,0)
  } 
}

## Drop old outcome variable and IFP variable
df <- df[,colnames(df)[!colnames(df) %in% c("OUTCOME", "ifp_denial")]]

################################################################################
## Define some tools and objects that will be used in analysis 
################################################################################

## Define a function that preprocesses the data to implement the blocked
## design, such as removing blocks with no variation on the treatment, etc.
Clean <- function(datafr, mask, out, treat, control, 
                  require_cluster_var = FALSE, extra.regressors = c()){
  
  ## Load required packages
  z1 <- require("dplyr")
  if((!z1)){
    print("Required packages (dplyr) not installed.")
    return(NA)
  }

  ## Check if mask argument is a list with a name and a boolean vector on datafr
  if(!is.list(mask) | length(mask) != 1){
    print(paste0("ERROR: you must supply a list for mask, which contains a",
                 " name and a boolean vector indicating a subset of datafr."))
    return(NA)
  } else {
    datafr <- datafr[mask[[names(mask)[1]]],]
  }

  ## Each observation should be in treatment or control
  datafr <- datafr[which(rowSums(datafr[,c(treat,control)]) == 1),]

  ## Drop blocks with insufficient variation
  
  ## 1. Drop blocks with no observations in either treatment or control
  no_treat_var <- left_join(summarize(group_by(datafr[datafr[treat] == 1,], block),
                                      tr = n(), .groups = "drop"),
                            summarize(group_by(datafr[datafr[treat] == 0,], block),
                                      co = n(), .groups = "drop"),
                            by = c("block" = "block"))
  no_treat_var <- no_treat_var[no_treat_var$tr > 0 & no_treat_var$co > 0,]
  datafr <- datafr[datafr$block %in% no_treat_var$block, ]
  rm(no_treat_var)
  
  ## 2. Drop blocks with only one judge in either treatment or control
  if(require_cluster_var == TRUE){
    no_jud_var <- summarize(group_by(datafr, block, !!sym(treat)), 
                            n = length(unique(jid_anon)), .groups = "drop")
    no_jud_var <- no_jud_var[no_jud_var$n > 1,]
    no_jud_var <- summarize(group_by(no_jud_var, block), n = mean(!!sym(treat)), 
                            .groups = "drop")
    no_jud_var <- no_jud_var[no_jud_var$n > 0 & no_jud_var$n < 1, ]
    datafr <- datafr[datafr$block %in% no_jud_var$block, ]
    rm(no_jud_var)
  }
  
  ## If no observations remaining in dataframe after cleaning, return NA
  if(nrow(datafr) == 0){
    print(paste0("Warning: there were no observations left after cleaning",
                 " the dataframe. No model will be run."))
    return(NA)
  }
  
  ## Drop one block dummy (the reference block)
  
  if(length(unique(datafr$block)) == 1){
    drop_block <- NA
  } else {
    drop_block <- left_join(summarize(group_by(datafr[datafr[treat] == 1,], block),
                                      tr = n(), .groups = "drop"),
                            summarize(group_by(datafr[datafr[treat] == 0,], block),
                                      co = n(), .groups = "drop"),
                            by = c("block" = "block"))
    if(nrow(drop_block[drop_block$tr > 1 & drop_block$co > 1,]) == 0){ 
      drop_block$count <- drop_block$tr + drop_block$co
      drop_block <- drop_block$block[which(drop_block$count == max(drop_block$count))][1]
    } else {
      drop_block <- drop_block[drop_block$tr > 1 & drop_block$co > 1,]
      drop_block$count <- drop_block$tr + drop_block$co
      drop_block <- drop_block$block[which(drop_block$count == max(drop_block$count))][1]
    }
    
  }

  ## Create dataframe for the regression, dropping observations and columns
  blocks <- unique(datafr$block)
  blocks <- blocks[which(blocks != drop_block)]
  datafr <- as.data.frame(datafr[, c("file", "jid_anon", 
                                     out, treat, extra.regressors, blocks)])
  
  ## Create treatment-demeaned-block interactions
  for(b in blocks){
    datafr[paste0("treat.",b)] <- datafr[treat] * (datafr[b] - mean(datafr[[b]]))
  }
  iblocks <- colnames(datafr)[grepl("treat.",colnames(datafr))]
  
  ## Rename treatment variable
  treat.control <- paste0(c(treat,control,extra.regressors), collapse = ".")
  colnames(datafr)[4] <- treat.control
  
  ## Formulate a regression model
  regressors <- c(treat.control, extra.regressors, blocks, iblocks)
  model <- as.formula(paste0(out, " ~ ", paste0(regressors, collapse = " + ") ))
  
  return(list("datafr" = datafr, "model" = model, "mask" = names(mask)[1]))
}


## Define a function to perform the desired regression and return row of results
Reg <- function(clean_object, cluster = TRUE, wdir = "~/"){
  
  ## Load required packages
  z1 <- require("dplyr")
  z2 <- require("estimatr")
  if((!z1) | (!z2)){
    print("Required packages (dplyr, estimatr) not installed.")
    return(NA)
  }
  
  ## Check if argument is a list returned by Clean() function
  if(!is.list(clean_object)){
    print(paste0("You must complete the preprocessing using the Clean() ", 
                 "function before running the regression."))
    return(NA)
  }
  
  datafr <- clean_object[["datafr"]]
  
  ## Estimate the model
  if(cluster == FALSE){
    dim_out <- lm_robust(clean_object[["model"]], 
                         se_type = "HC1", data = datafr)
  } else {
    dim_out <- lm_robust(clean_object[["model"]], 
                         clusters = jid_anon, data = datafr)
  }
  
  ## Generate a dataframe row returning relevant estimates from model
  
  out <- as.character(clean_object[["model"]][2])
  treat.control <- gsub("^([^ ]+) .+","\\1",clean_object[["model"]][3])
  
  row <- c(dim_out$coefficients[[treat.control]],
           dim_out$std.error[[treat.control]],
           dim_out$statistic[[treat.control]], 
           dim_out$p.value[[treat.control]],
           dim_out$conf.low[[treat.control]], 
           dim_out$conf.high[[treat.control]],
           dim_out$df[[treat.control]], 
           dim_out$nobs, 
           nrow(datafr[datafr[treat.control]==1,]),
           nrow(datafr[datafr[treat.control]==0,]),
           length(unique(datafr[datafr[[treat.control]]==1,"jid_anon"])),
           length(unique(datafr[datafr[[treat.control]]==0,"jid_anon"])),
           length(which(grepl("^(ca|or|wa).+[_][12]", 
                              names(dim_out$coefficients))))+1, 
           out, 
           clean_object[["mask"]],
           treat.control,
           "ols_lin")
  
  row <- as.data.frame(rbind(row))
  colnames(row) <- c("effect", "se", "tval", "pval", "ciL", "ciH", "df",
                     "obs", "obs_treat", "obs_control", "judges_treat", 
                     "judges_control", "fe_count", "outcome", "mask", 
                     "treatment", "estimator")
  rownames(row) <- NULL
  row[1:13] <- round(as.numeric(row[1:13]),5)
  
  row <- list("row" = row, "output" = dim_out)
  
  return(row)
}

## Define the relevant subsets
row <- Clean(df, list("main" = (df$judge_replaced==0)), "pro_defendant", 
             "jrepublican", "jdemocrat")$datafr$file
df$main_subset <- 0
df$main_subset[df$file %in% row] <- 1

df <- df[,c("file", "judge_replaced", "main_subset",
             colnames(df)[which(!colnames(df) %in% c("file", "judge_replaced",
                                                     "main_subset"))])]

save(Clean, Reg, file = paste0(root, "Analysis/MODEL.RData"))
write.csv(df, file = paste0(root, "Analysis/USDC-DATASET-CLEANED.csv"))