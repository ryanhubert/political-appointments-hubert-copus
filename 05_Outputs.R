# Ryan Hübert and Ryan Copus
# Political Appointments and Outcomes in Federal District Courts
# January 2021

require(tidyverse)
require(gridExtra)
require(extrafont)
require(ggalluvial)
require(cowplot)

## Set working directory
## Note: you should paste the full path to your local working directory
root <- ""

## Root directory must have a trailing slash
if(!grepl("/$", root)){
  root <- paste0(root,"/")
}

setwd(root)

## Define a function to convert points to mm for ggplot
unitpt <- function(x) {0.352778 * x} # convert points to mm

## Define a list of outcomes
ovars <- as.list(c("Pro-Defendant\nOutcomes", "Settlements", "Involuntary\nDismissals",
                   "Voluntary\nDismissals", "Judgments\nfor Defendant",
                   "Judgments\nfor Other", "Judgments\nfor Plaintiff", "Other\nOutcomes"))
ovars <- as.list(ovars)
names(ovars) <- c("pro_defendant", "settlement", "dism_invol", "dism_vol", 
                  "jud_def", "jud_oth", "jud_pla", "other")
outcomes <- names(ovars)

## Load datasets
load(paste0(root, "Analysis/effects_main.RData"))
df <- read.csv(paste0(root, "Analysis/USDC-DATASET-CLEANED.csv"), stringsAsFactors = FALSE)

## What is the main subset of data we will use in the analysis?
maskm <- (df$main_subset == 1)

## Drop rows with no models (no observations)
ef <- ef[(ef$obs > 0), ]

## Define a vector of objects in environment to keep when clearing workspace
kobj <- c("unitpt", "ovars", "outcomes", "root", "df", "ef", "maskm", 'kobj')

################################################################################
## Descriptive statistics
################################################################################

## Characterize what we drop when we analyze, as discussed in Section A.3 of 
## the online appendix as well as in the manuscript. 

## Block-level summary statistics
tp <- summarize(group_by(df[(df$judge_replaced == 0),], block), 
                dropped = 1- mean(main_subset), .groups = 'drop')
tp <- left_join(tp,
                summarize(group_by(df[(df$judge_replaced == 0),], block), 
                          n = n(), .groups = 'drop'),
                by = c('block' = 'block'))

## How many cases do we drop? 
print(paste0('Cases dropped due to insufficient treatment variation within blocks: ', 
             sum(tp[tp$dropped == 1,'n']),
             " (",
             100*round(sum(tp[tp$dropped == 1,'n'])/sum(tp$n),2),
             "%)"))

## How many blocks do we drop? 
print(paste0('Blocks dropped due to insufficient treatment variation within block: ', 
             nrow(tp[tp$dropped == 1,]),
             " (",
             100*round(nrow(tp[tp$dropped == 1,])/nrow(tp),2),
             "%)"))

## How many judges do we drop? 
all.jud = unique(df$jid_anon[df$judge_replaced == 0])
kept.jud = unique(df$jid_anon[df$judge_replaced == 0 & df$main_subset == 1])
print(paste0('Judges dropped due to insufficient treatment variation within block: ', 
             length(which(!all.jud %in% kept.jud))))

## Demonstrate that we drop disproportionately small blocks 
cat(paste0('Average number of cases in dropped blocks: ', 
           round(mean(tp$n[tp$dropped == 1]),0),
           '\nAverage number of cases in all blocks: ', 
           round(mean(tp$n),0)))
                         

## Plot Figure 2

## Create a dataframe to plot

tp <- cbind(outcomes[2:8],
            as.data.frame(colSums(df[maskm,outcomes[2:8]])),
            as.data.frame(colSums(df[maskm & (df$APPEAL == 1),outcomes[2:8]])))
colnames(tp) <- c('out', 'N', 'AN')
rownames(tp) <- NULL

tp$out <- factor(tp$out, levels = names(ovars)[2:length(names(ovars))])
outs <- as.vector(unlist(ovars)[2:length(ovars)])
tp$N <- tp$N/nrow(df[maskm,])
tp$AN <- tp$AN/nrow(df[maskm,])

z <- ggplot(tp) + 
  geom_bar(aes(x = out, y = N), stat='identity', alpha = 0.4) + 
  geom_bar(aes(x = out, y = AN), stat='identity', alpha = 1) + 
  geom_text(aes(x = out, y = N+0.005, label = sprintf("%.3f", round(N,3))), vjust = 0, size = unitpt(10)) +
  scale_x_discrete(limits = names(ovars)[2:length(names(ovars))],
                   labels = outs,
                   expand = c(0.1, 0.1)) + 
  scale_y_continuous(expand = c(0.01,0.01), 
                     limits = c(0, 0.4),
                     breaks = seq(0,0.5,0.1)) + 
  theme_bw() +   
  labs(y = 'Proportion of\nCases in Sample') + 
  theme(text = element_text(size=10),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=10), # Tick labels
        axis.text.x = element_text(size=10,angle = 45,
                                   lineheight = 0.75,
                                   vjust=1,hjust=1), # Tick labels
        strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 10))

fgfile <- paste0(root,"Plots/fg2")
figw <- 4; figh <- 2.25

tiff(paste0(fgfile, '.tif'), width = figw, height = figh, units = "in", res = 300,
     type = 'cairo', family = "Times")
print(z)
dev.off()

pdf(paste0(fgfile, '.pdf'), width = figw, height = figh, family = "Times")
print(z)
dev.off()

rm(list = ls()[which(!ls() %in% kobj)])

## Plot Figure B.1

tp <- summarize(group_by(df[maskm,], court, YEAR), tp = n())
tp$court <- factor(tp$court, levels = unique(tp$court))
tp$YEAR <- as.integer(tp$YEAR)
z <- ggplot(tp, aes(x = YEAR, y = tp, color = court, group = court)) +
  geom_area(aes(fill = court), color ='black', alpha=0.3) + 
  scale_x_continuous(limits = c(1995,2016),
                     breaks = seq(1996, 2016, 4)) +
  scale_fill_manual(name = "Court",
                    values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00", "#CC79A7")) +
  theme_bw() +
  labs(y = 'Total Number of Cases', x = 'Case Filing Year') +
  theme(text = element_text(size=10),
        legend.title = element_text(face = "bold", size=10),
        legend.text = element_text(size=10),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=10), # Tick labels
        axis.text.x = element_text(size=10), # Tick labels
        strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 10))


fgfile <- paste0(root,"Plots/cases_by_yr_court")
figw <- 5; figh <- 2.5

pdf(paste0(fgfile, '.pdf'), width = figw, height = figh, family = "Times")
print(z)
dev.off()

rm(list = ls()[which(!ls() %in% kobj)])

################################################################################
## Main effects
################################################################################

## Compile Table C.1 (main effects)

## Create a dataframe of the relevant effects
tp <- as.data.frame(ef)
masks <- c(unique(tp$mask)[1:5])
mm <- (tp$mask %in% masks)
mt <- (tp$treatment %in% c('jrepublican.jdemocrat', 'jrepublican.jdemocrat.jyears'))

tp <- tp[mm & mt,]

tp$model <- 0
tp$model[tp$mask=='main' & tp$treatment == 'jrepublican.jdemocrat'] <- 1
tp$model[tp$mask=='non_senior' & tp$treatment == 'jrepublican.jdemocrat'] <- 2
tp$model[tp$mask=='first_guess' & tp$treatment == 'jrepublican.jdemocrat'] <- 3
tp$model[tp$mask=='truncated' & tp$treatment == 'jrepublican.jdemocrat'] <- 4
tp$model[tp$mask=='judge_variation' & tp$treatment == 'jrepublican.jdemocrat'] <- 5
tp$model[tp$mask=='main' & tp$treatment == 'jrepublican.jdemocrat.jyears'] <- 6


## Write the table to a file 
fl <- paste0(root, "Tables/main_effects.txt")
cat(paste0("\\begin{tabular}{l", 
           paste0(rep('c',max(tp$model)),collapse = ''), 
           "}\n\\hline\\hline\n & ",
           paste0(paste0('(',seq(1,max(tp$model)),')'), collapse = ' & '),
           # "(1) & (2) & (3) & (4) & (5) & (6) & (7)",
           "\\\\\\hline\\hline\n"), file=fl)

for(y in names(ovars)){
  tf <- tp[tp$outcome == y,]
  tf <- tf[order(tf$model),]
  tf <- distinct(tf)
  row <- rbind(t(format(round(tf$effect,3),nsmall=3)), t(format(round(tf$se,3),nsmall=3)))
  row[1,] <- gsub('-(.+)', '--\\1~~', row[1,])
  row[2,] <- gsub('(.+)', '(\\1)', row[2,])
  cat(paste0("& \\multicolumn{", max(tp$model), "}{c}{\\textbf{", ovars[[y]],"}} \\\\\n"), file=fl, append=TRUE)
  cat(paste0("ARA Effect & ", paste0(row[1,], collapse = " & "), "\\\\\n"), file=fl, append=TRUE)
  cat(paste0(" & ", paste0(row[2,], collapse = " & "), "\\\\\\hline\n"), file=fl, append=TRUE)
  ## Get the model statistics: observations, degrees of freedom, number of judges, etc.
  if(y == 'settlement'){
    row1 <- rbind(t(tf$obs), t(tf$judges_treat), t(tf$judges_control), t(tf$fe_count), t(round(tf$df,2)))
    row1 <- cbind(c("Observations", "Treatment Judges", "Control Judges", "Division-Years", "Degrees of Freedom"), row1)
  }
  rm(tf, row)
}
cat(paste0("\\hline\n"), file=fl, append=TRUE)
for(r in 1:nrow(row1)){
  cat(paste0(paste0(row1[r,], collapse = ' & '), '\\\\\n'), file=fl, append=TRUE)  
}
cat(paste0("\\hline\\hline\n\\end{tabular}\n"), file=fl, append=TRUE)
rm(row1)

## Plot Figure 4

tp <- tp[tp$model == 1 & tp$outcome != 'pro_defendant', ]

MAXY <- round(max(abs(tp$ciH),abs(tp$ciL)),3) + 0.001

z <- ggplot(tp, aes(x = effect)) +
  geom_hline(yintercept = 0, size = 0.5, linetype = "dashed") +
  geom_point(aes(x = outcome, y = effect), size = 2, position = position_dodge(0.5)) + 
  geom_linerange(aes(x = outcome, ymax = ciH, ymin = ciL, group = estimator), size = 0.5, position = position_dodge(0.5)) + 
  labs(y = "ARA Effects", x = 'Outcome') +
  theme_bw() +
  scale_x_discrete(limits = outcomes[2:length(outcomes)], 
                   labels = unlist(lapply(outcomes[2:length(outcomes)], function(x) ovars[[x]])),
                   expand = c(0.1, 0.1)) + 
  scale_y_continuous(expand = c(0.01,0.01),
                     limits = c(-MAXY, MAXY),
                     breaks = seq(-0.2,0.2,0.02)) +
  theme(title = element_text(size=10),
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=10), # Tick labels
        axis.text.x = element_text(size=10, angle = 45, 
                                   lineheight = 0.75,
                                   hjust=1, vjust=1))

fgfile <- paste0(root,paste0("Plots/fg4"))
figw <- 3.5; figh <- 2.25

tiff(paste0(fgfile, '.tif'), width = figw, height = figh, units = "in", res = 300,
     type = 'cairo', family = "Times")
print(z)
dev.off()

pdf(paste0(fgfile, '.pdf'), width = figw, height = figh, family = "Times")
print(z)
dev.off()

rm(list = ls()[which(!ls() %in% kobj)])

################################################################################
## Causal identification
################################################################################

## Plot Figure 3

## Import and clean model performance data and predictions
auc <- read.csv(paste0(root, 'Analysis/usdc_balance_performance.csv'), colClasses = "character")
auc <- auc[auc$algorithm == 'my_ensemble', c('model', 'auc_tf_cv')]
auc$auc_tf_cv <- as.character(round(as.numeric(auc$auc_tf_cv),3))
auc$auc_tf_cv <- ifelse(nchar(auc$auc_tf_cv) == 4, paste0(auc$auc_tf_cv,'0'), auc$auc_tf_cv)
auc.full <- auc$auc_tf_cv[auc$model == 'full']
auc.bench <- auc$auc_tf_cv[auc$model == 'bench']

roc <- read.csv(paste0(root, 'Analysis/usdc_balance_roc.csv'), colClasses = "character")
roc <- roc[which(roc$fpr!="fpr" & roc$algorithm=="my_ensemble"),]
roc <- roc[,c('fpr', 'tpr', 'model')]
roc$fpr <- as.numeric(as.character(roc$fpr))
roc$tpr <- as.numeric(as.character(roc$tpr))
roc$model <- as.character(roc$model)
roc$model[which(roc$model == 'full')] <- "Full"
roc$model[which(roc$model == 'bench')] <- "Benchmark"
roc$model <- as.factor(roc$model)

pf <- read.csv(paste0(root, 'Analysis/usdc_balance_predictions.csv'), stringsAsFactors = FALSE)
pf <- pf[pf$model %in% c('full','bench'),c('model','file','outcome','my_ensemble')]
pf$outcome <- as.integer(pf$outcome)
pf$my_ensemble <- as.numeric(pf$my_ensemble)
pf$quantile <- 0
for(m in c('full', 'bench')){
  pf$quantile[which(pf$model == m)] <- round(ecdf(pf$my_ensemble[which(pf$model == m)])(pf$my_ensemble[which(pf$model == m)]),2)
}
pf1 <- as.data.frame(summarize(group_by(pf, model, quantile), mean_prediction = mean(my_ensemble)))
pf1 <- left_join(pf1[which(pf1$model=="full"),c('quantile','mean_prediction')], 
                 pf1[which(pf1$model=="bench"),c('quantile','mean_prediction')],
                 by = c('quantile' = 'quantile'))
colnames(pf1) <- c('percentile', 'mean_pred_full', 'mean_pred_bench')

## Make plots
z1 <- ggplot(roc) +  
  ylim(ymin=0,ymax=1)+
  xlim(xmin=0,xmax=1)+
  geom_abline(intercept = 0, slope = 1, linetype = 'solid', color = 'gray', size = 0.2) + 
  geom_line(aes(x = fpr, y = tpr), data = roc[which(roc$model == 'Benchmark'),],  size = 0.50, linetype = 'dashed', alpha = 0.9) + 
  geom_line(aes(x = fpr, y = tpr), data = roc[which(roc$model == 'Full'),],  size = 0.75, alpha = 0.5) + 
  labs(y = " \nTrue Positive Rate", x = "False Positive Rate\n ") + 
  theme_bw() +
  theme(panel.grid = element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(), 
        axis.title = element_text(size=10),
        text = element_text(size=10))

z2 <- ggplot(pf1) +  
  geom_abline(intercept = 0, slope = 1, linetype = 'solid', color = 'gray', size = 0.2) + 
  geom_point(aes(x = mean_pred_bench, y = mean_pred_full), size = 1, alpha = 0.5) + 
  labs(y = "Propensity Scores\nfrom Saturated Model", 
       x = "Propensity Scores\nfrom Benchmark Model") + 
  theme_bw() +
  theme(panel.grid = element_blank(), 
        axis.title = element_text(size=10),
        axis.ticks.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank())

## Save plots
z3 <- z1 + 
  annotate("text", x = 0.11, y = 0.13, size = unitpt(9), color = 'black',
           label = paste0("Benchmark AUC: ",auc.bench), hjust = 0) +
  annotate("text", x = 0.11, y = 0.04, size = unitpt(9), color = 'black',
           label = paste0("Saturated AUC: ",auc.full), hjust = 0)



fgfile <- paste0(root,paste0("Plots/fg3"))
figw <- 4; figh <- 2

tiff(paste0(fgfile, '.tif'), width = figw, height = figh, units = "in", res = 300,
     type = 'cairo', family = "Times")
grid.arrange(z3, z2, nrow = 1)
dev.off()

pdf(paste0(fgfile, '.pdf'), width = figw, height = figh, family = "Times")
grid.arrange(z3, z2, nrow = 1)
dev.off()

rm(list = ls()[which(!ls() %in% kobj)])

################################################################################
## Biased effects
################################################################################

## Compile Table C.2 (biased effects)

## Create a data frame
tp <- as.data.frame(ef)

mt <- (tp$treatment == 'jrepublican.jdemocrat')
mm <- (tp$mask == 'main' | grepl('biased',tp$mask))
my <- (tp$outcome == 'pro_defendant')

tp <- tp[mt & my & mm , ]

facs <- c('main', unique(tp$mask)[which(grepl('biased',unique(tp$mask)))])
tp$mask <- factor(tp$mask, levels = facs)

tp$model <- 0
for(i in seq(1,length(facs))){
  tp$model[tp$mask == facs[i]] <- i
}

fl <- paste0(root, "Tables/biased_effects.txt")
cat(paste0("\\begin{tabular}{l", 
           paste0(rep('c',max(tp$model)),collapse = ''), 
           "}\n\\hline\\hline\n & ",
           paste0(paste0('(',seq(1,max(tp$model)),')'), collapse = ' & '),
           "\\\\\\hline\\hline\n"), file=fl)

row <- rbind(t(format(round(tp$effect,3),nsmall=3)), t(format(round(tp$se,3),nsmall=3)))
row[1,] <- gsub('-(.+)', '--\\1~~', row[1,])
row[2,] <- gsub('(.+)', '(\\1)', row[2,])
cat(paste0("& \\multicolumn{", max(tp$model), "}{c}{\\textbf{", ovars[['pro_defendant']],"}} \\\\\n"), file=fl, append=TRUE)
cat(paste0("ARA Effect & ", paste0(row[1,], collapse = " & "), "\\\\\n"), file=fl, append=TRUE)
cat(paste0(" & ", paste0(row[2,], collapse = " & "), "\\\\\\hline\n"), file=fl, append=TRUE)

row1 <- rbind(t(tp$obs), t(tp$judges_treat), t(tp$judges_control), t(tp$fe_count), t(round(tp$df,2)))
row1 <- cbind(c("Observations", "Treatment Judges", "Control Judges", "Division-Years", "Degrees of Freedom"), row1)

cat(paste0("\\hline\n"), file=fl, append=TRUE)
for(r in 1:nrow(row1)){
  cat(paste0(paste0(row1[r,], collapse = ' & '), '\\\\\n'), file=fl, append=TRUE)  
}
cat(paste0("\\hline\\hline\n\\end{tabular}\n"), file=fl, append=TRUE)

## Plot the biased effects 
tp$mask <- factor(tp$mask, levels = c("main", "biased_not_settled", "biased_not_withdrawn", "biased_judgments", "biased_appealed"))
MAXY <- round(max(abs(tp$ciH),abs(tp$ciL)),3) + 0.001
z <- ggplot(tp, aes(x = effect)) +
  geom_hline(yintercept = 0, size = 0.5, linetype = "dashed") +
  geom_point(aes(x = mask, y = effect, shape = mask, color = mask, size = mask)) + 
  geom_linerange(aes(ymax = ciH, ymin = ciL, x = mask, color = mask), 
                 size = 0.5) + 
  labs(y = "ARA Effects", x = 'Case Subset') +
  theme_bw() +
  scale_x_discrete(limits = levels(tp$mask),
                   labels = rev(c("Appealed\nCases", "Cases Decided\nby Judgments", 
                                  "Cases Not\nWithdrawn", "Cases Not\nSettled", 
                                  "All Cases\n(Unbiased)")),
                   expand = c(0.1, 0.1)) + 
  scale_y_continuous(expand = c(0.01,0.01), 
                     limits = c(-MAXY, MAXY),
                     breaks = seq(-0.2,0.2,0.02)) + 
  scale_shape_manual(breaks = levels(tp$mask),
                     values = c(19,15,15,15,15)) +
  scale_size_manual(breaks = levels(tp$mask),
                    values = c(2,1.5,1.5,1.5,1.5)) +
  scale_color_manual(breaks = levels(tp$mask),
                     values = c('black','#808080','#808080','#808080','#808080')) +
  theme(title = element_text(size=10),
        plot.title = element_text(hjust = 0.5),
        legend.position = 'none',
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=10), # Tick labels
        axis.text.x = element_text(size=10, angle = 45, 
                                   lineheight = 0.75,
                                   hjust=1, vjust=1))


fgfile <- paste0(root, "Plots/fg5")
figw <- 3.2; figh <- 2.25

tiff(paste0(fgfile, '.tif'), width = figw, height = figh, units = "in", res = 300,
     type = 'cairo', family = "Times")
print(z)
dev.off()

pdf(paste0(fgfile, '.pdf'), width = figw, height = figh, family = "Times")
print(z)
dev.off()

rm(list = ls()[which(!ls() %in% kobj)])

################################################################################
## Judge-specific effects
################################################################################

## Plot Figure C.2

tp <- ef[grepl('^j[0-9]',ef$treatment),]
tp <- tp[order(tp$effect),]
tp$party <- ifelse(grepl('jrepublican',tp$treatment), 'Democratic Appointee', 'Republican Appointee')
tp$party <- factor(tp$party, levels = c( 'Republican Appointee', 'Democratic Appointee'))
tp$treatment <- factor(tp$treatment, levels = tp$treatment)

z <- ggplot(tp) +
  geom_hline(yintercept = 0, size = 0.5, linetype = "dashed") +
  geom_point(aes(x = treatment, y = effect, 
                 # shape = party, 
                 color = party), size = 0.5) +
  geom_linerange(aes(x = treatment, ymax = ciH, ymin = ciL, color = party), size = 0.2) +
  labs(y = "Judge Effects\n(Relative to Judges of Other Party)", x = 'Judges') +
  theme_bw() +
  scale_y_continuous(expand = c(0.01,0.01),
                     limits = c(-max(c(abs(tp$ciL), abs(tp$ciH))), max(c(abs(tp$ciL), abs(tp$ciH)))),
                     breaks = seq(-2,2,0.5)) +
  scale_x_discrete(expand = c(0.01,0.01)) +
  scale_color_manual(breaks = c('Republican Appointee', 'Democratic Appointee'),
                     labels=c('Republican Appointee', 'Democratic Appointee'),
                     name = "Party",
                     values = c('#FF0000', '#1E88E5')) +
  theme(legend.title = element_text(face = "bold", size=10),
        legend.text = element_text(size=10),
        legend.position = 'none',
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.y = element_text(size=10), # Tick labels
        axis.text.x = element_blank(), # Tick labels
        axis.title.x = element_blank(), axis.ticks.x = element_blank())


fgfile <- paste0(root,"Plots/judge_effects")
figw <- 6.5; figh <- 3

pdf(paste0(fgfile, '.pdf'), width = figw, height = figh, family = "Times")
print(z)
dev.off()

rm(list = ls()[which(!ls() %in% kobj)])

################################################################################
## Treatment assignment probabilities over time
################################################################################

## Plot Figure 6

## Treatment assignment probabilities over years (left panel)

tp <- summarize(group_by(df[maskm,], YEAR, jrepublican), countrep = n(), .groups = 'drop')
tp <- left_join(tp, summarize(group_by(df[maskm,], YEAR), count = n(), .groups = 'drop'),
                by = c('YEAR' = 'YEAR'))
tp$share <- tp$countrep / tp$count
tp$jrepublican <- factor(tp$jrepublican, levels = c(1,0))

z1 <- ggplot(tp) + 
  geom_area(aes(x = YEAR, y = share, fill = jrepublican), alpha=0.5) + 
  geom_line(aes(x = YEAR, y = share), data = tp[tp$jrepublican=="0",], size = 0.5, color = '#696969') + 
  scale_x_continuous(limits = c(1995,2016), 
                     breaks = seq(1996, 2016, 4)) + 
  scale_fill_manual(breaks = levels(tp$jrepublican),
                    labels = c('Republican Presidents', 'Democratic Presidents'),
                    name = "Cases Heard by Appointees of", 
                    values=c("#A9A9A9","#696969")) +
  theme_bw() + 
  geom_hline(yintercept = 0.5, size = 0.5, linetype = "dashed") +
  annotate("text", x = 2005, y = 0.75, label = "Cases Assigned to\nRepublican Appointees", lineheight = 0.75, size = unitpt(10)) + 
  annotate("text", x = 2005, y = 0.25, label = "Cases Assigned to\nDemocratic Appointees", lineheight = 0.75, size = unitpt(10)) + 
  labs(y = 'Proportion of All Civil\nRights Cases in Dataset', x = 'Year of Case Filing') + 
  theme(text = element_text(size=10),
        legend.position = 'none',
        legend.title = element_text(face = "bold", size=10),
        legend.text = element_text(size=10),
        panel.grid = element_blank(),
        axis.text.y = element_text(size=10), # Tick labels
        axis.text.x = element_text(size=10))

## Distribution of appointment years for Democratic and Republican appointees 
## (right panel)

tp <- df[maskm, ]

z2 <- ggplot(tp) + 
  geom_density(aes(x = jyears), data = tp[tp$jrepublican==0,], size = 0, fill = "#696969", alpha = 0.5) + 
  geom_density(aes(x = jyears), data = tp[tp$jrepublican==1,], size = 0, fill = '#A9A9A9', alpha = 0.3) + 
  geom_density(aes(x = jyears), data = tp[tp$jrepublican==0,], size = 0.4, color = 'black') + 
  geom_density(aes(x = jyears), data = tp[tp$jrepublican==1,], size = 0.4, color = 'black') + 
  labs(x = 'Years on Bench at Time of Filing', y = 'Density') + 
  annotate(geom = "curve", x = 8, y = 0.075, xend = 3, yend = 0.06, 
           curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 8, y = 0.075, 
           label = " Cases Assigned to \n Democratic Appointees", hjust = "left", lineheight = 0.75,
           size = unitpt(10)) + 
  annotate(geom = "curve", x = 18, y = 0.06, xend = 10, yend = 0.045, 
           curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 18, y = 0.06, 
           label = " Cases Assigned to \n Republican Appointees", hjust = "left", lineheight = 0.75,
           size = unitpt(10)) + 
  scale_x_continuous(breaks = seq(0,max(tp$jyears),4)) +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(), 
        axis.title = element_text(size=10),
        text = element_text(size=10),
        axis.text.x = element_text(size=10))


fgfile <- paste0(root, "Plots/fg6")
figw <- 6.5; figh <- 2

tiff(paste0(fgfile, '.tif'), width = figw, height = figh, units = "in", res = 300,
     type = 'cairo', family = "Times")
grid.arrange(z1, z2, nrow = 1, widths=unit(c(3.2,3.2), c("in", "in")))
dev.off()

pdf(paste0(fgfile, '.pdf'), width = figw, height = figh, family = "Times")
grid.arrange(z1, z2, nrow = 1, widths=unit(c(3.2,3.2), c("in", "in")))
dev.off()

rm(list = ls()[which(!ls() %in% kobj)])


################################################################################
## Adjacent president effects
################################################################################

## Compile Table C.3

mask1 <- ef$mask =="main" & ef$outcome =="pro_defendant"
mask2 <- ef$treatment %in% c('jobama.jbush43', 'jbush43.jclinton', 'jclinton.jbush41', 'jreagan.jcarter')
tp <- ef[mask1 & mask2, ]

## Convert president effects to ARA effects
tp$effect[tp$treatment == 'jobama.jbush43'] <- - tp$effect[tp$treatment == 'jobama.jbush43']
cis <- c(-tp$ciL[tp$treatment == 'jobama.jbush43'], -tp$ciH[tp$treatment == 'jobama.jbush43'])
tp$ciL[tp$treatment == 'jobama.jbush43'] <- min(cis)
tp$ciH[tp$treatment == 'jobama.jbush43'] <- max(cis)
tp$effect[tp$treatment == 'jclinton.jbush41'] <- - tp$effect[tp$treatment == 'jclinton.jbush41']
cis <- c(-tp$ciL[tp$treatment == 'jclinton.jbush41'], -tp$ciH[tp$treatment == 'jclinton.jbush41'])
tp$ciL[tp$treatment == 'jclinton.jbush41'] <- min(cis)
tp$ciH[tp$treatment == 'jclinton.jbush41'] <- max(cis)

tp$treatment <- factor(tp$treatment, c('jreagan.jcarter', 'jclinton.jbush41', 'jbush43.jclinton', 'jobama.jbush43'))

tp$model <- 0
tp$model[tp$treatment=='jreagan.jcarter'  ] <- 1
tp$model[tp$treatment=='jclinton.jbush41'] <- 2
tp$model[tp$treatment=='jbush43.jclinton'] <- 3
tp$model[tp$treatment=='jobama.jbush43'] <- 4
tp <- tp[order(tp$model),]

fl <- paste0(root, "Tables/adjacent_presidents.txt")
cat(paste0("\\begin{tabular}{l", 
           paste0(rep('c',max(tp$model)),collapse = ''), 
           "}\n\\hline\\hline\n & ",
           paste0(paste0('(',seq(1,max(tp$model)),')'), collapse = ' & '),
           "\\\\\\hline\\hline\n"), file=fl)

row <- rbind(t(format(round(tp$effect,3),nsmall=3)), t(format(round(tp$se,3),nsmall=3)))
row[1,] <- gsub('-(.+)', '--\\1~~', row[1,])
row[2,] <- gsub('(.+)', '(\\1)', row[2,])
cat(paste0("& \\multicolumn{", max(tp$model), "}{c}{\\textbf{", ovars[['pro_defendant']],"}} \\\\\n"), file=fl, append=TRUE)
cat(paste0("ARA Effect & ", paste0(row[1,], collapse = " & "), "\\\\\n"), file=fl, append=TRUE)
cat(paste0(" & ", paste0(row[2,], collapse = " & "), "\\\\\\hline\n"), file=fl, append=TRUE)

row1 <- rbind(t(tp$obs), t(tp$judges_treat), t(tp$judges_control), t(tp$fe_count), t(round(tp$df,2)))
row1 <- cbind(c("Observations", "Treatment Judges", "Control Judges", "Division-Years", "Degrees of Freedom"), row1)

cat(paste0("\\hline\n"), file=fl, append=TRUE)
for(r in 1:nrow(row1)){
  cat(paste0(paste0(row1[r,], collapse = ' & '), '\\\\\n'), file=fl, append=TRUE)  
}
cat(paste0("\\hline\\hline\n\\end{tabular}\n"), file=fl, append=TRUE)


## Plot Figure C.3
wave <- round(sum(tp$effect * (tp$obs/ef$obs[mask1 & ef$treatment == 'jrepublican.jdemocrat'][1])),3)

z <- ggplot(tp, aes(x = treatment)) +
  geom_hline(yintercept = 0, size = 0.5, linetype = "dashed") +
  geom_point(aes(x = treatment, y = effect)) + 
  geom_linerange(aes(x = treatment, ymax = ciH, ymin = ciL), size = 0.5) + 
  geom_hline(yintercept = wave, size = 0.5, linetype = "dotted") +
  annotate(geom = "curve", x = 3.5, y = -0.03, xend = 4.1, yend = wave-0.002,
           curvature = .2, arrow = arrow(length = unit(2, "mm"))) +
  annotate('text', x = 3.45, y = -0.03, label = paste0("Weighted Average: ", wave), hjust = 1, size = unitpt(10)) +
  labs(y = "ARA Effects") +
  theme_bw() +
  scale_x_discrete(limits = levels(tp$treatment),
                   labels = c('Carter and Reagan\nAppointees Only',
                              'G. H.W. Bush and Clinton\nAppointees Only',
                              'Clinton and G. W. Bush\nAppointees Only',
                              'G. W. Bush and Obama\nAppointees Only'),
                   expand = c(0.2, 0.2)) + 
  scale_y_continuous(expand = c(0.03,0.03)) +
  theme(legend.title = element_text(face = "bold", size=10),
        legend.text = element_text(size=10),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=10), # Tick labels
        axis.text.x = element_text(size=10, angle = 45, hjust=1, vjust=1), # Tick labels
        axis.title.x = element_blank())

fgfile <- paste0(root, "Plots/adjacent_presidents")
figw <- 4; figh <- 2.75

pdf(paste0(fgfile, '.pdf'), width = figw, height = figh, family = "Times")
print(z)
dev.off()

rm(list = ls()[which(!ls() %in% kobj)])

################################################################################
## Section 4.1: Comparing to Ninth Circuit
################################################################################

## Compile Table D.4 (appeals effects)
af <- read.csv(paste0(root, "Analysis/effects_appeals.csv"), stringsAsFactors = FALSE)

tp <- af
tp$model <- NA
tp$model[tp$estimator=='unadjusted'  ] <- 'Unadjusted'
tp$model[tp$estimator=='ipw'] <- 'IPW'
tp$model[tp$estimator=='aipw'] <- 'AIPW'
tp <- tp[order(tp$model, decreasing = TRUE),]

fl <- paste0(root, "Tables/appeals_effects.txt")
cat(paste0("\\begin{tabular}{l", 
           paste0(rep('c',length(unique(tp$model))),collapse = ''), 
           "}\n\\hline\\hline\n & ",
           paste0(unique(tp$model), collapse = ' & '),
           "\\\\\n & ",
           paste0(c('Estimate', 'Estimate', 'Estimate'), collapse = ' & '),
           "\\\\\\hline\\hline\n"), file=fl)

row <- rbind(t(format(round(tp$effect,3),nsmall=3)), t(format(round(tp$se,3),nsmall=3)))
row[1,] <- gsub('-(.+)', '--\\1~~', row[1,])
row[2,] <- gsub('(.+)', '(\\1)', row[2,])
cat(paste0("& \\multicolumn{", length(unique(tp$model)), "}{c}{\\textbf{", ovars[['pro_defendant']],"}} \\\\\n"), file=fl, append=TRUE)
cat(paste0("Treatment Effect & ", paste0(row[1,], collapse = " & "), "\\\\\n"), file=fl, append=TRUE)
cat(paste0(" & ", paste0(row[2,], collapse = " & "), "\\\\\\hline\n"), file=fl, append=TRUE)

row1 <- rbind(t(tp$obs), t(tp$panels_treat), t(tp$panels_control), t(tp$fe_count))
row1 <- cbind(c("Observations", "Treatment Panels", "Control Panels", "Division-Years"), row1)

cat(paste0("\\hline\n"), file=fl, append=TRUE)
for(r in 1:nrow(row1)){
  cat(paste0(paste0(row1[r,], collapse = ' & '), '\\\\\n'), file=fl, append=TRUE)  
}
cat(paste0("\\hline\\hline\n\\end{tabular}\n"), file=fl, append=TRUE)

## Plot Figure D.4

auc <- read.csv(paste0(root, 'Analysis/usca_balance_performance.csv'), colClasses = "character")
auc <- auc[auc$algorithm == 'my_ensemble' & auc$subset=='maj_rep-maj_rep', c('model', 'auc_tf_cv')]
auc$auc_tf_cv <- as.character(round(as.numeric(auc$auc_tf_cv),3))
auc$auc_tf_cv <- ifelse(nchar(auc$auc_tf_cv) == 4, paste0(auc$auc_tf_cv,'0'), auc$auc_tf_cv)
auc.full <- auc$auc_tf_cv[auc$model == 'full']
auc.bench <- auc$auc_tf_cv[auc$model == 'bench']

roc <- read.csv(paste0(root, 'Analysis/usca_balance_roc.csv'), colClasses = "character")
roc <- roc[which(roc$fpr!="fpr" & roc$algorithm=="my_ensemble" & roc$subset=='maj_rep-maj_rep'),]
roc <- roc[,c('fpr', 'tpr', 'model', 'subset')]
roc$fpr <- as.numeric(as.character(roc$fpr))
roc$tpr <- as.numeric(as.character(roc$tpr))
roc$model <- as.character(roc$model)
roc$model[which(roc$model == 'full')] <- "Full"
roc$model[which(roc$model == 'bench')] <- "Benchmark"
roc$model <- as.factor(roc$model)

pf <- read.csv(paste0(root, 'Analysis/usca_balance_predictions.csv'), stringsAsFactors = FALSE)
pf <- pf[pf$model %in% c('full','bench'),c('model','case_id','outcome','my_ensemble')]
pf$outcome <- as.integer(pf$outcome)
pf$my_ensemble <- as.numeric(pf$my_ensemble)
pf$quantile <- 0
for(m in c('full', 'bench')){
  pf$quantile[which(pf$model == m)] <- round(ecdf(pf$my_ensemble[which(pf$model == m)])(pf$my_ensemble[which(pf$model == m)]),2)
}
pf1 <- as.data.frame(summarize(group_by(pf, model, quantile), mean_prediction = mean(my_ensemble)))

pf1 <- left_join(pf1[which(pf1$model=="full"),c('quantile','mean_prediction')], 
                 pf1[which(pf1$model=="bench"),c('quantile','mean_prediction')],
                 by = c('quantile' = 'quantile'))
colnames(pf1) <- c('percentile', 'mean_pred_full', 'mean_pred_bench')

## Make plots
z1 <- ggplot(roc) +  
  ylim(ymin=0,ymax=1)+
  xlim(xmin=0,xmax=1)+
  geom_abline(intercept = 0, slope = 1, linetype = 'solid', color = 'gray', size = 0.2) + 
  geom_line(aes(x = fpr, y = tpr), data = roc[which(roc$model == 'Benchmark'),],  size = 0.50, linetype = 'dashed', alpha = 0.9) + 
  geom_line(aes(x = fpr, y = tpr), data = roc[which(roc$model == 'Full'),],  size = 0.75, alpha = 0.5) + 
  labs(y = " \nTrue Positive Rate", x = "False Positive Rate\n ") + 
  theme_bw() +
  theme(panel.grid = element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(), 
        axis.title = element_text(size=10),
        text = element_text(size=10))

z2 <- ggplot(pf1) +  
  geom_abline(intercept = 0, slope = 1, linetype = 'solid', color = 'gray', size = 0.2) + 
  geom_point(aes(x = mean_pred_bench, y = mean_pred_full), size = 1, alpha = 0.5) + 
  labs(y = "Propensity Scores\nfrom Saturated Model", 
       x = "Propensity Scores\nfrom Benchmark Model") + 
  theme_bw() +
  theme(panel.grid = element_blank(), 
        axis.title = element_text(size=10),
        axis.ticks.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank())

z3 <- z1 + 
  annotate("text", x = 0.15, y = 0.11, size = unitpt(10), color = 'black',
           label = paste0("Benchmark AUC: ",auc.bench), hjust = 0) +
  annotate("text", x = 0.15, y = 0.04, size = unitpt(10), color = 'black',
           label = paste0("Saturated AUC: ",auc.full), hjust = 0)
pdf(paste0(root,paste0("Plots/roc_eqq_usca.pdf")), width=4, height=2, family = "Times")
grid.arrange(z3, z2, nrow = 1)
dev.off()

rm(z1, z2, z3, auc, roc, pf)

################################################################################
## Over time effect
################################################################################

## Compile Table E.5 (overtime effects)

my <- (ef$outcome == 'pro_defendant')
mm <- grepl("^(cl|bush43|ob)",ef$mask)
mt <- grepl("^(jrepublican[.]jdemocrat|.+[.]jclinton)",ef$treatment)

tp <- ef[my & mm & mt,]
tp$mask <- factor(tp$mask, levels = c('clinton','bush43','obama'))

tr.brks <- c("jrepublican.jdemocrat", "jreagan.jclinton", "jbush41.jclinton", "jbush43.jclinton", "jobama.jclinton")
tr.labs <- c('All Appointees', 'Reagan and Clinton Appointees', 'G. H.W. Bush and Clinton Appointees', 'G. W. Bush and Clinton Appointees', 'Obama and Clinton Appointees')

tp$treatment <- factor(tp$treatment, levels = tr.brks)

tp$model <- 0
tp$model[tp$mask == 'clinton'] <- 1
tp$model[tp$mask == 'bush43'] <- 2
tp$model[tp$mask == 'obama'] <- 3

tp <- tp[order(tp$model),]

tvars <- vector(mode = "list", length = 0)
tvars[["jrepublican.jdemocrat"]] <- 'All Appointees'
tvars[["jreagan.jclinton"]] <- 'Reagan and Clinton Appointees'
tvars[["jbush41.jclinton"]] <- 'G. H.W. Bush and Clinton Appointees'
tvars[["jbush43.jclinton"]] <- 'G. W. Bush and Clinton Appointees'
tvars[["jobama.jclinton"]] <- 'Obama and Clinton Appointees'

fl <- paste0(root, "Tables/over_time_effects.txt")
cat('', file=fl)

for(t in names(tvars)){
  tbl <- paste0("\\begin{tabular}{l", 
                paste0(rep('c',max(tp$model)),collapse = ''), 
                "}\n")
  # cat(paste0("\\multicolumn{", max(tp$model) + 1, "}{c}{~} \\\\\n"), file=fl, append=TRUE)
  # cat(, file=fl, append=TRUE)
  tbl <- paste0(tbl, 
                paste0("\\multicolumn{", max(tp$model) + 1, "}{c}{\\textit{\\normalsize ",tvars[[t]],"}} \\\\\n"))
  
  # cat(, file=fl, append=TRUE)
  tbl <- paste0(tbl, paste0("\\hline\\hline\n & ",
                            paste0(paste0('~~~~~~(',seq(1,max(tp$model)),')~~~~~~'), 
                                   collapse = ' & '),
                            "\\\\\\hline\\hline\n"))
  
  tf <- tp[tp$treatment == t,]
  row <- rbind(t(format(round(tf$effect,3),nsmall=3)), t(format(round(tf$se,3),nsmall=3)))
  row9 <- rbind(t(tf$obs), t(tf$judges_treat), t(tf$judges_control), t(tf$fe_count), t(round(tf$df,2)))
  if(ncol(row) == 1){
    row <- cbind(c('NA', 'NA'), c('NA', 'NA'), row)
    row9 <- cbind(rep('NA', nrow(row9)), rep('NA', nrow(row9)), row9)
  } else if(ncol(row) == 2){
    row <- cbind(c('NA', 'NA'), row)
    row9 <- cbind(rep('NA', nrow(row9)), row9)
  }
  row9 <- cbind(c("Observations", "Treatment Judges", "Control Judges", "Division-Years", "Degrees of Freedom"), row9)
  
  row[1,] <- gsub('-(.+)', '--\\1~~', row[1,])
  row[2,] <- gsub('(.+)', '(\\1)', row[2,])
  
  tbl <- paste0(tbl, paste0("& \\multicolumn{", max(tp$model), "}{c}{\\textbf{", 
                            ovars[['pro_defendant']],"}} \\\\\n"))
  row1 <- gsub(' *[(]? *NA *[)]? *',' --- ',paste0("ARA Effect & ", paste0(row[1,], collapse = " & "), "\\\\\n"))
  
  tbl <- paste0(tbl, row1)
  row2 <- gsub(' *[(]? *NA *[)]? *',' --- ',paste0(" & ", paste0(row[2,], collapse = " & "), "\\\\\\hline\n"))
  
  tbl <- paste0(tbl, row2)
  
  for(r in 1:nrow(row9)){
    tbl <- paste0(tbl, paste0(paste0(gsub(' *[(]? *NA *[)]? *',' 0 ',row9[r,]), 
                                     collapse = ' & '), '\\\\\n'))
  }
  tbl <- paste0(tbl, paste0("\\hline\\hline\n",paste0("\\multicolumn{", max(tp$model) + 1, "}{c}{~} \\\\\n"),"\\end{tabular}"))

  if(t %in% c("jreagan.jclinton", "jbush43.jclinton")){ ## Left tables
    cat(paste0(tbl,'~~~~~~~~~~'), file=fl, append=TRUE)
  } else {
    cat(paste0(tbl,'\n\n\n'), file=fl, append=TRUE)
  } 
  rm(tf, row, row1, row2)
}


## Plot Figure 7

z <- ggplot(tp) +
  geom_hline(yintercept = 0, size = 0.5, linetype = "dashed") +
  geom_line(aes(x = mask, y = effect, group = treatment), data = tp[tp$treatment=='jrepublican.jdemocrat',], position=position_nudge(-0.17), 
            alpha = 0.5) + 
  geom_point(aes(x = mask, y = effect, shape = treatment, size = treatment), position=position_dodge(0.5)) +
  geom_linerange(aes(x = mask, ymax = ciL, ymin = ciH, group = treatment),
                 size = 0.5,
                 position=position_dodge(0.5)) +
  labs(y = "ARA Effects", x = "Case Filing Year") +
  theme_bw() +
  scale_x_discrete(limits = c('clinton', 'bush43', 'obama'), 
                   labels = c('1995-2000', '2001-2008', '2009-2016'),
                   expand = c(0.2, 0.2)) + 
  scale_shape_manual(breaks = tr.brks,
                     labels= tr.labs,
                     name = "Comparison Groups",
                     values = c(19, 0, 2, 5, 4)) +
  scale_size_manual(breaks = tr.brks,
                    labels=tr.labs,
                    name = "Comparison Groups",
                    values = c(3, 1.5, 1.5, 1.5, 1.5)) +
  theme(legend.title = element_text(face = "bold", size=10),
        legend.text = element_text(size=10),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.y = element_text(size=10), # Tick labels
        axis.text.x = element_text(size=10, hjust=0.5))

fgfile <- paste0(root, "Plots/fg7")
figw <- 6.5; figh <- 2

tiff(paste0(fgfile, '.tif'), width = figw, height = figh, units = "in", res = 300,
     type = 'cairo', family = "Times")
print(z)
dev.off()

pdf(paste0(fgfile, '.pdf'), width = figw, height = figh, family = "Times")
print(z)
dev.off()

rm(list = ls()[which(!ls() %in% kobj)])

## Plot Figure 8

tp <- summarize(group_by(df[maskm,], QUARTER), R = mean(jrepublican))
tp <- cbind(tp, summarize(group_by(df[maskm,], QUARTER), C = n())[,2])

## What is the correlation between variables?
varcorr <- round(cor(tp$R, tp$C, method = 'pearson'),2)

## Some plotting spacers and labels
coeff <- max(tp$C)/max(tp$R)
xlaby <- unique(tp$QUARTER)
xlaby <- xlaby[grepl("Q1",xlaby)]
xlaby1 <- gsub('Q1', '', xlaby)

z <- ggplot(tp) + 
  geom_point(aes(x = QUARTER, y = C/coeff), shape = 19, size = 2) +
  geom_point(aes(x = QUARTER, y = R), shape = 0, size = 2) +
  scale_y_continuous(name = "Share of Cases Heard by\nRepublican Appointees (squares)\n ", 
                     breaks = seq(0.3,0.7,0.1),
                     limits = c(0.295, 0.705),
                     sec.axis = sec_axis(~.*coeff, name="Quarterly Case Filings (circles)\n "), expand = c(0,0)) + 
  scale_x_discrete(breaks = xlaby, labels = xlaby1) + 
  annotate("rect", xmin = 0, xmax = 25, ymin = 0.295, ymax = 0.705,
           alpha = .2) +
  annotate("text", x = 13.3, y = 0.67, size = unitpt(10), color = 'black',
           label = "Clinton Presidency") +
  annotate("rect", xmin = 25, xmax = 57, ymin = 0.295, ymax = 0.705,
           alpha = .5) +
  annotate("text", x = 41, y = 0.67, size = unitpt(10), color = 'black',
           label = "G. W. Bush Presidency") +
  annotate("rect", xmin = 57, xmax = 89, ymin = 0.295, ymax = 0.705,
           alpha = .2) +
  annotate("text", x = 73, y = 0.67, size = unitpt(10), color = 'black',
           label = "Obama Presidency") +
  annotate("text", x = 60, y = 0.33, size = unitpt(10), color = 'black',
           label = bquote("(" * italic("Note: Pearson's correlation coefficient is ") * .(varcorr) * ")")) +
  labs(x = "") + 
  theme_bw() + 
  theme(legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.position = 'none',
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=10), # Tick labels
        axis.text.x = element_text(size=10,angle = 45,vjust=1,hjust=1), # Tick labels
        axis.title.x = element_blank())


fgfile <- paste0(root, "Plots/fg8")
figw <- 6.5; figh <- 2.6

tiff(paste0(fgfile, '.tif'), width = figw, height = figh, units = "in", res = 300,
     type = 'cairo', family = "Times")
print(z)
dev.off()

pdf(paste0(fgfile, '.pdf'), width = figw, height = figh, family = "Times")
print(z)
dev.off()

rm(list = ls()[which(!ls() %in% kobj)])