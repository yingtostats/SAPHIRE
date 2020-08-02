## IMPORTANT: Please set code_root variable properly. 
## code_root should be set to the directory where the repository README file is located. 
## For more information, please read the repository README file
code_root="/Users/ying/Desktop/SAPHIRE/"

setwd(paste0(code_root, "scripts_main"))
library(BayesianTools)
library(vioplot)
library("corrplot")
library(readr)
library(cairoDevice)

##
source(paste0(code_root, "R/fun_SEIRpred.R"))
source(paste0(code_root, "R/fun_SEIRsimu.R"))
source(paste0(code_root, "R/fun_SEIRfitting.R"))
source(paste0(code_root, "R/init_cond.R"))
source(paste0(code_root, "R/fun_R0estimate.R"))
source(paste0(code_root, "R/correlationPlot_modified.R"))
source(paste0(code_root, "R/fun_SEIRplot.R"))
source(paste0(code_root, "R/fun_Findzero.R"))
##

init_settings=get_init_sets_list(r0 = 0.23)

pars_estimate = read.table("../output/pars_est_run_main_analysis.txt",header=TRUE,sep='\t')
head(pars_estimate)

file_name = "main_analysis"

panel_B_R_ylim=4


init_settings$days_to_fit <- 1:68
library(vioplot)
##
onset_obs_all <- init_settings$daily_new_case_all
ptime <- 1:length(onset_obs_all)
mydate <- c(paste("Jan", 1:31), paste("Feb", 1:29), paste("Mar", 1:8))
#
pdf(paste0("../output/Figure_", file_name, ".pdf"), width = 9, height = 10)
par(mar = c(4, 5, 2.5, 1))
layout(matrix(c(1:6), byrow = T, nrow = 3))

##########################################   Panel A  ##########################################################
estN_mat <- apply(pars_estimate, 1, function(x) SEIRsimu(pars = x, init_settings = init_settings, num_periods = 5)[, "Onset_expect"])
estN_mean <- round(apply(estN_mat, 1, mean), 0)
estN_up <- round(apply(estN_mat, 1, function(x) quantile(x, 0.975)), 0)
estN_low <- round(apply(estN_mat, 1, function(x) quantile(x, 0.025)), 0)
########################################## Exponential Growth Rate #############################################
range = 1:23
y = log(c(estN_mat[range,]))
t = rep(c(range), length(y)/length(range))

fit = lm(y~t)
summary(fit)

### Est. Growth Rate = 0.188