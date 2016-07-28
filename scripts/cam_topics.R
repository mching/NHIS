# cam_topics.R
# 
# This file identifies the frequency of cam use and use in children with
# specific health needs including developmental delay, autism, etc.

# # Prerequisites:
# Download childcam and samchild files and convert to RDA using this script:
# https://github.com/mching/asdfree/blob/master/National%20Health%20Interview%20Survey/personsx%20plus%20samadult%20with%20multiple%20imputation%20-%20analyze.R
# 
# You also need to load and merge the sample child and child cam files using this script:
# https://github.com/mching/NHIS/blob/master/scripts/load_and_merge_data.R
# 
# You also need to run the demographics script to create some of the recoded disability and other demographic variables
# https://github.com/mching/NHIS/blob/master/scripts/demographics.R

# Helper functions
recode_yes_no_NA <- function(varname) {
  # takes variable then recodes it as 1, 2, with all else as NA
  tmpvar <- ifelse(varname >2, NA, varname)
  return(tmpvar)
}

tables_orig_recoded <- function(varname, recoded_varname) {
  # takes variable and recoded variable and gets frequency, cross-tab
  print(deparse(substitute(recoded_varname)))
  print(table(recoded_varname, useNA = "if"))
  print(prop.table(table(recoded_varname, useNA = "if")))
  print(table(Recoded = recoded_varname, Original = varname, useNA = "if"))
}

tot_cost_recode <- function(varname) {
  # recodes cost variable to make the Refused (99997), 
  # not ascertained (99998)
  # or DK values (99999) to NA
  tmpvar <- ifelse(varname > 90000, NA, varname)
  return(tmpvar)
}

avg_cost_recode <- function(varname) {
  # recodes cost variable to make the refused (997)
  # not ascertained (998) 
  # or DK values (999) to NA
  tmpvar <- ifelse(varname > 990, NA, varname)
  return(tmpvar)
}

tablen <- function(varname, ...) {
  return(table(varname, useNA = "if"))
}
xtablen <- function(var1, var2, ...) {
  return(table(var1, var2, useNA = "if"))
}

survey_total_mean <- function(varname) {
  # adds the variable to the survey object and reads out the survey total and proportion of the variable
  
}

#############################
# Chiropractic              #
#############################
# Ever used: cco_use
table(x.sa$cco_use, useNA = "if")
x.sa$cco_use_r <- ifelse(x.sa$cco_use > 2, NA, x.sa$cco_use)
table(x.sa$cco_use_r, x.sa$cco_use, useNA = "if")
prop.table(table(x.sa$cco_use_r))
table(chiro = x.sa$cco_use_r, asd = x.sa$ccondl6r_r, useNA = "if") # overlap Chiro & ASD

# Last 12 months used: cco_usm
x.sa$cco_usm_r <- recode_yes_no_NA(x.sa$cco_usm)
tables_orig_recoded(x.sa$cco_usm, x.sa$cco_usm_r)

# Knows exact # of times visited in 12 months
table(x.sa$cco_ptim, useNA = "if")
# Exact # of times
table(x.sa$cco_tmno, useNA = "if")
# Cross between these
with(x.sa, table(cco_ptim, cco_tmno, useNA = "if"))

# Approximate # of times visited in 12 months
table(x.sa$cco_tmct)
approx_freq <- c("1", "2-5", "6-10", "11-15", "16-20", "21-25", ">25", "Refused", "Not ascertained", "Don't Know")
x.sa$cco_tmct <- factor(x.sa$cco_tmct, levels = 1:10, labels = approx_freq)
table(x.sa$cco_tmct)

# Cross between exact and approximate number of times visited in 12 mos
# This shows that there is no overlap between the exact and approximate visit times
table(x.sa$cco_tmct, x.sa$cco_tmno, useNA = "if")

# Costs
# Total amount paid known
table(x.sa$cco_hit, useNA = "if")
# Total amount paid
x.sa$cco_hits <- tot_cost_recode(x.sa$cco_hits)
table(dollars = x.sa$cco_hits, exact_cost_known = x.sa$cco_hit, useNA = "if")
# Average amount paid known
tablen(x.sa$cco_avgc)
# Average amount paid per visit
tablen(x.sa$cco_avgs)
xtablen(x.sa$cco_avgs, x.sa$cco_avgc)

# Cross tab of total vs average shows that the ones who say they know the total
# were not asked the average. It also shows that the ones who don't know the
# total, 13 know the average and 30 don't know either total or average.
xtablen(x.sa$cco_avgc, x.sa$cco_hit)

# Get the total amount spent for those knowing the total
tablen(x.sa$cco_hits)
cam.design <- update(cam.design, cco_total = x.sa$cco_hits)
svytotal(~cco_total, cam.design, na.rm = T)
confint(svytotal(~cco_total, cam.design, na.rm = T))

# Get the amount spent by those who know exactly how many visits
x.sa$cco_avgs <- avg_cost_recode(x.sa$cco_avgs)
table(cost_per_visit = x.sa$cco_avgs, visits = x.sa$cco_tmno)
x.sa$cco_est_total <- x.sa$cco_avgs
