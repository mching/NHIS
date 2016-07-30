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
#   tmpvar <- factor(tmpvar, labels = c(1,2), levels = c("yes", "no"))
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
x.sa$cco_est_total <- x.sa$cco_avgs * x.sa$cco_tmno
tablen(x.sa$cco_est_total)
# Only 7 individuals were represented this way
tablen(x.sa$cco_avgs)
# There are only 14 total individuals who reported average per visit costs for cco
sum(table(x.sa$cco_avgs))

# How much was for ASD
svyby(~cco_total, by = ~ASD_, FUN = svytotal, design = cam.design, na.rm = T)

# Other DD
svyby(~cco_total, by = ~Other_DD_, FUN = svytotal, design = cam.design, na.rm = T)


#############################
# Supplements               #
#############################
# Multivitamin
# Ever used: cvt_use
x.sa$cvt_use_r <- recode_yes_no_NA(x.sa$cvt_use)
tables_orig_recoded(x.sa$cvt_use, x.sa$cvt_use_r)
# 12 month use
x.sa$cvt_usm_r <- recode_yes_no_NA(x.sa$cvt_usm)
tables_orig_recoded(x.sa$cvt_usm, x.sa$cvt_usm_r)
# ASD overlap
table(MV_ever = x.sa$cvt_use_r, asd = x.sa$ccondl6r_r, useNA = "if")
table(MV_12mo = x.sa$cvt_usm_r, asd = x.sa$ccondl6r_r, useNA = "if")
# ID overlap
table(MV_ever = x.sa$cvt_use_r, id = x.sa$intdis_r, useNA = "if")
table(MV_12mo = x.sa$cvt_usm_r, id = x.sa$intdis_r, useNA = "if")
# DD overlap
table(MV_ever = x.sa$cvt_use_r, dd = x.sa$othdd, useNA = "if")
table(MV_12mo = x.sa$cvt_usm_r, dd = x.sa$othdd, useNA = "if")
# ASD, DD, ID overlap
table(MV_ever = x.sa$cvt_use_r, combined = isASDDDID, useNA = "if")
table(MV_12mo = x.sa$cvt_usm_r, combined = isASDDDID, useNA = "if")


# Vitamin A, B, C, D, E, H, or K overlap with multivitamin
tablen(x.sa$cvt_abev)
xtablen(x.sa$cvt_abev, x.sa$cvt_use)
# Ever used: cvt_abev
x.sa$cvt_abev_r <- recode_yes_no_NA(x.sa$cvt_abev)
tables_orig_recoded(x.sa$cvt_abev, x.sa$cvt_abev_r)
# 12 month use: cvt_abum
x.sa$cvt_abum_r <- recode_yes_no_NA(x.sa$cvt_abum)
tables_orig_recoded(x.sa$cvt_abum, x.sa$cvt_abum_r)
# ASD overlap
table(ABC_ever = x.sa$cvt_abev_r, asd = x.sa$ccondl6r_r, useNA = "if")
table(ABC_12mo = x.sa$cvt_abum_r, asd = x.sa$ccondl6r_r, useNA = "if")
# ID overlap
table(ABC_ever = x.sa$cvt_abev_r, id = x.sa$intdis_r, useNA = "if")
table(ABC_12mo = x.sa$cvt_abum_r, id = x.sa$intdis_r, useNA = "if")
# DD overlap
table(ABC_ever = x.sa$cvt_abev_r, dd = x.sa$othdd, useNA = "if")
table(ABC_12mo = x.sa$cvt_abum_r, dd = x.sa$othdd, useNA = "if")

# Ca, Mg, Fe, Cr, Zn, Se, K
# Overlap with multivitamin
xtablen(x.sa$cvt_use, x.sa$cvt_caev)
# Ever used: cvt_caev
x.sa$cvt_caev_r <- recode_yes_no_NA(x.sa$cvt_caev)
tables_orig_recoded(x.sa$cvt_caev, x.sa$cvt_caev_r)
# 12 month use: cvt_caum
x.sa$cvt_caum_r <- recode_yes_no_NA(x.sa$cvt_caum)
tables_orig_recoded(x.sa$cvt_caum, x.sa$cvt_caum_r)
# ASD overlap
table(mineral_ever = x.sa$cvt_caev_r, asd = x.sa$ccondl6r_r, useNA = "if")
table(mineral_12mo = x.sa$cvt_caum_r, asd = x.sa$ccondl6r_r, useNA = "if")
# ID overlap
table(mineral_ever = x.sa$cvt_caev_r, id = x.sa$intdis_r, useNA = "if")
table(mineral_12mo = x.sa$cvt_caum_r, id = x.sa$intdis_r, useNA = "if")
# DD overlap
table(mineral_ever = x.sa$cvt_caev_r, dd = x.sa$othdd, useNA = "if")
table(mineral_12mo = x.sa$cvt_caum_r, dd = x.sa$othdd, useNA = "if")

# Herbs
# Ever used: chb_evr
x.sa$chb_evr_r <- recode_yes_no_NA(x.sa$chb_evr)
tables_orig_recoded(x.sa$chb_evr, x.sa$chb_evr_r)
# 12 month use: chb_usm
x.sa$chb_usm_r <- recode_yes_no_NA(x.sa$chb_usm)
tables_orig_recoded(x.sa$chb_usm, x.sa$chb_usm_r)
# ASD overlap
table(herb_ever = x.sa$chb_evr_r, asd = x.sa$ccondl6r_r, useNA = "if")
table(herb_12mo = x.sa$chb_usm_r, asd = x.sa$ccondl6r_r, useNA = "if")
# ID overlap
table(herb_ever = x.sa$chb_evr_r, id = x.sa$intdis_r, useNA = "if")
table(herb_12mo = x.sa$chb_usm_r, id = x.sa$intdis_r, useNA = "if")
# DD overlap
table(herb_ever = x.sa$chb_evr_r, dd = x.sa$othdd, useNA = "if")
table(herb_12mo = x.sa$chb_usm_r, dd = x.sa$othdd, useNA = "if")

# Fish oil
tablen(x.sa$chblst09)
xtablen(x.sa$chblst09, x.sa$ccondl6r_r)

# Melatonin
tablen(x.sa$chblst15)
xtablen(x.sa$chblst15, x.sa$ccondl6r_r)

# Costs for vitamins or minerals (last purchase)
x.sa$cvt_cst1_r <- ifelse(x.sa$cvt_cst1 > 9990, NA, x.sa$cvt_cst1)
tablen(x.sa$cvt_cst1_r)
cam.design <- update(cam.design, vitamin_total = x.sa$cvt_cst1_r)

# Frequency of purchase
x.sa$chb_bofn_r <- avg_cost_recode(x.sa$chb_bofn)
tablen(x.sa$chb_bofn) # number of times

x.sa$chb_boft_r <- ifelse(x.sa$chb_boft > 3, NA, x.sa$chb_boft)
# per week times 52, per month times 12, per year times 1
x.sa$chb_boft_r <- factor(x.sa$chb_boft_r, levels = 0:3, 
                          labels = c("never", "per week", "per month", "per year"))
tablen(x.sa$chb_boft_r)
x.sa$vit_purch_multipler <- rep(NA, nrow(x.sa))
tablen(x.sa$chb_boft_r)
for(i in 1:nrow(x.sa)) {
  if(!is.na(x.sa$chb_boft_r[i])) {
    if(x.sa$chb_boft_r[i] == "never") x.sa$vit_purch_multipler[i] <- 0
    else if(x.sa$chb_boft_r[i] == "per week") x.sa$vit_purch_multipler[i] <- 52
    else if(x.sa$chb_boft_r[i] == "per month") x.sa$vit_purch_multipler[i] <- 12
    else if(x.sa$chb_boft_r[i] == "per year") x.sa$vit_purch_multipler[i] <- 1
  }
}
xtablen(x.sa$vit_purch_multipler, x.sa$chb_boft_r)

# We can multiply the frequency of purchase per year by last purchase to get 
# annual spending with the caveat that the last purchase may not be
# representative of the year
x.sa$vitamin_annual_total <- x.sa$vit_purch_multipler * x.sa$cvt_cst1_r
cam.design <- update(cam.design, vitamin_annual_total = x.sa$vitamin_annual_total)

# Total $ spent on vitamins and minerals per year
svyby(~vitamin_annual_total, by = ~isASD_, FUN = svytotal, design = cam.design, na.rm = T)
svyby(~vitamin_annual_total, by = ~isASD_, FUN = svymean, design = cam.design, na.rm = T)

svyby(~vitamin_annual_total, by = ~isDD_, FUN = svytotal, design = cam.design, na.rm = T)
svyby(~vitamin_annual_total, by = ~isDD_, FUN = svymean, design = cam.design, na.rm = T)

# Something is off here... How come the ID population says only $9 per year?
svyby(~vitamin_annual_total, by = ~isID_, FUN = svytotal, design = cam.design, na.rm = T)
svyby(~vitamin_annual_total, by = ~isID_, FUN = svymean, design = cam.design, na.rm = T)

svyby(~vitamin_annual_total, by = ~isASDDDID_, FUN = svytotal, design = cam.design, na.rm = T)
svyby(~vitamin_annual_total, by = ~isASDDDID_, FUN = svymean, design = cam.design, na.rm = T)
