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
  print(table(recoded_varname, useNA = "if"))
  print(prop.table(table(recoded_varname), useNA = "if"))
  print(table(Recoded = recoded_varname, Original = varname, useNA = "if"))
}

survey_total_mean <- function(varname) {
  # adds the variable to the survey object and reads out the survey total and proportion of the variable
  
}

# Chiropractic
# Ever used: cco_use
table(x.sa$cco_use, useNA = "if")
x.sa$cco_use_r <- ifelse(x.sa$cco_use > 2, NA, x.sa$cco_use)
table(x.sa$cco_use_r, x.sa$cco_use, useNA = "if")
table(chiro = x.sa$cco_use_r, asd = x.sa$ccondl6r_r, useNA = "if")
prop.table(table(x.sa$cco_use_r))

# Last 12 months used: cco_usm
