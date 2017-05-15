# helper_functions.R

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

# Helper function to automate the estimation of proportions in the population
race_extract <- function(race_name, design) {
  m <- svymean(~I(race_r == race_name), design) # use if only 3 race categories used - white, black, other
#  m <- svymean(~I(race_ == race_name), design) # use if all race categories used
  print("======================================================")
  print(race_name)
  print(m)
  print(confint(m))
}
