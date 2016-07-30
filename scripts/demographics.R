# # demographics.R
#
# This file creates estimates for demographic info from the child and CAM merged dataset
#
# # Prerequisites:
# Download childcam and samchild files and convert to RDA using this script:
# https://github.com/mching/asdfree/blob/master/National%20Health%20Interview%20Survey/personsx%20plus%20samadult%20with%20multiple%20imputation%20-%20analyze.R
# 
# You also need to load and merge the sample child and child cam files using this script:
# https://github.com/mching/NHIS/blob/master/scripts/load_and_merge_data.R
# 

# The load_and_merge file creates a survey design object called cam.design
# This is replicated here for convenient access 
cam.design <- 
  svydesign(
    id = ~psu_p , 
    strata = ~strat_p ,
    nest = TRUE ,
    weights = ~wtfa_sc ,	# note the change in the weighting variable
    data = x.sa				# note the change in the source data frame
  )

# Number of children
nrow(x.sa)

svytotal(
  ~one ,
  cam.design 
)

# Gender breakdown
cam.design <- update(cam.design, sex_f_ = factor(x.sa$sex, labels = c("male", "female")))

table(x.sa$sex)
prop.table(table(x.sa$sex))

svytotal(~sex_f_, design = cam.design)
confint(svytotal(~sex_f_, design = cam.design))

svymean(~I(sex_f_ == "male"), cam.design)
confint(svymean(~I(sex_f_ == "male"), cam.design))

# Age breakdown
table(x.sa$age_p)
prop.table(table(x.sa$age_p))

svymean(~age_p, cam.design)
svyquantile(~age_p, cam.design, quantiles=c(0.25,0.5,0.75))

# Race
race_names <- c("White only",
                "Black/African American only",
                "AIAN only", 
                "Asian only",
                "Race group not releasable",
                "Multiple race")
cam.design <-update(cam.design,
                    race_ = factor(x.sa$racerpi2, labels = race_names))

table(factor(x.sa$racerpi2, labels = race_names))
prop.table(table(factor(x.sa$racerpi2, labels = race_names)))

svytotal(~race_, cam.design)

# Helper function to automate the estimation of proportions in the population
race_extract <- function(race_name) {
  m <- svymean(~I(race_ == race_name), cam.design)
  print("======================================================")
  print(race_name)
  print(m)
  print(confint(m))
}

sapply(race_names, race_extract)

# Hispanic
table(x.sa$hispan_i, useNA = "ifany")
x.sa$hispan_TF <- ifelse(x.sa$hispan==12, "Not Hispanic", "Hispanic")
x.sa$hispan_TF <- factor(x.sa$hispan_TF)
table(x.sa$hispan_TF, useNA = "ifany")
prop.table(table(x.sa$hispan_TF, useNA = "ifany"))

cam.design <- update(cam.design, hispanic_ = x.sa$hispan_TF)

svytotal(~hispanic_, cam.design)
confint(svytotal(~hispanic_, cam.design))

svymean(~I(hispanic_ == "Hispanic"), cam.design)
confint(svymean(~I(hispanic_ == "Hispanic"), cam.design))

# Diagnosis breakdown

# ADHD
table(x.sa$add2, useNA = "if")
x.sa$add2_r <- ifelse(x.sa$add2 > 2, NA, x.sa$add2)
table(x.sa$add2, x.sa$add2_r, useNA = "if")

cam.design <- update(cam.design,
                     ADHD_ = factor(x.sa$add2_r, labels = c("ADHD", "no ADHD")))

# This does not totally work yet. Object is not found when trying to do the ratios
svy_total_prop_CI <- function(var, true_label, design, ...) {
  print(svytotal(as.formula(paste("~", var)), design, ...))
  print(confint(svytotal(as.formula(paste("~", var)), design, ...)))
  
#  print(svymean(~I(as.formula(var) == true_label), design, ...))
#  print(confint(svymean(~I(as.formula(var) == true_label), design, ...)))
}

# Number and percentage of children with ADHD
svy_total_prop_CI("ADHD_", "ADHD", cam.design, na.rm = T)
svymean(~I(ADHD_ == "ADHD"), cam.design, na.rm = T)
confint(svymean(~I(ADHD_ == "ADHD"), cam.design, na.rm = T))

# Intellectual disability
# amr1r is ID < 1 year and amr2r is ID > 1 year
# Create variable to combine these two variables
x.sa$intdis <- x.sa$amr1r

# Show that there is no overlap between amr1r and amr2r 
table(x.sa$amr1r, x.sa$amr2r, useNA = "if")

# Go through each row in amr2r and if it is not NA, copy the value to intdis
for(i in 1:nrow(x.sa)) {
  if(!is.na(x.sa$amr2r[i])) {
    x.sa$intdis[i] <- x.sa$amr2r[i]
  }
}

# recode anything besides 1 or 2 to NA
x.sa$intdis <- ifelse(x.sa$intdis > 2, NA, x.sa$intdis)

# check to see that the categories lined up properly
with(x.sa, ftable(amr1r, amr2r, intdis, useNA = "if"))

table(x.sa$intdis, useNA = "if")
x.sa$intdis_r <- ifelse(x.sa$intdis > 2, NA, x.sa$intdis)
table(x.sa$intdis, x.sa$intdis_r, useNA = "if")

cam.design <- update(cam.design,
                     ID_ = factor(x.sa$intdis, labels = c("Intellectual disability", "no ID")))

# Number and percentage of children with Intellectual disability
svy_total_prop_CI("ID_", "Intellectual disability", cam.design, na.rm = T)
svymean(~I(ID_ == "Intellectual disability"), cam.design, na.rm = T)
confint(svymean(~I(ID_ == "Intellectual disability"), cam.design, na.rm = T))


# Other Developmental Delay
# Other developmental delay is encoded by aodd1 and aodd2 for age <1 yrs and age > 1 yrs respectively
# I will combine the two for the purpose of the analysis

table(x.sa$aodd1, x.sa$aodd2, useNA = "if")

# Create variable to combine these two variables
x.sa$othdd <- x.sa$aodd1

# Go through each row in aodd2 and if it is not NA, copy the value to othdd
for(i in 1:nrow(x.sa)) {
  if(!is.na(x.sa$aodd2[i])) {
    x.sa$othdd[i] <- x.sa$aodd2[i]
  }
}

# recode anything besides 1 or 2 to NA
x.sa$othdd <- ifelse(x.sa$othdd > 2, NA, x.sa$othdd)

# check to see that the categories lined up properly
with(x.sa, ftable(aodd1, aodd2, othdd, useNA = "if"))
cam.design <- update(cam.design,
                     Other_DD_ = factor(x.sa$othdd, labels = c("Other developmental disability", "no DD")))

# Number and percentage of children with Other developmental disability
svy_total_prop_CI("Other_DD_", "Other developmental disability", cam.design, na.rm = T)
svymean(~I(Other_DD_ == "Other developmental disability"), cam.design, na.rm = T)
confint(svymean(~I(Other_DD_ == "Other developmental disability"), cam.design, na.rm = T))



# Down syndrome
# Number is small, 15 total, maybe not usable
table(x.sa$ccondl01, useNA = "if")
x.sa$ccondl01_r <- ifelse(x.sa$ccondl01 > 2, NA, x.sa$ccondl01)
table(x.sa$ccondl01, x.sa$ccondl01_r, useNA = "if")

cam.design <- update(cam.design,
                     Down_syndrome_ = factor(x.sa$ccondl01_r, labels = c("Down syndrome", "no DS")))

# Number and percentage of children with Down syndrome
svy_total_prop_CI("Down_syndrome_", "Down syndrome", cam.design, na.rm = T)
svymean(~I(Down_syndrome_ == "Down syndrome"), cam.design, na.rm = T)
confint(svymean(~I(Down_syndrome_ == "Down syndrome"), cam.design, na.rm = T))

# Cerebral palsy
# Also small numbers, only 36 individuals, maybe not usable
table(x.sa$ccondl02, useNA = "if")
x.sa$ccondl02_r <- ifelse(x.sa$ccondl02 > 2, NA, x.sa$ccondl02)
table(x.sa$ccondl02, x.sa$ccondl02_r, useNA = "if")

cam.design <- update(cam.design,
                     Cerebral_palsy_ = factor(x.sa$ccondl02_r, labels = c("Cerebral_palsy", "no CP")))

# Number and percentage of children with Cerebral_palsy
svy_total_prop_CI("Cerebral_palsy_", "Cerebral_palsy", cam.design, na.rm = T)
svymean(~I(Cerebral_palsy_ == "Cerebral_palsy"), cam.design, na.rm = T)
confint(svymean(~I(Cerebral_palsy_ == "Cerebral_palsy"), cam.design, na.rm = T))


# Muscular dystrophy Small number, only 5 have muscular dystrophy, probably
# cannot use unless we lump into an "other disability category" combined with
# Down syndrome and cerebral palsy
table(x.sa$ccondl03, useNA = "if")
x.sa$ccondl03_r <- ifelse(x.sa$ccondl03 > 2, NA, x.sa$ccondl03)
table(x.sa$ccondl03, x.sa$ccondl03_r, useNA = "if")

cam.design <- update(cam.design,
                     Muscular_dystrophy_ = factor(x.sa$ccondl03_r, labels = c("Muscular_dystrophy", "no CP")))

# Number and percentage of children with Muscular_dystrophy
svy_total_prop_CI("Muscular_dystrophy_", "Muscular_dystrophy", cam.design, na.rm = T)
svymean(~I(Muscular_dystrophy_ == "Muscular_dystrophy"), cam.design, na.rm = T)
confint(svymean(~I(Muscular_dystrophy_ == "Muscular_dystrophy"), cam.design, na.rm = T))

# Autism Spectrum Disorder
table(x.sa$ccondl6r, useNA = "if")
x.sa$ccondl6r_r <- ifelse(x.sa$ccondl6r > 2, NA, x.sa$ccondl6r)
table(x.sa$ccondl6r, x.sa$ccondl6r_r, useNA = "if")

cam.design <- update(cam.design,
                     ASD_ = factor(x.sa$ccondl6r_r, labels = c("ASD", "no ASD")))

# Number and percentage of children with ASD
svy_total_prop_CI("ASD_", "ASD", cam.design, na.rm = T)
svymean(~I(ASD_ == "ASD"), cam.design, na.rm = T)
confint(svymean(~I(ASD_ == "ASD"), cam.design, na.rm = T))


# Display frequencies of other specific disorders
# Cystic fibrosis
table(x.sa$ccondl04, useNA = "if")
# sickle cell
table(x.sa$ccondl05, useNA = "if")
# diabetes
table(x.sa$ccondl07, useNA = "if")
# arthritis
table(x.sa$ccondl08, useNA = "if")
# congenital heart disease
table(x.sa$ccondl09, useNA = "if")
# other heart condition
table(x.sa$ccondl10, useNA = "if")

# What is the overlap between Other developmental disability and ID?
with(x.sa, table(othdd, intdis, useNA = "if"))
with(x.sa, table(ccondl6r_r, intdis, useNA = "if"))

# Combine ASD, other DD, and ID categories
isASD <- ifelse(is.na(x.sa$ccondl6r_r), NA, ifelse(x.sa$ccondl6r_r == 1, T, F))
isDD <- ifelse(is.na(x.sa$othdd), NA, ifelse(x.sa$othdd == 1, T, F))
isID <- ifelse(is.na(x.sa$intdis), NA, ifelse(x.sa$intdis == 1, T, F))
table(isASD, useNA = "if")
table(isDD, useNA = "if")
table(isID, useNA = "if")

isASDDDID <- rep(NA, nrow(x.sa))
for(i in 1:nrow(x.sa)) {
  isASDDDID[i] <- any(isASD[i], isDD[i], isID[i])
}
table(isASDDDID)
ftable(isASDDDID~isASD+isDD+isID)

cam.design <- update(cam.design, isASDDDID_ = isASDDDID)
cam.design <- update(cam.design, isASD_ = isASD)
cam.design <- update(cam.design, isDD_ = isDD)
cam.design <- update(cam.design, isID_ = isID)
svytotal(~isASDDDID_, cam.design, na.rm = T)
svytotal(~isASD, cam.design, na.rm = T)
svytotal(~isDD, cam.design, na.rm = T)
svytotal(~isID, cam.design, na.rm = T)
