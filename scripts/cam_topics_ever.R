# cam_topics_ever.R

# Biologically Based
# Diet

# # Chelation
# # Ever used: cch_use
# x.sa$cch_use_r <- recode_yes_no_NA(x.sa$cch_use)
# tables_orig_recoded(x.sa$cch_use, x.sa$cch_use_r)
# 
# # Update designs
# cam.design <- update(cam.design, CHELATION = factor(x.sa$cch_use_r, labels = c(T, F)))
# ASD.design <- subset(cam.design, isASD)
# DD.design <- subset(cam.design, isDD)
# ID.design <- subset(cam.design, isID)
# ASDDDID.design <- subset(cam.design, isASDDDID)
# 
# # Total population
# table(CHELATION = x.sa$cch_use_r, useNA = "if")
# svymean(~CHELATION, cam.design, na.rm = T)
# confint(svymean(~CHELATION, cam.design, na.rm = T))
# 
# # ASD
# table(CHELATION = x.sa$cch_use_r, asd = isASD, useNA = "if") # overlap CHELATION & ASD
# svymean(~CHELATION, ASD.design, na.rm = T)
# confint(svymean(~CHELATION, ASD.design, na.rm = T))
# 
# # ID
# table(CHELATION = x.sa$cch_use_r, id = isID, useNA = "if")
# svymean(~CHELATION, ID.design, na.rm = T)
# confint(svymean(~CHELATION, ID.design, na.rm = T))
# 
# # DD
# table(CHELATION = x.sa$cch_use_r, dd = isDD, useNA = "if")
# svymean(~CHELATION, DD.design, na.rm = T)
# confint(svymean(~CHELATION, DD.design, na.rm = T))
# 
# # ASD, ID, DD
# table(CHELATION = x.sa$cch_use_r, asdddid = isASDDDID, useNA = "if")
# svymean(~CHELATION, ASDDDID.design, na.rm = T)
# confint(svymean(~CHELATION, ASDDDID.design, na.rm = T))

######################################################################
# Any vitamin, mineral

######################################################################
# Multivitamin
# Ever used: cvt_use
x.sa$cvt_use_r <- recode_yes_no_NA(x.sa$cvt_use)
tables_orig_recoded(x.sa$cvt_use, x.sa$cvt_use_r)

# Update designs
cam.design <- update(cam.design, MVI = x.sa$cvt_use_r == 1)
ASD.design <- subset(cam.design, isASD)
DD.design <- subset(cam.design, isDD)
ID.design <- subset(cam.design, isID)
ASDDDID.design <- subset(cam.design, isASDDDID)
noASDDDID.design <- subset(cam.design, !isASDDDID)

# Total population
table(MVI = x.sa$cvt_use_r, useNA = "if")
svymean(~MVI, cam.design, na.rm = T)
confint(svymean(~MVI, cam.design, na.rm = T))

# ASD
table(MVI = x.sa$cvt_use_r, asd = isASD, useNA = "if") # overlap MVI & ASD
svymean(~MVI, ASD.design, na.rm = T)
confint(svymean(~MVI, ASD.design, na.rm = T))

# ID
table(MVI = x.sa$cvt_use_r, id = isID, useNA = "if")
svymean(~MVI, ID.design, na.rm = T)
confint(svymean(~MVI, ID.design, na.rm = T))

# DD
table(MVI = x.sa$cvt_use_r, dd = isDD, useNA = "if")
svymean(~MVI, DD.design, na.rm = T)
confint(svymean(~MVI, DD.design, na.rm = T))

# ASD, ID, DD
table(MVI = x.sa$cvt_use_r, asdddid = isASDDDID, useNA = "if")
svymean(~MVI, ASDDDID.design, na.rm = T)
confint(svymean(~MVI, ASDDDID.design, na.rm = T))

# no ASD, ID, DD
table(MVI = x.sa$cvt_use_r, asdddid = isASDDDID, useNA = "if")
svymean(~MVI, noASDDDID.design, na.rm = T)
confint(svymean(~MVI, noASDDDID.design, na.rm = T))

######################################################################
# Vitamin ABCDEHK...
# Ever used: cvt_abev
x.sa$cvt_abev_r <- recode_yes_no_NA(x.sa$cvt_abev)
tables_orig_recoded(x.sa$cvt_abev, x.sa$cvt_abev_r)

# Update designs
cam.design <- update(cam.design, ABCDE = x.sa$cvt_abev_r==1)
ASD.design <- subset(cam.design, isASD)
DD.design <- subset(cam.design, isDD)
ID.design <- subset(cam.design, isID)
ASDDDID.design <- subset(cam.design, isASDDDID)
noASDDDID.design <- subset(cam.design, !isASDDDID)

# Total population
table(ABCDE = x.sa$cvt_abev_r, useNA = "if")
svymean(~ABCDE, cam.design, na.rm = T)
confint(svymean(~ABCDE, cam.design, na.rm = T))

# ASD
table(ABCDE = x.sa$cvt_abev_r, asd = isASD, useNA = "if") # overlap ABCDE & ASD
svymean(~ABCDE, ASD.design, na.rm = T)
confint(svymean(~ABCDE, ASD.design, na.rm = T))

# ID
table(ABCDE = x.sa$cvt_abev_r, id = isID, useNA = "if")
svymean(~ABCDE, ID.design, na.rm = T)
confint(svymean(~ABCDE, ID.design, na.rm = T))

# DD
table(ABCDE = x.sa$cvt_abev_r, dd = isDD, useNA = "if")
svymean(~ABCDE, DD.design, na.rm = T)
confint(svymean(~ABCDE, DD.design, na.rm = T))

# ASD, ID, DD
table(ABCDE = x.sa$cvt_abev_r, asdddid = isASDDDID, useNA = "if")
svymean(~ABCDE, ASDDDID.design, na.rm = T)
confint(svymean(~ABCDE, ASDDDID.design, na.rm = T))

# no ASD, ID, DD
table(ABCDE = x.sa$cvt_abev_r, asdddid = isASDDDID, useNA = "if")
svymean(~ABCDE, noASDDDID.design, na.rm = T)
confint(svymean(~ABCDE, noASDDDID.design, na.rm = T))


######################################################################
# Minerals
# Ever used: cvt_caev
x.sa$cvt_caev_r <- recode_yes_no_NA(x.sa$cvt_caev)
tables_orig_recoded(x.sa$cvt_caev, x.sa$cvt_caev_r)

# Update designs
cam.design <- update(cam.design, MINERAL = x.sa$cvt_caev_r==1)
ASD.design <- subset(cam.design, isASD)
DD.design <- subset(cam.design, isDD)
ID.design <- subset(cam.design, isID)
ASDDDID.design <- subset(cam.design, isASDDDID)
noASDDDID.design <- subset(cam.design, !isASDDDID)

# Total population
table(MINERAL = x.sa$cvt_caev_r, useNA = "if")
svymean(~MINERAL, cam.design, na.rm = T)
confint(svymean(~MINERAL, cam.design, na.rm = T))

# ASD
table(MINERAL = x.sa$cvt_caev_r, asd = isASD, useNA = "if") # overlap MINERAL & ASD
svymean(~MINERAL, ASD.design, na.rm = T)
confint(svymean(~MINERAL, ASD.design, na.rm = T))

# ID
table(MINERAL = x.sa$cvt_caev_r, id = isID, useNA = "if")
svymean(~MINERAL, ID.design, na.rm = T)
confint(svymean(~MINERAL, ID.design, na.rm = T))

# DD
table(MINERAL = x.sa$cvt_caev_r, dd = isDD, useNA = "if")
svymean(~MINERAL, DD.design, na.rm = T)
confint(svymean(~MINERAL, DD.design, na.rm = T))

# ASD, ID, DD
table(MINERAL = x.sa$cvt_caev_r, asdddid = isASDDDID, useNA = "if")
svymean(~MINERAL, ASDDDID.design, na.rm = T)
confint(svymean(~MINERAL, ASDDDID.design, na.rm = T))

# no ASD, ID, DD
table(MINERAL = x.sa$cvt_caev_r, asdddid = isASDDDID, useNA = "if")
svymean(~MINERAL, noASDDDID.design, na.rm = T)
confint(svymean(~MINERAL, noASDDDID.design, na.rm = T))

######################################################################
# Any Herbal or supplement
# Ever used: chb_evr
x.sa$chb_evr_r <- recode_yes_no_NA(x.sa$chb_evr)
tables_orig_recoded(x.sa$chb_evr, x.sa$chb_evr_r)

# Update designs
cam.design <- update(cam.design, ANYHERB = x.sa$chb_evr_r==1)
ASD.design <- subset(cam.design, isASD)
DD.design <- subset(cam.design, isDD)
ID.design <- subset(cam.design, isID)
ASDDDID.design <- subset(cam.design, isASDDDID)
noASDDDID.design <- subset(cam.design, !isASDDDID)

str(factor(x.sa$chb_evr_r, labels = c(T,F)))
str(x.sa$chb_evr_r==1)

# Total population
table(ANYHERB = x.sa$chb_evr_r, useNA = "if")
svymean(~ANYHERB, cam.design, na.rm = T)
confint(svymean(~ANYHERB, cam.design, na.rm = T))

# ASD
table(ANYHERB = x.sa$chb_evr_r, asd = isASD, useNA = "if") # overlap ANYHERB & ASD
svymean(~ANYHERB, ASD.design, na.rm = T)
confint(svymean(~ANYHERB, ASD.design, na.rm = T))

# ID
table(ANYHERB = x.sa$chb_evr_r, id = isID, useNA = "if")
svymean(~ANYHERB, ID.design, na.rm = T)
confint(svymean(~ANYHERB, ID.design, na.rm = T))

# DD
table(ANYHERB = x.sa$chb_evr_r, dd = isDD, useNA = "if")
svymean(~ANYHERB, DD.design, na.rm = T)
confint(svymean(~ANYHERB, DD.design, na.rm = T))

# ASD, ID, DD
table(ANYHERB = x.sa$chb_evr_r, asdddid = isASDDDID, useNA = "if")
svymean(~ANYHERB, ASDDDID.design, na.rm = T)
confint(svymean(~ANYHERB, ASDDDID.design, na.rm = T))

# no ASD, ID, DD
table(ANYHERB = x.sa$chb_evr_r, asdddid = isASDDDID, useNA = "if")
svymean(~ANYHERB, noASDDDID.design, na.rm = T)
confint(svymean(~ANYHERB, noASDDDID.design, na.rm = T))

######################################################################

# Homeopathy
# Ever used: chm_use
x.sa$chm_use_r <- recode_yes_no_NA(x.sa$chm_use)
tables_orig_recoded(x.sa$chm_use, x.sa$chm_use_r)

# Update designs
cam.design <- update(cam.design, HOMEOPATHY = factor(x.sa$chm_use_r, labels = c(T, F)))
ASD.design <- subset(cam.design, isASD)
DD.design <- subset(cam.design, isDD)
ID.design <- subset(cam.design, isID)
ASDDDID.design <- subset(cam.design, isASDDDID)
noASDDDID.design <- subset(cam.design, !isASDDDID)

# Total population
table(HOMEOPATHY = x.sa$chm_use_r, useNA = "if")
svymean(~HOMEOPATHY, cam.design, na.rm = T)
confint(svymean(~HOMEOPATHY, cam.design, na.rm = T))

# ASD
table(HOMEOPATHY = x.sa$chm_use_r, asd = isASD, useNA = "if") # overlap HOMEOPATHY & ASD
svymean(~HOMEOPATHY, ASD.design, na.rm = T)
confint(svymean(~HOMEOPATHY, ASD.design, na.rm = T))

# ID
table(HOMEOPATHY = x.sa$chm_use_r, id = isID, useNA = "if")
svymean(~HOMEOPATHY, ID.design, na.rm = T)
confint(svymean(~HOMEOPATHY, ID.design, na.rm = T))

# DD
table(HOMEOPATHY = x.sa$chm_use_r, dd = isDD, useNA = "if")
svymean(~HOMEOPATHY, DD.design, na.rm = T)
confint(svymean(~HOMEOPATHY, DD.design, na.rm = T))

# ASD, ID, DD
table(HOMEOPATHY = x.sa$chm_use_r, asdddid = isASDDDID, useNA = "if")
svymean(~HOMEOPATHY, ASDDDID.design, na.rm = T)
confint(svymean(~HOMEOPATHY, ASDDDID.design, na.rm = T))


######################################################################

######################################################################
# Fish oil
# Melatonin
######################################################################
######################################################################
# Mind/Body
######################################################################
######################################################################
# Yoga
# Ever used: cyge_yog
x.sa$cyge_yog_r <- recode_yes_no_NA(x.sa$cyge_yog)
tables_orig_recoded(x.sa$cyge_yog, x.sa$cyge_yog_r)

# Update designs
cam.design <- update(cam.design, YOGA = x.sa$cyge_yog_r==1)
ASD.design <- subset(cam.design, isASD)
DD.design <- subset(cam.design, isDD)
ID.design <- subset(cam.design, isID)
ASDDDID.design <- subset(cam.design, isASDDDID)
noASDDDID.design <- subset(cam.design, !isASDDDID)

# Total population
table(YOGA = x.sa$cyge_yog_r, useNA = "if")
svymean(~YOGA, cam.design, na.rm = T)
confint(svymean(~YOGA, cam.design, na.rm = T))

# ASD
table(YOGA = x.sa$cyge_yog_r, asd = isASD, useNA = "if") # overlap YOGA & ASD
svymean(~YOGA, ASD.design, na.rm = T)
confint(svymean(~YOGA, ASD.design, na.rm = T))

# ID
table(YOGA = x.sa$cyge_yog_r, id = isID, useNA = "if")
svymean(~YOGA, ID.design, na.rm = T)
confint(svymean(~YOGA, ID.design, na.rm = T))

# DD
table(YOGA = x.sa$cyge_yog_r, dd = isDD, useNA = "if")
svymean(~YOGA, DD.design, na.rm = T)
confint(svymean(~YOGA, DD.design, na.rm = T))

# ASD, ID, DD
table(YOGA = x.sa$cyge_yog_r, asdddid = isASDDDID, useNA = "if")
svymean(~YOGA, ASDDDID.design, na.rm = T)
confint(svymean(~YOGA, ASDDDID.design, na.rm = T))

# ASD, ID, DD
table(YOGA = x.sa$cyge_yog_r, asdddid = isASDDDID, useNA = "if")
svymean(~YOGA, noASDDDID.design, na.rm = T)
confint(svymean(~YOGA, noASDDDID.design, na.rm = T))

######################################################################

# Traditional healer
# Ever used: ctr_evr
x.sa$ctr_evr_r <- recode_yes_no_NA(x.sa$ctr_evr)
tables_orig_recoded(x.sa$ctr_evr, x.sa$ctr_evr_r)

# Update designs
cam.design <- update(cam.design, TRADHEALER = factor(x.sa$ctr_evr_r, labels = c(T, F)))
ASD.design <- subset(cam.design, isASD)
DD.design <- subset(cam.design, isDD)
ID.design <- subset(cam.design, isID)
ASDDDID.design <- subset(cam.design, isASDDDID)
noASDDDID.design <- subset(cam.design, !isASDDDID)

# Total population
table(TRADHEALER = x.sa$ctr_evr_r, useNA = "if")
svymean(~TRADHEALER, cam.design, na.rm = T)
confint(svymean(~TRADHEALER, cam.design, na.rm = T))

# ASD
table(TRADHEALER = x.sa$ctr_evr_r, asd = isASD, useNA = "if") # overlap TRADHEALER & ASD
svymean(~TRADHEALER, ASD.design, na.rm = T)
confint(svymean(~TRADHEALER, ASD.design, na.rm = T))

# ID
table(TRADHEALER = x.sa$ctr_evr_r, id = isID, useNA = "if")
svymean(~TRADHEALER, ID.design, na.rm = T)
confint(svymean(~TRADHEALER, ID.design, na.rm = T))

# DD
table(TRADHEALER = x.sa$ctr_evr_r, dd = isDD, useNA = "if")
svymean(~TRADHEALER, DD.design, na.rm = T)
confint(svymean(~TRADHEALER, DD.design, na.rm = T))

# ASD, ID, DD
table(TRADHEALER = x.sa$ctr_evr_r, asdddid = isASDDDID, useNA = "if")
svymean(~TRADHEALER, ASDDDID.design, na.rm = T)
confint(svymean(~TRADHEALER, ASDDDID.design, na.rm = T))


######################################################################


# Biofeedback
# Ever used: cbi_use
x.sa$cbi_use_r <- recode_yes_no_NA(x.sa$cbi_use)
tables_orig_recoded(x.sa$cbi_use, x.sa$cbi_use_r)

# Update designs
cam.design <- update(cam.design, BIOFEEDBACK = factor(x.sa$cbi_use_r, labels = c(T, F)))
ASD.design <- subset(cam.design, isASD)
DD.design <- subset(cam.design, isDD)
ID.design <- subset(cam.design, isID)
ASDDDID.design <- subset(cam.design, isASDDDID)
noASDDDID.design <- subset(cam.design, !isASDDDID)

# Total population
table(BIOFEEDBACK = x.sa$cbi_use_r, useNA = "if")
svymean(~BIOFEEDBACK, cam.design, na.rm = T)
confint(svymean(~BIOFEEDBACK, cam.design, na.rm = T))

# ASD
table(BIOFEEDBACK = x.sa$cbi_use_r, asd = isASD, useNA = "if") # overlap BIOFEEDBACK & ASD
svymean(~BIOFEEDBACK, ASD.design, na.rm = T)
confint(svymean(~BIOFEEDBACK, ASD.design, na.rm = T))

# ID
table(BIOFEEDBACK = x.sa$cbi_use_r, id = isID, useNA = "if")
svymean(~BIOFEEDBACK, ID.design, na.rm = T)
confint(svymean(~BIOFEEDBACK, ID.design, na.rm = T))

# DD
table(BIOFEEDBACK = x.sa$cbi_use_r, dd = isDD, useNA = "if")
svymean(~BIOFEEDBACK, DD.design, na.rm = T)
confint(svymean(~BIOFEEDBACK, DD.design, na.rm = T))

# ASD, ID, DD
table(BIOFEEDBACK = x.sa$cbi_use_r, asdddid = isASDDDID, useNA = "if")
svymean(~BIOFEEDBACK, ASDDDID.design, na.rm = T)
confint(svymean(~BIOFEEDBACK, ASDDDID.design, na.rm = T))


######################################################################

# Hypnosis
# Ever used: chy_use
x.sa$chy_use_r <- recode_yes_no_NA(x.sa$chy_use)
tables_orig_recoded(x.sa$chy_use, x.sa$chy_use_r)

# Update designs
cam.design <- update(cam.design, HYPNOSIS = factor(x.sa$chy_use_r, labels = c(T, F)))
ASD.design <- subset(cam.design, isASD)
DD.design <- subset(cam.design, isDD)
ID.design <- subset(cam.design, isID)
ASDDDID.design <- subset(cam.design, isASDDDID)

# Total population
table(HYPNOSIS = x.sa$chy_use_r, useNA = "if")
svymean(~HYPNOSIS, cam.design, na.rm = T)
confint(svymean(~HYPNOSIS, cam.design, na.rm = T))

# ASD
table(HYPNOSIS = x.sa$chy_use_r, asd = isASD, useNA = "if") # overlap HYPNOSIS & ASD
svymean(~HYPNOSIS, ASD.design, na.rm = T)
confint(svymean(~HYPNOSIS, ASD.design, na.rm = T))

# ID
table(HYPNOSIS = x.sa$chy_use_r, id = isID, useNA = "if")
svymean(~HYPNOSIS, ID.design, na.rm = T)
confint(svymean(~HYPNOSIS, ID.design, na.rm = T))

# DD
table(HYPNOSIS = x.sa$chy_use_r, dd = isDD, useNA = "if")
svymean(~HYPNOSIS, DD.design, na.rm = T)
confint(svymean(~HYPNOSIS, DD.design, na.rm = T))

# ASD, ID, DD
table(HYPNOSIS = x.sa$chy_use_r, asdddid = isASDDDID, useNA = "if")
svymean(~HYPNOSIS, ASDDDID.design, na.rm = T)
confint(svymean(~HYPNOSIS, ASDDDID.design, na.rm = T))


######################################################################
# Meditation

# Ever used: cmb_use
x.sa$cmb_use_r <- recode_yes_no_NA(x.sa$cmb_use)
tables_orig_recoded(x.sa$cmb_use, x.sa$cmb_use_r)

# Update designs
cam.design <- update(cam.design, MEDITATION = x.sa$cmb_use_r==1)
ASD.design <- subset(cam.design, isASD)
DD.design <- subset(cam.design, isDD)
ID.design <- subset(cam.design, isID)
ASDDDID.design <- subset(cam.design, isASDDDID)
noASDDDID.design <- subset(cam.design, !isASDDDID)

# Total population
table(MEDITATION = x.sa$cmb_use_r, useNA = "if")
svymean(~MEDITATION, cam.design, na.rm = T)
confint(svymean(~MEDITATION, cam.design, na.rm = T))

# ASD
table(MEDITATION = x.sa$cmb_use_r, asd = isASD, useNA = "if") # overlap MEDITATION & ASD
svymean(~MEDITATION, ASD.design, na.rm = T)
confint(svymean(~MEDITATION, ASD.design, na.rm = T))

# ID
table(MEDITATION = x.sa$cmb_use_r, id = isID, useNA = "if")
svymean(~MEDITATION, ID.design, na.rm = T)
confint(svymean(~MEDITATION, ID.design, na.rm = T))

# DD
table(MEDITATION = x.sa$cmb_use_r, dd = isDD, useNA = "if")
svymean(~MEDITATION, DD.design, na.rm = T)
confint(svymean(~MEDITATION, DD.design, na.rm = T))

# ASD, ID, DD
table(MEDITATION = x.sa$cmb_use_r, asdddid = isASDDDID, useNA = "if")
svymean(~MEDITATION, ASDDDID.design, na.rm = T)
confint(svymean(~MEDITATION, ASDDDID.design, na.rm = T))

# no ASD, ID, DD
table(MEDITATION = x.sa$cmb_use_r, asdddid = isASDDDID, useNA = "if")
svymean(~MEDITATION, noASDDDID.design, na.rm = T)
confint(svymean(~MEDITATION, noASDDDID.design, na.rm = T))

######################################################################


######################################################################

# Manipulation and Body Based Methods
######################################################################
# Massage
# Ever used: cms_use
x.sa$cms_use_r <- recode_yes_no_NA(x.sa$cms_use)
tables_orig_recoded(x.sa$cms_use, x.sa$cms_use_r)

# Update designs
cam.design <- update(cam.design, MASSAGE = x.sa$cms_use_r==1)
ASD.design <- subset(cam.design, isASD)
DD.design <- subset(cam.design, isDD)
ID.design <- subset(cam.design, isID)
ASDDDID.design <- subset(cam.design, isASDDDID)
noASDDDID.design <- subset(cam.design, !isASDDDID)

# Total population
table(MASSAGE = x.sa$cms_use_r, useNA = "if")
svymean(~MASSAGE, cam.design, na.rm = T)
confint(svymean(~MASSAGE, cam.design, na.rm = T))

# ASD
table(MASSAGE = x.sa$cms_use_r, asd = isASD, useNA = "if") # overlap MASSAGE & ASD
svymean(~MASSAGE, ASD.design, na.rm = T)
confint(svymean(~MASSAGE, ASD.design, na.rm = T))

# ID
table(MASSAGE = x.sa$cms_use_r, id = isID, useNA = "if")
svymean(~MASSAGE, ID.design, na.rm = T)
confint(svymean(~MASSAGE, ID.design, na.rm = T))

# DD
table(MASSAGE = x.sa$cms_use_r, dd = isDD, useNA = "if")
svymean(~MASSAGE, DD.design, na.rm = T)
confint(svymean(~MASSAGE, DD.design, na.rm = T))

# ASD, ID, DD
table(MASSAGE = x.sa$cms_use_r, asdddid = isASDDDID, useNA = "if")
svymean(~MASSAGE, ASDDDID.design, na.rm = T)
confint(svymean(~MASSAGE, ASDDDID.design, na.rm = T))

# no ASD, ID, DD
table(MASSAGE = x.sa$cms_use_r, asdddid = isASDDDID, useNA = "if")
svymean(~MASSAGE, noASDDDID.design, na.rm = T)
confint(svymean(~MASSAGE, noASDDDID.design, na.rm = T))

######################################################################
# Chiropractic or Osteopathic
# Ever used: cco_use
# Recode original variable
table(x.sa$cco_use, useNA = "if")
x.sa$cco_use_r <- ifelse(x.sa$cco_use > 2, NA, x.sa$cco_use)
table(x.sa$cco_use_r, x.sa$cco_use, useNA = "if")

# Update designs
cam.design <- update(cam.design, chiro = x.sa$cco_use_r==1)
ASD.design <- subset(cam.design, isASD)
DD.design <- subset(cam.design, isDD)
ID.design <- subset(cam.design, isID)
ASDDDID.design <- subset(cam.design, isASDDDID)
noASDDDID.design <- subset(cam.design, !isASDDDID)

# Total population
table(chiro = x.sa$cco_use_r, useNA = "if")
svymean(~chiro, cam.design, na.rm = T)
confint(svymean(~chiro, cam.design, na.rm = T))

# ASD
table(chiro = x.sa$cco_use_r, asd = isASD, useNA = "if") # overlap Chiro & ASD
svymean(~chiro, ASD.design, na.rm = T)
confint(svymean(~chiro, ASD.design, na.rm = T))

# ID
table(chiro = x.sa$cco_use_r, id = isID, useNA = "if")
svymean(~chiro, ID.design, na.rm = T)
confint(svymean(~chiro, ID.design, na.rm = T))

# DD
table(chiro = x.sa$cco_use_r, dd = isDD, useNA = "if")
svymean(~chiro, DD.design, na.rm = T)
confint(svymean(~chiro, DD.design, na.rm = T))

# ASD, ID, DD
table(chiro = x.sa$cco_use_r, asdddid = isASDDDID, useNA = "if")
svymean(~chiro, ASDDDID.design, na.rm = T)
confint(svymean(~chiro, ASDDDID.design, na.rm = T))

# no ASD, ID, DD
table(chiro = x.sa$cco_use_r, asdddid = isASDDDID, useNA = "if")
svymean(~chiro, noASDDDID.design, na.rm = T)
confint(svymean(~chiro, noASDDDID.design, na.rm = T))
######################################################################
# Hypnosis


# Craniosacral
# Ever used: ccs_use
x.sa$ccs_use_r <- recode_yes_no_NA(x.sa$ccs_use)
tables_orig_recoded(x.sa$ccs_use, x.sa$ccs_use_r)

# Update designs
cam.design <- update(cam.design, CRANIOSACRAL = factor(x.sa$ccs_use_r, labels = c(T, F)))
ASD.design <- subset(cam.design, isASD)
DD.design <- subset(cam.design, isDD)
ID.design <- subset(cam.design, isID)
ASDDDID.design <- subset(cam.design, isASDDDID)
noASDDDID.design <- subset(cam.design, !isASDDDID)

# Total population
table(CRANIOSACRAL = x.sa$ccs_use_r, useNA = "if")
svymean(~CRANIOSACRAL, cam.design, na.rm = T)
confint(svymean(~CRANIOSACRAL, cam.design, na.rm = T))

# ASD
table(CRANIOSACRAL = x.sa$ccs_use_r, asd = isASD, useNA = "if") # overlap CRANIOSACRAL & ASD
svymean(~CRANIOSACRAL, ASD.design, na.rm = T)
confint(svymean(~CRANIOSACRAL, ASD.design, na.rm = T))

# ID
table(CRANIOSACRAL = x.sa$ccs_use_r, id = isID, useNA = "if")
svymean(~CRANIOSACRAL, ID.design, na.rm = T)
confint(svymean(~CRANIOSACRAL, ID.design, na.rm = T))

# DD
table(CRANIOSACRAL = x.sa$ccs_use_r, dd = isDD, useNA = "if")
svymean(~CRANIOSACRAL, DD.design, na.rm = T)
confint(svymean(~CRANIOSACRAL, DD.design, na.rm = T))

# ASD, ID, DD
table(CRANIOSACRAL = x.sa$ccs_use_r, asdddid = isASDDDID, useNA = "if")
svymean(~CRANIOSACRAL, ASDDDID.design, na.rm = T)
confint(svymean(~CRANIOSACRAL, ASDDDID.design, na.rm = T))


######################################################################

# Alternative systems

######################################################################
# Acupuncture
# Ever used: cac_use
x.sa$cac_use_r <- recode_yes_no_NA(x.sa$cac_use)
tables_orig_recoded(x.sa$cac_use, x.sa$cac_use_r)

# Update designs
cam.design <- update(cam.design, ACUPUNCTURE = factor(x.sa$cac_use_r, labels = c(T, F)))
ASD.design <- subset(cam.design, isASD)
DD.design <- subset(cam.design, isDD)
ID.design <- subset(cam.design, isID)
ASDDDID.design <- subset(cam.design, isASDDDID)

# Total population
table(ACUPUNCTURE = x.sa$cac_use_r, useNA = "if")
svymean(~ACUPUNCTURE, cam.design, na.rm = T)
confint(svymean(~ACUPUNCTURE, cam.design, na.rm = T))

# ASD
table(ACUPUNCTURE = x.sa$cac_use_r, asd = isASD, useNA = "if") # overlap ACUPUNCTURE & ASD
svymean(~ACUPUNCTURE, ASD.design, na.rm = T)
confint(svymean(~ACUPUNCTURE, ASD.design, na.rm = T))

# ID
table(ACUPUNCTURE = x.sa$cac_use_r, id = isID, useNA = "if")
svymean(~ACUPUNCTURE, ID.design, na.rm = T)
confint(svymean(~ACUPUNCTURE, ID.design, na.rm = T))

# DD
table(ACUPUNCTURE = x.sa$cac_use_r, dd = isDD, useNA = "if")
svymean(~ACUPUNCTURE, DD.design, na.rm = T)
confint(svymean(~ACUPUNCTURE, DD.design, na.rm = T))

# ASD, ID, DD
table(ACUPUNCTURE = x.sa$cac_use_r, asdddid = isASDDDID, useNA = "if")
svymean(~ACUPUNCTURE, ASDDDID.design, na.rm = T)
confint(svymean(~ACUPUNCTURE, ASDDDID.design, na.rm = T))


######################################################################

# Ayurveda
# Ever used: cay_use
x.sa$cay_use_r <- recode_yes_no_NA(x.sa$cay_use)
tables_orig_recoded(x.sa$cay_use, x.sa$cay_use_r)

# Update designs
cam.design <- update(cam.design, AYURVEDA = factor(x.sa$cay_use_r, labels = c(T, F)))
ASD.design <- subset(cam.design, isASD)
DD.design <- subset(cam.design, isDD)
ID.design <- subset(cam.design, isID)
ASDDDID.design <- subset(cam.design, isASDDDID)

# Total population
table(AYURVEDA = x.sa$cay_use_r, useNA = "if")
svymean(~AYURVEDA, cam.design, na.rm = T)
confint(svymean(~AYURVEDA, cam.design, na.rm = T))

# ASD
table(AYURVEDA = x.sa$cay_use_r, asd = isASD, useNA = "if") # overlap AYURVEDA & ASD
svymean(~AYURVEDA, ASD.design, na.rm = T)
confint(svymean(~AYURVEDA, ASD.design, na.rm = T))

# ID
table(AYURVEDA = x.sa$cay_use_r, id = isID, useNA = "if")
svymean(~AYURVEDA, ID.design, na.rm = T)
confint(svymean(~AYURVEDA, ID.design, na.rm = T))

# DD
table(AYURVEDA = x.sa$cay_use_r, dd = isDD, useNA = "if")
svymean(~AYURVEDA, DD.design, na.rm = T)
confint(svymean(~AYURVEDA, DD.design, na.rm = T))

# ASD, ID, DD
table(AYURVEDA = x.sa$cay_use_r, asdddid = isASDDDID, useNA = "if")
svymean(~AYURVEDA, ASDDDID.design, na.rm = T)
confint(svymean(~AYURVEDA, ASDDDID.design, na.rm = T))

######################################################################
# Naturopathy

# Ever used: cnt_use
x.sa$cnt_use_r <- recode_yes_no_NA(x.sa$cnt_use)
tables_orig_recoded(x.sa$cnt_use, x.sa$cnt_use_r)

# Update designs
cam.design <- update(cam.design, NATUROPATHY = factor(x.sa$cnt_use_r, labels = c(T, F)))
ASD.design <- subset(cam.design, isASD)
DD.design <- subset(cam.design, isDD)
ID.design <- subset(cam.design, isID)
ASDDDID.design <- subset(cam.design, isASDDDID)

# Total population
table(NATUROPATHY = x.sa$cnt_use_r, useNA = "if")
svymean(~NATUROPATHY, cam.design, na.rm = T)
confint(svymean(~NATUROPATHY, cam.design, na.rm = T))

# ASD
table(NATUROPATHY = x.sa$cnt_use_r, asd = isASD, useNA = "if") # overlap NATUROPATHY & ASD
svymean(~NATUROPATHY, ASD.design, na.rm = T)
confint(svymean(~NATUROPATHY, ASD.design, na.rm = T))

# ID
table(NATUROPATHY = x.sa$cnt_use_r, id = isID, useNA = "if")
svymean(~NATUROPATHY, ID.design, na.rm = T)
confint(svymean(~NATUROPATHY, ID.design, na.rm = T))

# DD
table(NATUROPATHY = x.sa$cnt_use_r, dd = isDD, useNA = "if")
svymean(~NATUROPATHY, DD.design, na.rm = T)
confint(svymean(~NATUROPATHY, DD.design, na.rm = T))

# ASD, ID, DD
table(NATUROPATHY = x.sa$cnt_use_r, asdddid = isASDDDID, useNA = "if")
svymean(~NATUROPATHY, ASDDDID.design, na.rm = T)
confint(svymean(~NATUROPATHY, ASDDDID.design, na.rm = T))


######################################################################

# Energy healing
# Ever used: ceh_use
x.sa$ceh_use_r <- recode_yes_no_NA(x.sa$ceh_use)
tables_orig_recoded(x.sa$ceh_use, x.sa$ceh_use_r)

# Update designs
cam.design <- update(cam.design, ENERGYTHERAPY = factor(x.sa$ceh_use_r, labels = c(T, F)))
ASD.design <- subset(cam.design, isASD)
DD.design <- subset(cam.design, isDD)
ID.design <- subset(cam.design, isID)
ASDDDID.design <- subset(cam.design, isASDDDID)

# Total population
table(ENERGYTHERAPY = x.sa$ceh_use_r, useNA = "if")
svymean(~ENERGYTHERAPY, cam.design, na.rm = T)
confint(svymean(~ENERGYTHERAPY, cam.design, na.rm = T))

# ASD
table(ENERGYTHERAPY = x.sa$ceh_use_r, asd = isASD, useNA = "if") # overlap ENERGYTHERAPY & ASD
svymean(~ENERGYTHERAPY, ASD.design, na.rm = T)
confint(svymean(~ENERGYTHERAPY, ASD.design, na.rm = T))

# ID
table(ENERGYTHERAPY = x.sa$ceh_use_r, id = isID, useNA = "if")
svymean(~ENERGYTHERAPY, ID.design, na.rm = T)
confint(svymean(~ENERGYTHERAPY, ID.design, na.rm = T))

# DD
table(ENERGYTHERAPY = x.sa$ceh_use_r, dd = isDD, useNA = "if")
svymean(~ENERGYTHERAPY, DD.design, na.rm = T)
confint(svymean(~ENERGYTHERAPY, DD.design, na.rm = T))

# ASD, ID, DD
table(ENERGYTHERAPY = x.sa$ceh_use_r, asdddid = isASDDDID, useNA = "if")
svymean(~ENERGYTHERAPY, ASDDDID.design, na.rm = T)
confint(svymean(~ENERGYTHERAPY, ASDDDID.design, na.rm = T))


######################################################################

any_alt <- x.sa$cal_cnt > 0

# Count of number people using any alternative therapies (excluding vitamins and
# minerals)

cam.design <- update(cam.design, any_alt = any_alt)
ASD.design <- subset(cam.design, isASD)
DD.design <- subset(cam.design, isDD)
ID.design <- subset(cam.design, isID)
ASDDDID.design <- subset(cam.design, isASDDDID)
nonASDDDID.design <- subset(cam.design, !isASDDDID)

table(any_alt, useNA = "if")
svymean(~any_alt, cam.design)
confint(svymean(~any_alt, cam.design))

table(any_alt, isASD, useNA = "if")
svymean(~any_alt, ASD.design)
confint(svymean(~any_alt, ASD.design))

table(any_alt, isID, useNA = "if")
svymean(~any_alt, ID.design)
confint(svymean(~any_alt, ID.design))

table(any_alt, isDD, useNA = "if")
svymean(~any_alt, DD.design)
confint(svymean(~any_alt, DD.design))

table(any_alt, isASDDDID, useNA = "if")
svymean(~any_alt, ASDDDID.design)
confint(svymean(~any_alt, ASDDDID.design))

table(any_alt, isASDDDID, useNA = "if")
svymean(~any_alt, nonASDDDID.design)
confint(svymean(~any_alt, nonASDDDID.design))


svychisq(~any_alt+isASD_, cam.design)
svychisq(~any_alt+isID_, cam.design)
svychisq(~any_alt+isDD_, cam.design)
svychisq(~any_alt+isASDDDID_, cam.design)

######################################################################
# Probiotics
# Ever used: chblst18
x.sa$chblst18_r <- recode_yes_no_NA(x.sa$chblst18)
tables_orig_recoded(x.sa$chblst18, x.sa$chblst18_r)

# Update designs
cam.design <- update(cam.design, PROBIOTICS = x.sa$chblst18_r==1)
ASD.design <- subset(cam.design, isASD)
DD.design <- subset(cam.design, isDD)
ID.design <- subset(cam.design, isID)
ASDDDID.design <- subset(cam.design, isASDDDID)
noASDDDID.design <- subset(cam.design, !isASDDDID)

# Total population
table(PROBIOTICS = x.sa$chblst18_r, useNA = "if")
svymean(~PROBIOTICS, cam.design, na.rm = T)
confint(svymean(~PROBIOTICS, cam.design, na.rm = T))

# ASD
table(PROBIOTICS = x.sa$chblst18_r, asd = isASD, useNA = "if") # overlap PROBIOTICS & ASD
svymean(~PROBIOTICS, ASD.design, na.rm = T)
confint(svymean(~PROBIOTICS, ASD.design, na.rm = T))

# ID
table(PROBIOTICS = x.sa$chblst18_r, id = isID, useNA = "if")
svymean(~PROBIOTICS, ID.design, na.rm = T)
confint(svymean(~PROBIOTICS, ID.design, na.rm = T))

# DD
table(PROBIOTICS = x.sa$chblst18_r, dd = isDD, useNA = "if")
svymean(~PROBIOTICS, DD.design, na.rm = T)
confint(svymean(~PROBIOTICS, DD.design, na.rm = T))

# ASD, ID, DD
table(PROBIOTICS = x.sa$chblst18_r, asdddid = isASDDDID, useNA = "if")
svymean(~PROBIOTICS, ASDDDID.design, na.rm = T)
confint(svymean(~PROBIOTICS, ASDDDID.design, na.rm = T))

# no ASD, ID, DD
table(PROBIOTICS = x.sa$chblst18_r, asdddid = isASDDDID, useNA = "if")
svymean(~PROBIOTICS, noASDDDID.design, na.rm = T)
confint(svymean(~PROBIOTICS, noASDDDID.design, na.rm = T))


