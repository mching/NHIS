# load_and_merge_data.R
# Michael Ching, MD, MPH
# 
# This file loads the sample child and child CAM supplement datasets.
#
# The file has been adapted from work by anthony joseph damico
#
# Before this file can be run, you have to obtain the NHIS data as rda files
# Download script here:
# https://github.com/mching/asdfree/blob/master/National%20Health%20Interview%20Survey/personsx%20plus%20samadult%20with%20multiple%20imputation%20-%20analyze.R
# 
# 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#####################################################################################################################################
# prior to running, the nhis 2014 personsx, samadult, incmimp# files must be loaded as an R data file (.rda) on the local machine.  #
# running the "download all microdata.R" script will create this R data file (note: only 2014 files need to be loaded)              #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# https://github.com/ajdamico/asdfree/blob/master/National%20Health%20Interview%20Survey/download%20all%20microdata.R #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# that script will create a files "/2014/_filename_.rda" in C:/My Directory/NHIS (or wherever the working directory was chosen)     #
#####################################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



#########################################################################################################
# Analyze the 2012 National Health Interview Survey samchild, childcam, and imputed income files with R #
#########################################################################################################


# set your working directory.
# the NHIS 2012 personsx, samadult, and incmimp# data files should have been
# stored in a year-specific directory within this folder.
# so if the file "samchild.rda" exists in the directory "C:/My Directory/NHIS/2012/" 
# then the working directory should be set to "C:/My Directory/NHIS/"
# use forward slashes instead of back slashes

# uncomment this line by removing the `#` at the front..
setwd( "~/Dropbox/Mike/NHIS_CAM/" )
# ..in order to set your current working directory

# remove the # in order to run this install.packages line only once
# install.packages( c( "survey" , "mitools" ) )

library(survey) 	# load survey package (analyzes complex design surveys)
library(mitools)	# allows analysis of multiply-imputed survey data
library(tidyverse)

# set R to produce conservative standard errors instead of crashing
# http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
options( survey.lonely.psu = "adjust" )
# this setting matches the MISSUNIT option in SUDAAN


# choose what year of data to analyze
# note: this can be changed to any year that has already been downloaded locally
# by the "1963-2014 - download all microdata.R" program above
year <- 2012


# construct the filepath (within the current working directory) to the three rda files
path.to.samchild.file <- paste( getwd() , year , "samchild.rda" , sep = "/" )
path.to.childcam.file <- paste( getwd() , year , "childcam.rda" , sep = "/" )
path.to.incmimp.file <- paste( getwd() , year , "incmimp.rda" , sep = "/" )

# print those filepaths to the screen
print( path.to.samchild.file )
print( path.to.childcam.file )
print( path.to.incmimp.file )


# now the "NHIS.12.samchild.df" data frame can be loaded directly
# from your local hard drive.  this is much faster.
load( path.to.samchild.file )		# this loads a data frame called NHIS.11.personsx.df
load( path.to.childcam.file )		# this loads a data frame called NHIS.11.samadult.df
# the five imputed income files will be loaded later

# all objects currently in memory can be viewed with the list function
ls()


# construct a string containing the data frame name of the samchild data table
# stored within the R data file (.rda)

df.name <- paste( "NHIS" , substr( year , 3 , 4 ) , "samchild" , "df" , sep = "." )

# repeat this for the child CAM data frame, 
# but not for the five imputed income data frames (which will be dealt with later)
childcam.name <- paste( "NHIS" , substr( year , 3 , 4 ) , "childcam" , "df" , sep = "." )


# copy the samchild data frame to the variable x for easier analyses
# (because NHIS.12.samchild.df is unwieldy to keep typing)
x <- get( df.name )

# copy the childcam data frame to the variable sa for easier typing
# (because NHIS.12.childcam.df is unwieldy to keep typing)
# sa originally stood for sample adult, but I chose not to change it in 
# subsequent lines to prevent mistakes
sa <- get( childcam.name )


# remove the original copy of the two data frames from memory
rm( list = c( df.name , childcam.name ) )

# clear up RAM
gc()


#####################################
# merge samchild and childcam files #
#####################################

# note: the logical steps taken here could also be applied to 
# merging the personsx and samchild files

# so merging them together will require three variables:
# hhx (household unique identifier)
# fmx (family unique identifier)
# fpx (person unique identifier)

# store the names of these three columns in a character vector
merge.vars <- c( "hhx" , "fmx" , "fpx" )

# these two files have multiple overlapping (redundant) columns,
# so determine which columns are included in both data frames
# at the same time, enclose this statement in () thereby printing the vector to the screen
( columns.in.both.dfs <- intersect( names( sa ) , names( x ) ) )


# since the merge.vars will be used to merge the two data frames,
# those three variables should be excluded from the list of redundant columns
# keep all column names that don't match the merge variables
# at the same time, enclose this statement in () thereby printing the vector to the screen
( redundant.columns <- columns.in.both.dfs[ !( columns.in.both.dfs %in% merge.vars ) ] )

# notice that the three merge.vars have disappeared


# so shave the redundant columns off of the childcam file
# keep all columns in the childcam file that are not in the redundant.columns vector
sa <- sa[ , !( names( sa ) %in% redundant.columns ) ]


# at this point, the only overlap between the samchild and childcam files
# should be the three merge.vars
# throw an error if that's not true
stopifnot( merge.vars == intersect( names( sa ) , names( x ) ) )


# remember that the childcam file contains a subset of the individuals in samchild
# therefore, an inner join of the two should have the same number of records as childcam

# perform the actual merge
x.sa <- merge( x , sa )
# note that the merge() function merges using all intersecting columns -

# uncomment this line to see intersecting columns
# intersect( names( sa ) , names( x ) )

# - by default, the 'by' parameter does not need to be specified
# for more detail about the merge function, type ?merge in the console

# throw an error if the number of records in the merged file
# does not match the number of records in the samadult file
stopifnot( nrow( x.sa ) == nrow( sa ) )


# now the x.sa data frame contains all of the rows in the childcam file and 
# all columns from both the samchild and childcam files
# therefore, there's no more need for the childcam file on its own
# so delete the childcam file
rm( sa )

# and clear up RAM
gc()

# construct the filepath (within the current working directory) to the three rda files
path.to.familyxx.file <- paste( getwd() , year , "familyxx.rda" , sep = "/" )

# print those filepaths to the screen
print( path.to.familyxx.file )


# now the "NHIS.12.samchild.df" data frame can be loaded directly
# from your local hard drive.  this is much faster.
load( path.to.familyxx.file )		# this loads a data frame called NHIS.11.familyxx.df
# the five imputed income files will be loaded later

# all objects currently in memory can be viewed with the list function
ls()


# construct a string containing the data frame name of the familyxx data table
# stored within the R data file (.rda)

df.name <- paste( "NHIS" , substr( year , 3 , 4 ) , "familyxx" , "df" , sep = "." )

# copy the familyxx data frame to the variable x for easier analyses
# (because NHIS.12.familyxx.df is unwieldy to keep typing)
x <- get( df.name )

# remove the original copy of the two data frames from memory
rm( list = c( df.name ) )

# clear up RAM
gc()

#####################################
# merge x.sa and familyxx files #
#####################################

# note: the logical steps taken here could also be applied to 
# merging the personsx and samchild files

# so merging them together will require three variables:
# hhx (household unique identifier)
# fmx (family unique identifier)
# fpx (person unique identifier)

# store the names of these three columns in a character vector
merge.vars <- c( "hhx" , "fmx" , "fpx" )

# these two files have multiple overlapping (redundant) columns,
# so determine which columns are included in both data frames
# at the same time, enclose this statement in () thereby printing the vector to the screen
( columns.in.both.dfs <- intersect( names( x.sa ) , names( x ) ) )


# since the merge.vars will be used to merge the two data frames,
# those three variables should be excluded from the list of redundant columns
# keep all column names that don't match the merge variables
# at the same time, enclose this statement in () thereby printing the vector to the screen
( redundant.columns <- columns.in.both.dfs[ !( columns.in.both.dfs %in% merge.vars ) ] )

# notice that the three merge.vars have disappeared


# so shave the redundant columns off of the x.sa file
# keep all columns in the childcam file that are not in the redundant.columns vector
x.sa <- x.sa[ , !( names( x.sa ) %in% redundant.columns ) ]


# at this point, the only overlap between the samchild and childcam files
# should be the three merge.vars
# throw an error if that's not true
# stopifnot( merge.vars == intersect( names( x.sa ) , names( x ) ) )


# remember that the childcam file contains a subset of the individuals in samchild
# therefore, an inner join of the two should have the same number of records as childcam

# perform the actual merge
x.sa <- merge( x , x.sa )
# note that the merge() function merges using all intersecting columns -

# uncomment this line to see intersecting columns
# intersect( names( sa ) , names( x ) )

# - by default, the 'by' parameter does not need to be specified
# for more detail about the merge function, type ?merge in the console

# throw an error if the number of records in the merged file
# does not match the number of records in the samadult file
# stopifnot( nrow( x.sa ) == nrow( sa ) ) # not going to be true because more adults sampled


# now the x.sa data frame contains all of the rows in the childcam file and 
# all columns from both the samchild and childcam files
# therefore, there's no more need for the childcam file on its own
# so delete the childcam file
rm( x )

# and clear up RAM
gc()


# at this point, taylor-series linearization survey objects can be created
# so long as the analysis does not involve the imputed income variables


#################################################
# survey design for taylor-series linearization #
# not using any imputed income variables        #
#################################################

# create survey design object with NHIS design information
# using samchild alone
# samchild.design <- 
#   svydesign(
#     id = ~psu_p , 
#     strata = ~strat_p ,
#     nest = TRUE ,
#     weights = ~wtfa ,
#     data = x
#   )


# create survey design object with NHIS design information
# using the merged samchild and childcam files
cam.design <- 
  svydesign(
    id = ~psu_p , 
    strata = ~strat_p ,
    nest = TRUE ,
    weights = ~wtfa_sc ,	# note the change in the weighting variable
    data = x.sa				# note the change in the source data frame
  )

# Clean up extra names but need to keep path to incmim because of use in mi_income
# data import file
rm(x)
rm(childcam.name)
rm(columns.in.both.dfs)
rm(df.name)
rm(merge.vars)
rm(redundant.columns)
rm(year)
rm(path.to.childcam.file)
rm(path.to.samchild.file)
rm(path.to.familyxx.file)

# notice these two 'design' objects can be used 
# in all subsequent analysis commands that do not involve imputed income


#######################
# analysis examples   #
# sans imputed income #
#######################

# count the total (unweighted) number of records in nhis #

# # simply use the nrow function on both designs
# # nrow( personsx.design )
# nrow( cam.design )
# 
# # count the weighted number of individuals in nhis #
# 
# add a new variable 'one' that simply has the number 1 for each record #

cam.design <-
  update(
    one = 1 ,
    cam.design
  )
# 
# # the child civilian, non-institutionalized population of the united states #
# svytotal(
#   ~one ,
#   cam.design 
# )


############################################

