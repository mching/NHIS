# mi_income_setup.R
# Michael Ching, MD, MPH
# 
# This file loads the sample child and child CAM supplement datasets.
#
# The file has been adapted from work by anthony joseph damico
#
# Before this file can be run, you have to obtain the NHIS data as rda files
#
# Download script here:
# https://github.com/mching/asdfree/blob/master/National%20Health%20Interview%20Survey/personsx%20plus%20samadult%20with%20multiple%20imputation%20-%20analyze.R
# 
# You also need to load and merge the sample child and child cam files using this script:
# https://github.com/mching/NHIS/blob/master/scripts/load_and_merge_data.R


#######################################
# prepare data frames for an analysis #
# involving multiply-imputed income   #
#######################################


# from the code above, two large data frames are still in memory:
# x (the personsx data frame) and
# x.sa (the merged personsx-samadult data frame)

# note: the following steps to create a survey design with multiply-imputed variables
# can be applied using either the x or x.sa data frame.  the following example will use x.sa,
# but x can be used with the same techniques (however, the weighting variable must be changed)


# this example uses x.sa (the merged data frame), so delete x (the personsx data frame)
rm( x )

# and immediately clear up RAM
gc()


# # # # # # # # # # # #
# beginning of thinning to conserve RAM
# create a character vector containing only
# the variables in the merged file that are needed
# for this specific analysis.  this will keep RAM usage to a minimum.
# note: if working with more than 4 gigabytes of RAM, this step can be commented out
# variables.to.keep <-
#   c( 
#     # survey variables
#     "wtfa_sa" , "strat_p" , "psu_p" , 
#     
#     # merge variables
#     "hhx" , "fmx" , "fpx" , 
#     
#     # analysis variables
#     "aworpay" , "age_p"
#   )
# 
# # now actually overwrite the personsx-samadult merged file with a 'thinned'
# # version of itself, only containing the columns specified in variables.to.keep
# x.sa <- x.sa[ , variables.to.keep ]
# 
# # look at the first six records of the 'thinned' data frame
# head( x.sa )

# clear up RAM
gc()

# end of thinning to conserve RAM
# # # # # # # # # # # #

# now load the imputed income data frames
load( path.to.incmimp.file )		# this loads five data frames called ii1, ii2, ii3, ii4, and ii5


# loop through all five imputed income files
for ( i in 1:5 ){
  
  # create a temporary current.i data frame
  # containing the current iteration's (1 through 5) imputed income file
  current.i <- get( paste0( "ii" , i ) )
  
  # the 2014 imputed income merge fields are currently stored as character variables
  # and should immediately be converted over to numeric types
  merge.vars <- intersect( names( x.sa ) , names( current.i ) )
  
  # loop through all variables used in the merge
  # overwrite each column with itself, only converted to a numeric field
  for ( j in merge.vars ) x.sa[ , j ] <- as.numeric( x.sa[ , j ] )
  
  
  # a handy trick to view the class of all columns within a data frame at once:
  # sapply( x.sa , class )
  
  
  # merge the merged file with each of the five imputed income files
  y <- 
    merge( 
      x.sa , # the 2014 samadult-personsx merged data frame
      current.i # ii1 - ii5, depending on the current iteration of this loop
    )
  
  # and confirm the new data frame (merged + the current iteration of the multiply-imputed data)
  # contains the same number of records as the original merged file
  stopifnot( nrow( x.sa ) == nrow( y ) )
  
  
  ##############################
  # START OF VARIABLE RECODING #
  # any new variables that the user would like to create should be constructed here #
  
  # create two different poverty category variables
  y <- 
    transform( 
      y , 
      
      # note that these poverty categories go out to the tenth decimal
      
      # create an 'at or above 200% fpl' flag
      at.or.above.200 = ifelse( povrati3 >= 2 , 1 , 0 ) ,
      
      # create a four-category poverty variable
      fine.povcat =
        cut( 
          povrati3 , 
          c( -Inf , 1.38 , 2 , 4 , Inf ) ,
          labels = c( "<138%" , "138-200%" , "200-399%" , "400%+" )
        )
    )
  
  # to look closely at, for example, the first imputed income file, uncomment these lines:
  # head( ii1 )				# first six records of ii1
  # summary( ii1$povrati3 )	# summary statistics of the povrati3 column in ii1
  
  # END OF VARIABLE RECODING #
  ############################
  
  # save the data frames as objects x1 - x5, depending on the iteration in the loop
  assign( paste0( 'x' , i ) , y )
  
  # delete the y and ii# data frames
  y <- NULL
  assign( paste0( "ii" , i ) , NULL )
  
  # garbage collection - free up RAM from recently-deleted data tables
  gc()
}

# now that the five imputed income data frames have been created,
# free up ram by removing the original data frames
rm( x.sa )

# and immediately clear up RAM
gc()


# build a new survey design object,
# but unlike the 'personsx.design' or 'psa.design' objects above,
# this object contains the five multiply-imputed data tables - imp1 through imp5
cam.imp <- 
  svydesign( 
    id = ~psu_p , 
    strata = ~strat_p ,
    nest = TRUE ,
    weights = ~wtfa_sc ,	# note the change in the weighting variable
    data = imputationList( list( x1 , x2 , x3 , x4 , x5 ) )
  )

# note that survey design objects containing multiply-imputed data
# (like income in this case) must be analyzed using the MIcombine function 
# in the format of the examples below


# calculate the mean of a linear variable #

# average age
MIcombine( 
  with( 
    cam.imp , 
    svymean( ~age_p ) 
  ) 
)

# broken out by fine poverty categories
MIcombine( 
  with( 
    cam.imp , 
    svyby( 
      ~age_p , 
      ~fine.povcat ,
      svymean 
    ) 
  ) 
)

# This doesn't work for some reason below. The aworpay variable is missing.

# calculate the distribution of a categorical variable #
# 
# # any worries about paying medical bills
# MIcombine( 
#   with( 
#     cam.imp , 
#     svymean( 
#       ~factor( aworpay ) 
#     ) 
#   ) 
# )
# 
# # broken out by above/below 200% fpl
# MIcombine( 
#   with( 
#     psa.imp , 
#     svyby( 
#       ~factor( aworpay ) , 
#       ~at.or.above.200 ,
#       svymean 
#     ) 
#   ) 
# )
