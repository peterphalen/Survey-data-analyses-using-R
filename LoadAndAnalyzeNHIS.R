  #credit to A. Damaco at asdfree.com for most of the data prep code

  wants <- c("SAScii", "RCurl", "downloader", "digest", "survey", "mitools", "lawstat")
  has   <- wants %in% rownames(installed.packages())
  if(any(!has)) install.packages(wants[!has])
  
  
  #load packages#
  library(survey)   # load survey package (analyzes complex design surveys)
  library(mitools)	# allows analysis of multiply-imputed survey data
  require(lawstat)
  require(xlsx)
  require(survival)
  require(rms)
  require(MASS)
  
  
  # # # # # # # # # # # # # # # # # # # #
  # # block of code get data from NHIS... warning: this can literally take all night. uncomment to run
  # # # # # # # # # # # # # # # # # # # # 
  #  options( encoding = "windows-1252" )  	# # only macintosh and *nix users need this line
  #    library(downloader)
  #    setwd( "/Users/PeterPhalen/Google Drive/Data analysis" )
  #    nhis.years.to.download <- 2010
  #   source_url( "https://raw.github.com/ajdamico/asdfree/master/National%20Health%20Interview%20Survey/download%20all%20microdata.R" , 
  #                prompt = FALSE , 
  #                echo = TRUE )
  # # # # # # # # # # # # # # #
  # # end of auto-run block # #
  # # # # # # # # # # # # # # #
  
  
  
  #########################################################################################################
  # Analyze the National Health Interview Survey personsx, samadult, and imputed income files with R #
  #########################################################################################################
  
  # choose what year of data to analyze
  # note: this can be changed to any year that has already been downloaded locally
  year <- 2014
  comparedToYear <- 2009
  
  ## Minimum and maximum ages to include in dataset. 
  ## Your choice here subsets down your models/stats.
  ## If you want to use the complete data, just set 
  ## minimumAge to 0 and maximumAge to 999
  ##
  ## Note: minimumAge =< x < maximumAge
  
  minimumAge <- 18
  maximumAge <- 65
  
  #use this as a 'greater than or equal to' cutoff to determine who counts as distressed. 
  #Most common cut-off is 13 for SMI, see Kessler et al 2010 http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3659799/pdf/nihms447801.pdf
  SMIthreshold <- 13
  
  ## Select the analysis variables that you're interested in
  ## You need to specify these or your analyses will take 
  ## forever and/or your computer may crash
  
  variables.to.keep <-
    c( 
      ## survey variables: do not alter these
      "psu_p",   # (cluster)
      "strat_p", # (stratum)
      "wtfa_sa", # (weight) 
      
      # merge variables: do not alter these
      "hhx" ,  # (household unique identifier)
      "fmx" ,  # (family unique identifier)
      "fpx" ,  # (person unique identifier)

      
      # analysis variables: you can change these
      "age_p",   
      "SMI" ,
      "YEAR", 
      "coverage", 
      "povrati3", 
      "fine.povcat", 
      "above.138",
      "racreci3",
      "sex",
      "ahcsyr1", ## MH prof last 12 mo
      "WHITE",
      "educ1",
      "PublicInsurance",
      "Unemployment",
      "K6"
    
    )
  
  

  
  
  # set working directory
  setwd( "/Users/PeterPhalen/Google Drive/Data analysis" )
   
  # set R to produce conservative standard errors instead of crashing
  # http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
  options( survey.lonely.psu = "adjust" )
  options(stringsAsFactors=FALSE)
  
  # construct the filepath (within the current working directory) to the three rda files
  path.to.personsx.file <- paste( getwd() , year , "personsx.rda" , sep = "/" )
  path.to.samadult.file <- paste( getwd() , year , "samadult.rda" , sep = "/" )
  path.to.incmimp.file <- paste( getwd() , year , "incmimp.rda" , sep = "/" )
  
  # print those filepaths to the screen
  print( path.to.personsx.file )
  print( path.to.samadult.file )
  print( path.to.incmimp.file )
  
  # now the "NHIS.11.personsx.df" data frame can be loaded directly
  # from your local hard drive.  this is much faster.
  load( path.to.personsx.file )		# this loads a data frame called NHIS.11.personsx.df
  load( path.to.samadult.file )		# this loads a data frame called NHIS.11.samadult.df
  # the five imputed income files will be loaded later
  
  # all objects currently in memory can be viewed with the list function
  ls()
  
  # construct a string containing the data frame name of the personsx data table
  # stored within the R data file (.rda)
  # note: for 2014, this data frame will be named "NHIS.11.personsx.df"
  # but constructing it dynamically will allow analyses of other years
  # by simply changing the 'year' variable above
  df.name <- paste( "NHIS" , substr( year , 3 , 4 ) , "personsx" , "df" , sep = "." )
  
  # repeat this for the sample adult data frame, 
  # but not for the five imputed income data frames (which will be dealt with later)
  samadult.name <- paste( "NHIS" , substr( year , 3 , 4 ) , "samadult" , "df" , sep = "." )
  
  # copy the personsx data frame to the variable x for easier analyses
  # (because NHIS.11.personsx.df is unwieldy to keep typing)
  x <- get( df.name )
  
  # copy the samadult data frame to the variable sa for easier typing
  # (because NHIS.11.samadult.df is unwieldy to keep typing)
  sa <- get( samadult.name )
  
  # remove the original copy of the two data frames from memory
  rm( list = c( df.name , samadult.name ) )
  
  # clear up RAM
  gc()
  
  
  
  #####################################
  # merge personsx and samadult files #
  #####################################

  # the personsx and samadult files are both at the individual or person-level
  # (as opposed to family-level or household-level)
  # so merging them together will require three variables:

  
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
  
  
  # most analyses start with the personsx file,
  # so shave the redundant columns off of the samadult file
  # keep all columns in the samadult file that are not in the redundant.columns vector
  sa <- sa[ , !( names( sa ) %in% redundant.columns ) ]
  
  # at this point, the only overlap between the personsx and samadult files
  # should be the three merge.vars
  # throw an error if that's not true
  stopifnot( merge.vars == intersect( names( sa ) , names( x ) ) )
  
  
  # remember that the samadult file contains a subset of the individuals in personsx
  # therefore, an inner join of the two should have the same number of records as samadult
  
  # perform the actual merge
  x.saPost <- merge( x , sa )
  # note that the merge() function merges using all intersecting columns -
  
  # uncomment this line to see intersecting columns
  # intersect( names( sa ) , names( x ) )
  
  # - by default, the 'by' parameter does not need to be specified
  # for more detail about the merge function, type ?merge in the console
  
  # throw an error if the number of records in the merged file
  # does not match the number of records in the samadult file
  stopifnot( nrow( x.saPost ) == nrow( sa ) )
  
  
  # now the x.saPost data frame contains all of the rows in the samadult file and 
  # all columns from both the samadult and personsx files
  # therefore, there's no more need for the samadult file on its own
  # so delete the samadult file
  rm( sa )
  
  # and clear up RAM
  gc()
  
  
  # since the samadult file is a subset of the personsx file,
  # the personsx.design should always be used if it contains
  # all variables necessary for the analysis.  the sample size is larger.
  
  
  
  #######################################
  # prepare data frames for an analysis #
  # involving multiply-imputed income   #
  #######################################
  
  rm( x )
  
  # and immediately clear up RAM
  gc()
  
  
  
  
  # now load the imputed income data frames
  load( path.to.incmimp.file )		# this loads five data frames called ii1, ii2, ii3, ii4, and ii5
  
  
  # loop through all five imputed income files
  for ( i in 1:5 ){
    
    # create a temporary current.i data frame
    # containing the current iteration's (1 through 5) imputed income file
    current.i <- get( paste0( "ii" , i ) )
    
    # the 2014 imputed income merge fields are currently stored as character variables
    # and should immediately be converted over to numeric types
    merge.vars <- intersect( names( x.saPost ) , names( current.i ) )
    
    # loop through all variables used in the merge
    # overwrite each column with itself, only converted to a numeric field
    for ( j in merge.vars ) x.saPost[ , j ] <- as.numeric( x.saPost[ , j ] )
    
    
    # a handy trick to view the class of all columns within a data frame at once:
    # sapply( x.saPost , class )
    
    
    # merge the merged file with each of the five imputed income files
    y <- 
      merge( 
        x.saPost , # the 2014 samadult-personsx merged data frame
        current.i # ii1 - ii5, depending on the current iteration of this loop
      )
    
    # and confirm the new data frame (merged + the current iteration of the multiply-imputed data)
    # contains the same number of records as the original merged file
    stopifnot( nrow( x.saPost ) == nrow( y ) )
    
    
    ##############################
    # START OF VARIABLE RECODING #
    # any new variables that the user would like to create should be constructed here #
    
    # create two different poverty category variables 
    y <- 
      transform(
        y, 
        povrati3 = ifelse(povrati3 == 999999, NA, povrati3),
        ahcsyr1 = ifelse(ahcsyr1 < 7, ahcsyr1, NA),        
        notcov = ifelse(notcov < 7, notcov, NA),
        asisad = ifelse(asisad < 7, asisad, NA),
        asinerv = ifelse(asinerv < 7, asinerv, NA),
        asihopls = ifelse(asihopls < 7, asihopls, NA),
        asirstls = ifelse(asirstls < 7, asirstls, NA),
        asieffrt = ifelse(asieffrt < 7, asieffrt, NA),
        asiwthls = ifelse(asiwthls < 7, asiwthls, NA)        
      )
    y <- 
      transform( 
        y , 
        
        # note that these poverty categories go out to the tenth decimal
        
        # create an 'at or above 138% fpl' flag
        above.138 = factor( as.numeric(povrati3 > 1.38) , labels = c("In poverty", "Above 138% poverty line"), levels=c(0,1), exclude=NA)  ,
        
        # create a four-category poverty variable
        fine.povcat =
          cut( 
            povrati3 , 
            c( -Inf , 1.38 , 2 , 4 , Inf ) ,
            labels = c( "<138%" , "138-200%" , "200-399%" , "400%+" )
          ),
        
        educ1 = ifelse(educ1>22, NA, as.numeric(educ1)),
        coverage = factor( as.numeric(notcov==2), labels = c("Not covered now", "Covered now"), levels=c(0,1),exclude=NA),
        ahcsyr1 = factor(as.numeric(ahcsyr1==1), labels = c("No mh prof", "Saw mh prof"), levels=c(0,1), exclude = NA),
        YEAR = factor(1, labels = year),
        WHITE = factor(as.numeric(racreci3 == 1), labels = c("NonWhite", "White"), exclude = NA),
        sex = factor(sex, labels=c("Male", "Female")),
        
        Unemployment = ifelse(wrklyr4 < 7, wrklyr4, NA),
        
        PublicInsurance = ifelse(medicaid <= 2, 1, ifelse( medicare <=2 ,1, 0))   
        
        
      )
    y <- transform(
      y,
    #create psychological distress variable
    
    K6 = 
      (5-asisad + 
         5-asinerv + 
         5-asihopls + 
         5-asirstls + 
         5-asieffrt + 
         5-asiwthls)
    )
    
    y <- transform(
      y,
    SMI = factor(as.numeric(K6 >= SMIthreshold), labels=c("No Serious Distress", "Serious Psych Distress (SMI)"), levels=c(0,1),exclude=NA)
    )
  
    # END OF VARIABLE RECODING #
    ############################
    
    # save the data frames as objects x1 - x5, depending on the iteration in the loop
    assign( paste0(  'x' , i , 'post') , y[,variables.to.keep] )
    
    # delete the y and ii# data frames
    y <- NULL
    assign( paste0( "ii" , i ) , NULL )
    
    # garbage collection - free up RAM from recently-deleted data tables
    gc()
  }
  rm(current.i)
  
 
    post.i <-  x1post 
  post.i <- post.i[ , variables.to.keep ]

  
  psa.Post <- 
    svydesign( 
      id = ~psu_p , 
      strata = ~strat_p ,
      nest = TRUE ,
      weights = ~wtfa_sa,  # note the change in the weighting variable
      data = post.i
    )
  psa.Post <- subset(psa.Post, age_p >= minimumAge & age_p < maximumAge)
  
  psa.impPost <- 
    svydesign( 
      id = ~psu_p , 
      strata = ~strat_p ,
      nest = TRUE ,
      weights = ~wtfa_sa,  # note the change in the weighting variable
      data = imputationList( list( x1post , x2post , x3post , x4post , x5post ) )
    )
  psa.impPost <- subset(psa.impPost, age_p >= minimumAge & age_p < maximumAge)
  
  

  
  year <- comparedToYear 
  
  path.to.incmimp.file <- paste( getwd() , year , "incmimp.rda" , sep = "/" )
  
  options( survey.lonely.psu = "adjust" )
  path.to.personsx.file <- paste( getwd() , year , "personsx.rda" , sep = "/" )
  path.to.samadult.file <- paste( getwd() , year , "samadult.rda" , sep = "/" )
  path.to.incmimp.file <- paste( getwd() , year , "incmimp.rda" , sep = "/" )
  print( path.to.personsx.file )
  print( path.to.samadult.file )
  print( path.to.incmimp.file )
  load( path.to.personsx.file )  	# this loads a data frame called NHIS.11.personsx.df
  load( path.to.samadult.file )		# this loads a data frame called NHIS.11.samadult.df
  ls()
  df.name <- paste( "NHIS" , substr( year , 3 , 4 ) , "personsx" , "df" , sep = "." )
  samadult.name <- paste( "NHIS" , substr( year , 3 , 4 ) , "samadult" , "df" , sep = "." )
  x <- get( df.name )
  
  sa <- get( samadult.name ) 
  
  rm( list = c( df.name , samadult.name ) )
  gc()
  #####################################
  # merge personsx and samadult files #
  #####################################
  merge.vars <- c( "hhx" , "fmx" , "fpx" )
   columns.in.both.dfs <- intersect( names( sa ) , names( x ) ) 
   redundant.columns <- columns.in.both.dfs[ !( columns.in.both.dfs %in% merge.vars ) ] 
  sa <- sa[ , !( names( sa ) %in% redundant.columns ) ]
  stopifnot( merge.vars == intersect( names( sa ) , names( x ) ) )
  x.saPre <- merge( x , sa )
  stopifnot( nrow( x.saPre ) == nrow( sa ) )
  load( path.to.incmimp.file )  	# this loads five data frames called ii1, ii2, ii3, ii4, and ii5
  # loop through all five imputed income files
  for ( i in 1:5 ){
    current.i <- get( paste0("ii" , i ) )
    merge.vars <- intersect( names( x.saPre ) , names( current.i ) )
    for ( j in merge.vars ) x.saPre[ , j ] <- as.numeric( x.saPre[ , j ] )
    # sapply( x.saPost , class )
    y <- 
      merge( 
        x.saPre , # the samadult-personsx merged data frame
        current.i # ii1 - ii5, depending on the current iteration of this loop
      )
    stopifnot( nrow( x.saPre ) == nrow( y ) )
    ##############################
    # START OF VARIABLE RECODING #
    # any new variables that the user would like to create should be constructed here #
    # create two different poverty category variables 
    y <- 
      transform( 
        y ,
        notcov <- ifelse(notcov < 7, notcov, NA),        
        ahcsyr1 <- ifelse(ahcsyr1 < 7, ahcsyr1, NA) ,
        educ1 = ifelse(educ1>22, NA, as.numeric(educ1))
 
      )
    
    y <- 
      transform( 
        y , 
        coverage = factor( as.numeric(notcov==2), labels = c("Not covered now", "Covered now"), levels=c(0,1),exclude=NA),
        ahcsyr1 = factor(as.numeric(ahcsyr1==1), labels = c("No mh prof", "Saw mh prof"), levels=c(0,1), exclude = NA),
        YEAR = factor(1, labels = year),
        WHITE = factor(as.numeric(racreci3 == 1), labels = c("NonWhite", "White"), exclude = NA),
        sex = factor(sex, labels=c("Male", "Female")),
        PublicInsurance = ifelse(medicaid <= 2, 1, ifelse( medicare <=2 ,1, 0))
      )
    
    
  #create psychological distress variable (the lables for these variables changed in 2013)
  if (year < 2013 ){
    
    y <- 
      transform(
        y ,
        sad = ifelse(sad < 7, sad, NA),
        nervous = ifelse(nervous < 7, nervous, NA),
        hopeless = ifelse(hopeless < 7, hopeless, NA),
        restless = ifelse(restless < 7, restless, NA),
        effort = ifelse(effort < 7, effort, NA),
        worthls = ifelse(worthls < 7, worthls, NA)
        
        )
    
    y <- 
      transform(
        y ,
        K6 = 
          (5-sad + 
             5-nervous + 
             5-hopeless + 
             5-restless + 
             5-effort + 
             5-worthls)     
    )
    
    y <- 
      transform( 
        y , 
  SMI = factor(as.numeric(K6 >= SMIthreshold), labels=c("No Serious Distress", "Serious Psych Distress (SMI)"), levels=c(0,1), exclude=NA)
  )
  
  }else{
    
    y <-
      transform(
        y ,
        asisad = ifelse(asisad < 7, asisad, NA),
        asinerv = ifelse(asinerv < 7, asinerv, NA),
        asihopls = ifelse(asihopls < 7, asihopls, NA),
        asirstls = ifelse(asirstls < 7, asirstls, NA),
        asieffrt = ifelse(asieffrt < 7, asieffrt, NA),
        asiwthls = ifelse(asiwthls < 7, asiwthls, NA)
      )
    
    y <- 
      transform( 
        y , 
        K6 = 
          (5-asisad + 
             5-asinerv + 
             5-asihopls + 
             5-asirstls + 
             5-asieffrt + 
             5-asiwthls) )
    
    y <- 
      transform( 
        y , 

        SMI = factor(as.numeric(K6 >= SMIthreshold), labels=c("No Serious Distress", "Serious Psych Distress (SMI)"), levels=c(0,1),exclude=NA))
        
  }
  if (year >= 2010){
    y <- 
      transform( 
        y , 
    povrati3 = ifelse(povrati3 == 999999, NA, povrati3/1000),
    Unemployment = ifelse(wrklyr4 < 7, wrklyr4, NA)
    
      )
     
  }
  if (year <= 2009){
    y <- 
      transform( 
        y , 
        povrati3 = ifelse(povrati2 == 9999, NA, povrati2/100),
        Unemployment = ifelse(wrklyr3 < 7, wrklyr3, NA)
        
      )
    
  }
  y <- 
    transform( 
      y , 
      above.138 = factor( as.numeric(povrati3 > 1.38) , labels = c("In poverty", "Above 138% poverty line"), levels=c(0,1), exclude=NA)  ,
      # create a four-category poverty variable
      fine.povcat =
        cut( 
          povrati3 , 
          c( -Inf , 1.38 , 2 , 4 , Inf ) ,
          labels = c( "<138%" , "138-200%" , "200-399%" , "400%+" )
        )
    )
  
  
  
    # END OF VARIABLE RECODING #
    ############################
    assign( paste0( 'x' , i ,'pre') , y[,variables.to.keep] )
  
    y <- NULL
    assign( paste0("ii" , i ) , NULL )
    gc()
  }
  

    pre.i <- x1pre
    pre.i <- pre.i[ , variables.to.keep ]
  
  
  psa.Pre <- 
    svydesign( 
      id = ~psu_p , 
      strata = ~strat_p ,
      nest = TRUE ,
      weights = ~wtfa_sa,  # note the change in the weighting variable
      data = pre.i
    )
  psa.Pre <- subset(psa.Pre, age_p >= minimumAge & age_p < maximumAge)
  
  psa.impPre <- 
    svydesign( 
      id = ~psu_p , 
      strata = ~strat_p ,
      nest = TRUE ,
      weights = ~wtfa_sa,  # note the change in the weighting variable
      data = imputationList( list( x1pre , x2pre , x3pre , x4pre , x5pre ) )
    )
  psa.impPre <- subset(psa.impPre, age_p >= minimumAge & age_p < maximumAge)
  
  
  psa.noImp <- 
    svydesign( 
      id = ~psu_p , # cluster ids
      strata = ~strat_p , # stratification levels 
      nest = TRUE , # stratification is nested
      weights = ~wtfa_sa,  # weights
      data = rbind(pre.i,post.i) ) 
    
  
  psa.noImp <- subset(psa.noImp, age_p >= minimumAge & age_p < maximumAge)
  
  
  
  for ( i in 1:5 ){
    # create a temporary current.i data frame
    # merge the years and delete the leftovers
    assign(paste0("x",i), rbind(pre.i,post.i))
    assign(paste0("x",i), rbind(pre.i,post.i))
    assign(paste0( "x" , i, "pre" ), NULL)
    assign(paste0( "x" , i, "post" ), NULL)
  }

   
  rm(sa)
  rm(pre.i)
  rm(post.i)
  rm(x)
  rm(x.saPre)
  rm(x.saPost)
  rm(current.i)
  
  # clear up RAMrm
  gc()
  
  
  psa.imp <- 
    svydesign( 
      id = ~psu_p , # cluster ids
      strata = ~strat_p , # stratification levels 
      nest = TRUE , # stratification is nested
      weights = ~wtfa_sa,	# weights
      data = imputationList( list( x1 , # income imputations
                                   x2 ,
                                   x3 ,
                                   x4 ,
                                   x5 ) ) 
    )
  
  psa.imp <- subset(psa.imp, age_p >= minimumAge & age_p < maximumAge)
 
  for (i in 1:5) { assign(paste0( "x" , i ), NULL)}  
  gc()
  
  
  
  
  ############################
  ##### END OF DATA PREP #####
  ############################
  
  
  
  sinkFile <- paste(getwd(),"/TempOutput", year,".txt", sep="")
  sink(sinkFile, type="output")
  
  
  cat("
################################################################################
######## This is the auto-generated output of R code written to display apparent
######## health disparities between people with/without Serious Psychological
######## Distress (SMI) in the United States. 
######## 
######## Author: Peter Phalen 
######## (with help from code by A. Damico [http://www.asdfree.com/])
######## Dataset: National Health Interview Survey (NHIS; http://www.cdc.gov/nchs/nhis.htm)
########          Representative household survey; complex sample design.
###########################


  Minimum age for inclusion: ", minimumAge,"
  Maximum age for inclusion: ", maximumAge,"
        

################################################################################
 
      
      
      Count of the USA population with and without Serious Psychological Distress (SMI)
            
      
      ###### in",year,"
      
      ")
  
  svytotal(~as.character(SMI),design = psa.Pre, na.rm=T)
  
  cat("
      
      ###### in 2014
      
      ")
  svytotal(~as.character(SMI),design = psa.Post, na.rm=T)
  
  
  ################################################################################
  
  
  cat("
  Percent (%) of the USA population with and without Serious Psychological Distress (SMI)
  
  
  ###### in",year,"
  
  ")
  
  svymean(~as.character(SMI),design = psa.Pre, na.rm=T)
  
  cat("
  
  ###### in 2014
  
  ")
  
  svymean(~as.character(SMI),design = psa.Post, na.rm=T)
  
  
    
  
cat(


"################################################################################
 


Percent (%) of the USA population holding health insurance coverage at the time of survey 
(Split up between those With vs. Without SMI)

 The \"results\" columns hold percentages. E.g., 0.30 = 30% of the corresponding population.
      

###### in",year,"

      ")
  
  (svyby(~factor(coverage),~as.character(SMI),svymean, vartype="ci",design = psa.Pre, na.rm=T))
  
  cat("

###### in 2014

      ")
  (svyby(~factor(coverage),~as.character(SMI),svymean, vartype="ci",design = psa.Post, na.rm=T) )
  
  cat("################################################################################




   % who saw a mental health professional in past 12 months 
       
####### in ",year,"
      
      ")
  
 ( svyby(~factor(ahcsyr1),~as.character(SMI),design = psa.Pre, vartype="ci", svymean,na.rm=T) )
  
  cat("
      
###### in 2014

      ")
  
 ( svyby(~factor(ahcsyr1),~as.character(SMI),svymean,design = psa.Post, vartype="ci", na.rm=T) )
  
  cat("################################################################################



#  % of people at 138% of poverty line. (Note: people below 138% qualified for expanded 
#  medicaid as of January 1, 2014)
      
###### in ",year,"
      
      ")
  
  summary(MIcombine(with( psa.impPre , 
                          svyby(~factor(above.138),~as.character(SMI),svymean,na.rm=T))) )  
  
  cat("
      
###### in 2014

      ")
  summary(MIcombine(with( psa.impPost , 
                          svyby(~factor(above.138),~as.character(SMI),svymean,na.rm=T))) )
  
  
  cat("################################################################################



Average ages of people with and without SMI
      
###### in ",year,"
      
      ")
  svyby(~age_p,~as.character(SMI),svymean,design = psa.Pre,vartype="ci",na.rm=T)
  cat("
      

###### in 2014

      ")
  svyby(~age_p,~as.character(SMI),svymean,design = psa.Post, vartype="ci",na.rm=T)
  
  
  cat("################################################################################

      
      
      Level of unemployment 
           0 = Had job last week
           1 = No job last week, had job past 12 months
           2 = No job last week, no job past 12 months
           3 = Never worked
      
      ###### in ",year,"
      
      ")
  svyby(~age_p,~as.character(SMI),svymean,design = psa.Pre,vartype="ci",na.rm=T)
  cat("
      
      
      ###### in 2014
      
      ")
  svyby(~age_p,~as.character(SMI),svymean,design = psa.Post, vartype="ci",na.rm=T)
  
  
  
  psa.CovPre <-  subset(psa.Pre, coverage=="Covered now")
  psa.CovPost <-  subset(psa.Post, coverage=="Covered now")
  
  
  cat("
  
  #########################################################################################################
  # What predicts SMI?
  #########################################################################################################
  
  ")
  
  summary(MIcombine( with( psa.impPost , 
                           svyglm(factor(SMI) ~
                                    as.character(above.138) +
                                    as.character(WHITE) +
                                    as.character(sex) +
                                    educ1 +
                                    age_p +
                                    Unemployment,
                                  family=quasibinomial())
  )
  ))
  
  cat("
  
  #########################################################################################################
  # Who is seeking MH treatment?
  #########################################################################################################
  
  ")
  
  
  summary(MIcombine( with( psa.impPost , 
                           svyglm(factor(ahcsyr1) ~
                                    as.character(above.138) +
                                    as.character(WHITE) +
                                    as.character(sex) +
                                    educ1 +
                                    age_p +
                                    Unemployment +
                                    as.character(SMI) ,
                                  family=quasibinomial())
  )
  ))
  
  
  cat("

################################################################################



Among people with any insurance, what % were covered via medicaid or medicare?
      
###### in ",year,"
      
      ")
  
  svyby(~factor(PublicInsurance),~as.character(SMI),svymean,vartype="ci",design=psa.CovPre,na.rm=T) 
  
  cat("
      
###### in 2014
      
      ")
  svyby(~factor(PublicInsurance),~as.character(SMI),svymean,design=psa.CovPost,vartype="ci",na.rm=T) 
  
  
  rm(psa.CovPre)
  rm(psa.CovPost)
  rm(psa.Pre)
  rm(psa.Post)
  
  # MIcombine(with( psa.impPre, svyplot(povrati3~age_p)))
  rm(psa.impPre)


  
  cat("
################################################################################

 #####################
 #### Differences in differences (DIDs) for health insurance coverage (SMI v. no-SMI).
 #### The interaction effect (SMI:YEAR) tells us how much the disparity between SMI
 #### and non-SMI closed during the intervening time. Positive numbers mean the gap  
 #### tended to close. Negative numbers mean the gap got worse.
 #######


      ")
  
  summary(
                           svyglm(as.numeric(coverage) ~
                                    as.character(SMI) + 
                                    as.character(YEAR) + 
                                    as.character(SMI):as.character(YEAR),
                                  design=psa.noImp)
                      )
                
          
  
  
  
  cat("

################################################################################


 ############    
 #### DIDs for seeing a MH professional in past 12 months (SMI v. no SMI).
 #### A positive interaction effect means people with SMI are starting to 
 #### see mental health professionals relatively more often as time goes on.
 #######

      ")
  summary(
                           svyglm(as.numeric(ahcsyr1) ~
                                    as.character(SMI) + 
                                    as.character(YEAR) + 
                                    as.character(SMI):as.character(YEAR), 
                                  design=psa.noImp)
  )
  
  
  cat("
################################################################################


 ###############  
 #### Predictors of health insurance coverage (2014 only)?
 #### What are the predictors of being covered by health insurance?
 ###### 

      ")
  summary(MIcombine( with( psa.impPost , 
                           svyglm(factor(coverage) ~
                                    as.character(above.138) +
                                    educ1 +
                                    as.character(WHITE) +
                                    as.character(sex) +
                                    age_p +
                                    Unemployment +
                                    as.character(SMI) ,
                                  family=quasibinomial())
  )
  ))
  
  cat("
################################################################################


 ###############  
 #### Predictors of seeing a mental health professional in past 12 months (2014 only)?
 #### What are the predictors of seeking treatment for mental health?
 ###### 

      ")

  summary(MIcombine( with( psa.impPost , 
                           svyglm(factor(ahcsyr1) ~
                                    as.character(above.138) +
                                    educ1 +
                                    as.character(WHITE) +
                                    as.character(sex) +
                                    age_p +
                                    coverage +
                                    Unemployment +
                                    as.character(SMI) ,
                                  family=quasibinomial())
  )
  )
  )

  cat("
################################################################################


 ###############  
 #### DIDs for health insurance coverage, SMI v. no-SMI 
 #### controlling for poverty status, race, sex, and age
 ######   
      

      ")
  
  
  summary(MIcombine( with( psa.imp , 
                           svyglm(as.numeric(coverage) ~
                                    educ1 +
                                    as.character(above.138) +
                                    as.character(WHITE) +
                                    as.character(sex) +
                                    age_p +
                                    Unemployment +
                                    as.character(SMI) + 
                                    as.character(YEAR) + 
                                    as.character(SMI):as.character(YEAR))
                                )
                  ) 
  )
  
  cat("
################################################################################

      
      ###############  
      #### Predictors of having public insurance, among those insured
      #### 
      ###### 
      
      ")
  
  psa.impCovPost <-  subset(psa.impPost, !is.na(coverage))
  
  psa.impCovPost <-  subset(psa.impCovPost, coverage=="Covered now")
  
  psa.impCov <-  subset(psa.imp, !is.na(coverage))
  
  psa.impCov <-  subset(psa.impCov, coverage=="Covered now")
  
  
  summary(MIcombine( with( psa.impCovPost , 
                           svyglm(factor(PublicInsurance) ~
                                    as.character(above.138) +
                                    educ1 +
                                    as.character(WHITE) +
                                    as.character(sex) +
                                    age_p +
                                    Unemployment +
                                    as.character(SMI) ,
                                  family=quasibinomial())
  )
  ))
  
  cat("
      ################################################################################
      

      
      ###############  
      #### DIDs for having public insurance, among those insured
      ###### 

")


  summary(
  svyglm(as.numeric(PublicInsurance) ~
  as.character(SMI) + 
  as.character(YEAR) + 
  as.character(SMI):as.character(YEAR),
  design=psa.noImp)
  )
  
  
  
cat("

      ###############  
      #### DIDs for having public insurance, among those insured
      #### controlling for poverty status, race, sex, and age
      ######   
      
      
      ")

  summary(MIcombine( with( psa.imp , 
                           svyglm(as.numeric(PublicInsurance) ~
                                    educ1 +
                                    as.character(above.138) +
                                    as.character(WHITE) +
                                    as.character(sex) +
                                    age_p +
                                    Unemployment +
                                    as.character(SMI) + 
                                    as.character(YEAR) + 
                                    as.character(SMI):as.character(YEAR))
  )
  ))
  
  rm(psa.impCov)
  rm(psa.impCovPost)
  
  cat("

################################################################################

 ###############  
 #### DIDs for seeing mental health clinician in past 12 months (SMI v. no-SMI) 
 ########       

      ")
  
  summary(
        svyglm(as.numeric(ahcsyr1) ~
                  as.character(SMI) + 
                  as.character(YEAR) +
                  as.character(SMI):as.character(YEAR),
               design=psa.noImp) )
  ))
  
  cat("

################################################################################
      
      ###############  
      #### DIDs for seeing mental health clinician in past 12 months (SMI v. no-SMI) 
      #### 2014
      ########       
      
      ")
  
  summary(MIcombine( with( psa.impPost , 
                           svyglm(as.numeric(ahcsyr1) ~
                                    as.character(above.138) +
                                    as.character(WHITE) +
                                    as.character(sex) +
                                    Unemployment +
                                     educ1+
                                    age_p +
                                    as.character(SMI)  ) 
  )))
  
  cat("

      
      ###############  
      #### DIDs for seeing mental health clinician in past 12 months (SMI v. no-SMI) 
      #### controlling for poverty status, race, sex, and age
      ########       
      
      ")
  summary( MIcombine( with( psa.imp , 
                                               svyglm(as.numeric(ahcsyr1) ~
                                                        as.character(above.138) +
                                                        as.character(WHITE) +
                                                        as.character(sex) +
                                                        age_p +
                                                        educ1 +
                                                        Unemployment +
                                                        as.character(SMI) + 
                                                        as.character(YEAR)  +
                                                        as.character(SMI):as.character(YEAR)
                                                      )
  )))
  
  
  
  cat("
      
Racial disparity
      
      ")
  
  summary(MIcombine( with( psa.imp , 
                                     svyglm(as.numeric(coverage) ~
                                              povrati3 +
                                              educ1 +
                                              as.character(SMI) +
                                              as.character(sex) +
                                              age_p +
                                              Unemployment +
                                              as.character(WHITE) + 
                                              as.character(YEAR)  +
                                              as.character(WHITE):as.character(YEAR)
                                     )
  )))
  
 

  
  
  sink()
  file.show(sinkFile)
  gc()
  rm(list = ls())
  
  

  
  