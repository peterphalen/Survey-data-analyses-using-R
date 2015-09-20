
#download packages you don't have. First four are only needed for getting 
#the data from NHIS in the first place  
wants <- c("SAScii", "RCurl", "downloader", "digest", "survey", "mitools")
  has   <- wants %in% rownames(installed.packages())
  if(any(!has)) install.packages(wants[!has])
  
  
  #load packages#
  library(survey)   # load survey package (analyzes complex design surveys)
  library(mitools)	# allows analysis of multiply-imputed survey data  
  
  # # # # # # # # # # # # # # # # # # # #
  # # block of code to get data from NHIS... warning: this can literally take all night. uncomment to run
  # # # # # # # # # # # # # # # # # # # # 
  #  options( encoding = "windows-1252" )  	# only macintosh and *nix users need this line
  #    library(downloader) 
  #    setwd( "/Users/PeterPhalen/Google Drive/Data analysis" )
  #    nhis.years.to.download <- c(2010,2014)  # choose the years that you want to get
  #    source_url( "https://raw.github.com/ajdamico/asdfree/master/National%20Health%20Interview%20Survey/download%20all%20microdata.R" , 
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
  comparedToYear <- 2010
  
  ## Minimum and maximum ages to include in dataset. 
  ## Your choice here subsets down your models/stats.
  ## If you want to use the complete data, just set 
  ## minimumAge to 0 and maximumAge to 999
  ##
  ## Note: minimumAge =< x < maximumAge
  
  minimumAge <- 18
  maximumAge <- 65
  
  #use this as a 'greater than or equal to' cutoff for the K6 
  #to determine who counts as distressed. Most common cut-off is 13 for SMI, 
  # see Kessler et al 2010 
  # http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3659799/pdf/nihms447801.pdf
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
      "age_p",       # age, continuous (native to dataset)
      "K6",          # K6 Serious Psychological Distress score, continuous, 0-24
      "SMI" ,        # K6 >= SMIthreshold, 0 = No Serious Mental Illness (SMI), 1 = SMI
      "YEAR",        # Survey year, 0 = Pre, 1 = Post, recoded later
      "coverage",    # Health insurance coverage, 0 = Not covered at time of 
                     # survey, 1 = covered
      "povrati3",    # Earnings / poverty line, continous, native to dataset, 
                     # e.g., 1 = at poverty line, 1.33 = 133% poverty line
      "fine.povcat", # Poverty in four categories 
      "above.138",   # 0 = Below 138% poverty line, 1 = Above
      "sex",         # 1 = Male, 2 = Female (native to dataset)
      "ahcsyr1",     # Saw any mental health professional within last 12 months
      "WHITE",       # 0 = non-White, 1 = White
      "educ1",       # Level of education, ordinal, ranges from 0-21
      "PublicInsurance",  # 1 = Covered by medicaid or medicare, 0 = Anyone else
      "Unemployment"      # level of unemployment, ordinal, 0 = Had job last week, 
                                                          # 1 = No job last week, had job past 12 months  
                                                          # 2 = No job last week, no job past 12 months, 
                                                          # 3 = Never worked
    
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
  
  
  ########################################################  
  ### BEGIN DATA PREP FOR 'POST' YEAR ####################  
  ######################################################## 
  
  
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
    sapply( x.saPost , class )
    
    
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
    
    y <- 
      transform(
        y, 
        
        #### Most variables have codes for things like "Refused to answer"
        #### This block of recodes is intended to set those kinds of codes
        #### to NA   
        povrati3 = ifelse(povrati3 == 999999, NA, povrati3), #poverty line ratio (999999 is an NA code)
        ahcsyr1 = ifelse(ahcsyr1 < 7, ahcsyr1, NA), #seen mh prof in past 12 months        
        notcov = ifelse(notcov < 7, notcov, NA), #Insurance coverage
        Unemployment = ifelse(wrklyr4 < 7, wrklyr4, NA), #level of unemployment
        educ1 = ifelse(educ1>22, NA, as.numeric(educ1)), #level of education
        asisad = ifelse(asisad < 7, asisad, NA), #The rest are K6 components
        asinerv = ifelse(asinerv < 7, asinerv, NA),
        asihopls = ifelse(asihopls < 7, asihopls, NA),
        asirstls = ifelse(asirstls < 7, asirstls, NA),
        asieffrt = ifelse(asieffrt < 7, asieffrt, NA),
        asiwthls = ifelse(asiwthls < 7, asiwthls, NA)        
      )
    y <- 
      transform( 
        y , 
                
        # create an 'at or above 138% fpl' flag
        # this number is significant because people below 138% of the poverty line 
        # qualified for the medicaid expansion on January 1, 2014
        above.138 = factor( as.numeric(povrati3 > 1.38) , labels = c("In poverty", "Above 138% poverty line"), levels=c(0,1), exclude=NA)  ,
        
        # create a four-category poverty variable
        fine.povcat =
          cut( 
            povrati3 , 
            c( -Inf , 1.38 , 2 , 4 , Inf ) ,
            labels = c( "<138%" , "138-200%" , "200-399%" , "400%+" )
          ),
        
        ## Recode variables of interest into factors and label the values
        coverage = factor( as.numeric(notcov==2), labels = c("Not covered now", "Covered now"), levels=c(0,1),exclude=NA),
        ahcsyr1 = factor(as.numeric(ahcsyr1==1), labels = c("No mh prof", "Saw mh prof"), levels=c(0,1), exclude = NA),
        YEAR = factor(1, labels = year),
        WHITE = factor(as.numeric(racreci3 == 1), labels = c("NonWhite", "White"), exclude = NA),
        sex = factor(sex, labels=c("Male", "Female")),
        PublicInsurance = ifelse(medicaid <= 2, 1, ifelse( medicare <=2 ,1, 0))   
      )
    
    y <- transform(
      y,
    # Calculate K6 score
    # Note: items are scored 1-5 in this data set
    # and need to be reverse scored and scaled to 
    # range from 0-24
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
    # Code 'Serious Psychological Distress (SMI)' variable based on specified K6 threshold
    # Note: because this recodes an already recoded variable (K6) it has to be calculated in
    #       its own block here 
    SMI = factor(as.numeric(K6 >= SMIthreshold), labels=c("No Serious Distress", "Serious Psych Distress (SMI)"), levels=c(0,1),exclude=NA)
    )
  
    # END OF VARIABLE RECODING #
    ############################
    
    # save the data frames as objects x1 - x5, depending on the iteration in the loop.
    # at the same time restrict the resulting dataset to the variables of interest 
    # specified above. If we don't do this the script takes much, much longer.
    assign( paste0(  'x' , i , 'post') , y[,variables.to.keep] )
    
    # delete the y and ii# data frames
    y <- NULL
    assign( paste0( "ii" , i ) , NULL )
    
    # garbage collection - free up RAM from recently-deleted data tables
    gc()
  }
  
  #clean up intermediary data
  rm(current.i)
  
  # To minimize processing time for analyses that *don't* require multiply imputed income
  # we want to have one dataset available that has all the recoded variables 
  post.i <-  x1post
  
  #create survey design object for all the Post analyses that don't require info on income
  psa.Post <- 
    svydesign( 
      id = ~psu_p , 
      strata = ~strat_p ,
      nest = TRUE ,
      weights = ~wtfa_sa,  
      data = post.i
    )
  #restrict to specified age range
  psa.Post <- subset(psa.Post, age_p >= minimumAge & age_p < maximumAge)
  
  #create multiply imputed survey design object for Post analyses that require income info
  psa.impPost <- 
    svydesign( 
      id = ~psu_p , 
      strata = ~strat_p ,
      nest = TRUE ,
      weights = ~wtfa_sa,  
      data = imputationList( list( x1post , x2post , x3post , x4post , x5post ) )
    )
  #restrict to specified age range
  psa.impPost <- subset(psa.impPost, age_p >= minimumAge & age_p < maximumAge)
  
  ############################ 
  ### End 'Post' data prep ###  
  ############################ 
  
  
  
  
  
  
  ################################################################################################################  
  
  ########################################################  
  ### BEGIN DATA PREP FOR 'PRE' YEAR ##################### 
  ######################################################## 
  
  # Now, we're going to repeat all of the above for our specified 'Pre' year
  # instead. Because it's essentially the same process, I'm only going to 
  # comment on the stuff that changes (mostly in the recoding steps)
  
  # Set the year to the comparison year that we specified above
  year <- comparedToYear 
  
  path.to.incmimp.file <- paste( getwd() , year , "incmimp.rda" , sep = "/" )
  options( survey.lonely.psu = "adjust" )
  path.to.personsx.file <- paste( getwd() , year , "personsx.rda" , sep = "/" )
  path.to.samadult.file <- paste( getwd() , year , "samadult.rda" , sep = "/" )
  path.to.incmimp.file <- paste( getwd() , year , "incmimp.rda" , sep = "/" )
  print( path.to.personsx.file )
  print( path.to.samadult.file )
  print( path.to.incmimp.file )
  load( path.to.personsx.file )  
  load( path.to.samadult.file )	
  ls()
  df.name <- paste( "NHIS" , substr( year , 3 , 4 ) , "personsx" , "df" , sep = "." )
  samadult.name <- paste( "NHIS" , substr( year , 3 , 4 ) , "samadult" , "df" , sep = "." )
  x <- get( df.name )
  sa <- get( samadult.name ) 
  rm( list = c( df.name , samadult.name ) )
  gc()
  merge.vars <- c( "hhx" , "fmx" , "fpx" )
   columns.in.both.dfs <- intersect( names( sa ) , names( x ) ) 
   redundant.columns <- columns.in.both.dfs[ !( columns.in.both.dfs %in% merge.vars ) ] 
  sa <- sa[ , !( names( sa ) %in% redundant.columns ) ]
  stopifnot( merge.vars == intersect( names( sa ) , names( x ) ) )
  x.saPre <- merge( x , sa )
  stopifnot( nrow( x.saPre ) == nrow( sa ) )
  load( path.to.incmimp.file )  	
  for ( i in 1:5 ){
    current.i <- get( paste0("ii" , i ) )
    merge.vars <- intersect( names( x.saPre ) , names( current.i ) )
    for ( j in merge.vars ) x.saPre[ , j ] <- as.numeric( x.saPre[ , j ] )
    sapply( x.saPost , class )
    y <- 
      merge( 
        x.saPre , 
        current.i 
      )
    stopifnot( nrow( x.saPre ) == nrow( y ) )
    ##############################
    # START OF VARIABLE RECODING #

    y <- 
      transform( 
        y ,
        #### Most variables have codes for things like "Refused to answer"
        #### This block of recodes is intended to set those kinds of codes
        #### to NA
        notcov <- ifelse(notcov < 7, notcov, NA),        
        ahcsyr1 <- ifelse(ahcsyr1 < 7, ahcsyr1, NA) ,
        educ1 = ifelse(educ1>22, NA, as.numeric(educ1))
      )
    
    y <- 
      transform( 
        y , 
        #Recode variables of interest just like we did for the 'Post' year above
        coverage = factor( as.numeric(notcov==2), labels = c("Not covered now", "Covered now"), levels=c(0,1),exclude=NA),
        ahcsyr1 = factor(as.numeric(ahcsyr1==1), labels = c("No mh prof", "Saw mh prof"), levels=c(0,1), exclude = NA),
        YEAR = factor(1, labels = year),
        WHITE = factor(as.numeric(racreci3 == 1), labels = c("NonWhite", "White"), exclude = NA),
        sex = factor(sex, labels=c("Male", "Female")),
        PublicInsurance = ifelse(medicaid <= 2, 1, ifelse( medicare <=2 ,1, 0))
      )
    
  ###############################################################
  # IMPORTANT NOTE: Some variable names for the National Health
  # Interview Survey have changed over time. Others have changed 
  # slightly in terms of format. So, our calculations have to 
  # change depending upon the year of the survey.
  ##################################
  
  
  if (year > 2013 ){  # Recode post-2013 variables 
                      # within these brackets 
    y <-
      transform(
        y ,
        #### set all the "I don't know" / "refused to answer" codes to NA
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
          # calculate K6 score
          (5-asisad + 
             5-asinerv + 
             5-asihopls + 
             5-asirstls + 
             5-asieffrt + 
             5-asiwthls) )
    
    y <- 
      transform( 
        y , 
        
        # code variable for "Serious Psychological Distress (SMI)" based on K6 score
        SMI = factor(as.numeric(K6 >= SMIthreshold), labels=c("No Serious Distress", "Serious Psych Distress (SMI)"), levels=c(0,1),exclude=NA))
    
    
  }else{  # Recode pre-2013 variables within 
          # these brackets
    y <- 
      transform(
        y ,
        #### set all the "I don't know" / "refused to answer" codes to NA
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
        # calculate K6 score
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
        # code variable for "Serious Psychological Distress (SMI)" based on K6 score
        SMI = factor(as.numeric(K6 >= SMIthreshold), labels=c("No Serious Distress", "Serious Psych Distress (SMI)"), levels=c(0,1), exclude=NA)
      )
    
  }
  if (year >= 2010){  # Recode post-2010 variables within these brackets
    y <- 
      transform( 
        y , 
    povrati3 = ifelse(povrati3 == 999999, NA, povrati3/1000),
    Unemployment = ifelse(wrklyr4 < 7, wrklyr4, NA)
    
      )
     
  }else{ # Recode pre-2010 variables within these brackets
    y <- 
      transform( 
        y , 
        povrati3 = ifelse(povrati2 == 9999, NA, povrati2/100),
        Unemployment = ifelse(wrklyr3 < 7, wrklyr3, NA)
        
      )
    
  }
  
  ### now we can recode some of the year-dependent variables that we generated above
  y <- 
    transform( 
      y , 
      # create a variable for people above 138% of the poverty line
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
  
    #create our simple dataset that contains recoded variables
    #for use in analyses that don't require income data
    pre.i <- x1pre  
  
  #create survey design object for unimputed 'Pre' analyses
  psa.Pre <- 
    svydesign( 
      id = ~psu_p , 
      strata = ~strat_p ,
      nest = TRUE ,
      weights = ~wtfa_sa,  
      data = pre.i
    )
  #subset to specified age ranges
  psa.Pre <- subset(psa.Pre, age_p >= minimumAge & age_p < maximumAge)
  
  #create multiply imputed survey design object for income-related 'Pre' analyses
  psa.impPre <- 
    svydesign( 
      id = ~psu_p , 
      strata = ~strat_p ,
      nest = TRUE ,
      weights = ~wtfa_sa, 
      data = imputationList( list( x1pre , x2pre , x3pre , x4pre , x5pre ) )
    )
  #subset to specified age ranges
  psa.impPre <- subset(psa.impPre, age_p >= minimumAge & age_p < maximumAge)
  
  
  #create Pre-Post survey design object for non-income 'Pre' analyses
  psa.noImp <- 
    svydesign( 
      id = ~psu_p , 
      strata = ~strat_p , 
      nest = TRUE , 
      weights = ~wtfa_sa, 
      data = rbind(pre.i,post.i) #combine unimputed pre and post datasets together
      ) 
  #subset to specified age ranges
  psa.noImp <- subset(psa.noImp, age_p >= minimumAge & age_p < maximumAge)
  
  
  for ( i in 1:5 ){
    # For each of the five imputed dataset, collapse Pre-Post for multi-year analyses
    assign(paste0("x",i), rbind(
                                get( paste0( "x" , i, "pre" ) ),
                                get( paste0( "x" , i, "post" ) )
                                )
                          )
    assign(paste0( "x" , i, "pre" ), NULL)
    assign(paste0( "x" , i, "post" ), NULL)
  }
  
  #Erase previous data that won't be needed 
  rm(sa)
  rm(pre.i)
  rm(post.i)
  rm(x)
  rm(x.saPre)
  rm(x.saPost)
  rm(current.i)
  
  # clear up RAM
  gc()
  
  
  ## Create multiply imputed survey design object, Pre-Post 
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
  #subset to specified age ranges
  psa.imp <- subset(psa.imp, age_p >= minimumAge & age_p < maximumAge)
 
  #erase the 5 component datasets which won't be needed anymore
  for (i in 1:5) { assign(paste0( "x" , i ), NULL)}  
  gc() #clear up ram
  
  
  
  
  ############################
  ##### END OF DATA PREP #####
  ############################
  
  
  ## Create a "sink" txt file that we can open at the end to show us all our results in a more readable way
  sinkFile <- paste(getwd(),"/TempOutput", year,".txt", sep="")
  sink(sinkFile, type="output")
  
  
  ## We use the cat() function to annotate the sink output
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

  2014 compared with", year,"
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
    
  
  cat("
################################################################################
     
    Percent (%) of the USA population with and without Serious Psychological Distress (SMI)
  
    The \"mean\" columns hold percentages. E.g., 0.30 = 30% of the corresponding population.

     ###### in",year,"
  
  ")
  
  svymean(~as.character(SMI),design = psa.Pre, na.rm=T)
  
  cat("
  
     ###### in 2014
  
  ")
  
  svymean(~as.character(SMI),design = psa.Post, na.rm=T)
  
  
    
  
cat("################################################################################
 

    Percent (%) of the USA population with health insurance coverage at the time of survey 
    (Split up between those With vs. Without SMI)

     ###### in",year,"

      ")
  
  (svyby(~factor(coverage),~as.character(SMI),svymean, vartype="ci",design = psa.Pre, na.rm=T))
  
  cat("

     ###### in 2014

      ")
  (svyby(~factor(coverage),~as.character(SMI),svymean, vartype="ci",design = psa.Post, na.rm=T) )
  
  cat("
################################################################################

   % who saw a mental health professional in past 12 months 
       
    ###### in ",year,"
      
      ")
  
 ( svyby(~factor(ahcsyr1),~as.character(SMI),design = psa.Pre, vartype="ci", svymean,na.rm=T) )
  
  cat("
      
    ###### in 2014

      ")
  
 svyby(~factor(ahcsyr1),~as.character(SMI),svymean,design = psa.Post, vartype="ci", na.rm=T) 
  
  cat(
"
################################################################################


    % of people at 138% of poverty line. (Note: people below 138% qualified for expanded 
    medicaid as of January 1, 2014)
      
     ###### in ",year,"
      
      ")
  
  summary(MIcombine(with( psa.impPre , 
                          svyby(~factor(above.138),~as.character(SMI),svymean,na.rm=T))) )  
  
  cat("
      
     ###### in 2014

      ")
  summary(MIcombine(with( psa.impPost , 
                          svyby(~factor(above.138),~as.character(SMI),svymean,na.rm=T))) )
  
  ## Plot income on K6 score
  ## Note: plots like these won't show up on your .txt sink file,
  ## but they'll pop up while the script is running to give you 
  ## something to look at while you wait. Also, this isn't multiply 
  ## imputed so treat it as a useful approximation rather than a 
  ## complete picture
  svyplot(povrati3~K6, 
          design=psa.Post, 
          style="transparent",
          ylab="Income (ratio of Federal Poverty Line)",
          xlab="K6 Psychological Distress")
  abline(1.38,0, col="darkred")    # mark poverty line
  text(22,2, "138% FPL", col=2)   #label poverty line
  title("Income and Psychological Distress (2014)") # Add a title
  
  
  cat("
################################################################################


    Average ages of people with and without SMI
      
      ###### in ",year,"
      
      ")
  svyby(~age_p,~as.character(SMI),svymean,design = psa.Pre,vartype="ci",na.rm=T)
  cat("
      

      ###### in 2014

      ")
  svyby(~age_p,~as.character(SMI),svymean,design = psa.Post, vartype="ci",na.rm=T)
  
  
  cat("
################################################################################

      
    Average level of unemployment by SMI status
           0 = Had job last week
           1 = No job last week, had job past 12 months
           2 = No job last week, no job past 12 months
           3 = Never worked
      
      ###### in ",year,"
      
      ")
  
  svyby(~Unemployment,~as.character(SMI),svymean,design = psa.Pre,vartype="ci",na.rm=T)
  cat("
      
      ###### in 2014
      
      ")
  svyby(~Unemployment,~as.character(SMI),svymean,design = psa.Post, vartype="ci",na.rm=T)
  
  
  
  cat("
      
################################################################################
      
      
    Among people with any insurance, what % were covered via medicaid or medicare?
      
      ###### in ",year,"
      
      ")
  
  ## create restricted unimputed datasets that only hold respondants covered by health insurance
  psa.CovPre <-  subset(psa.Pre, coverage=="Covered now")
  psa.CovPost <-  subset(psa.Post, coverage=="Covered now")
  
  svyby(~factor(PublicInsurance),~as.character(SMI),svymean,vartype="ci",design=psa.CovPre,na.rm=T) 
  
  cat("
      
      ###### in 2014
      
      ")
  svyby(~factor(PublicInsurance),~as.character(SMI),svymean,design=psa.CovPost,vartype="ci",na.rm=T) 
  
  
  #remove any survey design objects that we don't need for the remaining analyses
  rm(psa.CovPre)
  rm(psa.CovPost)  
  rm(psa.Pre)
  rm(psa.Post)
  rm(psa.impPre)
  

  
  
cat("
  

  #############################
  # What predicts SMI? (2014) #
  #############################
  
  ")
  
  summary(MIcombine( with( psa.impPost , 
                           svyglm(factor(SMI) ~
                                    povrati3 +
                                    as.character(WHITE) +
                                    as.character(sex) +
                                    educ1 +
                                    age_p +
                                    Unemployment,
                                  family=quasibinomial())
  )
  ))
  
  cat("
  

  #######################################
  # Who is seeking MH treatment? (2014) #
  #######################################
  
  ")
  
  
  summary(MIcombine( with( psa.impPost , 
                           svyglm(factor(ahcsyr1) ~
                                    povrati3 +
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
  

  ####################################
  # Who has health insurance? (2014) #
  ####################################
  
  ")
  summary(MIcombine( with( psa.impPost , 
                           svyglm(factor(coverage) ~
                                    povrati3 +
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
  # Among those insured, who has public insurance (medicaid or medicare)? (2014) #
  ################################################################################
      
      ")
  
  #create imputed survey design objects restricted to people with health insurance
  psa.impCovPost <-  subset(psa.impPost, !is.na(coverage))
  psa.impCovPost <-  subset(psa.impCovPost, coverage=="Covered now")
  
  
  summary(MIcombine( with( psa.impCovPost , 
                           svyglm(factor(PublicInsurance) ~
                                    povrati3 +
                                    educ1 +
                                    as.character(WHITE) +
                                    as.character(sex) +
                                    age_p +
                                    Unemployment +
                                    as.character(SMI) ,
                                  family=quasibinomial())
  )
  ))
  
  rm(psa.impCovPost)
  cat("
################################################################################


 #####################
 #### Differences in differences (DIDs) for health insurance coverage (SMI v. no-SMI).
 #### The interaction effect (SMI:YEAR) tells us how much the disparity between SMI
 #### and non-SMI improved or worsened during the intervening time. In this case,
 #### a positive interaction between SMI and YEAR would indicate that the health 
 #### insurance disparity is decreasing. 
 ####
 #### Note: only the interaction effect is interpretable in these DIDs because the
 #### other variables aren't mean-centered.
 #######


      ")
  
                  summary(svyglm(as.numeric(coverage) ~
                                    as.character(SMI) + 
                                    as.character(YEAR) + 
                                    as.character(SMI):as.character(YEAR),
                                  design=psa.noImp)
                      )
                
          
  
  
  
  cat("

      ###############  
      #### Now control for level of education, poverty status, race, sex,
      #### age, and unemployment
      ######   
      
      
      ")
  
  
  summary(MIcombine( with( psa.imp , 
                           svyglm(as.numeric(coverage) ~
                                    educ1 +
                                    povrati3 +
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
      #### Simple DIDs for having public insurance, among those insured
      ###### 

")

  ## restrict imputed multi-year dataset to people with health insurance
  psa.impCov <-  subset(psa.imp, !is.na(coverage))
  psa.impCov <-  subset(psa.impCov, coverage=="Covered now")
  
  summary(MIcombine( with( psa.impCov , 
                           svyglm(as.numeric(PublicInsurance) ~
                           as.character(SMI) + 
                             as.character(YEAR) + 
                             as.character(SMI):as.character(YEAR))
                    )
             )
      )
  
  
cat("

      ###############  
      #### Now control for poverty status, race, sex, and age
      ######   
      
      
      ")

  summary(MIcombine( with( psa.impCov , 
                           svyglm(as.numeric(PublicInsurance) ~
                                    educ1 +
                                    povrati3 +
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
  
  rm(psa.impCov)
  
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
               design=psa.noImp) 
        )
  
  
  cat("

################################################################################
      
      ###############  
      #### DIDs for seeing mental health clinician in past 12 months (SMI v. no-SMI) 
      #### controlling for lots of variables
      ########       
      
      ")

  summary( MIcombine( with( psa.imp , 
                                               svyglm(as.numeric(ahcsyr1) ~
                                                        povrati3 +
                                                        as.character(WHITE) +
                                                        as.character(sex) +
                                                        age_p +
                                                        educ1 +
                                                        Unemployment +
                                                        as.character(SMI) + 
                                                        as.character(YEAR)  +
                                                        as.character(SMI):as.character(YEAR)
                                                      )
              ) 
        )
  )
  
  
  cat("

################################################################################
      
      
      ###############  
      #### Simple DIDs for income (SMI v. no-SMI) 
      ########       
      
      ")
  
  summary(
    svyglm(as.numeric(povrati3) ~
             as.character(SMI) + 
             as.character(YEAR) +
             as.character(SMI):as.character(YEAR),
           design=psa.noImp) 
  )
  
  
  cat("
            
      ###############  
      #### DIDs for income (SMI v. no-SMI) 
      #### controlling for lots of variables.
      #### Income disparity is getting worse.
      ########       
      
      ")
  
  summary( MIcombine( with( psa.imp , 
                            svyglm(as.numeric(povrati3) ~
                                     as.character(WHITE) +
                                     as.character(sex) +
                                     age_p +
                                     educ1 +
                                     Unemployment +
                                     as.character(SMI) + 
                                     as.character(YEAR)  +
                                     as.character(SMI):as.character(YEAR)
                            )
                    ) 
            )
  )
  
  
  
  cat("
      
################################################################################
      
      ###############  
      #### DID for racial disparity in health coverage
      #### Things got a little better
      ########  

      
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
                                )
                            )
                        )
  
 

  
  # remove the sink
  sink()
  # automatically open the sink file to see the output of your analyses
  file.show(sinkFile)
  #clear RAM
  gc()
  # Erase all the objects we created
  rm(list = ls())
  
  

  
  