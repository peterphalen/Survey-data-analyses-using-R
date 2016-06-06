

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
  # # Code by A. Damico [http://www.asdfree.com/]
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
  




  ########################################
  ###        Begin user settings.       ##
  ###  (These variables are set by you. ## 
  ###  The rest of the code should      ##
  ###   depend on these settings.)      ##
  ########################################


  # set working directory
  setwd( "/Users/PeterPhalen/Google Drive/Data analysis" )


  # choose what year of data to analyze
  # note: this can be changed to any year that has already been downloaded locally
  # fair warning: I wrote this with reference to NHIS years 2009-2014. If you
  # want to run this with earlier years of data you may need to adjust the preprocessing 
  # function below to account for relevant changes in variable names and formats  

  latestYear <- 2014        #2014 is currently the latest available dataset 
  comparedToYear <- 2013
  

  ## Minimum and maximum ages to include in dataset. 
  ## Your choice here subsets down your models/stats.
  ## If you want to use the complete data, just set 
  ## minimumAge to 0 and maximumAge to 999
  ##
  ## Note: minimumAge <= x < maximumAge
  
  minimumAge <- 26
  maximumAge <- 65
  

  # use this as a 'greater than or equal to' cutoff for the K6 
  # to determine who counts as SMI. Most common cut-off is 13 for SMI, 
  # see Kessler et al 2010 
  # http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3659799/pdf/nihms447801.pdf

  SMIthreshold <- 13
  

  # Set this to the quarters you'd like to keep. Leave as c(1:4) or c(1,2,3,4) if you
  # want the complete year's worth of data. Otherwise, choose subsets like wh4)
  # This may be a good idea if you're analyzing the effect of the January 1, 2014
  # medicaid expansion, as these may only be visible in later quarters.

  quarters.to.keep <- c(2:4)


  # Set updatePreprocessingFunction to TRUE only if you have just manually changed 
  # the preprocessNHISdata function and need to force it to update (e.g., if you've 
  # just gone into the function code below and changed how variables are calculated
  # or formatted). 
  # Note: If this is a new R session, don't worry about this as the script
  # will update the preprocessing function automatically whenever it's not presently
  # declared in your Global Environment.

  updatePreprocessingFunction <- FALSE



  ## Select the analysis variables that you're interested in
  ## You need to specify these or your analyses will take 
  ## forever and/or your computer may crash

  variables.to.keep <-
    c( 
      ## survey variables: do not alter these
      "psu_p",   # (cluster)
      "strat_p", # (stratum)
      "wtfa_sa", # (weight) 
      "intv_qrt", # (interview quarter [1-4])
      
      # merge variables: do not alter these
      "hhx" ,  # (household unique identifier)
      "fmx" ,  # (family unique identifier)
      "fpx" ,  # (person unique identifier)

      
      
      # analysis variables: you can change these
      "age_p",       # age, continuous (native to dataset)
      "SOUTHMW",     # region 1 = NE 2 = MW 3 = South 4 = W  (note: region "South" and "Midwest"  
                     # account for all but one state with medicaid non-expansion, so region recoded to
                     # 1 for SOUTH or MIDWEST and 0 for the other three regions)
      "SOUTH",       # region with by far highest number of people in non-expanded medicaid states who would have been eligible
      "K6",          # K6 Serious Psychological Distress score, continuous, 0-24
      "SMI" ,        # K6 >= SMIthreshold, 0 = No Serious Mental Illness (SMI), 1 = SMI. see http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3659799/pdf/nihms447801.pdf
      "SMIonly" ,        # 0 = K6 < 5, 1 = K6 >= 13 see http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3659799/pdf/nihms447801.pdf
      "distressLevel", # 0 = K6 >= 5 but less than 13, indicating moderate clinical distress. see http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3370145/
      "anyDistress", # K6 greater than or equal to 5
      "modDistress", # K6 >= 5 < 13
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
      "Unemployment",      # level of unemployment, ordinal, 0 = Had job last week, 
                                                          # 1 = No job last week, had job past 12 months  
                                                          # 2 = No job last week, no job past 12 months, 
                                                          # 3 = Never worked
      "couldNotAffordMH"      #needed mental health care but couldn't afford it  1=Yes 2=No
    # "decentHCsatisfaction"     # 1 = Better 2 = Worse 3 = About the same, 
      
    )
  
#    if ( (latestYear >= 2013) & (comparedToYear >= 2013) ) {
#      c(variables.to.keep, "aworpay")
#    }

    #########################################
    ###          End user settings.        ##
    ###     (The rest of the script can    ##
    ###           run on its own)          ##
    #########################################










# If you've already got the preprocessing function in your system and
# don't need to force it to update within a single session, this if-statement
# saves time by skipping the function declaration 
if (!("preprocessNHISdata" %in% ls()) | (updatePreprocessingFunction == TRUE)){
  
#########################################################  
#### Create function to perform data preprocessing. ##### 
#########################################################  
#### This function will remain in your environment, #####
#### so unless you need to change the recoding      #####
#### scripts you can get away with running it just  #####
#### once per session                               #####
#########################################################

preprocessNHISdata <- function(year, mostRecentData = TRUE){

    #if the user asks to process pre-2009 data,
    #don't stop them but throw a warning
    if (year < 2009){ 
      warning("This script hasn't been tested on years prior to 2009")
    }
    

    if (mostRecentData == TRUE) {
      
      # For Pre-Post analyses, label the more recent dataset
      # with 'post' 
      chronology <- "post"
      
      #dummy code for year
      post <- 1

    }else{
      if (mostRecentData == FALSE){ 
        
        # and the earlier dataset with 'pre' 
        chronology <- "pre"
        
        #dummy code for year
        post <- 0

      }else{
        
        #If the user doesn't set mostRecentData to either T or F, throw an error
        stop("The mostRecentData argument must be set to either TRUE or FALSE. 
             For Pre-Post analyses, set mostRecentData to FALSE if this is your Pre dataset, 
             or TRUE if this is your Post dataset")
      }
    }
    
    # Throw an error and stop script if the user asked for a quarter greater than 4 or less than 1
    if (TRUE %in% c(quarters.to.keep > 4, quarters.to.keep < 1)){
      stop("You can only choose quarters between 1 and 4")
    }
    
    
    ########################################################  
    ##########          Load data           ################ 
    ########################################################
    
  # set R to produce conservative standard errors instead of crashing
  # http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
  options( survey.lonely.psu = "adjust" )
  options(stringsAsFactors=FALSE)
  
  # construct the filepath (within the current working directory) to the three rda files
  path.to.personsx.file <- paste( getwd() , year , "personsx.rda" , sep = "/" )
  path.to.samadult.file <- paste( getwd() , year , "samadult.rda" , sep = "/" )
  path.to.incmimp.file <- paste( getwd() , year , "incmimp.rda" , sep = "/" )
  
  # Tell us which filepaths we're pulling from
  cat("Pulling from the following files: 
",path.to.personsx.file,"
",path.to.samadult.file,"
",path.to.incmimp.file)
  
  # now the "NHIS.[year].personsx.df" data frame can be loaded directly
  # from your local hard drive.  this is much faster.
  load( path.to.personsx.file )		# this loads a data frame called NHIS.[year].personsx.df
  load( path.to.samadult.file )		# this loads a data frame called NHIS.[year].samadult.df
  # the five imputed income files will be loaded later
  
  
  # construct a string containing the data frame name of the personsx data table
  # stored within the R data file (.rda)
  # note: for 2014, this data frame will be named "NHIS.14.personsx.df"
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
  ##########       BEGIN DATA PREP        ################ 
  ######################################################## 
 
  # Status update
  cat("
Merging files...")
  
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
  x.sa <- merge( x , sa )
  # note that the merge() function merges using all intersecting columns -
  
  # uncomment this line to see intersecting columns
  # intersect( names( sa ) , names( x ) )
  
  # - by default, the 'by' parameter does not need to be specified
  # for more detail about the merge function, type ?merge in the console
  
  # throw an error if the number of records in the merged file
  # does not match the number of records in the samadult file
  stopifnot( nrow( x.sa ) == nrow( sa ) )
  
  # if the user chose to restrict the number of quarters to include
  # in the analysis, cut those quarters out of the data set
  if ( length(quarters.to.keep)<4 ){
    x.sa <- x.sa[which(x.sa$intv_qrt %in% quarters.to.keep),]
  }
  
  
  #subset down to specified age ranges
  x.sa <- x.sa[which((x.sa$age_p >= minimumAge) & (x.sa$age_p < maximumAge) ),]
  
  # now the x.sa data frame contains all of the rows in the samadult file and 
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
    merge.vars <- intersect( names( x.sa ) , names( current.i ) )
    
    # loop through all variables used in the merge
    # overwrite each column with itself, only converted to a numeric field
    for ( j in merge.vars ) x.sa[ , j ] <- as.numeric( x.sa[ , j ] )
    
    
    # a handy trick to view the class of all columns within a data frame at once:
    # sapply( x.sa , class )
    
    # merge the merged file with each of the five imputed income files
    y <- 
      merge( 
        x.sa , # the samadult-personsx merged data frame
        current.i # ii1 - ii5, depending on the current iteration of this loop
      )
    
    # and confirm the new data frame (merged + the current iteration of the multiply-imputed data)
    # contains the same number of records as the original merged file
    stopifnot( nrow( x.sa ) == nrow( y ) )
    
    
    ##############################
    # START OF VARIABLE RECODING #
    # any new variables that the user would like to create should be constructed here #
     
    # Status update
    loopNumber <- paste0("imputation (",i,"/5)")
    cat("
Recoding variables...",loopNumber)
    
    ###############################################################
    # IMPORTANT NOTE: Some variable names for the National Health
    # Interview Survey have changed over time. Others have changed 
    # slightly in terms of format. So, our calculations have to 
    # change depending upon the year of the survey.
    ##################################
    
    
    # start with variables that haven't changed over the years
    # recode them here
    y <- 
      transform( 
        y ,
        #### Most variables have codes for things like "Refused to answer"
        #### This block of recodes is intended to set those kinds of codes
        #### to NA
        notcov = ifelse(notcov < 7, notcov, NA),        
        ahcsyr1 = ifelse(ahcsyr1 < 7, ahcsyr1, NA) ,
        educ1 = ifelse(educ1>22, NA, as.numeric(educ1))
      )
    
    y <- 
      transform( 
        y , 
        #Recode variables of interest as needed. Here we're converting variables with multiple levels into binary variables
        coverage = factor( as.numeric(notcov==2), labels = c("Not covered now", "Covered now"), levels=c(0,1),exclude=NA),
        ahcsyr1 = factor(as.numeric(ahcsyr1==1), labels = c("No mh prof", "Saw mh prof"), levels=c(0,1), exclude = NA),
        
        #these variables either don't have missing values, or the missing values are irrelevant to our analyses
        YEAR = factor(post),
        WHITE = factor(as.numeric(racreci3 == 1), labels = c("NonWhite", "White"), exclude = NA),
        sex = factor(sex, labels=c("Male", "Female")),
        PublicInsurance = ifelse(medicaid <= 2, 1, ifelse( medicare <=2 ,1, 0)),
        couldNotAffordMH = ifelse(ahcafyr2 == 1, 1, ifelse( ahcafyr2 ==2 ,0, NA)),
        SOUTHMW = ifelse(region == 3 | region == 2, 1, 0),
        SOUTH = ifelse(region == 3 , 1, 0)
        
        
      )    
    
    ## the rest of the variables we're interested in are year-dependent
    
    if (year >= 2013 ){  # Recode post-2013 variables 
                        # within these brackets 
      y <-
        transform(
          y ,
          #### set all the "I don't know" / "refused to answer" codes to NA
          # these are the K6 components, which were renamed in 2013 for some 
          # reason
          asisad = ifelse(asisad < 7, asisad, NA),
          asinerv = ifelse(asinerv < 7, asinerv, NA),
          asihopls = ifelse(asihopls < 7, asihopls, NA),
          asirstls = ifelse(asirstls < 7, asirstls, NA),
          asieffrt = ifelse(asieffrt < 7, asieffrt, NA),
          asiwthls = ifelse(asiwthls < 7, asiwthls, NA)
         
          #this variable wasn't added until recently
     #     decentHCsatisfaction = ifelse(ahicomp > 3, NA, ifelse(ahicomp == 1 | ahicomp == 3, 1, 0))  
          
        )
      
      y <- 
        transform( 
          y , 
          K6 = 
            # calculate K6 score. Variables must be reverse coded and 
            # scaled from 0-24 to match the standard scoring.
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
          SMI = factor(as.numeric(K6 >= SMIthreshold), labels=c("No Serious Distress", "Serious Psych Distress (SMI)"), levels=c(0,1),exclude=NA),
          distressLevel = ifelse(K6 >= SMIthreshold, 1, ifelse( (K6 >= 5 ) & (K6 < 13) ,0, NA)), # 1 = SMI, 0 = moderate clinical distress, else NA http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3370145/x
          anyDistress = ifelse(K6 >= 5, 1, 0), # 1 = SMI, 0 = moderate clinical distress, else NA http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3370145/x
          modDistress = ifelse(K6 >=5 & K6 < 13,1,0),
          SMIonly = ifelse(K6 >= 13, 1, ifelse(K6 < 5, 0, NA))
          
          )
     
     y <- 
       transform( 
         y , 
         
         # code variable for "Serious Psychological Distress (SMI)" based on K6 score
         modDistress = factor(modDistress, labels=c("No distress","Moderate distress")),
         SMIonly = factor(SMIonly, labels=c("No distress","Moderate distress"))
         
       )
      
      
    }else{  # Recode pre-2013 variables within 
            # these brackets
      y <- 
        transform(
          y ,
          #### set all the "I don't know" / "refused to answer" codes to NA
          #These are the K6 variable names for pre-2013 NHIS survey years
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
          SMI = factor(as.numeric(K6 >= SMIthreshold), labels=c("No Serious Distress", "Serious Psych Distress (SMI)"), levels=c(0,1), exclude=NA),
          distressLevel = ifelse(K6 >= SMIthreshold, 1, ifelse( (K6 >= 5 ) & (K6 < 13) ,0, NA)), # 1 = SMI, 0 = moderate clinical distress, else NA http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3370145/x
          anyDistress = ifelse(K6 >= 5, 1, 0), # 1 = SMI, 0 = moderate clinical distress, else NA http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3370145/x
          modDistress = ifelse(K6 >=5 & K6 < 13,1,0),
          SMIonly = ifelse(K6 >= 13, 1, ifelse(K6 < 5, 0, NA))
        )
      
    }
    
    
    
    if (year >= 2011 ){  # Recode post-2011 variables within these brackets
      y <- 
        transform( 
          y , 
          Unemployment = ifelse(wrklyr4 < 7, wrklyr4, NA),
          # 999999 is NA code. Also need to add appropriate decimal place
          povrati3 = ifelse(povrati3 == 999999, NA, povrati3)
        )
    
    }else{     
      
      if (year == 2010){ # 2010 variables within these brackets
        y <- 
          transform( 
            y , 
            Unemployment = ifelse(wrklyr4 < 7, wrklyr4, NA),
            # 999999 is NA code. Also need to add appropriate decimal place
            povrati3 = ifelse(povrati3 == 999999, NA, povrati3/1000)
          )
      }else{ # pre-2010 variables within these brackets
      y <- 
        transform( 
          y , 
          povrati3 = ifelse(povrati2 == 9999, NA, povrati2/100),
          Unemployment = ifelse(wrklyr3 < 7, wrklyr3, NA)
          
        )
      }
      
    }
    
    ### now we can further recode some of the year-dependent variables that we just generated
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
    
    # save the data frames as objects x1post - x5post (or x1pre -x5pre), depending 
    # on the iteration in the loop.
    # at the same time restrict the resulting dataset to the variables of interest 
    # specified above. If we don't do this the script takes much, much longer.
    assign( paste0(  'x' , i , chronology) , y[,variables.to.keep] )
        
    # delete the y and ii# data frames
    y <- NULL
    assign( paste0( "ii" , i ) , NULL )
    

    # garbage collection - free up RAM from recently-deleted data tables
    gc()
  }
  
  #clean up intermediary data
  rm(current.i)
  
  # To minimize processing time for analyses that *don't* require multiply imputed income
  # we want to have a dataset available that has all the recoded variables 
  unimputedDataName <- paste0("unimputed",chronology)
  assign(unimputedDataName, get(paste0("x1",chronology)))
  
  #create a list holding multiple imputations
  output <- list(
              get(paste0(  'x1' , chronology)),
              get(paste0(  'x2' , chronology)),
              get(paste0(  'x3' , chronology)),
              get(paste0(  'x4' , chronology)),
              get(paste0(  'x5' , chronology))
              )
  
  #tack on the unimputed list
  output <- list(output, get(unimputedDataName) )
  
  # ouptut[[1]] holds the 5 imputed datasets
  # output[[2]] hold the unimputed dataset
  return(output)
  
  }

#####################################################################################
######### end of prepocessing function ##############################################
#####################################################################################


}





# use the function we just defined to pull lists of processed data.
# these datasets have been restricted to the variables.to.keep that 
# you specified earlier

#preOutputPool <- preprocessNHISdata(2012, mostRecentData=FALSE) # 'Pre' year for pooling
preOutput <- preprocessNHISdata(comparedToYear, mostRecentData=FALSE) # 'Pre' year
postOutput <- preprocessNHISdata(latestYear, mostRecentData=TRUE)     # 'Post' year




# get the list of imputed files for Pre and Post years
for (i in 1:5){
#assign(paste0("x",i,"prePool"), preOutputPool[[1]][[i]])               
assign(paste0("x",i,"pre"), preOutput[[1]][[i]])
assign(paste0("x",i,"post"), postOutput[[1]][[i]])
}

#get a set for non-imputed analyses of the PRE year
#preUnimputedPool <- preOutputPool[[2]]
preUnimputed <- preOutput[[2]]
postUnimputed <- postOutput[[2]]

#delete the stripped outputs
rm(preOutput)
rm(postOutput)
#rm(preOutputPool)

#clear up RAM
gc()

#preUnimputed <- rbind(preUnimputedPool, preUnimputed)
#rm(preUnimputedPool)

# pool data
#for ( i in 1:5 ){
#  # For each of the five imputed dataset, collapse Pre years for better power
#  assign(paste0("x",i,"pre"), 
#    rbind(
#    get( paste0( "x" , i, "pre" ) ),
#    get( paste0( "x" , i, "prePool" ) )
#  )
#  )
#  assign(paste0("x",i,"prePool"),NULL)
#}


  
  #create survey design object for all the Post analyses that don't require info on income
  psa.Post <- 
    svydesign( 
      id = ~psu_p , 
      strata = ~strat_p ,
      nest = TRUE ,
      weights = ~wtfa_sa,  
      data = postUnimputed
    )
  
  #create multiply imputed survey design object for Post analyses that require income info
  psa.impPost <- 
    svydesign( 
      id = ~psu_p , 
      strata = ~strat_p ,
      nest = TRUE ,
      weights = ~wtfa_sa,  
      data = imputationList(list(x1post,
                            x2post,
                            x3post,
                            x4post,
                            x5post)
                            )
                      )

    
  
  #create survey design object for unimputed 'Pre' analyses
  psa.Pre <- 
    svydesign( 
      id = ~psu_p , 
      strata = ~strat_p ,
      nest = TRUE ,
      weights = ~wtfa_sa,  
      data = preUnimputed
    )
  
  #create multiply imputed survey design object for income-related 'Pre' analyses
  psa.impPre <- 
    svydesign( 
      id = ~psu_p , 
      strata = ~strat_p ,
      nest = TRUE ,
      weights = ~wtfa_sa, 
      data = imputationList(list(x1pre,
                            x2pre,
                            x3pre,
                            x4pre,
                            x5pre) )
    )
  
  
  #create Pre-Post survey design object for non-income 'Pre' analyses
  psa.noImp <- 
    svydesign( 
      id = ~psu_p , 
      strata = ~strat_p , 
      nest = TRUE , 
      weights = ~wtfa_sa, 
      data = rbind(preUnimputed,postUnimputed) #combine unimputed pre and post datasets together
      ) 
  
rm(preUnimputed,postUnimputed)
  
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
 
  #erase the 5 component datasets which won't be needed anymore
  for (i in 1:5) { assign(paste0( "x" , i ), NULL)}  
  gc() #clear up ram
  
  
  
  
  ############################
  ##### END OF DATA PREP #####
  ############################
  
  
  ## Create a "sink" txt file that we can open at the end to show us all our results in a more readable way
  sinkFile <- paste(getwd(),"/TempOutput", substr( comparedToYear , 3 , 4 ),"-", substr( latestYear , 3 , 4 ),".txt", sep="")
  sink(sinkFile, type="output")
  
  
  ## We use the cat() function to annotate the sink output
  cat("
################################################################################
######## This is the auto-generated output of R code written to display apparent
######## health disparities between people with/without psychological distress
######## in the United States. 
######## 
######## Author: Peter Phalen 
######## Dataset: National Health Interview Survey (NHIS; http://www.cdc.gov/nchs/nhis.htm)
########          Representative household survey; complex sample design.
###########################

 ",latestYear,"compared with", comparedToYear,"
  Minimum age for inclusion: ", minimumAge,"
  Maximum age for inclusion: ", maximumAge,"
        
")
if (length(quarters.to.keep) < 4 ){
cat("
  You chose to restrict your dataset to these survey quarters: ", quarters.to.keep," 
    ")
}


  
  cat("

################################################################################
     
    Percent (%) of the USA population with and without clinically significant psychological distress (K6 >= 5) 
    
    The \"mean\" columns hold percentages. E.g., 0.30 = 30% of the corresponding population.

      ###### in",latestYear,"
  
  ")
  
svymean(~modDistress, design = psa.Post, na.rm=T)

cat("

################################################################################
 

    Percent (%) of the USA population with health insurance coverage at the time of survey 
    (Split up between those With vs. Without Distress)

     ###### in",comparedToYear,"

      ")
  
  (svyby(~factor(coverage),~as.character(anyDistress),svymean, vartype="ci",design = psa.Pre, na.rm=T))
  

cat("

      ###### 
     
    Did these proportions change from",comparedToYear,"?
  
  ")

psa.noImpModDistress <- subset(psa.noImp, modDistress == 1)
summary(svyglm(as.numeric(coverage)~YEAR, design=psa.noImpModDistress))


  cat("
################################################################################

   % who saw a mental health professional in past 12 months 
       
    ###### in ",comparedToYear,"
      
      ")
  
 ( svyby(~factor(ahcsyr1),~as.character(anyDistress),design = psa.Pre, vartype="ci", svymean,na.rm=T) )
  
  cat("
      
      ###### in",latestYear,"

      ")
  
 svyby(~factor(ahcsyr1),~as.character(anyDistress),svymean,design = psa.Post, vartype="ci", na.rm=T) 
  
  cat(
"
################################################################################


    % of people at 138% of poverty line. (Note: people below 138% qualified for expanded 
    medicaid as of January 1, 2014)
      
     ###### in",comparedToYear,"
      
      ")
  
  summary(MIcombine(with( psa.impPre , 
                          svyby(~factor(above.138),~as.character(anyDistress),svymean,na.rm=T))) )  
  
  cat("
      
      ###### in",latestYear,"

      ")
  summary(MIcombine(with( psa.impPost , 
                          svyby(~factor(above.138),~as.character(anyDistress),svymean,na.rm=T))) )
  
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
  title(paste0("Income and Psychological Distress (",latestYear,")")) # Add a title

  cat("
################################################################################


    Average ages of people with and without clinically significant psychological distress
      
     ###### in",comparedToYear,"
      
      ")
  svyby(~age_p,~as.character(anyDistress),svymean,design = psa.Pre,vartype="ci",na.rm=T)
  cat("
      

      ###### in",latestYear,"

      ")
  svyby(~age_p,~as.character(anyDistress),svymean,design = psa.Post, vartype="ci",na.rm=T)
  
  
  cat("
################################################################################

      
    Average level of unemployment by SMI status
           0 = Had job last week
           1 = No job last week, had job past 12 months
           2 = No job last week, no job past 12 months
           3 = Never worked
      
     ###### in",comparedToYear,"
      
      ")
  
  svyby(~Unemployment,~as.character(anyDistress),svymean,design = psa.Pre,vartype="ci",na.rm=T)
  cat("
      
      ###### in",latestYear,"
      
      ")
  svyby(~Unemployment,~as.character(anyDistress),svymean,design = psa.Post, vartype="ci",na.rm=T)
  
cat(
  "
################################################################################
  
  
  % of people who said they didn't see MH prof because they couldn't afford it
  
  ###### in",comparedToYear,"
  
  ")

summary(MIcombine(with( psa.impPre , 
                        svyby(~factor(couldNotAffordMH),~as.character(anyDistress),svymean,na.rm=T))) )  

cat("
    
    ###### in",latestYear,"
    
    ")
summary(MIcombine(with( psa.impPost , 
                        svyby(~factor(couldNotAffordMH),~as.character(anyDistress),svymean,na.rm=T))) )

  
  cat("
      
################################################################################
      
      
    Among people with any insurance, what % were covered via medicaid or medicare?
      
     ###### in",comparedToYear,"
      
      ")
  
  ## create restricted unimputed datasets that only hold respondants covered by health insurance
  psa.CovPre <-  subset(psa.Pre, coverage=="Covered now")
  psa.CovPost <-  subset(psa.Post, coverage=="Covered now")
  
  svyby(~factor(PublicInsurance),~as.character(anyDistress),svymean,vartype="ci",design=psa.CovPre,na.rm=T) 
  
  cat("
      
      ###### in",latestYear,"
      
      ")
  svyby(~factor(PublicInsurance),~as.character(anyDistress),svymean,design=psa.CovPost,vartype="ci",na.rm=T) 
  
  
  #remove any survey design objects that we don't need for the remaining analyses
  rm(psa.CovPre)
  rm(psa.CovPost)  
  rm(psa.Pre)
  rm(psa.Post)
  rm(psa.impPre)
  

  
  
cat("
  

  ###############################
  # What predicts SMI? (",latestYear,") #
  ###############################
  
  ")
  
  summary(MIcombine( with( psa.impPost , 
                           svyglm(factor(anyDistress) ~
                                    povrati3 +
                                    as.character(WHITE) +
                                    as.character(sex) +
                                    educ1 +
                                    age_p,
                                  family=quasibinomial())
  )
  ))
  
  cat("
  

  #########################################
  # Who is seeking MH treatment? (",latestYear,") #
  #########################################
  
  ")
  
  
  summary(MIcombine( with( psa.impPost , 
                           svyglm(factor(ahcsyr1) ~
                                    povrati3 +
                                    as.character(WHITE) +
                                    as.character(sex) +
                                    educ1 +
                                    age_p +
                                    as.character(coverage) +
                                    as.character(anyDistress) ,
                                  family=quasibinomial())
  )
  ))
  
  cat("
  

  ######################################
  # Who has health insurance? (",latestYear,") #
  ######################################
  
  ")
  summary(MIcombine( with( psa.impPost , 
                           svyglm(factor(coverage) ~
                                    povrati3 +
                                    educ1 +
                                    as.character(WHITE) +
                                    as.character(sex) +
                                    age_p +
                                    as.character(anyDistress) ,
                                  family=quasibinomial())
  )
  ))

  
  cat("


  ##################################################################################
  # Among those insured, who has public insurance (medicaid or medicare)? (",latestYear,") #
  ##################################################################################
      
      ")
  
  #create imputed survey design objects restricted to people with health insurance
#  psa.impCovPost <-  subset(psa.impPost, !is.na(coverage))
#  psa.impCovPost <-  subset(psa.impCovPost, coverage=="Covered now")
  
  
  summary(MIcombine( with( psa.impPost , 
                           svyglm(factor(PublicInsurance) ~
                                    povrati3 +
                                    educ1 +
                                    as.character(WHITE) +
                                    as.character(sex) +
                                    age_p +
                                    as.character(anyDistress) ,
                                  family=quasibinomial())
  )
  ))
  
 # rm(psa.impCovPost)
  cat("
################################################################################


 #####################
 #### Differences in differences (DIDs) for health insurance coverage (distress v. no-distress).
 #### The interaction effect (anyDistress:YEAR) tells us how much the disparity between distressed
 #### and non-distressed people improved or worsened during the intervening time. In this case,
 #### a positive interaction between anyDistress and YEAR would indicate that the health 
 #### insurance disparity is decreasing. 
 ####
 #### Note: only the interaction effect is interpretable in these DIDs because the
 #### other variables aren't mean-centered.
 #######


      ")

  
                  summary(svyglm(as.numeric(coverage) ~
                                    as.character(anyDistress) + 
                                    as.character(YEAR) + 
                                    as.character(anyDistress):as.character(YEAR),
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
                                  as.numeric(fine.povcat) +
                                  educ1 +
                                  as.character(WHITE) +
                                  as.character(sex) +
                                  age_p +
                                  Unemployment +
                                  as.character(anyDistress) + 
                                  as.character(YEAR) + 
                                  as.character(anyDistress):as.character(YEAR))
)
) 
)





 
# cat("
    
    ##############
    ### TELL ME ABOUT REGION
    ###
    
#    ")

# model <- svyglm(as.numeric(coverage) ~
#                                  as.character(SOUTHMW) +
#                                  as.character(YEAR) + 
#                                  as.character(SOUTHMW):as.character(YEAR),
#                                  design=psa.noImp)


# summary(
#           svyglm(as.numeric(PublicInsurance) ~
#                   as.character(SOUTHMW) +
#                   as.character(SMI) + 
#                   as.character(YEAR) + 
#                   as.character(SMI):as.character(YEAR) +
#                   as.character(SOUTHMW):as.character(YEAR) +
#                   as.character(SOUTHMW):as.character(SMI) +
#                   as.character(SMI):as.character(YEAR):as.character(SOUTHMW),
#                  design=psa.noImp)
#               )
           



  
  
cat("


#####################
#### DIDs for being unable to afford Mental healthcare
#######


")


summary(svyglm(as.numeric(couldNotAffordMH) ~
as.character(anyDistress) + 
as.character(YEAR) + 
as.character(anyDistress):as.character(YEAR),
design=psa.noImp)
)





cat("

###############  
#### Now control for other variables


")


summary(MIcombine( with( psa.imp , 
svyglm(as.numeric(couldNotAffordMH) ~
as.numeric(fine.povcat) + 
educ1 +
as.character(WHITE) +
as.character(sex) +
age_p +
as.character(anyDistress) + 
as.character(YEAR) + 
as.character(anyDistress):as.character(YEAR))
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
  
  summary( 
                           svyglm(as.numeric(PublicInsurance) ~
                           as.character(anyDistress) + 
                             as.character(YEAR) + 
                             as.character(anyDistress):as.character(YEAR),
                           design=psa.noImp)
                    )
      
  
  
cat("

      ###############  
      #### Now control for poverty status, race, sex, and age
      ######   
      
      
      ")

  summary(MIcombine( with( psa.imp , 
                           svyglm(as.numeric(PublicInsurance) ~
                                    as.numeric(fine.povcat) + 
                                    educ1 +
                                    as.character(WHITE) +
                                    as.character(sex) +
                                    age_p +
                                    Unemployment +
                                    as.character(anyDistress) + 
                                    as.character(YEAR) + 
                                    as.character(anyDistress):as.character(YEAR))
                    )
            )
    )
  
 # rm(psa.impCov)
  
  cat("

################################################################################


 ###############  
 #### DIDs for seeing mental health clinician in past 12 months (Distress v. no-Distress) 
 ########       

      ")
  
  summary(
        svyglm(as.numeric(ahcsyr1) ~
                  as.character(anyDistress) + 
                  as.character(YEAR) +
                  as.character(anyDistress):as.character(YEAR),
               design=psa.noImp) 
        )
  
  
  cat("

################################################################################
      
      ###############  
      #### DIDs for seeing mental health clinician in past 12 months (Distress v. no-Distress) 
      #### controlling for lots of variables
      ########       
      
      ")

  summary( MIcombine( with( psa.imp , 
                                               svyglm(as.numeric(ahcsyr1) ~
                                                        as.numeric(fine.povcat) +
                                                        as.character(WHITE) +
                                                        as.character(sex) +
                                                        age_p +
                                                        educ1 +
                                                        as.character(coverage) +
                                                        Unemployment +
                                                        as.character(anyDistress) + 
                                                        as.character(YEAR)  +
                                                        as.character(anyDistress):as.character(YEAR)
                                                      )
              ) 
        )
  )
  
  
  cat("

################################################################################
      
      
      ###############  
      #### Simple DIDs for income (Distress v. no-Distress) 
      ########       
      
      ")
  
summary( MIcombine( with( psa.imp , 
    svyglm(as.numeric(fine.povcat) ~
             as.character(anyDistress) + 
             as.character(YEAR) +
             as.character(anyDistress):as.character(YEAR),
           design=psa.Imp) 
  )))
  
  
  cat("
            
      ###############  
      #### DIDs for income (Distress v. no-Distress) 
      #### controlling for lots of variables.
      ########       
      
      ")
  
  summary( MIcombine( with( psa.imp , 
                            svyglm(as.numeric(fine.povcat) ~
                                     as.character(WHITE) +
                                     as.character(sex) +
                                     age_p +
                                     educ1 +
                                     Unemployment +
                                     as.character(anyDistress) + 
                                     as.character(YEAR)  +
                                     as.character(anyDistress):as.character(YEAR)
                            )
                    ) 
            )
  )
  
  

  
  # remove the sink
  sink()
  # automatically open the sink file to see the output of your analyses
  file.show(sinkFile)
  #tell us the file location for the outputs
  paste("Analysis results saved to:",sinkFile)
  #clear RAM
  gc()
  # Erase all the objects we created earlier except the function, which can be reused
  rm(list = ls()[!(ls() %in% "preprocessNHISdata")])
  
  

  
  