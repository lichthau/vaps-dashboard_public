#########################################################################
### Title:  Access PCDB on HU-server, read data from tables and view,
###          and create lists with variable codes and descriptive labels
###
### Author: Hauke Licht
### Data:   June 21, 2016
### produced under R version 3.2.3

if (!require(RPostgreSQL)) install.packages("RPostgreSQL")
if (!require(dplyr)) install.packages("dplyr")

if ( !grepl(".*/vaps-dashboard_public",getwd()) ) setwd("vaps-dashboard_public") ## set path to vaps-dashboard_public here ##

# (1) Connect to Database

  # define parameters as object
  dbname <- "polconfdb"
  dbuser <- "polconfdb_4"
  dbhost <- "moodledb.cms.hu-berlin.de"
  dbport <- "5432"
  dbpass <- "Zs%7f_+9;hcRRw"
  
  # connect to PostgreSQL Server
  drv <- dbDriver("PostgreSQL") 
  con <- dbConnect(drv, host=dbhost, port=dbport, dbname=dbname, user=dbuser, password=dbpass) 

  # remove parameter objects
  rm(list=(ls()[grepl("[db.{4}]",ls())]))

# (2) Read data tables
  # get dataframe with columns in TABLES in beta_version schema 
  ColumnsInTables <- dbGetQuery(con,"SELECT table_catalog, table_schema, table_name, column_name, ordinal_position, data_type 
                        FROM information_schema.columns
                        WHERE table_schema = 'beta_version'
                        AND table_name NOT LIKE 'view_%';")
  head(ColumnsInTables)
  # list tables ...
  TABLES <- sort(unique(ColumnsInTables$table_name))
  TABLES <- TABLES[!grepl("update_|matview|pview",TABLES)]  
    
    # ... and read Tables in beta_version schema into dataframes 
    for (i in 1:length(TABLES)) {
      assign(TABLES[i], dbReadTable(con, c("beta_version",TABLES[i])))
    }

  # get dataframe with columns in VIEWS in beta_version schema
  ColumnsInViews <- dbGetQuery(con,"SELECT table_catalog, table_schema, table_name, column_name, ordinal_position, data_type 
                        FROM information_schema.columns
                                WHERE table_schema = 'beta_version'
                                AND table_name LIKE 'view_%';")
  head(ColumnsInViews)
  # list views ...
  VIEWS <-  sort(unique(ColumnsInViews$table_name))
    
    # ... and read Views in beta_version schema into dataframes 
    for (i in 1:length(VIEWS)) {
      assign(gsub("../shiny-experiments/view_configuration_vto","vto",VIEWS)[i], dbReadTable(con, c("beta_version",VIEWS[i])))
    }  
  
  # before proceeding, disconnect from database  
  dbDisconnect(con)
  rm(con)
  
# (3) Create list with all countries in PCDB for selector-input choice 
  require(countrycode)
  # get country ISO-character codes
  country_selector_list <- rbind( "All" , country[,c("ctr_ccode2","ctr_ccode")] )  
  # use countrycode package to asign country names to all but first row names
  rownames(country_selector_list)[-1] <- countrycode(country_selector_list[-1,1],"iso2c","country.name") 
  # name first row manually
  rownames(country_selector_list)[1] <- "All countries"
  # define as list with country names as first-dimension names, and ISO-3-character codes as elements
  country_selector_list <- as.list( apply(country_selector_list[], 1,  function( e ) e = e[2]) )

# (4) Create list with column-label lists for later labeling of input-selector choices
  # load dataframe with all codes and descriptive labels  
  AbbrLabs <- read.csv("../shiny-experiments/AbbrLabs.csv",sep=",")[,c("abbr","label")]
   
  # create empty list
  colLabsList <- list()
  # and fill with lists that have code as character string as elements, and 'descriptive label (code)' as first-dimension names
  for (i in seq_along(TABLES)) {
    A <- unique( AbbrLabs[  AbbrLabs$abbr %in% names(get(TABLES[i])), ] )
    B <- apply(A, 1,  function( c )  c[2]= c[1]) 
    names(B) <- paste0(A[,2]," (",A[,1],")")
    B <- as.list(B)
    colLabsList[[i]] <-  B
    rm(A,B)
  }; rm(i)
  # name first dimension of column-labels list 
  names(colLabsList) <- paste0(tolower(TABLES))
    # this allows referncing as 'colLabsList$foo' (i.e., list dollar-sign table name)

  # Example: now one can get a named list with column descriptions ...
    # for instance, for the first four columns of the country table,
    colLabsList$country[colLabsList$country %in% colnames(country)[1:4] ]

    # or for the columns of a merge of the country table with the cabinet table
    ccv <- right_join(country[,1:4],cabinet,by="ctr_id",type="right")
    ccv_labs <- append(colLabsList$country[colLabsList$country %in% colnames(country)[1:4] ], colLabsList$cabinet )
    # NOTE that by default duplicate list elements (e.g, 'Country identifier (ctr_id)' appears in both lists) are only represented once
    
    all(TRUE== ( colnames(ccv) %in% ccv_labs ) )  # check: works
    # clean up
    rm(list=(ls()[grepl("ccv.*",ls())]))
    
    
  