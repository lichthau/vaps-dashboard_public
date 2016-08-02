#########################################################################
### Title:  Produce Excel workbook that provides overview over columns in 
###          tables and views used in RShiny app in one-sheet-per-table/view format
###
### Author: Hauke Licht
### Created on:   July  22, 2016
### Last modification:  August 1, 2016
### produced under R version 3.2.3

### Comment: (HL) the Excel workbook may be used to work on both wepage and Codebook 
###           content (e.g. descritpion texts)

if (!require(xlsx)) install.packages("xlsx")

## Only for first run in R-session, source script that gets data from server 
# source("~/Documents/Humboldt/Electoral_Vulnerability/Projects/vaps-dashboard_public/databaseAccessHauke.R")

# set path to local 'vaps-dashboard_public' directory 
path <- "~/Documents/Humboldt/Electoral_Vulnerability/Projects/vaps-dashboard_public"

if ( sub(".*/","",getwd()) != "vaps-dashboard_public" ) setwd(path) ## set path to vaps-dashboard_public here ##
rm(path)

if ( sub(".*/","",getwd()) == "vaps-dashboard_public" ) {
  
  setCacheRootPath(path=getwd()) 
  # load cached data list
  allPCDBObjects <- loadCache(key=list("PCDB","data"))   # key is PCDB + data
  # retrieve data from list
  for ( o in seq_along(allPCDBObjects) ) { seq_along(allPCDBObjects)
    assign(names(allPCDBObjects)[ o ], allPCDBObjects[[ o ]])
  }; rm(o)
  # ... and there we go        
} else warning("Cannot load cached data. Please setwd() to vaps-dashboard_public directory!")

# before proceeding, remove not-used objects from global environment
rm(list = setdiff(names(allPCDBObjects),c("AbbrLabs","ColumnsInTables","ColumnsInViews")) ) 

# (1) create one dataframe with column information on both tables and views
  Columns <- rbind(ColumnsInTables,ColumnsInViews)
  names(Columns) <- sub("table_","", names(Columns))
  tail(Columns)
  
# (2) merge descriptive column labels on table and view information by column names (i.e, variable abbrviations) 
  Codebook <- merge(Columns[,c("name", "column_name", "ordinal_position", "data_type")],AbbrLabs,by.x="column_name",by.y="abbr",all.x=T)
  # oder dataframe by table and column position in table
  Codebook <- Codebook[order(Codebook$name, Codebook$ordinal_position), ]
  # remove duplicates from dataframe
  Codebook <- unique(Codebook)
  Codebook <- Codebook[, c("name", "column_name", "label", "data_type", "ordinal_position")]
  
# (3) Produce list with tables/views as elements (containing table/view specific column information)
  Codebook <- split(Codebook, Codebook$name)
    # NOTE: list elements are referencable by table/view name, e.g. Codebook$cabinet

# (4) Write Excel workbook (.xlsx) file 'PCDB_data_overview' with one sheet per table/view
  
  # (a) create empty Excel file:
  wb <- createWorkbook()
  saveWorkbook(wb, "Codebook/PCDB_data_overview.xlsx")
  
  # (b) Then append each workbook:
  lapply(names(Codebook), function(x) write.xlsx(Codebook[[x]], "Codebook/PCDB_data_overview.xlsx", sheetName=x, append=TRUE))

