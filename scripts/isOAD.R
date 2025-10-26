isOAD <- function(date, region){

  # This function takes a given date and region, and tells the user if it is an OAD
  # date must be a date object can be multiple
  date <- as.Date(date)
  
  if (length(region) == 1){
    region <- rep(region, length(date))
  }
  
  response <- vector(length=length(date))
  
  dir <- "~/Library/CloudStorage/Box-Box/Research/Ozone/data/OADs_Scrape/"
  
  arr <- read.csv(paste0(dir,"Austin_Action_Days.csv"),
                  row.names = 1)
  bpa <- read.csv(paste0(dir,"Beaumont-Port Arthur_Action_Days.csv"),
                  row.names = 1)
  cc <- read.csv(paste0(dir,"Corpus Christi_Action_Days.csv"),
                 row.names = 1)
  dfw <- read.csv(paste0(dir,"Dallas-Fort Worth_Action_Days.csv"),
                  row.names = 1)
  elp <- read.csv(paste0(dir,"El Paso_Action_Days.csv"),
                  row.names = 1)
  hgb <- read.csv(paste0(dir,"Houston_Action_Days.csv"),
                  row.names = 1)
  netx <- read.csv(paste0(dir,"Tyler-Longview_Action_Days.csv"),
                   row.names = 1)
  san <- read.csv(paste0(dir,"San Antonio_Action_Days.csv"),
                  row.names = 1)
  vic <- read.csv(paste0(dir,"Victoria_Action_Days.csv"),
                  row.names = 1)
  
  all_areas <- list('arr' = arr, 'bpa'= bpa, 'cc' = cc, 'dfw' = dfw, 'elp'= elp, 
                    'hgb' = hgb, 'netx' = netx, 'san' = san, 'vic' = vic)
  
  for (i in 1:length(date)){
    working.date <- date[i]
    working.region <- region[i]
    oads.in.region <- all_areas[working.region][[1]]$x
    
    if (working.date %in% oads.in.region){
      response[i] <- T
    }
    
    return(response)
  }
}
