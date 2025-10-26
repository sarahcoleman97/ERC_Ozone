return_oad_area <- function(list, input.type = 'text', output.type = 'number'){

  # This function takes in input (list of counties) and returns a 
  # same-dimension list of their respective OAD areas, or None if none.
  
  if (input.type == 'text'){ # default; if the input data are a list of 
    # text like "xxx County"
    
    # define matching lists for each of the nine OADs
    arr <- paste(c('Williamson', 'Travis', 'Hays', 'Caldwell', 
                   'Bastrop'), 'County')
    bpa <- paste(c('Hardin', 'Orange', 'Jefferson'), 'County')
    cc <- paste(c('Nueces','San Patricio'),'County')
    dfw <- paste(c('Wise', 'Denton', 'Collin', 'Parker', 'Tarrant', 'Dallas', 
                   'Rockwall', 'Kaufman', 'Johnson', 'Ellis'), 'County')
    elp <-paste(c('El Paso'), 'County')
    hgb <- paste(c('Montgomery', 'Liberty', 'Waller', 'Harris', 'Chambers', 
                   'Fort Bend', 'Galveston', 'Brazoria'), 'County')
    netx <- paste(c('Smith', 'Upshur', 'Harrison', 'Gregg', 'Rusk'), 'County')
    san <- paste(c('Bexar','Comal', 'Guadalupe', 'Wilson'), 'County')
    vic <- paste(c('Victoria'), 'County')
    
    # combine all lists
    all_areas <- list('arr' = arr, 'bpa'= bpa, 'cc' = cc, 'dfw' = dfw, 'elp'= elp, 
                      'hgb' = hgb, 'netx' = netx, 'san' = san, 'vic' = vic)
    
    # set up storage vector for results
    oad_area <- rep('None',length(list)) 
    
    # iteratively search through list
    for (idx in seq_along(list)){ # for each index in the query list
      # idx is index, names(list)[idx] is key, list[[idx]] is value
      
      queryCounty <- list[idx]
      
      for (i in seq_along(all_areas)){ # for each OAD area
        current_oad_area <- names(all_areas)[i] # making this interpretable
        current_oad_counties <- all_areas[[i]] # same
        
        if (queryCounty %in% current_oad_counties){ # if there is match
          oad_area[idx] <- current_oad_area # update
        }
      }
    }
    return(oad_area)
  }
  
  else if (input.type == 'number'){ # optional; 
    # if the input data are GLC numbers (COUNTY LEVEL)
    
    # define matching lists for each of the nine OADs
    arr <- c(491, 453, 209, 55, 21)
    bpa <- c(199, 361, 245)
    cc <- c(355,409)
    dfw <- c(497, 121, 85, 367, 439, 113, 397, 257, 251, 139)
    elp <-c(141)
    hgb <- c(339, 291, 473, 201, 71, 157, 167, 39)
    netx <- c(423, 459, 203, 183, 401)
    san <- c(29,91, 187, 493)
    vic <- c(469)
    
    # combine all lists
    all_areas <- list('arr' = arr, 'bpa'= bpa, 'cc' = cc, 'dfw' = dfw, 'elp'= elp, 
                      'hgb' = hgb, 'netx' = netx, 'san' = san, 'vic' = vic)
    
    # set up storage vector for results
    oad_area <- rep('None',length(list)) 
    
    # iteratively search through list
    for (idx in seq_along(list)){ # for each index in the query list
      # idx is index, names(list)[idx] is key, list[[idx]] is value
      
      queryCounty <- list[idx]
      
      for (i in seq_along(all_areas)){ # for each OAD area
        current_oad_area <- names(all_areas)[i] # making this interpretable
        current_oad_counties <- all_areas[[i]] # same
        
        if (queryCounty %in% current_oad_counties){ # if there is match
          oad_area[idx] <- current_oad_area # update
        }
      }
    }
    
    return(oad_area)    
  }
  
  else {
    print("ERROR - input type not understood. Please use either 'text' or 'number'.")
    return(list)
  }

}