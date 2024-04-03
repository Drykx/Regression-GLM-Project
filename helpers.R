library(lubridate)
library(styler)
library(dplyr)


import_data <- function(data_path) {
  # import data from csv
  litter <-
    read.csv(
      file = data_path,
      header = TRUE,
      sep = ",",
      skip = 2,
      check.names = FALSE
    )
  
  nobs <- dim(litter)[1]
  
  # select rows: cigarette butts, month, year, beach and beach length
  butts <- cbind(litter[1:nobs, c(1, 6, 7, 20)])
  
  nvar <- dim(butts)[2]
  
  # convert to dataframe
  Butts <- data.frame(
    Beach = factor(butts$BeachName),
    Length = butts$BeachLength_m,
    Year = factor(year(mdy(butts$EventDate))),
    Month = factor(month(mdy(butts$EventDate))),
    y = butts[, nvar]
  )
  
  # Drop outliers, NA, and strange measurements
  
  drop1 <- c(1:nrow(Butts))[c(867, 868)] # Long beaches 
  drop2 <- c(1:nrow(Butts))[is.na(Butts$y)] # NA values
  drop3 <- which(Butts$Beach == "Rhein_Beach near Tinguely Museum_Bolger. O_Sigrist F.") # Dirty beach in Basel
  drop <- c(drop2)
  
  Butts <- Butts[-drop, ]
  
  # sort data by year then by month
  index_sort <- order(Butts$Year, Butts$Month)
  Butts <- Butts[index_sort,]
  
  # reset index
  rownames(Butts) <- 1:nrow(Butts)
  
  return(Butts)
}

import_data0 <-
  function(DATA_PATH = "../data/SLR-Data.csv",
           features = c(
             "BeachName",
             "BeachLength_m",
             "EventDate",
             "Gebiet",
             "Jahreszeit",
             # "Region",
             "Cigarette butts and filters"
           )) {
    # import data
    df <-
      read.csv(
        file = DATA_PATH,
        header = TRUE,
        sep = ",",
        skip = 2,
        check.names = FALSE,
        encoding = "latin1"
      )
    
    # only keep columns
    df <- df[, features]
    
    
    # create clean dataframe
    nvar <- length(features)
    weekday_order <-
      c("lundi",
        "mardi",
        "mercredi",
        "jeudi",
        "vendredi",
        "samedi",
        "dimanche")
    month_order <- 1:12
    
    df <- data.frame(
      # Region = factor(df$Region),
      Settlement = factor(df$Gebiet),
      Beach = factor(df$BeachName),
      Length = df$BeachLength_m,
      Year = factor(year(mdy(df$EventDate))),
      Season = factor(df$Jahreszeit),
      Month = factor(month(mdy(df$EventDate)), levels = month_order),
      Day = factor(weekdays(mdy(df$EventDate)), levels = weekday_order),
      y = df[, nvar]
    )
    
    # create column of custom seasons, ie months grouped as {1,2,3}, {4,5,6}, {7,8,9}, {10,11,12}
    df <- df %>%
      mutate(
        Season.custom = factor(
          case_when(
            Month %in% c(1, 2, 3) ~ "J/F/M",
            Month %in% c(4, 5, 6) ~ "A/M/J",
            Month %in% c(7, 8, 9) ~ "J/A/S",
            Month %in% c(10, 11, 12) ~ "O/N/D",
          ),
          levels = c("J/F/M", "A/M/J", "J/A/S", "O/N/D")
        )
      )
    
    # Drop outliers, NA, and strange measurements
    
    drop1 <- c(1:nrow(df))[c(867, 868)] # Long beaches 
    drop2 <- c(1:nrow(df))[is.na(df$y)] # NA values
    drop3 <- which(df$Beach == "Rhein_Beach near Tinguely Museum_Bolger. O_Sigrist F.") # Dirty beach in Basel
    drop4 <- c(1:nrow(df))[is.na(df$Day)] # Incorrect dates
    
    drop <- c(drop2,drop4)
    
    df <- df[-drop, ]
    
    
    
    # reorder columns
    ordered_columns <-
      c(
        # "Region",
        "Settlement",
        "Beach",
        "Length",
        "Year",
        "Season",
        "Season.custom",
        "Month",
        "Day",
        "y"
      )
    df <- df[, ordered_columns]
    
    return(df)
  }

format_files <- function(bool) {
  if (bool) {
    style_file("src/implementations.R")
    style_file("src/helpers.R")
  }
}

import_final_data <-
  function(DATA_PATH = "../data/SLR-Data.csv",
           features = c(
             "BeachName",
             "BeachLength_m",
             "EventDate",
             "Gebiet",
             "Jahreszeit",
             # "Region",
             "Cigarette butts and filters"
           )) {
    # import data
    df <-
      read.csv(
        file = DATA_PATH,
        header = TRUE,
        sep = ",",
        skip = 2,
        check.names = FALSE,
        encoding = "latin1"
      )
    
    # only keep columns
    df <- df[, features]
    
    
    # create clean dataframe
    nvar <- length(features)
    weekday_order <-
      c("lundi",
        "mardi",
        "mercredi",
        "jeudi",
        "vendredi",
        "samedi",
        "dimanche")
    month_order <- 1:12
    
    df <- data.frame(
      # Region = factor(df$Region),
      Settlement = factor(df$Gebiet),
      Beach = factor(df$BeachName),
      Length = df$BeachLength_m,
      Year = factor(year(mdy(df$EventDate))),
      Season = factor(df$Jahreszeit),
      Month = factor(month(mdy(df$EventDate)), levels = month_order),
      Day = factor(weekdays(mdy(df$EventDate)), levels = weekday_order),
      y = df[, nvar]
    )
    
    # create column of custom seasons, ie months grouped as {1,2,3}, {4,5,6}, {7,8,9}, {10,11,12}
    df <- df %>%
      mutate(
        Season.custom = factor(
          case_when(
            Month %in% c(1, 2, 3) ~ "J/F/M",
            Month %in% c(4, 5, 6) ~ "A/M/J",
            Month %in% c(7, 8, 9) ~ "J/A/S",
            Month %in% c(10, 11, 12) ~ "O/N/D",
          ),
          levels = c("J/F/M", "A/M/J", "J/A/S", "O/N/D")
        )
      )
    
    # Drop outliers, NA, and strange measurements
    
    drop1 <- c(1:nrow(df))[c(867, 868)] # Long beaches 
    drop2 <- c(1:nrow(df))[is.na(df$y)] # NA values
    drop3 <- which(df$Beach == "Rhein_Beach near Tinguely Museum_Bolger. O_Sigrist F.") # Dirty beach in Basel
    drop4 <- c(1:nrow(df))[is.na(df$Day)] # Incorrect dates

    drop <- c(drop1, drop2, drop3,drop4)
    
    df <- df[-drop, ]
    
    
    
    # reorder columns
    ordered_columns <-
      c(
        # "Region",
        "Settlement",
        "Beach",
        "Length",
        "Year",
        "Season",
        "Season.custom",
        "Month",
        "Day",
        "y"
      )
    df <- df[, ordered_columns]
    
    return(df)
  }

format_files <- function(bool) {
  if (bool) {
    style_file("src/implementations.R")
    style_file("src/helpers.R")
  }
}
