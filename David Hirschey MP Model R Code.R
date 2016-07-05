# David Hirschey
# Modeling Crude Oil Prices with Sentiment Data

# Load the Necessary Library
library (RPostgreSQL)

# Load the PostgreSQL Driver
driver = dbDriver('PostgreSQL')

# Connect to the Database
connection = dbConnect(driver, dbname = 'OMITTED', host = 'OMITTED', port = 'OMITTED', user = 'OMITTED', password = 'OMITTED')

# News and Social Media Daily TRMI
Crude.TRMI.dai.2011 = dbGetQuery (connection, 'SELECT * FROM trmiv22_dai_enm WHERE "Asset" = \'CRU\' AND "Date" >= \'2011-01-01\' AND "Date" < \'2015-01-01\' ORDER BY "Date"')
Crude.TRMI.dai.2011 = Crude.TRMI.dai.2011[which (Crude.TRMI.dai.2011$dataType == 'News_Social'),]
Crude.TRMI.dai.2015 = dbGetQuery (connection, 'SELECT * FROM trmiv22_dai_enm WHERE "Asset" = \'CRU\' AND "Date" >= \'2015-01-01\' AND "Date" < \'2016-01-01\'ORDER BY "Date"')
Crude.TRMI.dai.2015 = Crude.TRMI.dai.2015[which (Crude.TRMI.dai.2015$dataType == 'News_Social'),]

# Daily Crude Prices
Crude.PRICE.2011 = dbGetQuery (connection, 'SELECT * FROM prices_unadj WHERE "Asset" = \'CRU\' AND "Date" >= \'2011-01-01\' AND "Date" < \'2015-01-01\'ORDER BY "Date"')
Crude.PRICE.2015 = dbGetQuery (connection, 'SELECT * FROM prices_unadj WHERE "Asset" = \'CRU\' AND "Date" >= \'2015-01-01\' AND "Date" < \'2016-01-01\'ORDER BY "Date"')
# Minor Adjustment for Missing Data
Crude.PRICE.2015[353:365,] = rbind (c ('CRU', '2015-12-19', NA, NA, NA, NA, NA, NA),
                                    c ('CRU', '2015-12-19', NA, NA, NA, NA, NA, NA),
                                    c ('CRU', '2015-12-21', NA, 34.58, 34.86, 33.98, 34.66, NA),
                                    c ('CRU', '2015-12-22', NA, 35.80, 36.54, 35.66, 36.47, NA),
                                    c ('CRU', '2015-12-23', NA, 36.47, 37.95, 36.28, 37.89, NA),
                                    c ('CRU', '2015-12-24', NA, 37.86, 38.28, 37.38, 38.12, NA),
                                    c ('CRU', '2015-12-25', NA, NA, NA, NA, NA, NA),
                                    c ('CRU', '2015-12-26', NA, NA, NA, NA, NA, NA),
                                    c ('CRU', '2015-12-27', NA, NA, NA, NA, NA, NA),
                                    c ('CRU', '2015-12-28', NA, 38.00, 38.09, 36.60, 36.69, NA),
                                    c ('CRU', '2015-12-29', NA, 36.70, 37.94, 36.66, 37.33, NA),
                                    c ('CRU', '2015-12-30', NA, 37.36, 37.40, 36.40, 36.83, NA),
                                    c ('CRU', '2015-12-31', NA, 36.81, 37.79, 36.22, 37.07, NA)
)
Crude.PRICE.2015 = transform (Crude.PRICE.2015, Time = as.logical (Time), Open = as.numeric (Open), High = as.numeric (High), Low = as.numeric (Low), Close =  as.numeric (Close), Volume = as.numeric (Volume))

# Disconnect
dbDisconnect (connection)

# Single TRMI Crossover
single.crossover.fn = function (sentiment.vector, splitfactor = .5){
  
  # Adjust for Missing Values
  sentiment.vector[which (is.na (sentiment.vector))] = 0
  
  # Initial Value that Sets Up Forward Prediction
  single.binary.vector = c (TRUE, TRUE)
  
  # Exponential Crossovers
  smoother = HoltWinters (sentiment.vector, beta = FALSE, gamma = FALSE)
  smoother.alt = HoltWinters (sentiment.vector, alpha = splitfactor, beta = FALSE, gamma = FALSE)
  
  # Loop to Construct the Binary Signal
  for (i in 1:length (smoother$fitted[,1])){
    if (smoother$fitted[i,1] > smoother.alt$fitted[i,1]){
      single.binary.vector = c (single.binary.vector, TRUE)
    } else {
      single.binary.vector = c (single.binary.vector, FALSE)
    }
  }
  
  # Return Single Binary Vector
  return (single.binary.vector)
}

# Equity Curve
equity.fn = function (price.matrix, vote.vector, association.point = 1){
  
  # Initial Values
  equity = 1
  equity.hist = NULL
  
  # Loop to Construct the Equity Curve
  for (i in 1:length (price.matrix[,'Close'])){
    if (is.na (price.matrix[i,'Close'])){
      equity.hist = c (equity.hist, equity)
    } else if (is.na (price.matrix[i-1,'Close'])){
      step = 1
      
      # Look Backwards For the Previous Close
      close.temp = 0
      while (is.na (price.matrix[i - step, 'Close'])){
        if (price.matrix[i - step,'Date'] == price.matrix[1,'Date']){
          close.temp = price.matrix[i,'Open']
          break
        }
        step = step + 1
      }
      if (close.temp == 0){
        close.temp = price.matrix[i - step, 'Close']
      }
      
      # Compute Equity curve
      equity = equity * (1 + (price.matrix[i,'Close'] - close.temp) * association.point / close.temp) * vote.vector[i] + equity * (1 - (price.matrix[i,'Close'] - close.temp) * association.point / close.temp) * (1 - vote.vector[i])
      equity.hist = c (equity.hist, equity)
    } else {
      equity = equity * (1 + (price.matrix[i,'Close'] - price.matrix[i-1,'Close']) * association.point / price.matrix[i-1,'Close']) * vote.vector[i] + equity * (1 - (price.matrix[i,'Close'] - price.matrix[i-1,'Close']) * association.point / price.matrix[i-1,'Close']) * (1 - vote.vector[i])
      equity.hist = c (equity.hist, equity)
    }
  }
  
  # Return Equity Curve
  return (equity.hist)
}

# Single TRMI Optimization Subroutine With a Switch for Voting Threshold Optimization
single.optim.fn = function (splitfactor, sentiment.vector, price.matrix, association.point, multi.switch = FALSE, multi.binary.vector){
  
  # Get the Single Binary Vector or Multi Binary Vector
  if (multi.switch){
    single.binary.vector = multi.binary.vector
  } else {
    single.binary.vector = single.crossover.fn (sentiment.vector = sentiment.vector, splitfactor = splitfactor)
  }
  
  # Initialize Values
  updown = NULL
  
  # Loop to construct the sequence of price directions
  for (i in 1:length (Crude.PRICE.2011[,'Close'])){
    if (is.na (Crude.PRICE.2011[i,'Close'])){
      updown = c (updown, NULL)
    } else if (is.na (Crude.PRICE.2011[i-1,'Close'])){
      step = 1
      close.temp = 0
      while (is.na (Crude.PRICE.2011[i - step, 'Close'])){
        if (Crude.PRICE.2011[i - step,'Date'] == Crude.PRICE.2011[1,'Date']){
          close.temp = Crude.PRICE.2011[i,'Open']
          break
        }
        step = step + 1
      }
      if (close.temp == 0){
        close.temp = Crude.PRICE.2011[i - step, 'Close']
      }
      if (Crude.PRICE.2011[i, 'Close'] > close.temp){
        updown = c (updown, TRUE)
      } else {
        updown = c (updown, FALSE)
      }
    } else {
      if (Crude.PRICE.2011[i, 'Close'] > Crude.PRICE.2011[i - 1,'Close']){
        updown = c (updown, TRUE)
      } else {
        updown = c (updown, FALSE)
      }
    }
  }
  
  # Count portion of correct predictions
  if (association.point == 1){
    return (sum (single.binary.vector[!is.na(Crude.PRICE.2011[,'Close'])] == updown) / length (updown))
  } else {
    return (sum (single.binary.vector[!is.na(Crude.PRICE.2011[,'Close'])] == !updown) / length (updown))
  }
}

# Multi TRMI Crossover
multi.crossover.fn = function (sentiment.matrix, association.vector, splitfactor.optim.vector){
  
  # Initialize Values
  single.binary.matrix = single.crossover.fn (sentiment.matrix[,1], splitfactor.optim.vector[1])
  vote.vector = rep (TRUE, times = length (sentiment.matrix[,1]))
  
  # Loop to Append Values
  for (i in 2:length (sentiment.matrix[1,])){
    single.binary.matrix = cbind (single.binary.matrix, single.crossover.fn (sentiment.matrix[,i], splitfactor.optim.vector[i]))
  }
  
  # Adjust to Appropriate Association
  for (i in 1:length (association.vector)){
    if (association.vector[i] == -1){
      single.binary.matrix[,i] = !single.binary.matrix[,i]
    }
  }  
  
  # Sum Votes
  for (i in 1:length (sentiment.matrix[,1])){
    vote.vector[i] = sum (single.binary.matrix[i,]) / length (single.binary.matrix[i,])
  }
  
  # Return Voting Vector
  return (vote.vector)
}

# Testing Returns
testing.return.fn = function (trmi.train, trmi.test, prices.train, prices.test, sentiments, association.vector, splitfactor.optim.vector, multi.binary.vector, threshold.optim.point, vote.vector){
  
  # Initialize value to end of 2014
  umbrella.hist.2015 = equity.fn (prices.train, vote.vector)[length (prices.train[,'Close'])]
  
  # Loop to calculate equity curve
  for (j in 1:length (prices.test[,'Open'])){
    prices.test.2015 = rbind (prices.train, prices.test[1:j,])
    return.crossover.test = multi.crossover.fn (rbind (trmi.train[,sentiments], trmi.test[1:j,sentiments]), association.vector, splitfactor.optim.vector)
    if (is.na (prices.test.2015[length (prices.test.2015[,'Close']),'Close'])){
      return.2015 = 1
    } else if (is.na (prices.test.2015[length (prices.test.2015[,'Close']) - 1,'Close'])){
      step = 1
      close.temp = 0
      while (is.na (prices.test.2015[length (prices.test.2015[,'Close']) - step, 'Close'])){
        if (prices.test.2015[j - step,'Date'] == prices.test.2015[1,'Date']){
          close.temp = prices.test.2015[length (prices.test.2015[,'Close']),'Open']
          break
        }
        step = step + 1
      }
      if (close.temp == 0){
        close.temp = prices.test.2015[length (prices.test.2015[,'Close']) - step, 'Close']
      }
      return.2015 = (1 + (prices.test.2015[length (prices.test.2015[,'Close']),'Close'] - close.temp) / close.temp) * return.crossover.test[j] + (1 - (prices.test.2015[length (prices.test.2015[,'Close']),'Close'] - close.temp) / close.temp) * (1 - return.crossover.test[j])
    } else {
      return.2015 = (1 + (prices.test.2015[length (prices.test.2015[,'Close']),'Close'] - prices.test.2015[length (prices.test.2015[,'Close']) - 1,'Close']) / prices.test.2015[length (prices.test.2015[,'Close']) - 1,'Close']) * return.crossover.test[j] + (1 - (prices.test.2015[length (prices.test.2015[,'Close']),'Close'] - prices.test.2015[length (prices.test.2015[,'Close']) - 1,'Close']) / prices.test.2015[length (prices.test.2015[,'Close']) - 1,'Close']) * (1 - return.crossover.test[j])
    }
    umbrella.hist.2015 = c (umbrella.hist.2015, return.2015 * umbrella.hist.2015[length (umbrella.hist.2015)])
    if (j == length (prices.test[,'Open'])){
      assign ('vote.vector.test', return.crossover.test, envir = globalenv ())
    }
  }
  
  return (umbrella.hist.2015)
}

# Umbrella Plotter
umbrella.plotter.fn = function (TRMI.train, TRMI.test, prices.train, prices.test, sentiments, association.vector, store.printables = FALSE){
  
  # Produce Vector of Optimal Smoothing Alpha
  splitfactor.optim.vector = NULL
  for (i in 1:length (sentiments)){
    if (association.vector[i] == 1){
      splitfactor.optim.vector = c (splitfactor.optim.vector, optimize (f = single.optim.fn, interval = c (0,1), maximum = TRUE, sentiment.vector = TRMI.train[,sentiments[i]], price.matrix = prices.train, association.point = association.vector[i])$maximum)
    } else {
      splitfactor.optim.vector = c (splitfactor.optim.vector, optimize (f = single.optim.fn, interval = c (0,1), maximum = FALSE, sentiment.vector = TRMI.train[,sentiments[i]], price.matrix = prices.train, association.point = association.vector[i])$minimum)
    }
  }
  
  # Produce Vector of Votes
  vote.vector = multi.crossover.fn (sentiment.matrix = TRMI.train[,sentiments], association.vector = association.vector, splitfactor.optim.vector = splitfactor.optim.vector)
  
  # Initialize Plotting Objects
  prices.2015 = rbind (prices.train, prices.test)
  baseline = equity.fn (prices.2015, rep (T, length (prices.2015[,'Open'])))
  printable = testing.return.fn (TRMI.train, TRMI.test, prices.train, prices.test, sentiments, association.vector, splitfactor.optim.vector, multi.binary.vector, threshold.optim.point, vote.vector)
  
  # Plot  
  plot (x = prices.2015[,'Date'], y = baseline,  type = 'l', xlab = 'Date', ylab = 'Value', main = 'General Crossover Strategy Performance', ylim = c(0, 2))
  legendcode = paste ('Forward Testing', ' (alpha = ', formatC ((printable[length(printable)] / printable[1] - baseline[length(baseline)] / baseline[length (prices.train[,'Open']) - 1] ) * 100, digits = 4), '%)', sep =  '')
  legend ('topleft', legend = c ('Baseline Returns', 'Training Set', legendcode), col = c('black', 'red', 'blue'),  pch = 15)
  lines (x = prices.train[,'Date'], y = equity.fn (prices.train, vote.vector), col = 'red')
  lines (x = prices.2015[,'Date'], y = c (rep (NA, times = length (prices.train[,'Open']) - 1), printable), col = 'blue')
  
  # Statistical Tests
  if (store.printables){
    printables.fn (c (vote.vector, vote.vector.test)[3:length (baseline)] > .5, baseline, length (baseline) - length (printable) - 1, is.na (prices.train[3:length (prices.train[,'Close']),'Close']), is.na (prices.test[,'Close']))
  }
}

# Printables Function
printables.fn = function (strategy.printable, baseline.printable, length.train, weekends.train, weekends.test){
  
  # Price Direction
  baseline.updown = c (baseline.printable[3:length (baseline.printable)], FALSE) > baseline.printable[2:length (baseline.printable)]
  baseline.updown = baseline.updown[1:(length (baseline.updown) - 1)]
  
  # Both  Periods
    # All
    print (binom.test (sum (strategy.printable == baseline.updown, na.rm = TRUE), length (strategy.printable) - sum (weekends.train, weekends.test), p = .5, 'greater'))
    # Longs
    print (binom.test (sum ((strategy.printable == baseline.updown)[which (strategy.printable == TRUE)], na.rm = TRUE), length (strategy.printable) - sum (c (weekends.train, weekends.test) | (strategy.printable == FALSE), na.rm = TRUE), p = .5, 'greater'))
    # Shorts
    print (binom.test (sum ((strategy.printable == baseline.updown)[which (strategy.printable == FALSE)], na.rm = TRUE), length (strategy.printable) - sum (c (weekends.train, weekends.test) | (strategy.printable == TRUE), na.rm = TRUE), p = .5, 'greater'))
  # Training
    # All
    print (binom.test (sum ((strategy.printable == baseline.updown)[1:length.train], na.rm = TRUE), length.train - sum (weekends.train), p = .5, 'greater'))
    # Longs
    print (binom.test (sum ((strategy.printable == baseline.updown)[1:length.train][which (strategy.printable == TRUE)], na.rm = TRUE), length.train - sum (weekends.train | (strategy.printable == FALSE)[1:length.train], na.rm  = TRUE), p = .5, 'greater'))
    # Shorts
    print (binom.test (sum ((strategy.printable == baseline.updown)[1:length.train][which (strategy.printable == FALSE)], na.rm = TRUE), length.train - sum (weekends.train | (strategy.printable == TRUE)[1:length.train], na.rm  = TRUE), p = .5, 'greater'))
  # Testing
    # All
    print (binom.test (sum ((strategy.printable == baseline.updown)[(length.train + 1):length (strategy.printable)], na.rm = TRUE), length (strategy.printable) - length.train - sum (weekends.test), p = .5, 'greater'))
    # Longs
    print (binom.test (sum ((strategy.printable == baseline.updown)[(length.train + 1):length (strategy.printable)][which (strategy.printable == TRUE)], na.rm = TRUE), length (strategy.printable) - length.train - sum (weekends.test | (strategy.printable == FALSE)[(length.train + 1):length (strategy.printable)], na.rm  = TRUE), p = .5, 'greater'))
    # Shorts
    print (binom.test (sum ((strategy.printable == baseline.updown)[(length.train + 1):length (strategy.printable)][which (strategy.printable == FALSE)], na.rm = TRUE), length (strategy.printable) - length.train - sum (weekends.test | (strategy.printable == TRUE)[(length.train + 1):length (strategy.printable)], na.rm  = TRUE), p = .5, 'greater'))
}

# Sentiment Names
sentiments.testing = names (Crude.TRMI.dai.2011)[8:33]

# Sentiment Association Directions from Individual Testing
association.vector = c (-1, 1, -1, 1, 1, 1, 1, -1, -1, 1, 1, 1, 1, 1, -1, 1, -1, 1, -1, 1, 1, 1, 1, 1, 1, -1)

# Run The Program
umbrella.plotter.fn (Crude.TRMI.dai.2011, Crude.TRMI.dai.2015, Crude.PRICE.2011, Crude.PRICE.2015, sentiments.testing, association.vector, store.printables = TRUE)
