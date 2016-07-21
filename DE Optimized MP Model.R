# David Hirschey
# Daily Commodity Trading
# Smoothing Parameter by Double Optimization

# Load the Necessary Library
library (RPostgreSQL)
library (DEoptim)
library (PerformanceAnalytics)

# Load the PostgreSQL Driver
driver = dbDriver ('PostgreSQL')

# Connect to the Database
connection = dbConnect (driver, dbname = 'OMITTED', host = 'OMITTED', port = 'OMITTED', user = 'OMITTED', password = 'OMITTED')

# Commodity Names
commodityNames.vector = dbGetQuery (connection, 'SELECT DISTINCT "Asset" FROM trmiv22_dai_enm')

# News and Social Media Daily TRMI
TRMI.dai.2011 = dbGetQuery (connection, 'SELECT * FROM trmiv22_dai_enm WHERE "Asset" = \'CRU\' AND "Date" >= \'2011-01-01\' AND "Date" < \'2015-01-01\' ORDER BY "Date"')
TRMI.dai.2011 = TRMI.dai.2011[which (TRMI.dai.2011$dataType == 'News_Social'),]
TRMI.dai.2015 = dbGetQuery (connection, 'SELECT * FROM trmiv22_dai_enm WHERE "Asset" = \'CRU\' AND "Date" >= \'2015-01-01\' AND "Date" < \'2016-01-01\'ORDER BY "Date"')
TRMI.dai.2015 = TRMI.dai.2015[which (TRMI.dai.2015$dataType == 'News_Social'),]

# Daily CRUde Prices
PRICE.2011 = dbGetQuery (connection, 'SELECT * FROM prices_unadj WHERE "Asset" = \'CRU\' AND "Date" >= \'2011-01-01\' AND "Date" < \'2015-01-01\'ORDER BY "Date"')
PRICE.2015 = dbGetQuery (connection, 'SELECT * FROM prices_unadj WHERE "Asset" = \'CRU\' AND "Date" >= \'2015-01-01\' AND "Date" < \'2016-01-01\'ORDER BY "Date"')
# Minor Adjustment for Missing Crude Oil Data
PRICE.2015[353:365,] = rbind (c ('CRU', '2015-12-19', NA, NA, NA, NA, NA, NA),
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
PRICE.2015 = transform (PRICE.2015, Time = as.logical (Time), Open = as.numeric (Open), High = as.numeric (High), Low = as.numeric (Low), Close =  as.numeric (Close), Volume = as.numeric (Volume))

# Disconnect from the Database
dbDisconnect (connection)

# Produce Vector of Price Directions
priceDirection.fn = function (price.matrix){
  
  # Initialize Values
  updown = NULL
  
  # Loop to construct the sequence of price directions
  for (i in 1:length (price.matrix[, 'Close'])){
    if (is.na (price.matrix[i, 'Close'])){
      updown = c (updown, NA)
    } else if (is.na (price.matrix[i-1, 'Close'])){
      step = 1
      close.temp = 0
      while (is.na (price.matrix[i - step, 'Close'])){
        if (price.matrix[i - step, 'Date'] == price.matrix[1, 'Date']){
          close.temp = price.matrix[i, 'Open']
          break
        }
        step = step + 1
      }
      if (close.temp == 0){
        close.temp = price.matrix[i - step, 'Close']
      }
      if (price.matrix[i, 'Close'] > close.temp){
        updown = c (updown, TRUE)
      } else {
        updown = c (updown, FALSE)
      }
    } else {
      if (price.matrix[i, 'Close'] > price.matrix[i - 1, 'Close']){
        updown = c (updown, TRUE)
      } else {
        updown = c (updown, FALSE)
      }
    }
  }
  
  # Retrun The Vector
  return (updown)
}

# Equity Curve
equity.fn = function (price.matrix, vote.vector, start.value = 1){
  
  # Initial Values
  equity = start.value
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
      equity = equity * (1 + (price.matrix[i,'Close'] - close.temp) / close.temp) * vote.vector[i] + equity * (1 - (price.matrix[i,'Close'] - close.temp) / close.temp) * (1 - vote.vector[i])
      equity.hist = c (equity.hist, equity)
    } else {
      equity = equity * (1 + (price.matrix[i,'Close'] - price.matrix[i-1,'Close']) / price.matrix[i-1,'Close']) * vote.vector[i] + equity * (1 - (price.matrix[i,'Close'] - price.matrix[i-1,'Close']) / price.matrix[i-1,'Close']) * (1 - vote.vector[i])
      equity.hist = c (equity.hist, equity)
    }
  }
  
  # Return Equity Curve
  return (equity.hist)
}

# Single TRMI Crossover
singleBinary.fn = function (sentiment.vector, parameters.vector){
  
  # Adjust for Missing Values
  sentiment.vector[which (is.na (sentiment.vector))] = 0
  
  # Initial Value that Sets Up Forward Prediction
  singleBinary.vector = c (TRUE, TRUE)
  
  # Exponential Crossovers
  smoother.one = HoltWinters (sentiment.vector, alpha = parameters.vector[1], beta = FALSE, gamma = FALSE)
  smoother.two = HoltWinters (sentiment.vector, alpha = parameters.vector[2], beta = FALSE, gamma = FALSE)
  
  # Loop to Construct the Binary Signal
  for (i in 1:length (smoother.one$fitted[, 1])){
    if (smoother.one$fitted[i, 1] > smoother.two$fitted[i, 1]){
      singleBinary.vector = c (singleBinary.vector, TRUE)
    } else {
      singleBinary.vector = c (singleBinary.vector, FALSE)
    }
  }
  
  # Return Single Binary Vector
  return (singleBinary.vector)
}

# Optimization Subroutine
optimizer.fn = function (parameters.vector, priceDirection.vector, sentiment.vector, maximum){
  
  # Get the Sentiment Crossover Vector
  singleBinary.vector = singleBinary.fn (sentiment.vector, parameters.vector)
  
  # Return Portion of Matches Between Sentiment Crossovers and Price Directions
  if (maximum){
    return (1 - sum (singleBinary.vector[!is.na (priceDirection.vector)] == priceDirection.vector[!is.na (priceDirection.vector)]) / length (priceDirection.vector[!is.na (priceDirection.vector)]))
  } else {
    return (    sum (singleBinary.vector[!is.na (priceDirection.vector)] == priceDirection.vector[!is.na (priceDirection.vector)]) / length (priceDirection.vector[!is.na (priceDirection.vector)]))
  }
}

# General Plotter
generalPlotter.fn = function (TRMI.train, TRMI.test, prices.train, prices.test, sentiments, parameters.matrix, maximized = TRUE, store.printables = FALSE){
  
  # Training Sentiment Crossover Binaries
  singleBinary.matrix = matrix (NA, nrow = length (c (TRMI.train[, 1], TRMI.test[, 1])), ncol = length (sentiments))
  colnames (singleBinary.matrix) = sentiments
  for (i in sentiments){
    singleBinary.matrix[1:(length (TRMI.train[,1]) + 1), i] = singleBinary.fn (TRMI.dai.2011[, i], parameters.matrix[i, 1:2])
  }
  
  # Testing Sentiment Crossover Binaries
  for (i in sentiments){
    for (j in length (TRMI.train[, 1]):length (c (TRMI.train[, 1], TRMI.test[, 1]))){
      singleBinary.matrix[j,i] = singleBinary.fn (c (TRMI.train[,i], TRMI.test[1:j,i]), parameters.matrix[i, 1:2])[j]
    }
  }
  
  # Adjust for Maximized or Minimized Optimization
  if (maximized == FALSE){
    singleBinary.matrix = !singleBinary.matrix
  }
  
  # Produce Vector of Votes
  vote.vector = apply (singleBinary.matrix, 1, sum) / length (sentiments)
  
  # Initialize Plotting Objects
  prices.all = rbind (prices.train, prices.test)
  baseline = equity.fn (prices.all, rep (T, length (prices.all[,'Open'])))
  equity.train = equity.fn (prices.train, vote.vector)
  equity.test = equity.fn (prices.test, vote.vector[(length (prices.all[,'Close']) - length (prices.train[,'Close'])):length (prices.all[,'Close'])], start.value = equity.train[length (equity.train)])
  
  # Plot
  titleCode = TRMI.train[1, 'Asset']
  plot (x = prices.all[,'Date'], y = baseline,  type = 'l', xlab = 'Date', ylab = 'Value', main = paste ('General ', titleCode, ' Strategy Performance', sep = ''), ylim = c(0, 3))
  legendCode = paste ('Forward Testing', ' (alpha = ', formatC ((equity.test[length(equity.test)] / equity.test[1] - baseline[length(baseline)] / baseline[length (prices.train[,'Open']) - 1] ) * 100, digits = 4), '%)', sep =  '')
  legend ('topleft', legend = c ('Baseline Returns', 'Training Set', legendCode), col = c('black', 'red', 'blue'),  pch = 15)
  lines (x = prices.train[,'Date'], equity.train, col = 'red')
  lines (x = prices.all  [,'Date'], y = c (rep (NA, times = length (prices.train[,'Open'])), equity.test), col = 'blue')
  
  # William Sharpe Information Ratio
  print (InformationRatio (Ra = Return.calculate (as.ts (equity.test)), Rb = Return.calculate (as.ts (baseline[(length (equity.train) + 1):length (baseline)])), scale = 365))
  
  # Statistical Tests
  if (store.printables){
    equity.tests.fn (c (vote.vector, vote.vector.test)[3:length (baseline)] > .5, baseline, length (baseline) - length (equity.test) - 1, is.na (prices.train[3:length (prices.train[,'Close']),'Close']), is.na (prices.test[,'Close']))
  }
}

# Descriptive Statistics
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

# Testing Sentiments
sentiments.vector = names (TRMI.dai.2011[8:33])

# Price Direction
priceDirection.vector = priceDirection.fn (PRICE.2011)

# Optimization Driver
optimizerMax.matrix = NULL
#optimizerMin.matrix = NULL
for (i in sentiments.vector){
  optimizerMax.list = DEoptim (fn = optimizer.fn, lower = c (0.01, 0.01), upper = c (1, 1), control = DEoptim.control (itermax = 50), priceDirection.vector = priceDirection.fn (PRICE.2011), sentiment.vector = TRMI.dai.2011[, i], maximum = TRUE)
  #optimizerMin.list = DEoptim (fn = optimizer.fn, lower = c (0.01, 0.01), upper = c (1, 1), control = DEoptim.control (itermax = 50), priceDirection.vector = priceDirection.fn (PRICE.2011), sentiment.vector = TRMI.dai.2011[, i], maximum = FALSE)
  optimizerMax.matrix = rbind (optimizerMax.matrix, c (optimizerMax.list$optim$bestmem, 1 - optimizerMax.list$optim$bestval))
  #optimizerMin.matrix = rbind (optimizerMin.matrix, c (optimizerMin.list$optim$bestmem,     optimizerMin.list$optim$bestval))
}
rownames (optimizerMax.matrix) = sentiments.vector
#rownames (optimizerMin.matrix) = sentiments.vector

# Vote Aggregation Strategy
generalPlotter.fn (TRMI.dai.2011, TRMI.dai.2015, PRICE.2011, PRICE.2015, sentiments.vector, optimizerMax.matrix)

# Strategy Construction
# The idea here is to choose a mix of sentiments superior to simpy using all of them
