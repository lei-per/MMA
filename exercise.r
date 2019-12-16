# Read data from CSV file
financials <- read.csv("financials.csv", row.names = 1, header = TRUE)
financials

# Calculate profit or loss totals for each financial year
profit_loss_total <- colSums(financials[1:8, ])

# Set the number of ordinary shares used in the calculation of earnings per share
ordinary_shares <- c(402771, 678468, 858077)
names(ordinary_shares) <- c("FY17", "FY18", "FY19")

# Calculate earnings per share (in cents per share)
earnings_per_share <- round(profit_loss_total / ordinary_shares * 100, 3)

# Summarise results of the analysis in a dataframe
analysis <- data.frame(profit_loss_total, ordinary_shares, earnings_per_share)
analysis

# Calculate a forecast for the 2020 financial year by assuming all income and expenses grow by 5%
FY20_forecast <- round(1.05 * financials[1:8, "FY19"])
names(FY20_forecast) <- rownames(financials[1:8, ])

# Update the revenue forecast using last years revenue growth rate
growth_rate <- financials["revenue", "FY19"] / financials["revenue", "FY18"]
FY20_forecast["revenue"] <- round(growth_rate * financials["revenue", "FY19"])
data.frame(FY20_forecast)

# Calculate profit or loss total for the 2020 financial year forecast
profit_loss_total_FY20_forecast <- sum(FY20_forecast)
names(profit_loss_total_FY20_forecast) <- "profit_loss_total_FY20_forecast"
profit_loss_total_FY20_forecast

# Load the quantmod package
library("quantmod")

# Use getSymbols from the quantmod package to get stock data from yahoo finance
MRM.AX <- getSymbols("MRM.AX", src = "yahoo", auto.assign = FALSE)

# Display the first and last three rows of the stock data
data.frame(head(MRM.AX, 3))
data.frame(tail(MRM.AX, 3))

# Subset stock data to select prices (in cents) one month after each report was released
price_per_share <- as.vector(MRM.AX[c("2017-09-28", "2018-09-27", "2019-09-26"), "MRM.AX.Adjusted"] * 100)

# Calculate PE ratios
pe_ratio <- round(price_per_share / earnings_per_share, 3)

# Add new columns to the analysis dataframe with the results
analysis$price_per_share <- price_per_share
analysis$pe_ratio <- pe_ratio
analysis

# Load the timeDate package
library("timeDate")

# Subset stock data to select prices for the 2018 calendar year (in cents)
days_CY18 <- seq(as.Date("2018-01-01"), as.Date("2018-12-31"), by="days")
prices_CY18 <- MRM.AX[days_CY18, "MRM.AX.Adjusted"] * 100

# Use apply.monthly from the timeDate package to select prices at the end of each month
prices_CY18_monthly <- apply.monthly(prices_CY18, head, 1)
names(prices_CY18_monthly) <- "price_monthly"
data.frame(prices_CY18_monthly)

# Calculate monthly PE ratio
pe_ratio_monthly <- round(prices_CY18_monthly / analysis["FY18", "earnings_per_share"], 3)
names(pe_ratio_monthly) <- "pe_ratio_monthly"

# Summarise results in the analysis_monthly dataframe
analysis_monthly <- data.frame(prices_CY18_monthly, pe_ratio_monthly)
analysis_monthly

# Add depreciation data to the financials data
depreciation <- c(45541, 31903, 35319)
financials["depreciation",] <- depreciation

# Calculate EBITDA 
ebitda <- colSums(financials[1:9, ]) - colSums(financials[6:8,])
data.frame(ebitda)

# Add cash equivalents and non current liabilities to the financials data
cash_equivalents <- c(28757, 69648, 70155)
noncurrent_liabilities <- c(323929, 265215, 268255)
financials["cash_equivalents",] <- cash_equivalents
financials["noncurrent_liabilities",] <- noncurrent_liabilities
financials

# Calculate Enterprise Value 
enterprise_value <- round(ordinary_shares * price_per_share / 100 + noncurrent_liabilities - cash_equivalents)

# Calculate EV/EBITDA
ev_ebitda_ratio <- round(enterprise_value / ebitda, 3)

# Add new columns to the analysis dataframe with the results
analysis$ebitda <- ebitda
analysis$enterprise_value <- enterprise_value
analysis$ev_ebitda_ratio <- ev_ebitda_ratio
analysis

# Calculate monthly EV / EBITDA ratios
enterprise_value_monthly <- analysis["FY18", "ordinary_shares"] * prices_CY18_monthly / 100 + 
    financials["noncurrent_liabilities", "FY18"] - financials["cash_equivalents", "FY18"]
ev_ebitda_ratio_monthly <- round(enterprise_value_monthly / analysis["FY18", "ebitda"], 3)
names(ev_ebitda_ratio_monthly) <- "ev_ebitda_ratio"

# Add new columns to the analysis_monthly dataframe with the results
analysis_monthly$ev_ebitda_ratio <- ev_ebitda_ratio_monthly
analysis_monthly
