# Load libraries
library(dplyr)
library(ggplot2)
library(quantmod)

# Set working directory
setwd('/Users/rohitkumawat/Desktop/Jaishree_Thesis_MA')

####################### Exchange Rate Data Prep #####################
# Load INR-USD data
df_INRUSD_daily <- read.csv(file = 'INRUSD_Hist_Data_Daily.csv')
df_INRUSD_daily$DEXINUS <- as.numeric(df_INRUSD_daily$DEXINUS)
df_INRUSD_daily$DATE <- as.Date(df_INRUSD_daily$DATE)
df_INRUSD_daily <- na.omit(df_INRUSD_daily)

# Select data from Jan-1991 to Feb-2021
df_INRUSD_daily <- df_INRUSD_daily %>% filter(DATE > '1991-01-01')
df_INRUSD_daily <- df_INRUSD_daily %>% filter(DATE < '2021-03-01')

# plot the data
df_INRUSD_daily_plot <- ggplot(data = df_INRUSD_daily) +
                        ggplot2::geom_line(mapping = aes(x = DATE, y = DEXINUS)) +
                        ggplot2::ylim(0,80) +
                        ggplot2::labs(x = "Date", y = "INR/USD", title = "INR/USD daily exchange rate (1991 - 2021)")
df_INRUSD_daily_plot + ggplot2::ggsave(filename = "INRUSD_daily_exchange_rate_plot.jpg")



####################### SENSEX Data Prep #####################################
# Load SENSEX data
df_sensex_daily <- read.csv(file = 'SENSEX_Hist_Data_Daily.csv')
df_sensex_daily <- df_sensex_daily %>% select(c("Date", "Close"))
df_sensex_daily$Date <- as.Date(df_sensex_daily$Date)
df_sensex_daily$Close <- as.numeric(df_sensex_daily$Close)
df_sensex_daily <- na.omit(df_sensex_daily)

# Select data from Jan-1991 and Feb-2021
df_sensex_daily <- df_sensex_daily %>% filter(Date > "1991-01-01")
df_sensex_daily <- df_sensex_daily %>% filter(Date < "2021-03-01")

# Plot the data
df_sensex_daily_plot <- ggplot2::ggplot(data = df_sensex_daily) +
                   ggplot2::geom_line(mapping = aes(x = Date, y = Close)) +
                   ggplot2::ylim(0, 60000) +
                   ggplot2::labs(x = "Date", y = "SENSEX", title = "SENSEX daily close value (1991 - 2021)")

df_sensex_daily_plot + ggplot2::ggsave(filename = "SENSEX_daily_close_plot.jpg")


####################### DOLLEX 30 Data Prep #####################################
# Load DOLLEX 30 data
df_dollex30_daily <- read.csv(file = "DOLLEX30_Hist_Data_Daily.csv")
df_dollex30_daily <- df_dollex30_daily %>% select(c("Date", "Close"))
df_dollex30_daily$Date <- as.Date(df_dollex30_daily$Date)
df_dollex30_daily$Close <- as.numeric(df_dollex30_daily$Close)
df_dollex30_daily <- na.omit(df_dollex30_daily)

# Select data from Jan-1991 to Feb-2021
df_dollex30_daily <- df_dollex30_daily %>% filter(Date > "1991-01-01")
df_dollex30_daily <- df_dollex30_daily %>% filter(Date < "2021-03-01")

# Daily Percent Change -- Volatility
df_dollex30_daily$pctChange <- quantmod::Delt(x1 = df_dollex30_daily$Close, k = 1, type = "arithmetic")
df_dollex30_daily <- df_dollex30_daily[-1,]
write.csv(x = df_dollex30_daily, file = "DOLLEX30_cleaned_data.csv")


####################### DOLLEX 200 Data Prep #####################################
# Load DOLLEX 200 data
df_dollex200_daily <- read.csv(file = "DOLLEX200_Hist_Data_Daily.csv")
df_dollex200_daily <- df_dollex200_daily %>% select(c("Date", "Close"))
df_dollex200_daily$Date <- as.Date(df_dollex200_daily$Date)
df_dollex200_daily$Close <- as.numeric(df_dollex200_daily$Close)
df_dollex200_daily <- na.omit(df_dollex200_daily)

# Select data from Jan-1991 to Feb-2021
df_dollex200_daily <- df_dollex200_daily %>% filter(Date > "1991-01-01")
df_dollex200_daily <- df_dollex200_daily%>% filter(Date < "2021-03-01")

# Daily Percent Change -- Volatility
df_dollex200_daily$pctChange <- quantmod::Delt(x1 = df_dollex200_daily$Close, k = 1, type = "arithmetic")
df_dollex200_daily <- df_dollex200_daily[-1,]
write.csv(x = df_dollex200_daily, file = "DOLLEX200_cleaned_data.csv")


####################### DOLLEX 100 Data Prep #####################################
# Load DOLLEX 100 data
df_dollex100_daily <- read.csv(file = "DOLLEX100_Hist_Data_Daily.csv")
df_dollex100_daily<- df_dollex100_daily %>% select(c("Date", "Close"))
df_dollex100_daily$Date <- as.Date(df_dollex100_daily$Date)
df_dollex100_daily$Close <- as.numeric(df_dollex100_daily$Close)
df_dollex100_daily <- na.omit(df_dollex100_daily)

# Select data from Jan-1991 to Feb-2021
df_dollex100_daily <- df_dollex100_daily %>% filter(Date > "1991-01-01")
df_dollex100_daily <- df_dollex100_daily %>% filter(Date < "2021-03-01")

# Daily Percent Change -- Volatility
df_dollex100_daily$pctChange <- quantmod::Delt(x1 = df_dollex100_daily$Close, k = 1, type = "arithmetic")
df_dollex100_daily <- df_dollex100_daily[-1,]
write.csv(x = df_dollex100_daily, file = "DOLLEX100_cleaned_data.csv")


####################### SENSEX 200 Data Prep #####################################
# Load SENSEX 200 data
df_sensex200_daily <- read.csv(file = "SENSEX200_Hist_Data_Daily.csv")
df_sensex200_daily<- df_sensex200_daily %>% select(c("Date", "Close"))
df_sensex200_daily$Date <- as.Date(df_sensex200_daily$Date)
df_sensex200_daily$Close <- as.numeric(df_sensex200_daily$Close)
df_sensex200_daily <- na.omit(df_sensex200_daily)

# Select data from Jan-1991 to Feb-2021
df_sensex200_daily <- df_sensex200_daily %>% filter(Date > "1991-01-01")
df_sensex200_daily <- df_sensex200_daily %>% filter(Date < "2021-03-01")

# Daily Percent Change -- Volatility
df_sensex200_daily$pctChange <- quantmod::Delt(x1 = df_sensex200_daily$Close, k = 1, type = "arithmetic")
df_sensex200_daily <- df_sensex200_daily[-1,]
write.csv(x = df_sensex200_daily, file = "SENSEX200_cleaned_data.csv")


####################### SENSEX 100 Data Prep #####################################
# Load SENSEX 100 data
df_sensex100_daily <- read.csv(file = "SENSEX100_Hist_Data_Daily.csv")
df_sensex100_daily<- df_sensex100_daily %>% select(c("Date", "Close"))
df_sensex100_daily$Date <- as.Date(df_sensex100_daily$Date)
df_sensex100_daily$Close <- as.numeric(df_sensex100_daily$Close)
df_sensex100_daily <- na.omit(df_sensex100_daily)

# Select data from Jan-1991 to Feb-2021
df_sensex100_daily <- df_sensex100_daily %>% filter(Date > "1991-01-01")
df_sensex100_daily <- df_sensex100_daily %>% filter(Date < "2021-03-01")

# Daily Percent Change -- Volatility
df_sensex100_daily$pctChange <- quantmod::Delt(x1 = df_sensex100_daily$Close, k = 1, type = "arithmetic")
df_sensex100_daily <- df_sensex100_daily[-1,]
write.csv(x = df_sensex100_daily, file = "SENSEX100_cleaned_data.csv")


####################### SENSEX 500 Data Prep #####################################
# Load SENSEX 500 data
df_sensex500_daily <- read.csv(file = "SENSEX500_Hist_Data_Daily.csv")
df_sensex500_daily <- df_sensex500_daily %>% select(c("Date", "Close"))
df_sensex500_daily$Date <- as.Date(df_sensex500_daily$Date)
df_sensex500_daily$Close <- as.numeric(df_sensex500_daily$Close)
df_sensex500_daily <- na.omit(df_sensex500_daily)

# Select data from Jan-1991 to Feb-2021
df_sensex500_daily <- df_sensex500_daily %>% filter(Date > "1991-01-01")
df_sensex500_daily <- df_sensex500_daily %>% filter(Date < "2021-03-01")

# Daily Percent Change -- Volatility
df_sensex500_daily$pctChange <- quantmod::Delt(x1 = df_sensex500_daily$Close, k = 1, type = "arithmetic")
df_sensex500_daily <- df_sensex500_daily[-1,]
write.csv(x = df_sensex500_daily, file = "SENSEX500_cleaned_data.csv")



####################### SENSEX SMALLCAP Data Prep #####################################
# Load SENSEX SMALLCAP data
df_smallcap_daily <- read.csv(file = "BSE_SmallCap_Hist_Data_Daily.csv")
df_smallcap_daily <- df_smallcap_daily %>% select(c("Date", "Close"))
df_smallcap_daily$Date <- as.Date(df_smallcap_daily$Date)
df_smallcap_daily$Close <- as.numeric(df_smallcap_daily$Close)
df_smallcap_daily <- na.omit(df_smallcap_daily)

# Select data from Jan-1991 to Feb-2021
df_smallcap_daily <- df_smallcap_daily %>% filter(Date > "1991-01-01")
df_smallcap_daily <- df_smallcap_daily %>% filter(Date < "2021-03-01")

# Daily Percent Change -- Volatility
df_smallcap_daily$pctChange <- quantmod::Delt(x1 = df_smallcap_daily$Close, k = 1, type = "arithmetic")
df_smallcap_daily <- df_smallcap_daily[-1,]
write.csv(x = df_smallcap_daily, file = "BSE_SmallCap_cleaned_data.csv")


####################### SENSEX MIDCAP Data Prep #####################################
# Load SENSEX MIDCAP data
df_midcap_daily <- read.csv(file = "BSE_MidCap_Hist_Data_Daily.csv")
df_midcap_daily <- df_midcap_daily %>% select(c("Date", "Close"))
df_midcap_daily$Date <- as.Date(df_midcap_daily$Date)
df_midcap_daily$Close <- as.numeric(df_midcap_daily$Close)
df_midcap_daily <- na.omit(df_midcap_daily)

# Select data from Jan-1991 to Feb-2021
df_midcap_daily <- df_midcap_daily %>% filter(Date > "1991-01-01")
df_midcap_daily <- df_midcap_daily %>% filter(Date < "2021-03-01")

# Daily Percent Change -- Volatility
df_midcap_daily$pctChange <- quantmod::Delt(x1 = df_midcap_daily$Close, k = 1, type = "arithmetic")
df_midcap_daily <- df_midcap_daily[-1,]
write.csv(x = df_midcap_daily, file = "BSE_MidCap_cleaned_data.csv")


####################### SENSEX LARGECAP Data Prep #####################################
# Load SENSEX LARGECAP data
df_largecap_daily <- read.csv(file = "BSE_LargeCap_Hist_Data_Daily.csv")
df_largecap_daily <- df_largecap_daily %>% select(c("Date", "Close"))
df_largecap_daily$Date <- as.Date(df_largecap_daily$Date)
df_largecap_daily$Close <- as.numeric(df_largecap_daily$Close)
df_largecap_daily <- na.omit(df_largecap_daily)

# Select data from Jan-1991 to Feb-2021
df_largecap_daily <- df_largecap_daily %>% filter(Date > "1991-01-01")
df_largecap_daily <- df_largecap_daily %>% filter(Date < "2021-03-01")

# Daily Percent Change -- Volatility
df_largecap_daily$pctChange <- quantmod::Delt(x1 = df_largecap_daily$Close, k = 1, type = "arithmetic")
df_largecap_daily <- df_largecap_daily[-1,]
write.csv(x = df_largecap_daily, file = "BSE_LargeCap_cleaned_data.csv")


###################### Exchange Rate Volatility (Percent Change) ##############
df_ex_pctChange <- df_INRUSD_daily
df_ex_pctChange$pctChange <- quantmod::Delt(x1 = df_ex_pctChange$DEXINUS, k = 1, type = "arithmetic")
df_ex_pctChange <- df_ex_pctChange[-1,]
write.csv(x = df_ex_pctChange, file = "INRUSD_daily_cleaned_data.csv")

# Plot the data
df_ex_pctChange_plot <- ggplot2::ggplot(data = df_ex_pctChange) +
                        ggplot2::geom_line(mapping = aes(x = DATE , y = pctChange)) +
                        ggplot2::labs(x = "Date", y = "Percent Change", title = "INR/USD Exchange Rate Daily Volatility")


df_ex_pctChange_plot + ggplot2::ggsave(filename = "INRUSD_daily_volatility.jpg")


####################### SENSEX Volatility (Percent Change) #####################
df_sensex_pctChange <- df_sensex_daily
df_sensex_pctChange$pctChange <- quantmod::Delt(x1 = df_sensex_pctChange$Close, k = 1, type = "arithmetic")
df_sensex_pctChange <- df_sensex_pctChange[-1,]
write.csv(x = df_sensex_pctChange, file = "SENSEX_daily_cleaned_data.csv")

# Plot the data
df_sensex_pctChange_plot <- ggplot2::ggplot(data = df_sensex_pctChange) +
                            ggplot2::geom_line(mapping = aes(x = Date, y = pctChange)) +
                            ggplot2::labs(x = "Date", y = "Percent Change", title = "SENSEX Daily Volatility")

df_sensex_pctChange_plot + ggplot2::ggsave(filename = "SENSEX_daily_volatility.jpg")





