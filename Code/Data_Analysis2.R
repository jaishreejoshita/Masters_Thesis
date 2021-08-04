# Load libraries
library(dplyr)
library(ggplot2)
library(quantmod)
library(reshape2)
library(tseries)
library(Hmisc)
library(urca)
library(stats)
library(lmtest)
# Set working directory

setwd('/Users/rohitkumawat/Desktop/Jaishree_Thesis_MA')


# Read data
df_ex_daily <- read.csv(file = "INRUSD_daily_cleaned_data.csv")
df_sensex_daily <- read.csv(file = "SENSEX_daily_cleaned_data.csv")

df_ex_daily <- df_ex_daily[,-1]
df_sensex_daily <- df_sensex_daily[,-1]
colnames(df_ex_daily) <- c("Date", "INRUSD", "pctChange_EX")
colnames(df_sensex_daily) <- c("Date", "Close", "pctChange_SENX")

############################## Volatility Comparison ##########################
# Merging the two data frame
df_ex_sensex <- merge(x = df_ex_daily, y = df_sensex_daily, by = "Date")
df_ex_sensex <- na.omit(df_ex_sensex)


# Plot volatility comparison
df_volComp_plot <- df_ex_sensex %>% select(c("Date", "pctChange_EX", "pctChange_SENX"))
df_volComp_plot_melt <- reshape2::melt(data = df_volComp_plot, id.vars = c("Date"))
df_volComp_plot_melt$Date <- as.Date(df_volComp_plot_melt$Date)

volComp_plot <- ggplot2::ggplot(data = df_volComp_plot_melt) +
                ggplot2::geom_line(mapping = aes(x = Date, y = value, color = variable, group = variable)) +
                ggplot2::labs(x = "Date", y = "Percent Change", title = "Volatility comparison of SENSEX and INR/USD ex-rate") +
                ggplot2::scale_color_manual(labels = c("INR/USD ex-rate volatility", "SENSEX volatility"), values = c("brown","black")) +
                ggplot2::theme(legend.position = "bottom")
                

volComp_plot + ggplot2::ggsave(filename = "Daily_Volatility_Comparison_SENSEX_EX.jpg")



############################ Dividing data into four phases #####################
# Phase 1 --> 1991 to 2006
# Phase 2 --> 2007 to 2010
# Phase 3 --> 2011 to 2019
# Phase 4 --> 2020 to 2021
df_ex_sensex$Date <- as.Date(df_ex_sensex$Date)

df_ex_sensex_p1 <- df_ex_sensex %>% filter(Date <= '2006-12-31')
df_ex_sensex_p2 <- df_ex_sensex %>% filter(Date > '2006-12-31' & Date <= '2010-12-31')
df_ex_sensex_p3 <- df_ex_sensex %>% filter(Date > '2010-12-31' & Date <= '2019-12-31')
df_ex_sensex_p4 <- df_ex_sensex %>% filter(Date > '2019-12-31')

# Granger Causality Test for all data set
grangertest(pctChange_SENX ~ pctChange_EX, data = df_ex_sensex)
grangertest(pctChange_EX ~ pctChange_SENX, data = df_ex_sensex)



############################# Phase 1 (1991 - 2006) ###############################
# Summary statistics for each column
summaryStats_p1 <- sapply(df_ex_sensex_p1, summary)
summaryStats_p1 <- as.data.frame(summaryStats_p1)
summaryStats_p1 <- rbind(summaryStats_p1, c(NA, sd(summaryStats_p1$INRUSD), sd(summaryStats_p1$pctChange_EX), 
                                            sd(summaryStats_p1$Close), sd(summaryStats_p1$pctChange_SENX)))
write.csv(x = summaryStats_p1, file = "Summary_Statistics_P1.csv")

# Combined Plot for SENSEX and Ex-Rate
df_ex_sensex_p1_plot <- ggplot2::ggplot(data = df_ex_sensex_p1, mapping = aes(x = Date)) +
                        ggplot2::geom_line(mapping = aes(y = INRUSD, colour = "INR/USD Ex-Rate")) +
                        ggplot2::geom_line(mapping = aes(y = Close/250, colour = "SENSEX")) +
                        ggplot2::scale_y_continuous(sec.axis = sec_axis(~.*250, name = "SENSEX daily close"))+
                        ggplot2::labs(y = "INR/USD daily ex-rate", title = "SENSEX and Ex-Rate plot for Phase 1 (1991-2006)",colour = "Parameter")+
                        ggplot2::theme(legend.position = "bottom")
df_ex_sensex_p1_plot + ggplot2::ggsave(filename = "Ex_SENSEX_daily_plot_p1.jpg")

# Ex-Rate plot for P1
df_ex_p1_plot <- ggplot2::ggplot(data = df_ex_sensex_p1) +
                 ggplot2::geom_line(mapping = aes(x = Date, y = INRUSD))+
                 ggplot2::ylim(0,80) +
                 ggplot2::labs(x = "Date", y = "INR/USD", title = "INR/USD daily exchange rate (1991-2006)")
df_ex_p1_plot + ggplot2::ggsave(filename = "INRUSD_daily_exchange_rate_plot_p1.jpg")

# SENSEX plot for P1
df_sensex_p1_plot <- ggplot2::ggplot(data = df_ex_sensex_p1) +
                     ggplot2::geom_line(mapping = aes(x = Date, y = Close)) +
                     ggplot2::ylim(0, 15000) +
                     ggplot2::labs(x = "Date", y = "SENSEX Close", title = "SENSEX daily close value (1991-2006)")
                  
df_sensex_p1_plot + ggplot2::ggsave(filename = "SENSEX_daily_close_plot_p1.jpg")


# Granger Causality Test
grangertest(pctChange_SENX ~ pctChange_EX, data = df_ex_sensex_p1)
grangertest(pctChange_EX ~ pctChange_SENX, data = df_ex_sensex_p1)


# Regression for P1
regVol_p1 <- lm(formula = pctChange_SENX ~ pctChange_EX, data = df_ex_sensex_p1)
summary(regVol_p1)

regVol_p1_alt1 <- lm(formula = pctChange_EX ~ pctChange_SENX, data = df_ex_sensex_p1)
summary(regVol_p1_alt1)

df_ex_sensex_p1_alt1 <- data.frame(pctChange_SENX = df_ex_sensex_p1$pctChange_SENX)
ex_lag_p1 <- lag(x = df_ex_sensex_p1$pctChange_EX, n = 1)
df_ex_sensex_p1_alt1 <- cbind(df_ex_sensex_p1_alt1, ex_lag_p1)
regVol_p1_alt2 <- lm(formula = pctChange_SENX ~ ex_lag_p1, data = df_ex_sensex_p1_alt1)
summary(regVol_p1_alt2)

### Time Series tests
df_tsTest_p1 <- data.frame(Test = c(NA), Statistic = c(NA), PValue = c(NA), Result = c(NA), row.names = NULL)

# Test for Stationary
adfTest_ex_p1 <- adf.test(x = df_ex_sensex_p1$pctChange_EX)
df_tsTest_p1 <- rbind(df_tsTest_p1, c("ADF_Test_Ex", adfTest_ex_p1$statistic, adfTest_ex_p1$p.value, "Stationary"))

adfTest_sensex_p1 <- adf.test(x = df_ex_sensex_p1$pctChange_SENX)
df_tsTest_p1 <- rbind(df_tsTest_p1, c("ADF_Test_Sensex", adfTest_sensex_p1$statistic, adfTest_sensex_p1$p.value, "Stationary"))
 
df_tsTest_p1 <- df_tsTest_p1[-1,]
write.csv(x = df_tsTest_p1, file = "Regression_Test_Results_p1.csv")

# Regression Plot
regVolPlot_p1 <- ggplot2::ggplot(data = df_ex_sensex_p1, mapping = aes(x = pctChange_EX, y = pctChange_SENX)) +
                 ggplot2::geom_point(color = 'blue') +
                 ggplot2::geom_smooth(method = 'lm', color = 'red', se = FALSE) +
                 ggplot2::labs(x = "Ex-Rate Volatility", y = "SENSEX Volatility", title = "Regression Plot for Phase 1 (1991-2006)")
regVolPlot_p1 + ggplot2::ggsave(filename = "Regression_plot_p1.jpg")


#########################  Phase 2 (2007 - 2010) ############################
# Summary statistics for each column
summaryStats_p2 <- sapply(df_ex_sensex_p2, summary)
summaryStats_p2 <- as.data.frame(summaryStats_p2)
summaryStats_p2 <- rbind(summaryStats_p2, c(NA, sd(summaryStats_p2$INRUSD), sd(summaryStats_p2$pctChange_EX), 
                                            sd(summaryStats_p2$Close), sd(summaryStats_p2$pctChange_SENX)))
write.csv(x = summaryStats_p2, file = "Summary_Statistics_P2.csv")

# Combined Plot for SENSEX and Ex-Rate
df_ex_sensex_p2_plot <- ggplot2::ggplot(data = df_ex_sensex_p2, mapping = aes(x = Date)) +
  ggplot2::geom_line(mapping = aes(y = INRUSD, colour = "INR/USD Ex-Rate")) +
  ggplot2::geom_line(mapping = aes(y = Close/500, colour = "SENSEX")) +
  ggplot2::scale_y_continuous(sec.axis = sec_axis(~.*500, name = "SENSEX daily close"))+
  ggplot2::labs(y = "INR/USD daily ex-rate", title = "SENSEX and Ex-Rate plot for Phase 2 (2007-2010)",colour = "Parameter")+
  ggplot2::theme(legend.position = "bottom")
df_ex_sensex_p2_plot + ggplot2::ggsave(filename = "Ex_SENSEX_daily_plot_p2.jpg")

# Ex-Rate plot for P2
df_ex_p2_plot <- ggplot2::ggplot(data = df_ex_sensex_p2) +
  ggplot2::geom_line(mapping = aes(x = Date, y = INRUSD))+
  ggplot2::ylim(0,80) +
  ggplot2::labs(x = "Date", y = "INR/USD", title = "INR/USD daily exchange rate (2007-2010)")
df_ex_p2_plot + ggplot2::ggsave(filename = "INRUSD_daily_exchange_rate_plot_p2.jpg")

# SENSEX plot for P2
df_sensex_p2_plot <- ggplot2::ggplot(data = df_ex_sensex_p2) +
  ggplot2::geom_line(mapping = aes(x = Date, y = Close)) +
  ggplot2::ylim(0, 25000) +
  ggplot2::labs(x = "Date", y = "SENSEX Close", title = "SENSEX daily close value (2007-2010)")

df_sensex_p2_plot + ggplot2::ggsave(filename = "SENSEX_daily_close_plot_p2.jpg")


# Granger Causality Test for P2
grangertest(pctChange_SENX ~ pctChange_EX, data = df_ex_sensex_p2)
grangertest(pctChange_EX ~ pctChange_SENX, data = df_ex_sensex_p2)



# Regression for P2
regVol_p2 <- lm(formula = pctChange_SENX ~ pctChange_EX, data = df_ex_sensex_p2)
summary(regVol_p2)

### Time Series tests
df_tsTest_p2 <- data.frame(Test = c(NA), Statistic = c(NA), PValue = c(NA), Result = c(NA), row.names = NULL)

# Test for Stationary
adfTest_ex_p2 <- adf.test(x = df_ex_sensex_p2$pctChange_EX)
df_tsTest_p2 <- rbind(df_tsTest_p2, c("ADF_Test_Ex", adfTest_ex_p2$statistic, adfTest_ex_p2$p.value, "Stationary"))

adfTest_sensex_p2 <- adf.test(x = df_ex_sensex_p2$pctChange_SENX)
df_tsTest_p2 <- rbind(df_tsTest_p2, c("ADF_Test_Sensex", adfTest_sensex_p2$statistic, adfTest_sensex_p2$p.value, "Stationary"))

df_tsTest_p2 <- df_tsTest_p2[-1,]
write.csv(x = df_tsTest_p2, file = "Regression_Test_Results_p2.csv")



# Regression Plot
regVolPlot_p2 <- ggplot2::ggplot(data = df_ex_sensex_p2, mapping = aes(x = pctChange_EX, y = pctChange_SENX)) +
  ggplot2::geom_point(color = 'blue') +
  ggplot2::geom_smooth(method = 'lm', color = 'red', se = FALSE) +
  ggplot2::labs(x = "Ex-Rate Volatility", y = "SENSEX Volatility", title = "Regression Plot for Phase 2 (2007-2010)")
regVolPlot_p2 + ggplot2::ggsave(filename = "Regression_plot_p2.jpg")


############################## Phase 3 (2011 - 2019) ################################
# Summary statistics for each column
summaryStats_p3 <- sapply(df_ex_sensex_p3, summary)
summaryStats_p3 <- as.data.frame(summaryStats_p3)
summaryStats_p3 <- rbind(summaryStats_p3, c(NA, sd(summaryStats_p3$INRUSD), sd(summaryStats_p3$pctChange_EX), 
                                            sd(summaryStats_p3$Close), sd(summaryStats_p3$pctChange_SENX)))
write.csv(x = summaryStats_p3, file = "Summary_Statistics_P3.csv")

# Combined Plot for SENSEX and Ex-Rate
df_ex_sensex_p3_plot <- ggplot2::ggplot(data = df_ex_sensex_p3, mapping = aes(x = Date)) +
  ggplot2::geom_line(mapping = aes(y = INRUSD, colour = "INR/USD Ex-Rate")) +
  ggplot2::geom_line(mapping = aes(y = Close/500, colour = "SENSEX")) +
  ggplot2::scale_y_continuous(sec.axis = sec_axis(~.*500, name = "SENSEX daily close"))+
  ggplot2::labs(y = "INR/USD daily ex-rate", title = "SENSEX and Ex-Rate plot for Phase 3 (2011-2019)",colour = "Parameter")+
  ggplot2::theme(legend.position = "bottom")
df_ex_sensex_p3_plot + ggplot2::ggsave(filename = "Ex_SENSEX_daily_plot_p3.jpg")

# Ex-Rate plot for P3
df_ex_p3_plot <- ggplot2::ggplot(data = df_ex_sensex_p3) +
  ggplot2::geom_line(mapping = aes(x = Date, y = INRUSD))+
  ggplot2::ylim(0,80) +
  ggplot2::labs(x = "Date", y = "INR/USD", title = "INR/USD daily exchange rate (2011-2019)")
df_ex_p3_plot + ggplot2::ggsave(filename = "INRUSD_daily_exchange_rate_plot_p3.jpg")

# SENSEX plot for P1
df_sensex_p3_plot <- ggplot2::ggplot(data = df_ex_sensex_p3) +
  ggplot2::geom_line(mapping = aes(x = Date, y = Close)) +
  ggplot2::ylim(0, 50000) +
  ggplot2::labs(x = "Date", y = "SENSEX Close", title = "SENSEX daily close value (2011-2019)")

df_sensex_p3_plot + ggplot2::ggsave(filename = "SENSEX_daily_close_plot_p3.jpg")


# Granger Causality Test for P3
grangertest(pctChange_SENX ~ pctChange_EX, data = df_ex_sensex_p2)
grangertest(pctChange_EX ~ pctChange_SENX, data = df_ex_sensex_p2)


# Regression for P3
regVol_p3 <- lm(formula = pctChange_SENX ~ pctChange_EX, data = df_ex_sensex_p3)
summary(regVol_p3)

### Time Series tests
df_tsTest_p3 <- data.frame(Test = c(NA), Statistic = c(NA), PValue = c(NA), Result = c(NA), row.names = NULL)

# Test for Stationary
adfTest_ex_p3 <- adf.test(x = df_ex_sensex_p3$pctChange_EX)
df_tsTest_p3 <- rbind(df_tsTest_p3, c("ADF_Test_Ex", adfTest_ex_p3$statistic, adfTest_ex_p3$p.value, "Stationary"))

adfTest_sensex_p3 <- adf.test(x = df_ex_sensex_p3$pctChange_SENX)
df_tsTest_p3 <- rbind(df_tsTest_p3, c("ADF_Test_Sensex", adfTest_sensex_p3$statistic, adfTest_sensex_p3$p.value, "Stationary"))

df_tsTest_p3 <- df_tsTest_p3[-1,]
write.csv(x = df_tsTest_p3, file = "Regression_Test_Results_p3.csv")





# Regression Plot
regVolPlot_p3 <- ggplot2::ggplot(data = df_ex_sensex_p3, mapping = aes(x = pctChange_EX, y = pctChange_SENX)) +
  ggplot2::geom_point(color = 'blue') +
  ggplot2::geom_smooth(method = 'lm', color = 'red', se = FALSE) +
  ggplot2::labs(x = "Ex-Rate Volatility", y = "SENSEX Volatility", title = "Regression Plot for Phase 3 (2011-2019)")
regVolPlot_p3 + ggplot2::ggsave(filename = "Regression_plot_p3.jpg")


############################ Phase 4 (2020 - 2021) ##################################
# Summary statistics for each column
summaryStats_p4 <- sapply(df_ex_sensex_p4, summary)
summaryStats_p4 <- as.data.frame(summaryStats_p4)
summaryStats_p4 <- rbind(summaryStats_p4, c(NA, sd(summaryStats_p4$INRUSD), sd(summaryStats_p4$pctChange_EX), 
                                            sd(summaryStats_p4$Close), sd(summaryStats_p4$pctChange_SENX)))
write.csv(x = summaryStats_p4, file = "Summary_Statistics_P4.csv")

# Combined Plot for SENSEX and Ex-Rate
df_ex_sensex_p4_plot <- ggplot2::ggplot(data = df_ex_sensex_p4, mapping = aes(x = Date)) +
  ggplot2::geom_line(mapping = aes(y = INRUSD, colour = "INR/USD Ex-Rate")) +
  ggplot2::geom_line(mapping = aes(y = Close/500, colour = "SENSEX")) +
  ggplot2::scale_y_continuous(sec.axis = sec_axis(~.*500, name = "SENSEX daily close"))+
  ggplot2::labs(y = "INR/USD daily ex-rate", title = "SENSEX and Ex-Rate plot for Phase 4 (2020-2021)",colour = "Parameter")+
  ggplot2::theme(legend.position = "bottom")
df_ex_sensex_p4_plot + ggplot2::ggsave(filename = "Ex_SENSEX_daily_plot_p4.jpg")

# Ex-Rate plot for P4
df_ex_p4_plot <- ggplot2::ggplot(data = df_ex_sensex_p4) +
  ggplot2::geom_line(mapping = aes(x = Date, y = INRUSD))+
  ggplot2::ylim(0,80) +
  ggplot2::labs(x = "Date", y = "INR/USD", title = "INR/USD daily exchange rate (2020-2021)")
df_ex_p4_plot + ggplot2::ggsave(filename = "INRUSD_daily_exchange_rate_plot_p4.jpg")

# SENSEX plot for P1
df_sensex_p4_plot <- ggplot2::ggplot(data = df_ex_sensex_p4) +
  ggplot2::geom_line(mapping = aes(x = Date, y = Close)) +
  ggplot2::ylim(0, 70000) +
  ggplot2::labs(x = "Date", y = "SENSEX Close", title = "SENSEX daily close value (2020-2021)")

df_sensex_p4_plot + ggplot2::ggsave(filename = "SENSEX_daily_close_plot_p4.jpg")


#Granger Causality Test for P4
grangertest(pctChange_SENX ~ pctChange_EX, data = df_ex_sensex_p4)
grangertest(pctChange_EX ~ pctChange_SENX, data = df_ex_sensex_p4)


# Regression for P4
regVol_p4 <- lm(formula = pctChange_SENX ~ pctChange_EX, data = df_ex_sensex_p4)
summary(regVol_p4)

### Time Series tests
df_tsTest_p4 <- data.frame(Test = c(NA), Statistic = c(NA), PValue = c(NA), Result = c(NA), row.names = NULL)

# Test for Stationary
adfTest_ex_p4 <- adf.test(x = df_ex_sensex_p4$pctChange_EX)
df_tsTest_p4 <- rbind(df_tsTest_p4, c("ADF_Test_Ex", adfTest_ex_p4$statistic, adfTest_ex_p4$p.value, "Stationary"))

adfTest_sensex_p4 <- adf.test(x = df_ex_sensex_p4$pctChange_SENX)
df_tsTest_p4 <- rbind(df_tsTest_p4, c("ADF_Test_Sensex", adfTest_sensex_p4$statistic, adfTest_sensex_p4$p.value, "Stationary"))

df_tsTest_p4 <- df_tsTest_p4[-1,]
write.csv(x = df_tsTest_p4, file = "Regression_Test_Results_p4.csv")




# Regression Plot
regVolPlot_p4 <- ggplot2::ggplot(data = df_ex_sensex_p4, mapping = aes(x = pctChange_EX, y = pctChange_SENX)) +
  ggplot2::geom_point(color = 'blue') +
  ggplot2::geom_smooth(method = 'lm', color = 'red', se = FALSE) +
  ggplot2::labs(x = "Ex-Rate Volatility", y = "SENSEX Volatility", title = "Regression Plot for Phase 4 (2020-2021)")
regVolPlot_p4 + ggplot2::ggsave(filename = "Regression_plot_p4.jpg")





