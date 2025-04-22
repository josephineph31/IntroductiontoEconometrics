P <- read.csv ('Growth.csv')
x <- as.matrix(P$tradeshare)
y <- as.matrix(P$growth)
trade_share = c(P$tradeshare)
growth_rate = c(P$growth)
df <- data.frame (trade_share, growth_rate)
df
#Scatter plot for all countries
plot(x, y, main = "Growth Rate and Trade Share", xlab = "Trade Share", ylab = "Growth", pch = 21)
lines(lowess(x, y), col = "pink", lwd = 3)
A <- which(P$country_name == "Malta")
text(x[A], y[A], labels = "Malta", pos = 4, col = "blue")
model_1 = lm (growth_rate ~ trade_share)
#Prediction for growth rate based on trade share
summary(model_1)
predict(model_1, data.frame(trade_share = 0.5))
predict(model_1, data.frame(trade_share = 1))
#Exclude outliers such as Malta from the model
M <- P[-c(65),]
trade_share_1 = c(M$tradeshare)
growth_rate_1 = c(M$growth)
df1 <- data.frame (trade_share_1, growth_rate_1)
df1
plot(trade_share_1, growth_rate_1, main = "Growth Rate and Trade Share", xlab = "Trade Share", ylab = "Growth", pch = 21)
lines(lowess(x, y), col = "pink", lwd = 3)
model_2 = lm (growth_rate_1 ~ trade_share_1)
summary(model_2)
#Prediction for growth rate based on trade share, updated version
predict(model_2, data.frame(trade_share_1 = 0.5))
predict(model_2, data.frame(trade_share_1= 1))