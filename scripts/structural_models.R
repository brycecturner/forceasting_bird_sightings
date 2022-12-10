fitNile = StructTS(Nile, "level")
# level: local level model
# trend: local linear trend
# BSM: a local trend with an additional seasonal component



getSymbols("DCOILWTICO", src="FRED")  # WTI Crude Oil Prices
oilm = apply.monthly(DCOILWTICO, mean, na.rm=T) 
m = nrow(oilm)-3              # Hold out final 3 months as a testset
oil = ts(oilm[1:m], frequency=12, start=c(1986,1))
plot(oil)

fit01 = StructTS(oil, "level")
fit02 = StructTS(oil, "trend")

## Compare Test-set forecast errors
auto.arima(oil) %>% forecast(h=3)
forecast(fit01, h = 3) 
forecast(fit02, h = 3) 
tail(oilm)