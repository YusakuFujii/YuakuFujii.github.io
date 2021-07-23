#  Data cleaning for time series analysis in R

### Loading and cleaning data

JP <- JPN_right

UK <- UK_right

US <- USA_right2

### remove NA

JP <- na.omit(JP)

UK <- na.omit(UK)

US <- na.omit(US)

### convert to time series

UK <- as.xts(read.zoo(UK))

US <- as.xts(read.zoo(US))

JP <- as.xts(read.zoo(JP))

### check the data

sum(index(JP) != index(UK)) 

sum(index(JP) != index(US))

length(JP)
length(US)
length(UK)

### conbine each data

MSCI <- cbind(JP, UK, US) 

### count the number of NA

sum(is.na(MSCI)) 


