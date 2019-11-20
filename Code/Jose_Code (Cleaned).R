library(ggplot2)
library(GGally)
library(Hmisc)
library(corrplot)
library(psych)
library(anomalize)
library(tidyverse)
library(gridExtra)
setwd("C:/Users/josea/Desktop/Stat 7331 Multivariate/Final Project")

## Loading Data
bit <- read.csv("Bitcoin.csv")
eth <- read.csv("Ethereum.csv")
gold <- read.csv("gold_prices.csv")
lite <- read.csv("Litecoin.csv")
sp <- read.csv("SP.csv")
oil <- read.csv("CrudeOil.csv")
dj <- read.csv("DowJones.csv")
epu <- read.csv("EPU.csv")
nas <- read.csv("Nasdaq.csv")
ny <- read.csv("NYSE.csv")
cny <- read.csv("USDCNY.csv")  ## China
eur <- read.csv("USDEUR.csv")  ## Euro
gbp <- read.csv("USDGBP.csv")  ## Pound
vxo <- read.csv("VXO.csv")

### Preprocessing Data Before Merging
# Getting Rid of Other Columns
bit2 <- bit[, c(1, 3)]
eth2 <- eth[, c(1, 3)]
gold2 <- gold[, c(1, 2)]
lite2 <- lite[, c(1, 3)]
sp2 <- sp[, c(1, 3)]
oil2 <- oil[, c(1, 2)]
dj2 <- dj[, c(1, 3)]
epu2 <- epu[, c(5, 4)]
nas2 <- nas[, c(1, 3)]
cny2 <- cny[, c(1, 4)]
ny2 <- ny[, c(1, 3)]
eur2 <- eur[, c(1, 4)]
gbp2 <- gbp[, c(1, 4)]
vxo2 <- vxo[, c(1, 2)]

# Renaming Columns Before Merge
names(bit2)[2] <- "Bitcoin"
names(eth2)[2] <- "Ethereum"
names(gold2)[2] <- "Gold"
names(lite2)[2] <- "Litecoin"
names(sp2)[2] <- "SP"
names(oil2)[2] <- "Oil"
names(dj2)[2] <- "Dow Jones"
names(epu2)[2] <- "EPU Index"
names(nas2)[2] <- "NASDAQ"
names(ny2)[2] <- "New York Stock Exchange"
names(cny2)[2] <- "Chinese Yuan"
names(eur2)[2] <- "Europe"
names(gbp2)[2] <- "Pound"
names(vxo2)[2] <- "Volatil Index"

# Merging Datasets into One Set
temp <- merge(bit2, eth2, all = T)
temp2 <- merge(temp, lite2, all = T)
temp3 <- merge(temp2, gold2, all = T)
temp4 <- merge(temp3, oil2, all = T)
temp5 <- merge(temp4, cny2, all = T)
temp6 <- merge(temp5, eur2, all = T)
temp7 <- merge(temp6, gbp2, all = T)
temp8 <- merge(temp7, sp2, all = T)
temp9 <- merge(temp8, dj2, all = T)
temp10 <- merge(temp9, epu2, all = T)
temp11 <- merge(temp10, nas2, all = T)
temp12 <- merge(temp11, ny2, all = T)
temp13 <- merge(temp12, vxo2, all = T)

# Ordering by Date
tmp <- temp13[order(as.Date(temp13$Date, format = "%m/%d/%Y")), ]

# Remove NA's
tmp <- na.omit(tmp)
dta <- tmp[, -1]
dta$`Volatil Index` <- as.numeric(dta$`Volatil Index`)
dim(dta)
head(dta)

# Plotting Cryptocurrencies, Fiat Currencies, Commodities
crypto <- dta[, 1:3]
fiat <- dta[, 6:8]
comm <- dta[, 4:5]

ggpairs(crypto)
ggpairs(fiat)
ggpairs(comm)

# Computing Correlation Matrix
res <- cor(dta)
round(dta, 2)
res2 <- rcorr(as.matrix(dta))
res2

corrplot(
  res,
  type = "upper",
  order = "original",
  tl.col = "black",
  tl.srt = 45
)
corrplot(
  res,
  type = "upper",
  order = "hclust",
  tl.col = "black",
  tl.srt = 45
)

# Factor Analysis
dta1 <- dta[, c(1, 2, 3, 6, 7, 8)]
dta1 <- as.data.frame(scale(dta1))
fa.parallel(dta1)

my.vss <- vss(dta1, n.obs = dim(dta1)[1])
my.vss
plot(my.vss)

FA1 <- factanal(dta1, 2, rotation = "varimax")

## Plotting Cryptocurrencies and Fiat Currencies over Time
# Fiat Currencies
dta1$Date <- tmp$Date
ggplot(data = dta1, aes(x = Date)) +
  geom_line(aes(y = Europe, colour = "Europe", group = 1), color = "blue") +
  geom_line(aes(y = Pound, colour = "Pound", group = 1), color = "red") +
  geom_line(aes(y = `Chinese Yuan`, colour = "Chinese", group = 1), color = "green") +
  xlab('Dates') +
  ylab('Currency Change') +
  theme(legend.position = "right") +
  scale_colour_manual(
    name = "Legend",
    values = c(
      "Europe" = "blue",
      "Pound" = "red",
      "Chinese" = "green"
    )
  )

ggplot(data = dta1, aes(x = Date)) +
  geom_line(aes(y = Litecoin, colour = "Lite", group = 1), color = "blue") +
  geom_line(aes(y = Bitcoin, colour = "Bit", group = 1), color = "red") +
  geom_line(aes(y = Ethereum, colour = "Eth", group = 1), color = "green") +
  xlab('Dates') +
  ylab('Currency Change') +
  theme(legend.position = "right") +
  scale_colour_manual(name = "Legend",
                      values = c(
                        "Lite" = "blue",
                        "Bit" = "red",
                        "Eth" = "green"
                      ))


ggplot(dta1, aes(x = Date, y = Europe, group = 1)) + geom_line(colour = "#00FF00", show.legend = T) + ylab("Value")  +
  geom_line(aes(x = Date, y = Pound),
            colour = "#000099",
            show.legend = T) +
  geom_line(aes(x = Date, y = `Chinese Yuan`),
            colour = "#FF0000",
            show.legend = T) +
  ggtitle("Fiat Currencies") +
  theme(legend.position = "right")

# Cryptocurrencies
ggplot(dta1, aes(x = Date, y = Bitcoin, group = 1)) + ylab("Value") +
  geom_line(aes(x = Date, y = Litecoin), colour = "#000099") +
  geom_line(aes(x = Date, y = Ethereum), colour = "#FF0000") +
  geom_line(aes(x = Date, y = Bitcoin), colour = "#00FF00") +
  ggtitle("Cryptocurrencies")

## Anomaly Detection
ggplot(df1, aes(
  x = Date,
  y = Bitcoin,
  color = Bitcoin,
  group = 1
)) + geom_line()

#Apply anomaly detection and plot the results
dta1$Date <- as.Date(dta1$Date, "%m/%d/%y")

df <- tbl_df(dta1)

#Bitcoin
df %>% time_decompose(Bitcoin,
                      method = "stl",
                      frequency = "auto",
                      trend = "auto") %>% anomalize(remainder,
                                                    method = "gesd",
                                                    alpha = 0.05,
                                                    max_anoms = 0.1) %>% plot_anomaly_decomposition()

#Removes Trend and Seasonality
p1 <- df %>% time_decompose(Bitcoin) %>% anomalize(remainder) %>% time_recompose() %>%  plot_anomalies(time_recomposed = TRUE,
                                                                                                 ncol = 3,
                                                                                                 alpha_dots = 0.5) + labs(title = "Bitcoin")

#Extract the anomalies
anomalies <-
  df %>% time_decompose(Bitcoin) %>%  anomalize(remainder) %>%  time_recompose() %>%  filter(anomaly == 'Yes')

###Litecoin
df %>% time_decompose(Litecoin,
                      method = "stl",
                      frequency = "auto",
                      trend = "auto") %>% anomalize(remainder,
                                                    method = "gesd",
                                                    alpha = 0.05,
                                                    max_anoms = 0.1) %>% plot_anomaly_decomposition()

#Removes Trend and Seasonality
p2 <- df %>% time_decompose(Litecoin) %>% anomalize(remainder) %>% time_recompose() %>%  plot_anomalies(time_recomposed = TRUE,
                                                                                                  ncol = 3,
                                                                                                  alpha_dots = 0.5) + labs(title = "Litecoin")

#Extract the anomalies
anomalies <-
  df %>% time_decompose(Litecoin) %>%  anomalize(remainder) %>%  time_recompose() %>%  filter(anomaly == 'Yes')

###Ethereum
df %>% time_decompose(Ethereum,
                      method = "stl",
                      frequency = "auto",
                      trend = "auto") %>% anomalize(remainder,
                                                    method = "gesd",
                                                    alpha = 0.05,
                                                    max_anoms = 0.1) %>% plot_anomaly_decomposition()

#Removes Trend and Seasonality
p3 <- df %>% time_decompose(Ethereum) %>% anomalize(remainder) %>% time_recompose() %>%  plot_anomalies(time_recomposed = TRUE,
                                                                                                  ncol = 3,
                                                                                                  alpha_dots = 0.5) + labs(title = "Ethereum")

#Extract the anomalies
anomalies <-
  df %>% time_decompose(Ethereum) %>%  anomalize(remainder) %>%  time_recompose() %>%  filter(anomaly == 'Yes')


###Pound
df %>% time_decompose(Pound,
                      method = "stl",
                      frequency = "auto",
                      trend = "auto") %>% anomalize(remainder,
                                                    method = "gesd",
                                                    alpha = 0.05,
                                                    max_anoms = 0.1) %>% plot_anomaly_decomposition()

#Removes Trend and Seasonality
p4 <- df %>% time_decompose(Pound) %>% anomalize(remainder) %>% time_recompose() %>%  plot_anomalies(time_recomposed = TRUE,
                                                                                               ncol = 3,
                                                                                               alpha_dots = 0.5) + labs(title = "Pound")

#Extract the anomalies
anomalies <-
  df %>% time_decompose(Pound) %>%  anomalize(remainder) %>%  time_recompose() %>%  filter(anomaly == 'Yes')


###Europe
df %>% time_decompose(Europe,
                      method = "stl",
                      frequency = "auto",
                      trend = "auto") %>% anomalize(remainder,
                                                    method = "gesd",
                                                    alpha = 0.05,
                                                    max_anoms = 0.1) %>% plot_anomaly_decomposition()

#Removes Trend and Seasonality
p5 <- df %>% time_decompose(Europe) %>% anomalize(remainder) %>% time_recompose() %>%  plot_anomalies(time_recomposed = TRUE,
                                                                                               ncol = 3,
                                                                                               alpha_dots = 0.5) + labs(title = "Europe Euro")

#Extract the anomalies
anomalies <-
  df %>% time_decompose(Europe) %>%  anomalize(remainder) %>%  time_recompose() %>%  filter(anomaly == 'Yes')


###Chinese Yuan
df %>% time_decompose(`Chinese Yuan`,
                      method = "stl",
                      frequency = "auto",
                      trend = "auto") %>% anomalize(remainder,
                                                    method = "gesd",
                                                    alpha = 0.05,
                                                    max_anoms = 0.1) %>% plot_anomaly_decomposition()

#Removes Trend and Seasonality
p6 <- df %>% time_decompose(`Chinese Yuan`) %>% anomalize(remainder) %>% time_recompose() %>%  plot_anomalies(time_recomposed = TRUE,                                                                                             ncol = 3,
                                                                                               alpha_dots = 0.5) + labs(title = "Chinese Yuan")

#Extract the anomalies
anomalies <-
  df %>% time_decompose(`Chinese Yuan`) %>%  anomalize(remainder) %>%  time_recompose() %>%  filter(anomaly == 'Yes')

grid.arrange(p1, p2, p3, nrow = 1)
grid.arrange(p4, p5, p6, nrow = 1)
