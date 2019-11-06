setwd("C:/Users/JFA170001/Desktop/Stat 7331/Final Project")

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
# vxo$Date <- as.Date(vxo$Date)
# vxo$VXOCLS <- as.numeric(vxo$VXOCLS)

### Preprocessing Data Before Merging
#   Getting Rid of Other Columns
bit2 <- bit[, c(1, 3)]
eth2 <- eth[, c(1, 3)]
gold2 <- gold[, c(1, 2)]
lite2 <- lite[, c(1, 3)]
sp2 <- sp[, c(1, 3)]
oil2 <- oil[, c(1,2)]
dj2 <- dj[, c(1,3)]
epu2 <- epu[, c(5, 4)]
nas2 <- nas[, c(1, 3)]
cny2 <- cny[, c(1, 4)]
ny2 <- ny[, c(1, 3)]
eur2 <- eur[, c(1, 4)]
gbp2 <- gbp[, c(1, 4)]
vxo2 <- vxo[, c(1, 2)]


#   Renaming Columns Before Merge
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


#   Merging Datasets into One Set
temp <- merge(bit2, eth2, all = TRUE)
temp2 <- merge(temp, gold2, all = TRUE)
temp3 <- merge(temp2, lite2, all = TRUE)
temp4 <- merge(temp3, sp2, all = TRUE)
temp5 <- merge(temp4, dj2, all = T)
temp6 <- merge(temp5, epu2, all = T)
temp7 <- merge(temp6, nas2, all = T)
temp8 <- merge(temp7, cny2, all = T)
temp9 <- merge(temp8, ny2, all = T)
temp10 <- merge(temp9, eur2, all = T)
temp11 <- merge(temp10, gbp2, all = T)
temp12 <- merge(temp11, vxo2, all = T)
temp13 <- merge(temp12, oil2, all = T)

#   Ordering by Date
tmp <- temp13[order(as.Date(temp13$Date, format = "%m/%d/%Y")),]

# Remove NA's
x <- na.omit(tmp)
x <- x[,-1]
dim(x)
head(x)
