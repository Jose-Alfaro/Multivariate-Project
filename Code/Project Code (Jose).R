library(ggplot2)
library(GGally)
library(Hmisc)
library(corrplot)
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
tmp <- temp13[order(as.Date(temp13$Date, format = "%m/%d/%Y")),]

# Remove NA's
x <- na.omit(tmp)
x <- x[,-1]
x$`Volatil Index` <- as.numeric(x$`Volatil Index`)
dim(x)
head(x)

# Plotting Cryptocurrencies, Fiat Currencies, Commodities
crypto <- x[, 1:3]
fiat <- x[, 6:8]
comm <- x[, 4:5]

ggpairs(crypto)
ggpairs(fiat)
ggpairs(comm)

# Computing Correlation Matrix
res <- cor(x)
round(res, 2)
res2 <- rcorr(as.matrix(x))
res2

corrplot(res, type = "upper", order = "original", tl.col = "black", tl.srt = 45)
corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)


# K-Means Clustering
set.seed(3983)
km <- kmeans(x, centers = 3, nstart = 50)
fviz_cluster(
  km,
  data = xv,
  ellipse.type = "convex",
  palette = "jco",
  ggtheme = theme_minimal()
)

## Gets Current Crypto Currency Prices
crypto_prices <- function(coin = NULL, limit = 0, currency = NULL) {
  options(scipen = 999)
  url <- "https://api.coinmarketcap.com/v1/ticker/"
  if (is.null(coin)) {
    url <- paste0(url, "?limit=", limit)
  }
  if (!is.null(coin)) {
    url <- paste0(url, coin, "/")
  }
  if (!is.null(currency)) {
    currency <- toupper(currency)
    
    if (is.null(coin)) {
      url <- paste0(url, "&convert=", currency)
    }
    if (!is.null(coin)) {
      url <- paste0(url, "?convert=", currency)
    }
  }
  prices                  <- jsonlite::read_json(url, simplifyVector = TRUE)
  prices$market_cap_usd   <- prices$market_cap_usd %>% tidyr::replace_na(0) %>% as.numeric()
  prices$available_supply <- prices$available_supply %>% tidyr::replace_na(0) %>% as.numeric()
  prices$max_supply       <- prices$max_supply %>% tidyr::replace_na(0) %>% as.numeric()
  prices$total_supply     <- prices$total_supply %>% tidyr::replace_na(0) %>% as.numeric()
  cols <- c(4:14)
  prices[, cols] <- apply(prices[, cols], 2, function(x) replace(x, is.na(x), 0))
  prices[, cols] <- suppressWarnings(apply(prices[, cols], 2, function(x) as.numeric(x)))
  prices[, 15]   <- as.POSIXct(as.numeric(prices[, 15]), origin = "1970-01-01")
  if (!is.null(currency)) {
    concols <- c(16:18)
    prices[, concols] <- suppressWarnings(apply(prices[, concols], 2, function(x) as.numeric(x)))
  }
  return(prices)
}

crypto_timeseries <- function(coin = NULL) {
  if (is.null(coin)) {
    coin <- "bitcoin"
  }
  json <- "https://s2.coinmarketcap.com/generated/search/quick_search.json"
  coins <- jsonlite::read_json(json, simplifyVector = TRUE)
  if (!is.null(coin)) {
    name <- coins$name
    slug <- coins$slug
    symbol <- coins$symbol
    c1 <- subset(coins, toupper(name) %in% toupper(coin))
    c2 <- subset(coins, symbol %in% toupper(coin))
    c3 <- subset(coins, slug %in% tolower(coin))
    coins <- tibble::tibble()
    if (nrow(c1) > 0) {
      coins <- rbind(coins, c1)
    }
    if (nrow(c2) > 0) {
      coins <- rbind(coins, c2)
    }
    if (nrow(c3) > 0) {
      coins <- rbind(coins, c3)
    }
    if (nrow(coins) > 1L) {
      coins <- unique(coins)
    }
  }
  slug <- coins$slug %>% as.character()
  url <- paste0("https://graphs2.coinmarketcap.com/currencies/", slug)
  df <- jsonlite::fromJSON(url, flatten = TRUE)
  if (length(df) >= 5L) {
    df$price_platform <- NULL
  }
  df <- as.data.frame(df)
  df[, c(3, 5, 7)] <- NULL
  names(df) <- c("timestamp", "market_cap", "price_btc", "price_usd", "volume")
  df$timestamp <- as.POSIXct(as.numeric(df$timestamp)/1000, origin = "1970-01-01", 
                             tz = "UTC")
  df$slug <- slug
  market_data <- df %>% as.data.frame()
  return(market_data)
}

scraper <- function(attributes, slug, sleep = NULL) {
  .            <- "."
  history_url  <- as.character(attributes)
  coin_slug    <- as.character(slug)
  if (!is.null(sleep)) Sys.sleep(sleep)
  
  page <- tryCatch(
    xml2::read_html(history_url,
                    handle = curl::new_handle("useragent" = "Mozilla/5.0")),
    error = function(e) e)
  
  if (inherits(page, "error")) {
    closeAllConnections()
    message("\n")
    message(cli::cat_bullet("Rate limit hit. Sleeping for 60 seconds.", bullet = "warning", bullet_col = "red"), appendLF = TRUE)
    Sys.sleep(65)
    page <- xml2::read_html(history_url,
                            handle = curl::new_handle("useragent" = "Mozilla/5.0"))
  }
  
  table   <- rvest::html_nodes(page, css = "table") %>% .[1] %>%
    rvest::html_table(fill = TRUE) %>%
    replace(!nzchar(.), NA)
  
  scraper <- table[[1]] %>% tibble::as.tibble() %>%
    dplyr::mutate(slug = coin_slug)
  
  return(scraper)
}

crypto_global_market <- function(market = NULL) {
  if (is.null(market)) {
    market <- "total"
  }
  
  if (market != "total") {
    if (market != "altcoin") {
      message("Valid options are 'total' or 'altcoin'.", appendLF = TRUE)
      market <- "total"
    }
  }
  
  url <- paste0("https://graphs2.coinmarketcap.com/global/marketcap-", market)
  df <- jsonlite::fromJSON(url, flatten = TRUE) %>% as.data.frame()
  df[, 3] <- NULL
  names(df) <- paste0(market, "_", c("timestamp", "market_cap", "volume"))
  df[, 1] <- as.POSIXct(as.numeric(df[, 1]) / 1000, origin = "1970-01-01", tz = "UTC")
  crypto_global_markets <- df %>% as.data.frame()
  return(crypto_global_markets)
}
