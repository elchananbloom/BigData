#noam semhoun 1721743
#elchanan bloom 207193855

library(lubridate)


pdf( file.path('Week3_power.pdf'))
Sys.setlocale(category = "LC_ALL", locale = "english")

table <- read.delim('table.tsv')
table$DateTime <- as.POSIXct(table$megawatthours, tz = "EST", "%H:%M EST %m/%d/%Y" )
summary(table)

table <- table[order(table$DateTime), ]


#range between 7-14 Feb.
rng <-  which ( B[, 'DateTime'] < as.POSIXct("2021-02-15 00:00:00 IST") &
                  B[, 'DateTime'] >= as.POSIXct("2021-02-08 00:00:00 IST" ) )



net.generation.table <- with(table, cbind( Net.generation.1 , Net.generation.2 , Net.generation.3, Net.generation.4 , Net.generation.5 , Net.generation.6,
                                           Net.generation.7 , Net.generation.8 , Net.generation.9   ))

net.generation.table <- rowSums(net.generation.table, na.rm = TRUE, dims = 1)

net.generation.table <- as.data.frame(net.generation.table)
colnames(net.generation.table) <- 'sum.net.gen'

net.generation.table$DateTime <- as.POSIXct(table$megawatthours, tz = "EST", "%H:%M EST %m/%d/%Y" )
net.generation.table <- net.generation.table[order(net.generation.table$DateTime), ]
net.generation.table <- net.generation.table[rng, ]


net.generation.table$DateTime <- as.Date(net.generation.table$DateTime, tz = "EST", "%m/%d/%Y")
net.generation.table.by.day <- aggregate(net.generation.table$sum.net.gen, as.list(as.data.frame(net.generation.table$DateTime)), function(x) sum(x, na.rm = TRUE))
colnames(net.generation.table.by.day) <- c('DateTime', 'sum.net.gen')

net.generation.table.by.day$sum.net.gen <- net.generation.table.by.day$sum.net.gen/1000000

plot(x = net.generation.table.by.day$DateTime, y = net.generation.table.by.day$sum.net.gen, type="l", xlab="Date", ylab="Net generation [in millions]")
M <- mean(net.generation.table.by.day$sum.net.gen)
abline(h= M, col="blue")


demand.table <- with(table, cbind(Demand.2, Demand.3 , Demand.6 , Demand.8, Demand.9))

demand.table <- as.data.frame(demand.table)



demand.table$DateTime <- as.POSIXct(table$megawatthours, tz = "EST", "%H:%M EST %m/%d/%Y" )
demand.table$DateTime <- sapply(demand.table$DateTime, function (x) hour(x))


demand.table <- demand.table[order(demand.table$DateTime),  ]

demand.table <- aggregate(demand.table, as.list(as.data.frame(demand.table$DateTime)), function(x) mean(x, na.rm = TRUE))



drop <- c("demand.table$DateTime", "DateTime")

demand.table <- demand.table[,!(names(demand.table) %in% drop)]

# mean values
M <- list( Demand.2 = NA, Demand.3 = NA, Demand.6 = NA, Demand.8 = NA, Demand.9 = NA )
# standard deviations
S <- list( Demand.2 = NA, Demand.3 = NA, Demand.6 = NA, Demand.8 = NA, Demand.9 = NA )
# linear fit
LM <- list( Demand.2 = NA, Demand.3 = NA, Demand.6 = NA, Demand.8 = NA, Demand.9 = NA)



# calculate means and stdev
demands <- seq(5)
for ( i in demands ) {
  M[[ i ]] <- mean( demand.table[ !is.na(demand.table[, i]) ,i ] )
  S[[ i ]] <- sd( demand.table[ !is.na(demand.table[, i]), i ] )
}

# scale and center the consumption series (normalize)
norm.demand.table <- t((t(demand.table) - unlist(M)) / unlist(S) )



demand.by.time <- function (start, end, norm.demand.table){
  start.index <- start + 1
  end.index <- end + 1
  
  length <- abs(end.index-start.index) + 1
  
  norm.demand.table.by.time <- norm.demand.table[start.index:end.index,]
  
  if (end < start){
    length <- 24 - abs(end.index-start.index) + 1
    end.index <-  length - (24 - start)
    norm.demand.table.by.time <- rbind(norm.demand.table[start.index:24,], norm.demand.table[0:end.index,])
  }
  
  plot(1, type="n", xlab="", ylab="", xlim = c(0, length), ylim=c(min(norm.demand.table.by.time), max(norm.demand.table.by.time)))
  
  
  for ( i in demands ) {
    # rearrange in a new, temporary dataframe
    DF <- data.frame ( Time = seq(length), Demand = norm.demand.table.by.time[ , i ] )
    # plot
    lines( DF, col = i, type = 'b' )
    # linear fit
    LM[[ i ]] <- lm( Demand ~ Time, data = DF)
    a <- coef(LM[[ i ]])[1]
    b <- coef(LM[[ i ]])[2]
    abline(a, b, col = i, lw =2)
  }
  
  # mean regression line
  lm.df <- sapply(LM[ -c(1,4,5,7,10) ], function(c) coef(c))
  n <- dim(lm.df)[2]
  tot <- rowSums(lm.df)
  a <- tot[1] / n
  b <- tot[2] / n
  
  # plot the mean regression line
  abline(a, b, col = 'black', lw = 4, lt = 2)
  
  title(paste0("Normalized demand over " , start, '-', end) )
  
}


demand.by.time(10, 18, norm.demand.table)
demand.by.time(20, 3, norm.demand.table)


dev.off()