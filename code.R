f <- file('D:/Courses/data file/GG Data File/data.log') 
f <- gsub('[[:punct:]]', '', readLines(f))
writeLines(f, 'D:/Courses/data file/GG Data File/clean.txt')
 
data <- read.table('D:/Courses/data file/GG Data File/clean.txt')
 
columns <- c(2, 3, 11, 12,13,14,17,18,19,20,21)
data <- data[columns]
 
n <- length(data[, 8])

 data[,8] <- sapply(data[1:n, 8], function(x) paste(substr(x, 1,4), substr(x, 5,6), substr(x, 7,8)))
 data[,8] <- sapply(data[, 8], function(x) as.Date(x, format = '%Y%m%d',origin = '1970-01-01' ))
 data[,9] <- sapply(data[, 9], function(x) paste0('0',substr(x, 1,1),':' ,substr(x, 2,3), ':',substr(x, 4,5),'.', substr(x,6,11) ))
 data[,9] <- sapply(data[,9], function(x) as.POSIXct(x, format = '%H:%M:%S'))* 1000
 
 data <- data[
         with (data, order(data[,2])), ]
 
 write.csv(data, 'D:/Courses/data file/GG Data File/clean.csv')
 a <- 1
 index <- list()
 for (fac in myfac){
         for (i in 1:n){
              if (f[i,2] == fac & f[i,4] == 'ggstop' & f[i+1,4] == 'ggstop'){
                      index[a] <- i+1
                      a <- a+1
                        }
                 
                else if (f[i,2] == fac & f[i,4] == 'ggstart' & f[i+1, 4] == 'ggstart'){
                        index[a] <- i
                        a <- a+1
                        }
         }
 }
                 
 data <- data[-index,]
 l <- 1:(length(data[,1]) - 1):2
 for (fac in myfac){
         temp <- data$V3 == fac
         p <- length(temp)
         q <- 1
         for (i in 1:p){
                 if (temp[i,4] == 'ggstop' & (temp[i+1] - temp[i]) <= 30 ){
                         session$fac[q] <- session[q] + (temp[i] - temp[i-1])/1000
                 }
                else if (temp[i, 4] == 'ggstop' & (temp[i+1] - temp[i]) > 30){
                        q <- q+1
                        session$fac[q] <- session$fac[q] +(temp[i] - temp[i-1])/1000
                }
                 
         }
 }
 s <- length(session[1,])
 for (j in 1:s){
         r <- length(session[,1])
         for (i in 1:r){
                 if (sum(session[i,j]) < 60){
                         session <- session[-i, -j]
                 }
         }
         
 }
        
 avg <- mean(session)
 count <- length(session) 
