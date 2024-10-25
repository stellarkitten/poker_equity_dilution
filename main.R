# Data: https://homes.luddy.indiana.edu/kapadia/nofoldem/
files <- c("data/2_wins.stats.txt", "data/3_wins.stats.txt", "data/4_wins.stats.txt", "data/5_wins.stats.txt", "data/6_wins.stats.txt", "data/7_wins.stats.txt", "data/8_wins.stats.txt", "data/9_wins.stats.txt", "data/10_wins.stats.txt")
hand_matrix <- c("AA", "KA", "QA", "JA", "TA", "9A", "8A", "7A", "6A", "5A", "4A", "3A", "2A", "KAs", "KK", "QK", "JK", "TK", "9K", "8K", "7K", "6K", "5K", "4K", "3K", "2K", "QAs", "QKs", "QQ", "JQ", "TQ", "9Q", "8Q", "7Q", "6Q", "5Q", "4Q", "3Q", "2Q", "JAs", "JKs", "JQs", "JJ", "TJ", "9J", "8J", "7J", "6J", "5J", "4J", "3J", "2J", "TAs", "TKs", "TQs", "TJs", "TT", "9T", "8T", "7T", "6T", "5T", "4T", "3T", "2T", "9As", "9Ks", "9Qs", "9Js", "9Ts", "99", "89", "79", "69", "59", "49", "39", "29", "8As", "8Ks", "8Qs", "8Js", "8Ts", "89s", "88", "78", "68", "58", "48", "38", "28", "7As", "7Ks", "7Qs", "7Js", "7Ts", "79s", "78s", "77", "67", "57", "47", "37", "27", "6As", "6Ks", "6Qs", "6Js", "6Ts", "69s", "68s", "67s", "66", "56", "46", "36", "26", "5As", "5Ks", "5Qs", "5Js", "5Ts", "59s", "58s", "57s", "56s", "55", "45", "35", "25", "4As", "4Ks", "4Qs", "4Js", "4Ts", "49s", "48s", "47s", "46s", "45s", "44", "34", "24", "3As", "3Ks", "3Qs", "3Js", "3Ts", "39s", "38s", "37s", "36s", "35s", "34s", "33", "23", "2As", "2Ks", "2Qs", "2Js", "2Ts", "29s", "28s", "27s", "26s", "25s", "24s", "23s", "22")
equities <- c()
equity_matrices <- c()

for (i in 1:9)
{
data <- read.table(files[i], header=TRUE)  

hand <- data$Hole
data <- data[match(hand_matrix, hand), ]

equity <- data$Total
equities[[i]] <- equity

equity_matrix <- matrix(equity, ncol=13)
equity_matrix <- equity_matrix[nrow(equity_matrix):1, ]
equity_matrices[[i]] <- equity_matrix
}

n <- 2:10
Bs <- c()
B_error <- c()

for (i in 1:169)
{
hand_equities <- c()

for (j in 1:9)
{
equity <- equities[[j]][i]
hand_equities <- append(hand_equities, equity)
}

fit <- nls(hand_equities~A*exp(-n/B), start=list(A=100, B=10))
fit_summary <- summary(fit)
coeff <- fit_summary$coefficients

Bs <- append(Bs, coeff[2])
B_error <- append(B_error, coeff[4])
}

B_matrix <- matrix(Bs, ncol=13)
B_matrix <- B_matrix[nrow(equity_matrix):1, ]
heads_up_equity <- equities[[1]]

plot(heads_up_equity, Bs, xlab="Heads-up Equity", ylab="Equity Dilution", main="Hand Equities")
arrows(heads_up_equity, Bs-B_error, heads_up_equity, Bs+B_error, length=0.1, angle=90, code=3)

heatmap(B_matrix, Rowv=NA, Colv=NA, scale="none", col=topo.colors(256), main="Equity Dilution Matrix")