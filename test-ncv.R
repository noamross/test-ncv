library(mgcv)

set.seed(0)

# Generate data
n = 500
dat <- gamSim(n = n)[, c("x0", "x1", "x2", "x3", "y")]

# Break up data into clustered blocks using kmeans
clusters <- 4
clust <- stats::kmeans(dat[, c("x0","x1", "x2", "x3")], clusters)
dat$j <- clust$cluster
#j <- rep(1:4, each = 125) # Alternate option, testing if the issue is that we need equal-size groups. Nope, still crashes.
# Look at the clustered points, out-of-sample will be the unfilled ones.
if(interactive()) pairs(dat[, c("x0", "x1", "x2", "x3")], col = dat$j, pch=ifelse(dat$j == clusters, 1, 20), cex = 2)

dat <- dat[order(dat$j), ]
rownames(dat) <- NULL
# Train on 3 clusters to be CV'd, leave out last cluster for extrapolation
train <- dat[dat$j != clusters,]
test <- dat[dat$j == clusters,]

# Assemble neighborhood argument
nb <- list(
  k = seq_len(nrow(train)),
  m = c(which(diff(sort(train$j)) == 1), nrow(train))
)
nb$i <- nb$k
nb$mi <- nb$m

# Fit models, (but ncv_cluster always crashes)
models <- list(
  reml = gam(y~s(x0)+s(x1)+s(x2)+s(x3),data = train, method = "REML"),
  ncv_1 = gam(y~s(x0)+s(x1)+s(x2)+s(x3),data = train, method = "NCV"),
  ncv_cluster = gam(y~s(x0)+s(x1)+s(x2)+s(x3),data = train, method = "NCV", nei = nb)
)

# Look at RMSE performance in and out of sample
performance <- data.frame(
  model = names(models),
  rmse_train = sapply(models, function(b) sqrt(sum((predict(b) - train$y)^2)/nrow(train))),
  rmse_test = sapply(models, function(b) sqrt(sum((predict(b, newdata = test) - test$y)^2)/nrow(test)))
)
performance
