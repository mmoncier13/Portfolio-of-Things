# Madison Moncier
# Exercise 13

# Problem One
cars_path <- "C:\\Users\\mmonc\\Downloads\\carsDataset.csv"

if (file.exists(cars_path)) {
  cars_raw <- read.csv(cars_path, row.names = 1)
} else {
  data(mtcars)
  cars_raw <- mtcars
}

cars <- cars_raw[, c("mpg","qsec","hp")]

cat("\n=== Problem 1: Univariate Outliers (IQR rule) ===\n")

get_univariate_outliers <- function(x) {
  bp <- boxplot.stats(x)
  list(
    values = bp$out,
    indices = which(x %in% bp$out)
  )
}

out_mpg  <- get_univariate_outliers(cars$mpg)
out_qsec <- get_univariate_outliers(cars$qsec)
out_hp   <- get_univariate_outliers(cars$hp)

cat("\nmpg outliers:\n")
print(data.frame(car = rownames(cars)[out_mpg$indices],
                 mpg = cars$mpg[out_mpg$indices]))

cat("\nqsec outliers:\n")
print(data.frame(car = rownames(cars)[out_qsec$indices],
                 qsec = cars$qsec[out_qsec$indices]))

cat("\nhp outliers:\n")
print(data.frame(car = rownames(cars)[out_hp$indices],
                 hp = cars$hp[out_hp$indices]))

# Outliers Using Mahalanobis Distance
cat("\n=== Problem 1: Multivariate Outliers (Mahalanobis) ===\n")

cars_scaled <- scale(cars)  # standardize so variables comparable
md <- mahalanobis(cars_scaled, colMeans(cars_scaled), cov(cars_scaled))

# Chi-square Cutoff
cutoff_975 <- qchisq(0.975, df = ncol(cars))
cutoff_99  <- qchisq(0.99,  df = ncol(cars))

mult_out_975 <- which(md > cutoff_975)
mult_out_99  <- which(md > cutoff_99)

cat("\nMahalanobis cutoff 97.5% =", cutoff_975, "\n")
cat("Outliers (97.5%):\n")
print(data.frame(car = rownames(cars)[mult_out_975],
                 MD = md[mult_out_975]))

cat("\nMahalanobis cutoff 99% =", cutoff_99, "\n")
cat("Outliers (99%):\n")
print(data.frame(car = rownames(cars)[mult_out_99],
                 MD = md[mult_out_99]))

top_md <- sort(md, decreasing = TRUE)
cat("\nTop multivariate standouts (by MD):\n")
print(head(data.frame(car = names(top_md), MD = top_md), 10))


# Problem Two

cat("\n\n============================================\n")
cat("=== Problem 2: bankloan.csv Outliers ===\n")
cat("============================================\n")

bank_path <- "C:\\Users\\mmonc\\Downloads\\bankloan.csv"
bank <- read.csv(bank_path)

# Variables
uni_vars <- c("x1","x5","x6","x7","x11","x13","x14")

# Return Outlier Indices & Values
uni_outliers <- lapply(uni_vars, function(v) {
  x <- bank[[v]]
  bp <- boxplot.stats(x)
  list(
    var = v,
    values = bp$out,
    indices = which(x %in% bp$out),
    fences = bp$stats[c(1,5)]
  )
})
names(uni_outliers) <- uni_vars

cat("\n=== Univariate outliers (IQR/boxplot rule) ===\n")
for (v in uni_vars) {
  info <- uni_outliers[[v]]
  cat("\n", v, "fences:",
      paste0("[", round(info$fences[1],4), ", ", round(info$fences[2],4), "]"),
      "\nOutlier count:", length(info$indices), "\n")
  if (length(info$indices) > 0) {
    print(head(data.frame(row = info$indices,
                          value = bank[[v]][info$indices]), 20))
  } else {
    cat("No outliers detected.\n")
  }
}

# 7 Boxplots
par(mfrow = c(2,4), mar = c(4,4,2,1))  # 7 plots in a grid
for (v in uni_vars) {
  boxplot(bank[[v]],
          main = paste("Boxplot of", v),
          ylab = v,
          col = "lightgray")
}
par(mfrow=c(1,1))  # reset


# Outliers Using Gower
cat("\n=== Multivariate outliers (Gower distance) ===\n")
feat_vars <- paste0("x", 1:18)
bank_feat <- bank[, feat_vars]

# Gower Distances
library(cluster)
gower_dist <- daisy(bank_feat, metric = "gower")

# Mean Distance = Outlier Score
D <- as.matrix(gower_dist)
mean_gower <- rowMeans(D)

top10_idx <- order(mean_gower, decreasing = TRUE)[1:10]
top10 <- data.frame(
  row_index = top10_idx,
  mean_gower_dist = mean_gower[top10_idx]
)

print(top10)

cat("\nTop 10 multivariate outlier rows (features):\n")
print(bank[top10_idx, c(feat_vars, "x19")])
