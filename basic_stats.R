basic_stats <- function(x) {
  c(min = min(x), mean = mean(x), median = median(x), max = max(x), sd = sd(x), diff(x))
}

stats_result <- vapply(data, basic_stats, numeric(6))

stats_result <- as.data.frame(stats_result)