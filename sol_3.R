
theo_params <- read.csv('gpd_parameters.csv')
sample_data <- read.csv('gpd_samples.csv')

qgpd <- function(p, u, sigma, xi) {
  # Input validation
  if (!is.numeric(p) ||
      !is.numeric(u) || !is.numeric(sigma) || !is.numeric(xi)) {
    print("qgpd expects numeric values for all its inputs!")
    return(NaN)
  }
  if (any(p < 0 | p > 1)) {
    print("Probability passed to qgpd must be in [0,1]!")
    return(NaN)
  }
  if (sigma <= 0) {
    print("sigma must be greater than zero!")
    return(NaN)
  }

  if (xi == 0) {
    x <- u - sigma * (log(1 - p))
  }
  else{
    x <- u + (sigma * ((1 - p)^(-xi) - 1)) / xi
  }
  return(x)
}


theo_params <- read.csv('gpd_parameters.csv')
sample_data <- read.csv('gpd_samples.csv')
generate_theoretical <- function(label){
  label_params <- theo_params[theo_params$id == label,]
  theo_qs <- qgpd(
    p=ppoints(label_params$size),
    u=label_params$u,
    sigma=label_params$sigma,
    xi=label_params$xi
  )
}

build_qq_plot <- function(label){
  theo_qs <- generate_theoretical(label)
  qqplot(sample_data[sample_data$set_id == label, ]$value, theo_qs)
}

dist_labels <- c('a', 'b', 'c', 'd', 'e', 'f')

for (label in dist_labels){
  build_qq_plot(label)
}
