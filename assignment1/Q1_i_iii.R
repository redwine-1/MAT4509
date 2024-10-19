library(readxl)
library(rstudioapi)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load dataset
data_file <- "United Airlines Aircraft Operating Statistics- Cost Per Block Hour (Unadjusted).xlsx"
all_data <- read_excel(data_file, range = "b2:w158")

# Helper function to extract salary data by row
get_salary_wages <- function(row_num, data = all_data) {
  return(na.omit(as.numeric(data[row_num, -1])))
}

# Extract salary and wages data for different fleets
salary_wages_snbodies <- get_salary_wages(6)
salary_wages_lnbodies <- get_salary_wages(45)
salary_wages_wbodies <- get_salary_wages(84)
salary_wages_tfleet <- get_salary_wages(123)


get_modes <- function(data) {
  freq_table <- table(data)
  max_freq <- max(freq_table)
  modes <- as.numeric(freq_table[freq_table == max_freq])
  if (length(modes) == length(data)) {
    return(NULL)
  }
  return(modes)
}
get_frequency_distribution <- function(
    wage_data) {
  # number of observations
  n <- length(wage_data)

  # calculating the value of k (smallest k such that 2^k > n)
  k <- 0
  for (i in 1:(n / 2)) {
    if (2^i > n) {
      k <- i
      break
    }
  }

  # calculate class interval ( interval >= (max - min)/k)
  min_salary <- min(wage_data)
  max_salary <- max(wage_data)
  class_interval <- (max_salary - min_salary) / k
  class_interval <- ceiling(class_interval)



  # Create breakpoints
  break_points <- seq(
    min_salary - (class_interval / 2),
    max_salary + (class_interval / 2),
    by = class_interval
  )

  # Create frequency distribution
  salary_bins <- cut(wage_data, breaks = break_points, right = TRUE)
  frequency_distribution <- table(salary_bins)

  return(frequency_distribution)
}
print_analysis <- function(wage_data, title) {
  mean <- mean(wage_data)
  median <- median(wage_data)
  modes <- get_modes(wage_data)
  sample_sd <- sd(wage_data) # sample
  sample_var <- var(wage_data) # sample
  quartiles <- quantile(wage_data, probs = c(0.25, 0.5, 0.75))
  tenth_percentile <- quantile(wage_data, probs = 0.10)
  ninth_decile <- quantile(wage_data, probs = 0.90)
  range <- max(wage_data) - min(wage_data)

  # print results
  cat("Analysis of ", title, "::\n")
  cat("Mean:", mean, "\n")
  cat("Median:", median, "\n")
  if (is.null(modes) || length(modes) == 0) {
    cat("Modes: None\n")
  } else {
    cat("Modes:", paste(modes, collapse = ", "), "\n")
  }

  cat("Sample Standard Deviation:", sample_sd, "\n")
  cat("Sample Variance:", sample_var, "\n")
  cat("Quartiles (Q1, Q2, Q3):", quartiles, "\n")
  cat("10th Percentile:", tenth_percentile, "\n")
  cat("9th Decile:", ninth_decile, "\n")
  cat("Range:", range, "\n")
  cat("\n\n")
}
set_window_size <- function(window_title) {
  windows(width = 1920 / 200, height = 1080 / 200, title = window_title)
}

plot_histogram <- function(frequency_distribution, window_title) {
  set_window_size(window_title)
  barplot(frequency_distribution,
    xlab = "Salary Ranges",
    ylab = "Frequency",
    col = "lightblue",
    border = "black",
    space = 0, # No space between bars
    width = 1 # Adjust width to fill the space better
  )
}

# get frequency distribution (i)
snbodies_fdistribution <- get_frequency_distribution(salary_wages_snbodies)
lbodies_fdistribution <- get_frequency_distribution(salary_wages_lnbodies)
wbodies_fdisgribution <- get_frequency_distribution(salary_wages_wbodies)
tfleet_fdistribution <- get_frequency_distribution(salary_wages_tfleet)

# print Frequency Distribution
cat("Frequency Distribution for Small Narrowbodies:\n")
print(snbodies_fdistribution)
cat("\nFrequency Distribution for Large Narrowbodies:\n")
print(lbodies_fdistribution)
cat("\nFrequency Distribution for Widebodies:\n")
print(wbodies_fdisgribution)
cat("\nFrequency Distribution for Total Fleet:\n")
print(tfleet_fdistribution)

# print analysis (ii)
print_analysis(salary_wages_snbodies, "salary wages of small narrowbodies")
print_analysis(salary_wages_lnbodies, "salary wages of large narrowbodies")
print_analysis(salary_wages_wbodies, "salary wages of widebodies")
print_analysis(salary_wages_tfleet, "salary wages of total fleet")

# histogram using i. (iii)
plot_histogram(snbodies_fdistribution, "salary wages of small narrowbodies")
plot_histogram(lbodies_fdistribution, "salary wages of large narrowbodies")
plot_histogram(wbodies_fdisgribution, "salary wages of widebodies")
plot_histogram(tfleet_fdistribution, "salary wages of total fleet")
