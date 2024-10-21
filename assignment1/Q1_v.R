library(readxl)
library(rstudioapi)

# Set working directory and load data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load dataset
data_file <- "United Airlines Aircraft Operating Statistics- Cost Per Block Hour (Unadjusted).xlsx"
all_data <- read_excel(data_file, range = "b2:w158")

# define categories
daily_utilization_categories <- c("Block hours", "Airborne hours", "Departures")
ownership_categories <- c("Rental", "Depreciation and Amortization")
purchased_goods_categories <- c("Fuel/Oil", "Insurance", "Other (inc. Tax)")
fleet_category <- c(
  "small narrowbodies",
  "large narrowbodies",
  "widebodies",
  "total fleet"
)

# row numbers
purchased_goods_rows <- c(16, 55, 94, 133) - 5
ownership_rows <- purchased_goods_rows + 12
daily_utilization_rows <- ownership_rows + 13

get_data_by_row <- function(row_num) {
  return(na.omit(as.numeric(all_data[row_num, -1])))
}

get_category_data <- function(row_num, categories) {
  rows_data <- lapply(
    seq_along(categories),
    function(i) get_data_by_row(row_num + i)
  )
  costs <- unlist(rows_data)
  category <- factor(rep(categories, sapply(rows_data, length)))
  return(data.frame(costs = costs, category = category))
}

box_plot <- function(data, title1, title2, ylab) {
  file_name <- paste0(
    "report/images/",
    gsub(" ", "_", title1),
    "_",
    gsub(" ", "_", title2),
    ".png"
  )
  png(file_name, width = 500, height = 400)
  boxplot(costs ~ category,
    data = data,
    col = "lightblue",
    ylab = ylab,
    border = "black"
  )
  dev.off()
}

plot_category <- function(rows, categories, title, ylab) {
  # Create the box plot using the formula interface
  lapply(
    seq_along(rows),
    function(i) {
      box_plot(
        get_category_data(rows[i], categories),
        title,
        fleet_category[i], ylab
      )
    }
  )
}

plot_category(
  purchased_goods_rows,
  purchased_goods_categories,
  "Purchased Goods",
  "Cost ($)"
)
plot_category(
  ownership_rows,
  ownership_categories,
  "Aircraft Ownership",
  "Cost ($)"
)
plot_category(
  daily_utilization_rows,
  daily_utilization_categories,
  "Daily Utilization",
  "Hours"
)
