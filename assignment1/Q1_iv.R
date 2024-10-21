library(readxl)
library(rstudioapi)

# Set working directory and load data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load dataset
data_file <- "United Airlines Aircraft Operating Statistics- Cost Per Block Hour (Unadjusted).xlsx"
all_data <- read_excel(data_file, range = "b2:w158")

maintenance_categories <- c("labor", "materials", "third party", "burden")
maintenance_rows <- c(16, 55, 94, 133)

years <- 1995:2015
load_factor_rows <- maintenance_rows + 18

# Function to extract data for a given row number (Maintenance/Load Factor)
get_data_by_row <- function(row_num) {
    return(na.omit(as.numeric(all_data[row_num, -1])))
}
get_maintenace_category <- function(row_num) {
    labor <- get_data_by_row(row_num + 1)
    materials <- get_data_by_row(row_num + 2)
    third_party <- get_data_by_row(row_num + 3)
    burden <- get_data_by_row(row_num + 5)
    return(setNames(
        c(sum(labor), sum(materials), sum(third_party), sum(burden)),
        maintenance_categories
    ))
}

# For ploting Load Factor bar plot
plot_bar <- function(data, title) {
    barplot(data,
        xlab = "Years",
        ylab = "Load Factor (%)",
        col = "lightblue",
        border = "black"
    )
}



fleet_category <- c(
    "small narrowbodies",
    "large narrowbodies",
    "widebodies",
    "total fleet"
)

# pie chart for maintenance
lapply(1:4, function(i) {
    file_name <- paste0(
        "report/images/",
        gsub(" ", "_", fleet_category[i]),
        "_maintenace_pie", ".png"
    )
    png(file_name, width = 300, height = 300)
    data <- get_maintenace_category(maintenance_rows[i])
    pie(data)
    dev.off()
})


# bar chart for load factor
lapply(1:4, function(i) {
    data <- setNames(get_data_by_row(load_factor_rows[i]), years)
    file_name <- paste0(
        "report/images/",
        gsub(" ", "_", fleet_category[i]),
        "_load_factor_bar", ".png"
    )
    png(file_name, width = 1000, height = 500)
    plot_bar(data, fleet_category[i])
    dev.off()
})
