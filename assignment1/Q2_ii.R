library(readxl)
library(rstudioapi)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load dataset
data_file <- "United Airlines Aircraft Operating Statistics- Cost Per Block Hour (Unadjusted).xlsx"
all_data <- read_excel(data_file, range = "b2:w158")

get_data_by_rows <- function(row_nums) {
    extracted_data <- matrix(NA, nrow = 4, ncol = 21)
    for (i in seq_along(row_nums)) {
        row_num <- row_nums[i]
        row_data <- na.omit(as.numeric(all_data[row_num, -1]))
        extracted_data[i, 1:length(row_data)] <- row_data[1:21]
    }
    return(extracted_data)
}


# define rows
salary_wages_rows <- c(6, 45, 84, 123)
pilot_training_rows <- salary_wages_rows + 1
benefits_rows <- salary_wages_rows + 2
per_diem_personnel_rows <- salary_wages_rows + 3
maintenance_rows <- salary_wages_rows + 11
aircraft_ownership_rows <- salary_wages_rows + 18

# get data
salary_wages_data <- get_data_by_rows(salary_wages_rows)
pilot_training_data <- get_data_by_rows(pilot_training_rows)
benefits_data <- get_data_by_rows(benefits_rows)
per_diem_personnel_data <- get_data_by_rows(per_diem_personnel_rows)
maintenance_data <- get_data_by_rows(maintenance_rows)
aircraft_ownership_data <- get_data_by_rows(aircraft_ownership_rows)

# categories
fleet_category <- c(
    "small narrowbodies",
    "large narrowbodies",
    "widebodies",
    "total fleet"
)

# function to calculate r Square
calculate_r_squared <- function(data, independent_variable) {
    cat("R-squared for Dependent Variable 'Salaries and Wages' and Independent Variable '", independent_variable, "'\n", sep = "")
    for (i in 1:4) {
        model <- lm(salary_wages_data[i, ] ~ data[i, ])
        r_squared <- summary(model)$r.squared
        cat("R-squared for ", fleet_category[i], ":", r_squared, "\n")
    }
    cat("\n\n")
}

calculate_r_squared(pilot_training_data, "Pilot Training Data")
calculate_r_squared(benefits_data, " Benefits and Payroll Taxes")
calculate_r_squared(per_diem_personnel_data, "Per Diem/ Personnel")
calculate_r_squared(maintenance_data, "Maintenance")
calculate_r_squared(aircraft_ownership_data, "Aircraft Ownership")
