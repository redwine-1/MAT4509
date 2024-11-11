library(readxl)
library(rstudioapi)
library(corrplot)

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

# R squared (ii)
print_r_squared <- function(model_summary) {
    r_squared <- model_summary$r.squared
    cat("\tR-squared:", r_squared, "\n")
}

# Standard Error (iii)
print_std_err <- function(model_summary) {
    std_err <- model_summary$sigma
    cat("\tStandard Error:", std_err, "\n")
}

# Correlation Matrix (iv)
create_corr_matrix <- function(data, fleet_category) {
    file_name <- paste0(
        "report/images/",
        gsub(" ", "_", fleet_category),
        "_corr_matrix", ".png"
    )
    png(file_name, width = 600, height = 600)
    correlation_matrix <- cor(data)
    # print(correlation_matrix)
    corrplot(
        correlation_matrix,
        method = "number",
        type = "full",
        tl.col = "black",
        tl.srt = 45
    )
    dev.off()
}

# F value and P value for global analysis (v)
global_test <- function(model_summary) {
    f_statistic <- model_summary$fstatistic
    p_value <- pf(f_statistic[1],
        f_statistic[2],
        f_statistic[3],
        lower.tail = FALSE
    )
    cat("\tF-statistic:", f_statistic[1], "\n\tP-value:", p_value, "\n")
}


get_coefficients <- function(model_summary) {
    model_coefficients <- model_summary$coefficients
    cat("P values")
    print(model_coefficients)
}
# function to calculate r Square
regression_model_analysis <- function() {
    for (i in 1:4) {
        data <- data.frame(
            salary_wages = salary_wages_data[i, ],
            pilot_training = pilot_training_data[i, ],
            benefits = benefits_data[i, ],
            per_diem_personnel = per_diem_personnel_data[i, ],
            maintenance = maintenance_data[i, ],
            aircraft_ownership = aircraft_ownership_data[i, ]
        )
        model <- lm(
            salary_wages ~ pilot_training + benefits + per_diem_personnel + maintenance + aircraft_ownership,
            data = data
        )
        model_summary <- summary(model)
        cat(fleet_category[i], ":\n")
        print_r_squared(model_summary)
        print_std_err(model_summary)
        create_corr_matrix(data, fleet_category[i])
        global_test(model_summary)
        get_coefficients(model_summary)
        cat("\n")
    }
    cat("\n\n")
}

regression_model_analysis()
