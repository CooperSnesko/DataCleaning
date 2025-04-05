
# Function to read CSV data
readCSVData <- function(fileName) {
  if (!file.exists(fileName)) {
    stop(paste("File not found:", fileName))
  }
  data <- read.csv(fileName, stringsAsFactors = FALSE)
  cat("Data read from", fileName, "\n")
  return(data)
}

# Display data summary
displaySummary <- function(df) {
  cat("\nData Summary:\n")
  print(summary(df))
}

# Convert data types and add new columns
convertDataTypes <- function(df) {
  if ("num_value" %in% names(df)) {
    df$num_value <- as.numeric(df$num_value)
  }
  df$int_value <- as.integer(runif(nrow(df), 1, 100))
  
  if ("name" %in% names(df)) {
    df$name <- as.character(df$name)
  } else {
    df$name <- paste("Item", seq_len(nrow(df)))
  }
  
  df$is_valid <- df$int_value > 50
  df$group <- factor(sample(c("A", "B", "C"), nrow(df), replace = TRUE))
  
  return(df)
}

# Replace missing numeric values
handleMissingValues <- function(df) {
  for (col in names(df)) {
    if (is.numeric(df[[col]])) {
      if (any(is.na(df[[col]]))) {
        df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
      }
    }
  }
  return(df)
}

# Process list elements
processList <- function(myList) {
  for (i in seq_along(myList)) {
    cat("Element", i, ":", myList[[i]], "\n")
  }
}

# Additional cleaning
additionalCleaning <- function(df) {
  df <- unique(df)
  for (col in names(df)) {
    if (is.character(df[[col]])) {
      df[[col]] <- trimws(df[[col]])
    }
  }
  return(df)
}

# Write cleaned data to CSV
writeCleanData <- function(df, outputFileName) {
  write.csv(df, outputFileName, row.names = FALSE)
  cat("Cleaned data written to", outputFileName, "\n")
}

# Main Execution
set.seed(123)
csvFile <- "sample_data.csv"


if (!file.exists(csvFile)) {
  sample_data <- data.frame(
    id = 1:20,
    num_value = round(rnorm(20, 50, 10), 2),
    name = paste("Sample", 1:20),
    stringsAsFactors = FALSE
  )
  sample_data$num_value[sample(1:20, 5)] <- NA
  write.csv(sample_data, csvFile, row.names = FALSE)
  cat("Sample data created as", csvFile, "\n")
}

data <- readCSVData(csvFile)
displaySummary(data)
data <- convertDataTypes(data)
data <- handleMissingValues(data)
data <- additionalCleaning(data)
cat("\nFinal Data Summary:\n")
print(summary(data))

# Process a sample list
sample_list <- list("first element", 42, TRUE, 3.14, factor("A"))
processList(sample_list)

# Write the cleaned data
output_csv <- "cleaned_data.csv"
writeCleanData(data, output_csv)

