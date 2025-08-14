install.packages(c("rvest","dplyr","stringr"))
# Load required libraries
library(rvest)
library(dplyr)
library(stringr)
# Function to scrape data from a given URL
scrape_data <- function(url) {
# Read the HTML content of the webpage
webpage <- tryCatch(
{ read_html(url) },
error = function(e) {
message("Error fetching the webpage. Please check the URL.")
return(NULL)
}
)
# Exit if the webpage couldn't be read
if (is.null(webpage)) return(NULL)
# Extract all headings (e.g., H1, H2, H3) as key points
headings <- webpage %>%
html_nodes("h1, h2, h3") %>%
html_text() %>%
str_squish() # Remove extra whitespace
# Extract first 500 characters of the body text
text_content <- webpage %>%
html_nodes("body") %>%
html_text() %>%
str_squish() # Remove extra whitespace
first_500_chars <- substr(text_content, 1, 500) # Extract first 500 characters
# Extract all links
links <- webpage %>%
html_nodes("a") %>%
html_attr("href")
# Extract all images
images <- webpage %>%
html_nodes("img") %>%
html_attr("src")
# Extract table data (if any tables are present)
tables <- webpage %>%
html_nodes("table") %>%
html_table(fill = TRUE)
# Output a list with scraped data
scraped_data <- list(
KeyPoints = headings,
First500Chars = first_500_chars,
Links = links,
Images = images,
Tables = tables
)
return(scraped_data)
}
# Ensure user input works interactively
cat("Enter the URL to scrape:\n")
url <- readline() # Prompt for user input
# Test fallback for non-interactive environments (hardcode a URL if needed)
if (url == "") {
url <- "http://books.toscrape.com" # Default test URL
message("No URL provided. Using default: ", url)
}
# Call the scrape_data function
scraped_content <- scrape_data(url)
# Process and display results
if (!is.null(scraped_content)) {
# Display key points (headings)
cat("\nExtracted Key Points (Headings):\n")
if (length(scraped_content$KeyPoints) > 0) {
print(scraped_content$KeyPoints)
} else {
cat("No headings found on the webpage.\n")
}
# Display first 500 characters
cat("\nFirst 500 Characters of Text Content:\n")
print(scraped_content$First500Chars)
# Display links
cat("\nExtracted Links:\n")
print(scraped_content$Links)
# Display image sources
cat("\nExtracted Image Sources:\n")
print(scraped_content$Images)
# Display table data
if (length(scraped_content$Tables) > 0) {
cat("\nExtracted Table Data:\n")
print(scraped_content$Tables[[1]])
} else {
cat("\nNo tables found on the webpage.\n")
}
} else {
cat("\nNo data extracted. Please try a valid URL.\n")
}
