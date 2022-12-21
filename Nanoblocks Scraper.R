# Libraries ---------------------------------------------------------------
# Extracting data of static single products.
library(rvest)
# Automatically get a free port for Selenium.
library(netstat)
# Web scraping all the links as they are dynamically generated.
library(RSelenium)
# Parsing, cleaning, extracting, identifying and other tasks with strings. 
library(tidyverse)

# Creating the connection with the dynamic page to scrap ------------------
# Base url for Nanoblock catalog
url = "https://www.kawada-toys.com/en/brand/nanoblock/catalog/"

# Start a server
rs_driver_object <- rsDriver(browser = "chrome",
                             verbose = FALSE,
                             chromever = "108.0.5359.22",
                             port = free_port(),
                             check = FALSE)

# Create a client object
remDr <- rs_driver_object$client

# open a browser
remDr$open()

# Navigate to catalog website of the nanoblocks.
remDr$navigate(url)

## Scrolling through the page dynamically ---------------------------------
# Go to the bottom of the page to load all the products.
last_height <- remDr$executeScript("return document.body.scrollHeight")

while (TRUE) {
  # Scroll down to bottom
  remDr$executeScript("window.scrollTo(0, document.body.scrollHeight);")
  
  # Wait time
  Sys.sleep(0.5)
  
  # Calculate new scroll height and compare with last scroll
  new_height <- remDr$executeScript("return document.body.scrollHeight")
  if(new_height[[1]] == last_height[[1]])
    break
  last_height <- new_height
}

## Retrieve individual links for each product ------------------------------
# Get all the links for the Nanoblocks products
links <- remDr$findElements(using = "class name", "p-product__name-link")

# Applying the function to get the individual links as a list, not a nested list.
nanoblock_links <- lapply(links, function (x) x$getElementAttribute("href")) %>% 
  unlist()

## Terminate selenium server ----------------------------------------------
# Terminate the selenium session as the links for each products are static pages.
system("taskkill /im java.exe /f")

# Nanoblock Individual Information ----------------------------------------
## Nanoblock debug items --------------------------------------------------
# The following links are a preview of the types of items that Nanoblock has,
# this includes tool-kits, having a Recommended Difficulty Level or been a 
# collection set with several characters in one box.
# Nanoblock toolkit 
url_product <- "https://www.kawada-toys.com/en/brand/nanoblock/catalog/nb-053/"
# Difficulty level
url_product <- "https://www.kawada-toys.com/en/brand/nanoblock/catalog/nb-057/"
# Box set 
url_product <- "https://www.kawada-toys.com/en/brand/nanoblock/catalog/nbmc_38/"


## Scrapping individual items ----------------------------------------------
for (i in 1:length(nanoblock_links)) {
  url_product <- nanoblock_links[i]
  #print(url_product)

  # Base information of the product. Just for reducing code.
  base_info <- url_product %>% 
    read_html() %>% 
    html_elements(".p-product-info")
  
  # Get the text/description of the product. 
  # If it is a box set it will include the number of pieces otherwise is just
  # one piece per box.
  nanoblock_text <- url_product %>% 
    read_html() %>% 
    html_element(".p-product-text__block") %>% 
    html_text2()
  
  # Total products in the box set or individual box.
  nanoblock_box <- nanoblock_text %>% 
    str_extract(".BOX.個")
  
  # Name of the Nanoblock product.
  nanoblock_name <- base_info %>% 
    html_element(".p-product-title__title") %>% 
    html_text2()
  
  # General specifications from the Nanoblock.
  nanoblock_all <- base_info %>% 
    html_elements(".p-specification__item") %>% 
    html_text2()
  
  # Get Difficulty level
  nanoblock_difficulty <- base_info %>% 
    html_element(".c-rating") %>% 
    html_attr("aria-label")
  
  # Get the Item Number
  nanoblock_item_number <- nanoblock_all[str_detect(nanoblock_all, "Item")] %>% 
    str_sub(str_length("Item number\n") + 1, 1000)
  
  # Get the Release Date in Japan
  nanoblock_date <- nanoblock_all[str_detect(nanoblock_all, "Release")] %>% 
    str_sub(str_length("Release date(Japan)\n") + 1, 1000) %>% 
    str_sub(1, 10)
  
  # Get the Package Size
  nanoblock_package <- nanoblock_all[str_detect(nanoblock_all, "Package")] %>% 
    str_sub(str_length("Package size(WxHxD)\n") + 1, 1000)
  
  # Get the Built Size
  nanoblock_built <- nanoblock_all[str_detect(nanoblock_all, "Built")] %>% 
    str_sub(str_length("Built Up Size(WxHxD)\n") + 1, 1000)
  
  # Get the Recommended Age
  nanoblock_age <- nanoblock_all[str_detect(nanoblock_all, "Target Age")] %>%
    str_sub(str_length("Target Age(Japan)\n") + 1, 1000) %>% 
    str_sub(1, 2)
  
  # Get the number of pieces by each individual character in the box or in the
  # box set.
  # To get all the pieces, multiply nanoblock_pieces by nanoblock_box.
  nanoblock_pieces <- nanoblock_all[str_detect(nanoblock_all, "Pieces")] %>% 
    str_sub(str_length("Pieces\n") + 1, 1000)
  
  # Check if it is a toolkit, so the are no blocks or pieces.
  nanoblock_pieces <- ifelse(identical(nanoblock_pieces, integer(0)), NA, nanoblock_pieces)
  
  if(i == 1){
    old_tb <- tibble(
      name = nanoblock_name,
      item_number = nanoblock_item_number,
      release_date_japan = nanoblock_date,
      package_size_mm = nanoblock_package,
      built_up_size_mm = nanoblock_built,
      pieces_single = nanoblock_pieces,
      pieces_box = nanoblock_box,
      target_age = nanoblock_age,
      difficulty_level = nanoblock_difficulty
    )
  }
  
  # Tibble
  new_tb <- tibble(
    name = nanoblock_name,
    item_number = nanoblock_item_number,
    release_date_japan = nanoblock_date,
    package_size_mm = nanoblock_package,
    built_up_size_mm = nanoblock_built,
    pieces_single = nanoblock_pieces,
    pieces_box = nanoblock_box,
    target_age = nanoblock_age,
    difficulty_level = nanoblock_difficulty
  )
  
  # Union of each new element scraped to the tibble.
  old_tb <- bind_rows(old_tb, new_tb)
  Sys.sleep(0.5)
}

# As the way the loop appends new items, the first entry will be repeated.
# Delete the first row in the set to get a "clean" version of scraped items.
nanoblock_tb <- old_tb %>% 
  filter(row_number() > 1)

nanoblock_tb %>% 
  view()
# Debugging of the scaper. 
# remove(new_tb)
# remove(old_tb)
# remove(nanoblock_tb)

# Writing to a file -------------------------------------------------------
write_csv(nanoblock_tb, "Nanoblocks_information.csv", local)