# Nanoblock Scraper JPN version with prices.

# Libraries ---------------------------------------------------------------
# The following libraries where used to do the web scraping easier.
# To install all the packages:
# install.packages(c("rvest", "netstat", "RSelenium", "tidyverse"))
# Web scraping of static pages.
library(rvest)
# Automatically get a free port for Selenium.
library(netstat)
# Web scraping of dynamic pages.
library(RSelenium)
# Make tidy data. 
library(tidyverse)

# Extracting links of the catalog -----------------------------------------
# The first part of the process is to get all the individual links of each
# product. As the page is dynamic, the "only" way is using RSelenium.

# Catalog URL for the Japanese website.
url <- "https://www.kawada-toys.com/brand/nanoblock/catalog/"

# Web_navigation function.
# Multipurpose web navigation, needs at least a url to open using RSelenium.
# scroll: set TRUE if the webpage needs to be scrolled to load information dinamically.
# scroll_distance: set only if scroll is needed, is the distance to reach the visual bottom
# of the current page.
# Scrolling too much can stop the page from loading more content.
# Scrolling too little can make that the page does not trigger the height to load more.
# get_links: set TRUE to retrieve all the individual links.
web_navigation <- function(url, scroll = FALSE, scroll_distance = 0, get_links = FALSE){
  # Start the server. In this case using Chrome.
  rs_driver_object <- rsDriver(browser = "chrome",
                               verbose = FALSE,
                               chromever = "108.0.5359.22",
                               port = free_port(),
                               check = FALSE)
  
  # Create the client object.
  remDr <- rs_driver_object$client
  remDr$close()
  
  # Open the web browser without any
  remDr$open()
  
  # Navigate to catalog website of the nanoblocks.
  remDr$navigate(url)
  
  if(scroll){
    last_height <- remDr$executeScript("return document.body.scrollHeight")
    while (TRUE) {
      # Scroll down until the bottom is reached.
      script_scroll <- str_c("window.scrollTo(0, document.body.scrollHeight - ", scroll_distance, ");")
      remDr$executeScript(script_scroll)
      
      # Wait time
      Sys.sleep(0.5)
      
      # Calculate new scroll height and compare with last scroll
      new_height <- remDr$executeScript("return document.body.scrollHeight")
      if(new_height[[1]] == last_height[[1]])
        break
      last_height <- new_height
    }
  }
  
  if(get_links){
    # Get all the links for the Nanoblocks products
    links <- remDr$findElements(using = "class name", "p-product__name-link")
    links <- lapply(links, function (x) x$getElementAttribute("href")) %>% 
      unlist()
    remDr$close()
    system("taskkill /im java.exe /f")
    return(links)
  }
}

# The individual products links are now saved into the a structure.
nanoblock_ind_links <- web_navigation(url, scroll = TRUE, scroll_distance = 1500, get_links = TRUE)

# Extracting information from individual products -------------------------
# The next part is to retrieve the information from each individual product in the
# catalog. 
# The good part is that each page is static so we can use rvest to scrap the information.
# The bad part is that not all the information, such as price, is on that page.

## Test links -------------------------------------------------------------
# The following test were used during the scraping process as not all the pages
# contained the same information.
# Nanoblock toolkit 
url_product <- "https://www.kawada-toys.com/brand/nanoblock/catalog/nb-053/"
# Difficulty level
url_product <- "https://www.kawada-toys.com/brand/nanoblock/catalog/nb-057/"
# Box set 
url_product <- "https://www.kawada-toys.com/brand/nanoblock/catalog/nbmc_53/"
# Other
url_product <- "https://www.kawada-toys.com/brand/nanoblock/catalog/nbh_230/"

## Scraping individual items ----------------------------------------------
# Using the Japanese version of the page offers a better experience as it includes
# the direct link to the Rakuten page which includes more information of the product.
for (i in 323:length(nanoblock_ind_links)) {
  url_product <- nanoblock_ind_links[i]
  print(str_c(i, url_product, sep = " "))
  
  # If the product can be bought then get the link to the Rakuten page.
  buy_info <- tryCatch(url_product |> 
                         read_html() |> 
                         html_element(".p-product-action") |> 
                         html_element("a") |> 
                         html_attr("href"),
                       error = function(e){return(NULL)})
  
  # Base information of the product.
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
  nanoblock_item_number <- nanoblock_all[str_detect(nanoblock_all, "品番")] %>% 
    str_sub(str_length("品番\n") + 1, 1000)
  
  # Get the Release Date in Japan
  nanoblock_date <- nanoblock_all[str_detect(nanoblock_all, "発売日")] %>% 
    str_sub(str_length("発売日\n") + 1, 1000) %>% 
    str_sub(1, 10)
  
  # Get the Package Size
  nanoblock_package <- nanoblock_all[str_detect(nanoblock_all, "PKGサイズ")] %>% 
    str_sub(str_length("PKGサイズ(W×H×D)\n") + 1, 1000)
  
  # Get the Built Size
  nanoblock_built <- nanoblock_all[str_detect(nanoblock_all, "完成サイズ")] %>% 
    str_sub(str_length("完成サイズ(W×H×D)\n") + 1, 1000)
  
  # Get the Recommended Age
  nanoblock_age <- nanoblock_all[str_detect(nanoblock_all, "対象年齢")] %>%
    str_sub(str_length("対象年齢\n") + 1, 1000) %>% 
    str_sub(1, 2)
  
  # Get the number of pieces by each individual character in the box or in the
  # box set.
  # To get all the pieces, multiply nanoblock_pieces by nanoblock_box.
  nanoblock_pieces <- nanoblock_all[str_detect(nanoblock_all, "ピース数")] %>% 
    str_sub(str_length("ピース数\n") + 1, 1000)
  
  # Check if it is a toolkit, so the are no blocks or pieces.
  nanoblock_pieces <- ifelse(identical(nanoblock_pieces, integer(0)), NA, nanoblock_pieces)
  
  # Prepare a dummy tibble for the first iteration.
  # I could find another way to do it nor wanted to correct it at this time, 
  # it is easier to drop the first/duplicate observation(s).
  if(i == 1){
    nanoblock_tb <- tibble(
      name = nanoblock_name,
      item_number = nanoblock_item_number,
      release_date_japan = nanoblock_date,
      package_size_mm = nanoblock_package,
      built_up_size_mm = nanoblock_built,
      pieces_single = nanoblock_pieces,
      pieces_box = nanoblock_box,
      target_age = nanoblock_age,
      difficulty_level = nanoblock_difficulty,
      link_rakuten = buy_info
    )
  }
  
  # Tibble to be append with the new info.
  new_info <- tibble(
    name = nanoblock_name,
    item_number = nanoblock_item_number,
    release_date_japan = nanoblock_date,
    package_size_mm = nanoblock_package,
    built_up_size_mm = nanoblock_built,
    pieces_single = nanoblock_pieces,
    pieces_box = nanoblock_box,
    target_age = nanoblock_age,
    difficulty_level = nanoblock_difficulty,
    link_rakuten = buy_info
  )
  
  # Append of each new element scraped to the tibble.
  nanoblock_tb <- bind_rows(nanoblock_tb, new_info)
  #Sys.sleep(0.5)
}

# Remove duplicates
nanoblock_raw <- nanoblock_tb |> 
  group_by(item_number) |> 
  filter(row_number() < 2)

## Writing to a file ------------------------------------------------------
# The last part of the initial process is to write the raw information.
# We just got the basic information.
write_csv(nanoblock_raw, "Nanoblock_raw_JPN.csv")

# Opening files -----------------------------------------------------------
# In case that we need to stop the process. Read the raw information and continue
# with the next part.
nanoblock_raw <- read_csv("Nanoblock_raw_JPN.csv")

# Preprocessing links -----------------------------------------------------
# Some of the links do not go to the appropriate site, so some of the known 
nanoblock_raw$link_rakuten[102] <- "https://item.rakuten.co.jp/jism/4972825222881-55-62911-n/"
nanoblock_raw$link_rakuten[292] <- "https://item.rakuten.co.jp/charashop-twinkle/202211122259284972825202821/"

# Dynamic retrieve of prices ----------------------------------------------
for (i in 315:nrow(nanoblock_tb)) {
  print(str_c(i, nanoblock_tb$link_rakuten[i], sep = " "))
  url <-  nanoblock_tb$link_rakuten[i]
  
  if(is.na(url)){
    next 
  }
  
  # Start a server
  rs_driver_object <- rsDriver(browser = "chrome",
                               verbose = FALSE,
                               chromever = "108.0.5359.22",
                               port = free_port(),
                               check = FALSE)
  
  # Create a client object
  remDr <- rs_driver_object$client
  remDr$close()
  
  # open a browser
  remDr$open()
  
  # Navigate to catalog website of the nanoblocks.
  remDr$navigate(url)
  Sys.sleep(3)
  item_desc <- remDr$findElement(using = "class name", "item_desc")
  item_price <- remDr$findElement(using = "class name", "price2")
  item_points <- remDr$findElement(using = "class name", "point-summary__total___3rYYD")
  
  if(i == 1){
    rakuten_tb <-  tibble(
      nanoblock_item_number = nanoblock_tb$item_number[i],
      rakuten_item_description = item_desc$getElementText() |> unlist(),
      rakuten_price = item_price$getElementText() |> unlist(),
      rakuten_points = item_points$getElementText() |> unlist()
    )
  }
  
  rakuten_new <- tibble(
    nanoblock_item_number = nanoblock_tb$item_number[i],
    rakuten_item_description = item_desc$getElementText() |> unlist(),
    rakuten_price = item_price$getElementText() |> unlist(),
    rakuten_points = item_points$getElementText() |> unlist()
  )
  
  rakuten_tb <- bind_rows(rakuten_tb, rakuten_new)
  remDr$close()
  system("taskkill /im java.exe /f")
}

rakuten_tb |> 
  view()

# Writing to a file -------------------------------------------------------
write_csv(rakuten_tb, "rakuten_raw_JPN.csv")
read_csv("rakuten_raw_JPN.csv")