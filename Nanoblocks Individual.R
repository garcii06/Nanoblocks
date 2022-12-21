library(rvest)

url_product <- "https://www.kawada-toys.com/en/brand/nanoblock/catalog/nbmc_53/"

base_info <- url_product %>% 
  read_html() %>% 
  html_elements(".p-product-info")

# Title or name of the Nanoblock
nanoblock_name <- base_info %>% 
  html_element(".p-product-title__title") %>% 
  html_text2()

# Specs from the Nanoblock
nanoblock_specs <- base_info %>% 
  html_elements(".p-specification__body") %>% 
  html_text2()

data.frame(
  name = nanoblock_name,
  item_number = nanoblock_specs[1]
)
