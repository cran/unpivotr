## ---- out.width = "850px"------------------------------------------------
knitr::include_graphics(system.file("extdata/enron-screenshot.png",
                                    package = "unpivotr"))

## ------------------------------------------------------------------------
library(unpivotr)
library(tidyxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

## ------------------------------------------------------------------------
path <- system.file("extdata/enron.xlsx",
                    package = "unpivotr")

## ------------------------------------------------------------------------
cells <- tidy_xlsx(path)$data[[1]]

## ---- eval = FALSE-------------------------------------------------------
#  formatting <- tidy_xlsx(path)$formats

## ------------------------------------------------------------------------
fixed_price <- filter(cells, character == "Fixed Price")[1, ]

## ------------------------------------------------------------------------
col_headers <-
  fixed_price %>%
  offset_N(cells, n = 1) %>%    # Offset up one row to "IF NWPL Rocky Mountains"
  extend_E(cells, 3) %>%        # Extend to the right edge of the table
  extend_S(cells, 2) %>%        # Extend down to the third row of the headers
  filter(!is.na(content)) %>%   # Remove blanks
  select(row, col, value = character) %>% # Prepare for joining to data cells
  split(.$row)                  # Separate the row of headers into list elements
col_headers

## ------------------------------------------------------------------------
datacells <-
  fixed_price %>%
  offset_S(cells, n = 2) %>%
  extend_E(cells, 4) %>%
  extend_S(cells,                       # Extend down to a blank row
           boundary = ~ is.na(content), # The formula detects blank cells
           edge = TRUE) %>%          # Require the whole row to be blank
  filter(!is.na(content)) %>%           # Remove remaining blanks
  mutate(value = as.double(content)) %>%# Convert the values to double
  select(row, col, value)               # Prepare for joining to headers
print(datacells, n = Inf)

## ------------------------------------------------------------------------
datacells %>%
  NNW(col_headers[[1]]) %>% # This header isn't in every column
  NNW(col_headers[[2]]) %>%  # Nor is this header
  N(col_headers[[3]])    # But this one is

## ------------------------------------------------------------------------
fixed_price <-
  cells %>%
  filter(character == "Fixed Price") %>%
  split(paste(.$row, .$col))

## ------------------------------------------------------------------------
tidy <- function(x) {
  col_headers <-
    x %>%
    offset_N(cells, n = 1) %>%
    extend_E(cells, 3) %>%
    extend_S(cells, 2) %>%
    filter(!is.na(content)) %>%
    select(row, col, value = character) %>%
    split(.$row)
  datacells <-
    x %>%
    offset_S(cells, n = 2) %>%
    extend_E(cells, 4) %>%
    extend_S(cells,
             boundary = ~ is.na(content),
             edge = TRUE) %>%
    filter(!is.na(content)) %>%
    mutate(value = as.double(content)) %>%
    select(row, col, value)
  datacells %>%
    NNW(col_headers[[1]]) %>%
    NNW(col_headers[[2]]) %>%
    N(col_headers[[3]])
}

## ------------------------------------------------------------------------
map_df(fixed_price, tidy) %>%
  arrange(col, row) # See that, from row 39, the region changes, as it ought.

## ------------------------------------------------------------------------
row_headers <-
  cells %>%
  filter(character == "Cash") %>%
  split(paste(.$row, .$col))

row_headers <-
  map_df(row_headers,
    ~ .x %>%
      extend_S(cells, boundary = ~ is.na(content)) %>%
      extend_E(cells, boundary = ~ is.na(content), edge = TRUE) %>%
      filter(!is.na(content)) %>%
      # This concatenates the "Dec-20 to Mar-20" cells into one column.
      # First it converts Excel dates, via R dates, into text.
      mutate(character = ifelse(!is.na(character),
                                character,
                                format(as.POSIXct(as.integer(content) * (60*60*24),
                                       origin="1899-12-30",
                                       tz="GMT"), "%b-%C"))) %>%
      # Then it concatentates them by row.
      select(row, col, character) %>%
      spread(col, character, fill = "") %>%
      mutate(col = 1, value = str_trim(paste(`2`, `3`, `4`))) %>%
      select(row, col, value))

## ------------------------------------------------------------------------
tidy <- function(x) {
  col_headers <-
    x %>%
    offset_N(cells, n = 1) %>%
    extend_E(cells, 3) %>%
    extend_S(cells, 2) %>%
    filter(!is.na(content)) %>%
    select(row, col, value = character) %>%
    split(.$row)
  datacells <-
    x %>%
    offset_S(cells, n = 2) %>%
    extend_E(cells, 4) %>%
    extend_S(cells,
             boundary = ~ is.na(content),
             edge = TRUE) %>%
    filter(!is.na(content)) %>%
    mutate(value = as.double(content)) %>%
    select(row, col, value)
  datacells %>%
    NNW(col_headers[[1]]) %>%
    NNW(col_headers[[2]]) %>%
    N(col_headers[[3]]) %>%
    W(row_headers) # This is the only new line
}

map_df(fixed_price, tidy) %>%
  arrange(col, row) # See that, from row 39, the context loops, as it ought.

## ---- eval = FALSE-------------------------------------------------------
#  library(unpivotr)
#  library(dplyr)
#  library(tidyr)
#  library(purrr)
#  library(stringr)
#  
#  path <- system.file("extdata/enron.xlsx",
#                      package = "unpivotr")
#  cells <- tidy_xlsx(path)$[[1]]
#  
#  fixed_price <-
#    cells %>%
#    filter(character == "Fixed Price") %>%
#    split(paste(.$row, .$col))
#  
#  row_headers <-
#    cells %>%
#    filter(character == "Cash") %>%
#    split(paste(.$row, .$col))
#  
#  row_headers <-
#    map_df(row_headers,
#      ~ .x %>%
#        extend_S(cells, boundary = ~ is.na(content)) %>%
#        extend_E(cells, boundary = ~ is.na(content), edge = TRUE) %>%
#        filter(!is.na(content)) %>%
#        mutate(character = ifelse(!is.na(character),
#                                  character,
#                                  format(as.POSIXct(as.integer(content) * (60*60*24),
#                                         origin="1899-12-30",
#                                         tz="GMT"), "%b-%C"))) %>%
#        select(row, col, character) %>%
#        spread(col, character, fill = "") %>%
#        mutate(col = 1, value = str_trim(paste(`2`, `3`, `4`))) %>%
#        select(row, col, value))
#  
#  tidy <- function(x) {
#    col_headers <-
#      x %>%
#      offset_N(cells, n = 1) %>%
#      extend_E(cells, 3) %>%
#      extend_S(cells, 2) %>%
#      filter(!is.na(content)) %>%
#      select(row, col, value = character) %>%
#      split(.$row)
#    datacells <-
#      x %>%
#      offset_S(cells, n = 2) %>%
#      extend_E(cells, 4) %>%
#      extend_S(cells,
#               boundary = ~ is.na(content),
#               edge = TRUE) %>%
#      filter(!is.na(content)) %>%
#      mutate(value = as.double(content)) %>%
#      select(row, col, value)
#    datacells %>%
#      NNW(col_headers[[1]]) %>%
#      NNW(col_headers[[2]]) %>%
#      N(col_headers[[3]]) %>%
#      W(row_headers) # This is the only new line
#  }
#  
#  map_df(fixed_price, tidy)

