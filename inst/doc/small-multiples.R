## ---- out.width = "850px"------------------------------------------------
knitr::include_graphics("enron-screenshot.png")

## ------------------------------------------------------------------------
library(unpivotr)
library(tidyxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

## ------------------------------------------------------------------------
path <- system.file("extdata/enron.xlsx", package = "unpivotr")

## ------------------------------------------------------------------------
cells <- xlsx_cells(path)

## ---- eval = FALSE-------------------------------------------------------
#  formatting <- xlsx_formats(path)

## ------------------------------------------------------------------------
fixed_price <- filter(cells, character == "Fixed Price")[1, ]

## ------------------------------------------------------------------------
col_headers <-
  fixed_price %>%
  offset_N(cells, n = 1) %>%    # Offset up one row to "IF NWPL Rocky Mountains"
  extend_E(cells, 3) %>%        # Extend to the right edge of the table
  extend_S(cells, 2) %>%        # Extend down to the third row of the headers
  filter(!is_blank) %>%   # Remove blanks
  select(row, col, value = character) %>% # Prepare for joining to data cells
  split(.$row)                  # Separate the row of headers into list elements
col_headers

## ------------------------------------------------------------------------
datacells <-
  fixed_price %>%
  offset_S(cells, n = 2) %>%
  extend_E(cells, 4) %>%
  extend_S(cells,                       # Extend down to a blank row
           boundary = ~ is_blank, # The formula detects blank cells
           edge = TRUE) %>%          # Require the whole row to be blank
  filter(!is_blank) %>%           # Remove remaining blanks
  mutate(value = as.double(numeric)) %>%# Convert the values to double
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
    filter(!is_blank) %>%
    select(row, col, value = character) %>%
    split(.$row)
  datacells <-
    x %>%
    offset_S(cells, n = 2) %>%
    extend_E(cells, 4) %>%
    extend_S(cells,
             boundary = ~ is.na(numeric),
             edge = TRUE) %>%
    filter(!is.na(numeric)) %>%
    select(row, col, value = numeric)
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
  filter(between(row, 17, 56), between(col, 2, 4), !is_blank) %>%
  # Concatenate rows like "Dec-01", "to", "Mar-02"
  mutate(character = ifelse(!is.na(character),
                            character,
                            format(date, origin="1899-12-30", "%b-%y"))) %>%
  select(row, col, character) %>%
  nest(-row) %>%
  mutate(data = map(data, ~ paste(.x$character, collapse = " "))) %>%
  unnest() %>%
  mutate(col = 2L) %>%
  select(row, col, value = data)

## ------------------------------------------------------------------------
tidy <- function(x) {
  col_headers <-
    x %>%
    offset_N(cells, n = 1) %>%
    extend_E(cells, 3) %>%
    extend_S(cells, 2) %>%
    filter(!is_blank) %>%
    select(row, col, value = character) %>%
    split(.$row)
  datacells <-
    x %>%
    offset_S(cells, n = 2) %>%
    extend_E(cells, 4) %>%
    extend_S(cells,
             boundary = ~ is.na(numeric),
             edge = TRUE) %>%
    filter(!is_blank) %>%
    mutate(value = numeric) %>%
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
#  cells <- xlsx_cells(path)
#  
#  fixed_price <-
#    cells %>%
#    filter(character == "Fixed Price") %>%
#    split(paste(.$row, .$col))
#  
#  row_headers <-
#    cells %>%
#    filter(between(row, 17, 56), between(col, 2, 4), !is_blank) %>%
#    # Concatenate rows like "Dec-01", "to", "Mar-02"
#    mutate(character = ifelse(!is.na(character),
#                              character,
#                              format(date, "%b-%y"))) %>%
#    select(row, col, character) %>%
#    nest(-row) %>%
#    mutate(value = map(data, ~ paste(.x$character, collapse = " ")),
#           col = 2L) %>%
#    select(row, col, value)
#  
#  tidy <- function(x) {
#    col_headers <-
#      x %>%
#      offset_N(cells, n = 1) %>%
#      extend_E(cells, 3) %>%
#      extend_S(cells, 2) %>%
#      filter(!is_blank) %>%
#      select(row, col, value = character) %>%
#      split(.$row)
#    datacells <-
#      x %>%
#      offset_S(cells, n = 2) %>%
#      extend_E(cells, 4) %>%
#      extend_S(cells,
#               boundary = ~ is.na(numeric),
#               edge = TRUE) %>%
#      filter(!is_blank) %>%
#      mutate(value = numeric) %>%
#      select(row, col, value)
#    datacells %>%
#      NNW(col_headers[[1]]) %>%
#      NNW(col_headers[[2]]) %>%
#      N(col_headers[[3]]) %>%
#      W(row_headers) # This is the only new line
#  }
#  
#  map_df(fixed_price, tidy)

