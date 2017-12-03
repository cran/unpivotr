## ---- echo = TRUE--------------------------------------------------------
library(dplyr)
library(tidyxl)
library(unpivotr)

## ---- echo = TRUE--------------------------------------------------------
(original <- purpose$`NNW WNW`)
tail(cells <- tidy_table(original))

## ---- echo = TRUE--------------------------------------------------------
row_headers <-
  cells %>%
  filter(col <= 2, !is.na(chr)) %>% # Select all rows of headers at once
  select(row, col, header = chr) %>%
  split(.$col) # Return each row of headers in its own element of a list
row_headers

col_headers <-
  cells %>%
  filter(row <= 2, !is.na(chr)) %>%
  select(row, col, header = chr) %>%
  split(.$row)
col_headers

## ---- echo = TRUE--------------------------------------------------------
datacells <-
  cells %>%
  filter(row >= 3, col >= 3, !is.na(chr)) %>%
  mutate(value = as.integer(chr)) %>%
  select(row, col, value)
head(datacells)

## ---- echo = TRUE--------------------------------------------------------
datacells <-
  datacells %>%
  NNW(col_headers[[1]]) %>%
  N(col_headers[[2]]) %>%
  WNW(row_headers[[1]]) %>%
  W(row_headers[[2]])

## ---- echo = TRUE--------------------------------------------------------
datacells %>% as.data.frame
NNW_WNW <- datacells %>% arrange(row, col)

## ---- echo = TRUE--------------------------------------------------------
cells <- tidy_table(purpose$`NNE WSW`)

## ---- echo = TRUE--------------------------------------------------------
row_headers <-
  cells %>%
  filter(col <= 2, !is.na(chr)) %>%
  select(row, col, header = chr) %>%
  split(.$col)
row_headers

col_headers <-
  cells %>%
  filter(row <= 2, !is.na(chr)) %>%
  select(row, col, header = chr) %>%
  split(.$row)
col_headers

## ---- echo = TRUE--------------------------------------------------------
datacells <-
  cells %>%
  filter(row >= 3, col >= 3, !is.na(chr)) %>%
  mutate(value = as.integer(chr)) %>%
  select(row, col, value)
datacells

## ---- echo = TRUE--------------------------------------------------------
datacells <-
  datacells %>%
  NNE(col_headers[[1]]) %>% # Different from NNW WNW
  N(col_headers[[2]]) %>% # Same as NNW WNW
  WSW(row_headers[[1]]) %>% # Different from NNW WNW
  W(row_headers[[2]]) # Same as NNW WNW

## ---- echo = TRUE--------------------------------------------------------
datacells %>% as.data.frame
NNE_WSW <- datacells %>% arrange(row, col)

## ---- echo = TRUE--------------------------------------------------------
cells <- tidy_table(purpose$`SSE ESE`)

## ---- echo = TRUE--------------------------------------------------------
row_headers <-
  cells %>%
  filter(col >= 5, !is.na(chr)) %>%
  select(row, col, header = chr) %>%
  split(.$col)
row_headers

col_headers <-
  cells %>%
  filter(row >= 21, !is.na(chr)) %>%
  select(row, col, header = chr) %>%
  split(.$row)
col_headers

## ---- echo = TRUE--------------------------------------------------------
datacells <-
  cells %>%
  filter(row <= 20, col <= 4, !is.na(chr)) %>%
  mutate(value = as.integer(chr)) %>%
  select(row, col, value)
datacells

## ---- echo = TRUE--------------------------------------------------------
datacells <-
  datacells %>%
  SSE(col_headers[[2]]) %>%
  S(col_headers[[1]]) %>%
  ESE(row_headers[[2]]) %>%
  E(row_headers[[1]])

## ---- echo = TRUE--------------------------------------------------------
datacells %>% as.data.frame
SSE_ESE <- datacells %>% arrange(row, col)

## ---- echo = TRUE--------------------------------------------------------
cells <- tidy_table(purpose$`SSW ENE`)

## ---- echo = TRUE--------------------------------------------------------
row_headers <-
  cells %>%
  filter(col >= 5, !is.na(chr)) %>%
  select(row, col, header = chr) %>%
  split(.$col)
row_headers

col_headers <-
  cells %>%
  filter(row >= 21, !is.na(chr)) %>%
  select(row, col, header = chr) %>%
  split(.$row)
col_headers

## ---- echo = TRUE--------------------------------------------------------
datacells <-
  cells %>%
  filter(row <= 20, col <= 4, !is.na(chr)) %>%
  mutate(value = as.integer(chr)) %>%
  select(row, col, value)
datacells

## ---- echo = TRUE--------------------------------------------------------
datacells <-
  datacells %>%
  SSW(col_headers[[2]]) %>% # Different from SSE ESE
  S(col_headers[[1]]) %>% # Same as SSE ESE
  ENE(row_headers[[2]]) %>% # Different from SSE ESE
  E(row_headers[[1]]) # Same as SSE ESE

## ---- echo = TRUE--------------------------------------------------------
datacells %>% as.data.frame
SSW_ENE <- datacells %>% arrange(row, col)

## ---- echo = TRUE--------------------------------------------------------
identical(NNW_WNW, NNE_WSW)
identical(SSW_ENE, SSE_ESE)
identical(NNW_WNW[, -1:-2], SSW_ENE[, -1:-2])

## ---- echo = TRUE--------------------------------------------------------
cells <- tidy_table(purpose$`ABOVE LEFT`)

## ---- echo = TRUE--------------------------------------------------------
row_headers <-
  cells %>%
  filter(col <= 2, !is.na(chr)) %>%
  select(row, col, header = chr) %>%
  split(.$col)
row_headers

col_headers <-
  cells %>%
  filter(row <= 2, !is.na(chr)) %>%
  select(row, col, header = chr) %>%
  split(.$row)
col_headers

## ---- echo = TRUE--------------------------------------------------------
datacells <-
  cells %>%
  filter(row >= 3, col >= 3, !is.na(chr)) %>%
  mutate(value = as.integer(chr)) %>%
  select(row, col, value)
datacells

## ---- echo = TRUE--------------------------------------------------------
datacells <-
  datacells %>%
  ABOVE(col_headers[[1]]) %>% # Different from SSE ESE
  N(col_headers[[2]]) %>% # Same as SSE ESE
  LEFT(row_headers[[1]]) %>% # Different from SSE ESE
  W(row_headers[[2]]) # Same as SSE ESE

## ---- echo = TRUE--------------------------------------------------------
datacells %>% as.data.frame
ABOVE_LEFT <- datacells %>% arrange(row, col)

## ---- echo = TRUE--------------------------------------------------------
cells <- tidy_table(purpose$`BELOW RIGHT`)

## ---- echo = TRUE--------------------------------------------------------
row_headers <-
  cells %>%
  filter(col >= 7, !is.na(chr)) %>%
  select(row, col, header = chr) %>%
  split(.$col)
row_headers

col_headers <-
  cells %>%
  filter(row >= 11, !is.na(chr)) %>%
  select(row, col, header = chr) %>%
  split(.$row)
col_headers

## ---- echo = TRUE--------------------------------------------------------
datacells <-
  cells %>%
  filter(row <= 10, col <= 6, !is.na(chr)) %>%
  mutate(value = as.integer(chr)) %>%
  select(row, col, value)
datacells

## ---- echo = TRUE--------------------------------------------------------
datacells <-
  datacells %>%
  BELOW(col_headers[[2]]) %>%
  S(col_headers[[1]]) %>%
  RIGHT(row_headers[[2]]) %>%
  E(row_headers[[1]])

## ---- echo = TRUE--------------------------------------------------------
datacells %>% as.data.frame
BELOW_RIGHT <- datacells %>% arrange(row, col)

## ---- echo = TRUE--------------------------------------------------------
identical(ABOVE_LEFT[, -1:-2], BELOW_RIGHT[, -1:-2])

## ---- echo = TRUE--------------------------------------------------------
spreadsheet <- system.file("extdata/purpose.xlsx", package = "unpivotr")
cells <- tidy_xlsx(spreadsheet, "ABOVE LEFT border")$data[[1]]

## ---- echo = TRUE--------------------------------------------------------
# Same as ABOVE LEFT without borders
row_headers <-
  cells %>%
  filter(col <= 3, !is_blank) %>%
  select(row, col, header = character) %>%
  split(.$col)
row_headers

col_headers <-
  cells %>%
  filter(row <= 3, !is_blank) %>%
  select(row, col, header = character) %>%
  split(.$row)
col_headers

## ---- echo = TRUE--------------------------------------------------------
# Same as ABOVE LEFT without borders
datacells <-
  cells %>%
  filter(row >= 4, col >= 4, !is_blank) %>%
  mutate(content = ifelse(is.na(character), numeric, NA)) %>%
  mutate(value = as.integer(content)) %>%
  select(row, col, value)
datacells

## ---- echo = TRUE--------------------------------------------------------
# Different from ABOVE LEFT without borders
# Find cells with borders on the bottom, and ones with borders on the left
formatting <- tidy_xlsx(spreadsheet)$formats
left_borders <- which(!is.na(formatting$local$border$left$style))
top_borders <- which(!is.na(formatting$local$border$top$style))

left_border_cells <-
  cells %>%
  filter(row == 2, local_format_id %in% left_borders) %>%
  select(row, col)
top_border_cells <-
  cells %>%
  filter(col == 2, local_format_id %in% top_borders) %>%
  select(row, col)

## ---- echo = TRUE--------------------------------------------------------
datacells <-
  datacells %>%
  ABOVE(col_headers[[1]], left_border_cells) %>% # Different from ABOVE LEFT
  N(col_headers[[2]]) %>% # Same as ABOVE LEFT
  LEFT(row_headers[[1]], top_border_cells) %>% # Different from ABOVE LEFT
  W(row_headers[[2]]) # Same as ABOVE LEFT

## ---- echo = TRUE--------------------------------------------------------
datacells %>% as.data.frame
ABOVE_LEFT_borders <- datacells %>% arrange(row, col)

## ---- echo = TRUE--------------------------------------------------------
cells <- tidy_xlsx(spreadsheet, "BELOW RIGHT border")$data[[1]]

## ---- echo = TRUE--------------------------------------------------------
# Same as BELOW RIGHT without borders
row_headers <-
  cells %>%
  filter(col >= 10, !is_blank) %>%
  select(row, col, header = character) %>%
  split(.$col)
row_headers

col_headers <-
  cells %>%
  filter(row >= 14, !is_blank) %>%
  select(row, col, header = character) %>%
  split(.$row)
col_headers

## ---- echo = TRUE--------------------------------------------------------
# Same as BELOW RIGHT without borders
datacells <-
  cells %>%
  filter(row <= 13, col <= 9, !is_blank) %>%
  mutate(content = ifelse(is.na(character), numeric, NA)) %>%
  mutate(value = as.integer(content)) %>%
  select(row, col, value)
datacells

## ---- echo = TRUE--------------------------------------------------------
# Different from BELOW RIGHT without borders
# Find cells with borders on the bottom, and ones with borders on the left
formatting <- tidy_xlsx(spreadsheet)$formats
left_borders <- which(!is.na(formatting$local$border$left$style))
top_borders <- which(!is.na(formatting$local$border$top$style))

left_border_cells <-
  cells %>%
  filter(row == 15, local_format_id %in% left_borders) %>%
  select(row, col)
top_border_cells <-
  cells %>%
  filter(col == 11, local_format_id %in% top_borders) %>%
  select(row, col)

## ---- echo = TRUE--------------------------------------------------------
datacells <-
  datacells %>%
  BELOW(col_headers[[2]], left_border_cells) %>% # Different from BELOW RIGHT
  S(col_headers[[1]]) %>% # Same as BELOW RIGHT
  RIGHT(row_headers[[2]], top_border_cells) %>% # Different from BELOW RIGHT
  E(row_headers[[1]]) # Same as BELOW RIGHT

## ---- echo = TRUE--------------------------------------------------------
datacells %>% as.data.frame
BELOW_RIGHT_borders <- datacells %>% arrange(row, col)

## ---- echo = TRUE--------------------------------------------------------
identical(ABOVE_LEFT[, -1:-2], BELOW_RIGHT[, -1:-2])
identical(ABOVE_LEFT[, -1:-2], ABOVE_LEFT_borders[, -1:-2])
identical(ABOVE_LEFT[, -1:-2], BELOW_RIGHT_borders[, -1:-2])

