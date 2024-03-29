% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/behead.R
\name{behead}
\alias{behead}
\alias{behead_if}
\title{Strip a level of headers from a pivot table}
\usage{
behead(
  cells,
  direction,
  name,
  values = NULL,
  types = data_type,
  formatters = list(),
  drop_na = TRUE
)

behead_if(
  cells,
  ...,
  direction,
  name,
  values = NULL,
  types = data_type,
  formatters = list(),
  drop_na = TRUE
)
}
\arguments{
\item{cells}{Data frame. The cells of a pivot table, usually the output of
\code{\link[=as_cells]{as_cells()}} or \code{\link[tidyxl:xlsx_cells]{tidyxl::xlsx_cells()}}, or of a subsequent operation on
those outputs.}

\item{direction}{The direction between a data cell and its header, one of
\code{"up"}, \code{"right"}, \code{"down"}, \code{"left"}, \code{"up-left"}, \code{"up-right"},
\code{"right-up"}, \code{"right-down"}, \code{"down-right"}, \code{"down-left"}, \code{"left-down"},
\code{"left-up"}.  See \code{?direction}.  \code{"up-ish"}, \code{"down-ish"}, \code{"left-ish"} and
\code{"right-ish"} aren't available because they require certain ambiguities that
are better handled by using \code{\link[=enhead]{enhead()}} directly rather than via \code{\link[=behead]{behead()}}.}

\item{name}{A name to give the new column that will be created, e.g.
\code{"location"} if the headers are locations.  Quoted (\code{"location"}, not
\code{location}) because it doesn't refer to an actual object.}

\item{values}{Optional. The column of \code{cells} to use as the values of each
header.  Given as a bare variable name.  If omitted (the default), the
\code{types} argument will be used instead.}

\item{types}{The name of the column that names the data type of each cell.
Usually called \code{data_types} (the default), this is a character column that
names the other columns in \code{cells} that contain the values of each cell.
E.g.  a cell with a character value will have \code{"character"} in this column.
Unquoted(\code{data_types}, not \code{"data_types"}) because it refers to an actual
object.}

\item{formatters}{A named list of functions for formatting each data type in
a set of headers of mixed data types, e.g. when some headers are dates and
others are characters.  These can be given as \code{character = toupper} or
\code{character = ~ toupper(.x)}, similar to \link[purrr:map]{purrr::map}.}

\item{drop_na}{logical Whether to filter out headers that have \code{NA} in the
\code{value} column.  Default: \code{TRUE}.  This can happen with the output of
\code{tidyxl::xlsx_cells()}, when an empty cell exists because it has formatting
applied to it, but should be ignored.}

\item{...}{Passed to \link[dplyr:filter]{dplyr::filter}. logical predicates defined in terms of
the variables in \code{.data}.  Multiple conditions are combined with \code{&}. Only
rows where the condition evaluates to \code{TRUE} are kept.

The arguments in \code{...} are automatically \link[rlang:defusing-advanced]{quoted} and
\link[rlang:eval_tidy]{evaluated} in the context of the data frame. They
support \link[rlang:topic-inject]{unquoting} and splicing. See the dplyr
\code{vignette("programming")} for an introduction to these concepts.}
}
\value{
A data frame
}
\description{
\code{\link[=behead]{behead()}} takes one level of headers from a pivot table and
makes it part of the data.  Think of it like \code{\link[tidyr:gather]{tidyr::gather()}}, except that
it works when there is more than one row of headers (or more than one column
of row-headers), and it only works on tables that have first come through
\code{\link[=as_cells]{as_cells()}} or \code{\link[tidyxl:xlsx_cells]{tidyxl::xlsx_cells()}}.
}
\examples{
# A simple table with a row of headers
(x <- data.frame(a = 1:2, b = 3:4))

# Make a tidy representation of each cell
(cells <- as_cells(x, col_names = TRUE))

# Strip the cells in row 1 (the original headers) and use them as data
behead(cells, "N", foo)

# More complex example: pivot table with several layers of headers
(x <- purpose$`up-left left-up`)

# Make a tidy representation
cells <- as_cells(x)
head(cells)
tail(cells)

# Strip the headers and make them into data
tidy <-
  cells \%>\%
  behead("up-left", Sex) \%>\%
  behead("up", `Sense of purpose`) \%>\%
  behead("left-up", `Highest qualification`) \%>\%
  behead("left", `Age group (Life-stages)`) \%>\%
  dplyr::mutate(count = as.integer(chr)) \%>\%
  dplyr::select(-row, -col, -data_type, -chr)
head(tidy)

# Check against the provided 'tidy' version of the data.
dplyr::anti_join(tidy, purpose$Tidy)

# The provided 'tidy' data is missing a row for Male 15-24-year-olds with a
# postgraduate qualification and a sense of purpose between 0 and 6.  That
# seems to have been an oversight by Statistics New Zealand.

cells <- tibble::tribble(
       ~X1, ~adult, ~juvenile,
    "LION",    855,       677,
    "male",    496,       322,
  "female",    359,       355,
   "TIGER",    690,       324,
    "male",    381,       222,
  "female",    309,       102
  )
cells <- as_cells(cells, col_names = TRUE)

cells \%>\%
  behead_if(chr == toupper(chr), direction = "left-up", name = "species") \%>\%
  behead("left", "sex") \%>\%
  behead("up", "age") \%>\%
  dplyr::select(species, sex, age, population = dbl)
}
