#' Default colours for kataegis plot.
#'
#' Returns a named vector providing the default colours to use in kataegis
#' plots.
#'
#' @export
kataegis.colours <- function() {
  c('C>A' = 'darkblue',
    'C>G' = 'black',
    'C>T' = 'red',
    'T>A' = 'purple',
    'T>C' = 'yellow',
    'T>G' = 'green',
    'G>T' = 'darkblue',
    'G>C' = 'black',
    'G>A' = 'red',
    'A>T' = 'purple',
    'A>G' = 'yellow',
    'A>C' = 'green')
}
