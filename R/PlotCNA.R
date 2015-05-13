#' @import grid
PlotCNA <- function(segments.gr) {
  grid.text('CNA', -0.05, 0.5, rot = 90)
  grid.lines(c(0, 1), c(0.5, 0.5), gp = gpar(col = 'grey'))
  for (i in 1:length(segments.gr)) {
    copy <- segments.gr[i]$copy
    begin <- start(segments.gr[i])
    end <- end(segments.gr[i])
    if (copy != 2) {

      colour <- if (copy > 7)
	'orange'
      else if (copy > 2)
	'darkred'
      else if (copy < 2)
	'darkblue'
      # else if (mallele != 1)
      #   'black'
      else
	'grey'

      y <- if (copy == 7)
	0.9 - 0.5
      else if (copy == 6)
	0.82 - 0.5
      else if (copy == 5)
	0.74 - 0.5
      else if (copy == 4)
	0.66 - 0.5
      else if (copy == 3)
	0.58 - 0.5
      else if (copy == 1)
	0.42
      else if (copy == 0)
	0.34

      width <- end - begin
      # message(sprintf("%s - %s\t%s\t%s", begin, end, width, copy))
      grid.rect(
	# begin, 0.5, width, 0.25,
	begin, 0.5, width, y,
	default.units = 'native',
	just = c('left', 'bottom'),
	gp = gpar(col = colour, fill = colour)
      )
    } else {
      grid.lines(
	c(begin, end), c(0.5, 0.5), 
	default.units = 'native',
	gp = gpar(col = 'blue', lwd = 1)
      )
    }
  }
}
