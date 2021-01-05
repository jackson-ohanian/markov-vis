require("ggplot2")
require("ggforce")
require("animation")
require("gganimate")
require("gifski")
#' DF to Markov DF
#'
#' Find the markov chain matrix for the given input dataframe
#' @param data a dataframe holding inputs / outputs - reformats as markov
#' @return A markov data frame
#' @export
to.markov <- function(data) {
  data = t(data)/colSums(data)
  return(t(data))
}

#' Find the steady state of the Markov Matrix via simulation
#' @param data a markov style dataframe
#' @param itts the number of iterations to perform
#' @param x0 an initial state of the chain
#' @return The simulated steady state vector
#' @export
find.steady <- function(data, itts, x0) {
  for (i in 1:itts) {
    x0 = data %*% x0
  }
  return(x0)
}

animate.markov <- function(data, itts, x0) {
  x <- data.frame(length(data[0]), itts)
  plots.frames <- c(itts)

  ## CALCULATE all steps of steady state simulation and save to DF
  for (i in 1:itts) {
    x0 = data %*% x0
      for (p in 1:length(x0)) {
        x[p, i] = x0[p]
      }
    }
    ### RENDER frames itt. with respect to a maximum size of 1.0
    x.x <- 1:length(x0)
    x.y <- rep(0.5, length(x0))
    plt <- ggplot() + coord_fixed()
    anim <- saveGIF({
    for (xi in 1:length(x)) {
      x.r <- as.vector(x[,xi])
      circles <- data.frame(
        x0 = x.x,
        y0 = x.y,
        r = x.r)

      plt = plt + geom_circle(aes(x0 = x0, y0 = y0, r = r^0.5, fill = r), data = circles)
      print((plt))
      plt <- ggplot() + coord_fixed()
    }
    }, clean=TRUE, convert="magick", movie.name="out.gif")
  
  
  ## ANIMATE
  return(anim)
}




## DRIVER TO DEL TODO DEL

df <- data.frame(c(1, 200, 0), c(1, 4, 3), c(2, 2, 4))
df

mk = to.markov(df)
mk
find.steady(mk, 100, c(1, 0, 0))
plotdat <- animate.markov(mk, 100, c(1,0,0))
animate(plotdat, renderer = gifski_renderer(), end_pause = 30)

