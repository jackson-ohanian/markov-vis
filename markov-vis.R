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

#' FQuick function to simply plot the steady state found with find.steady
#' @param data the (1xN) steady state vector of a Markov Matrix
#' @param col the colors of the plot (vector-like)
#' @param markov a df-like (must be coercible by fortify) original markov
#' @return a plot showing the column-wise proportion of final states
#' @export
plot.steady <- function(data, col = geom_col(), markov) {
    ggplot(aes(x=1:length(data), y=data), data=markov)+
    geom_col() + 
    coord_flip() + ylab("Proportion After Simulation") + 
    xlab("State (appearance order)") + ggtitle("Steady State")
}

#' Visualize the steady state calculation 
#' @param data a markov style dataframe (NxN)
#' @param itts the number of iterations to perform
#' @param x0 an initial state of the chain
#' @return The simulated steady state vector
#' @export
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

    return(anim)
}

#' Visualize the steady state result
#' @param data a markov style dataframe (NxN)
#' @return Quick plot of the steady state
#' @export
visual.markov <- function(data) {
  n <- length(data)^0.5
  print(n)
  x.val <- rep(0, n)
  y.val <- rep(0, n)
  
  ### PLOT equidistant around a circle
  x0 = 50
  y0 = 50
  r = 1
  for (i in 0:n-1) {
    x.val[i+1] = x0 + r * cos(2 * pi * i / n)
    y.val[i+1] = y0 + r * sin(2 * pi * i / n)
  }
  
  r.val = rep(0, n)
  r.val[1] = 1
  circles <- data.frame(
    x0 = x.val,
    y0 = y.val,
    r = find.steady(data, 100, r.val))
  plt <- ggplot() + coord_fixed()
  ### Plot Circles and Labels
  plt = plt + geom_circle(aes(x0 = x0, y0 = y0, r = r, fill = r, alpha=0.1), data = circles) + 
    geom_label(aes(x = x0, y = y0, label = 1:n, fill = r, alpha=0.1), data = circles)
  i <- 0:(n-1)
  j <- 0:(n-1)
  for (i in 1:n) {
    for (j in 1:n) {
      plt <- plt + geom_segment(aes(x = x.val[i], y = y.val[i], xend = x.val[j], yend = y.val[j]), inherit.aes = TRUE)
    }
  }
  plt <- plt + xlab("") + ylab("") + theme(axis.ticks.y=element_blank(),axis.ticks.x=element_blank()) + ggtitle("Steady State")
  return(plt)
}
