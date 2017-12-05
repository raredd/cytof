## fns

fdl_split <- function(data, group = 'Cluster',
                      split = 'Group', legend = 'topright') {
  sp <- split(data, data[, split])
  
  op <- par(oma = c(0,0,0,0))
  on.exit(par(op))
  
  lapply(seq_along(sp), function(ii) {
    x <- sp[[ii]]
    
    fdl(data, group, FALSE, 'grey50')
    par(new = TRUE)
    fdl(x, group, legend, NULL)
    
    mtext(names(sp)[ii], at = grconvertX(0, 'nfc'),
          adj = -1, line = 2.5, font = 2L, cex = 1.5)
  })
  
  invisible(NULL)
}

fdl <- function(data, group = 'Cluster', legend = 'topright', col = NULL) {
  var <- if (length(var <- grep(group, names(data), value = TRUE)) > 1L)
    grep(sprintf('\\b%s\\b', group), names(data), value = TRUE)
  else if (!length(var))
    'Cluster' else var
  
  data <- data[order(data[, var]), ]
  fvar <- as.factor(data[, var])
  
  colors <- if (!(ok <- nlevels(fvar) < 50L))
    c('blue', 'cyan', 'green', 'yellow', 'red') else NULL
  nc <- seq_along(colors)
  
  op <- par(mar = c(0, 0, 0, 0))
  on.exit(par(op))
  
  plot(Y ~ X, data, type = 'n', ann = FALSE, axes = FALSE, bty = 'n')
  
  if (!is.null(col))
    points(Y ~ X, data, pch = '.', cex = 2,
           col = adjustcolor(col[1L], alpha.f = .5))
  else
    points(
      Y ~ X, data, pch = '.', cex = 2,
      col = if (ok)
        fvar else {
          ## interpolate colors based on continuous values
          n    <- 100000L
          from <- range(data[, var], na.rm = TRUE)
          to <- 0:1 * n
          x  <- (data[, var] - from[1L]) / diff(from) * diff(to) + to[1L]
          colorRampPalette(colors)(n + 1L)[as.integer(x) + 1L]
        }
    )
  
  if (!identical(legend, FALSE))
    legend(
      legend, pch = 16L, ncol = 2L, title = var,
      col = if (ok)
        seq_along(levels(fvar)) else colors,
      legend = if (ok)
        levels(fvar) else
          sprintf('%.1f', quantile(data[, var], (nc - 1L) / diff(range(nc))))
    )
  
  invisible(NULL)
}
