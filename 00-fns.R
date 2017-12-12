## fns

fdl_split <- function(data, group = 'ClusterID', split = 'Group',
                      legend = list(), reset_par = TRUE) {
  sp <- split(data, data[, split])
  gr <- grep(sprintf('\\b%s\\b', group), names(data))
  xr <- range(data[, gr], na.rm = TRUE)
  
  op <- par(oma = c(0,0,0,0))
  if (reset_par)
    on.exit(par(op))
  
  lapply(seq_along(sp), function(ii) {
    x <- sp[[ii]]
    
    fdl(data, group, FALSE, 'grey50', xr)
    par(new = TRUE)
    fdl(x, group, FALSE, NULL, xr, FALSE)
    
    mtext(names(sp)[ii], at = grconvertX(0, 'nfc'),
          adj = -1, line = -2, font = 2L, cex = 1.5)
  })
  
  cc <- c('blue', 'cyan', 'green', 'yellow', 'red')
  nc <- seq_along(cc)
  largs <- list(
    x = 'topright', pch = 16L, title = gsub(':.*', '', group), xpd = NA,
    col = cc,
    legend = sprintf('%.1f', quantile(data[, gr], (nc - 1L) / diff(range(nc))))
  )
  do.call('legend', modifyList(largs, legend))
  
  invisible(NULL)
}

fdl <- function(data, group = 'ClusterID', legend = list(),
                col = NULL, from = NULL, reset_par = TRUE) {
  ## interpolate colors based on continuous values
  interp <- function(x, col, alpha = FALSE, n = 1e5L,
                     fr = NULL, to = 0:1) {
    fr <- if (is.null(fr))
      range(x, na.rm = TRUE) else fr
    to <- to[1:2] * if (alpha) 1 else n
    x  <- (x - fr[1L]) / diff(fr) * diff(to) + to[1L]
    
    if (alpha)
      Vectorize(adjustcolor)(col[1L], alpha.f = x)
    else colorRampPalette(col)(n + 1L)[as.integer(x) + 1L]
  }
  
  var <- if (is.null(group))
    'ClusterID'
  else if (length(var <- grep(group, names(data), value = TRUE)) > 1L)
    grep(sprintf('\\b%s\\b', group), names(data), value = TRUE)
  else if (!length(var))
    'ClusterID'
  else var
  
  data <- data[order(data[, var]), ]
  fvar <- as.factor(data[, var])
  
  colors <- if (!(ok <- nlevels(fvar) < 50L))
    c('blue', 'cyan', 'green', 'yellow', 'red') else 'grey50'
  if (!is.null(col))
    colors <- c('transparent', col)
  nc <- seq_along(colors)
  
  op <- par(mar = c(0, 0, 0, 0))
  if (reset_par)
    on.exit(par(op))
  
  plot(Y ~ X, data, type = 'n', ann = FALSE, axes = FALSE, bty = 'n')
  
  if (is.null(group))
    points(Y ~ X, data, pch = '.', cex = 2,
           col = adjustcolor(colors[1L], alpha.f = 0.5))
  else
    points(Y ~ X, data, pch = '.', cex = 2,
           col = if (ok) fvar else interp(data[, var], colors, fr = from))
  
  if (!identical(legend, FALSE) && !is.null(group)) {
    largs <- list(
      x = 'topright', pch = 16L, title = gsub(':.*', '', var),
      col = if (ok)
        seq_along(levels(fvar)) else colors,
      legend = if (ok)
        levels(fvar) else
          sprintf('%.1f', quantile(data[, var], (nc - 1L) / diff(range(nc))))
    )
    do.call('legend', modifyList(largs, legend))
  }
  
  invisible(NULL)
}

km <- function(data, centers = length(unique(data$ClusterID)),
               spider = TRUE, hull = TRUE, groups = NULL, col = 'grey') {
  suppressPackageStartupMessages(
    require('vegan', character.only = TRUE, quietly = TRUE)
  )
  
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  set.seed(1)
  xy <- as.matrix(data[, c('X', 'Y')])
  km <- kmeans(xy, centers, 1e5L)
  cl <- factor(km$cluster)
  
  if (spider)
    ordispider(xy, factor(km$cluster, groups %||% levels(cl)),
               label = TRUE, col = adjustcolor(col, alpha.f = 0.1))
  if (hull)
    ordihull(xy, factor(km$cluster, groups %||% levels(cl)),
             lty = 'dashed', col = col)
  
  invisible(km)
}
