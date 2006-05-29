findNonFinite3d <- function(x, y, z, group) {
  n <- length(x) / group
  nf <- ! (is.finite(x) & is.finite(y) & is.finite(z))
  if (any(nf)) {
    idxg <- group * (1 : n)
    if (group == 2)
      rep(nf[idxg] | nf[idxg - 1], rep(2, n))
    else if (group == 3)
      rep(nf[idxg] | nf[idxg - 1] | nf[idxg - 2], rep(3, n))
    else if (group == 4)
      rep(nf[idxg] | nf[idxg - 1] | nf[idxg - 2] | nf[idxg - 3], rep(4, n))
    else stop("unknown group size for NA grouping")
  }
  else nf
}

parametric3d <- function(fx, fy, fz, umin, umax, vmin, vmax, n=100,
                         add = FALSE,...){
    origion.u <- seq(umin, umax, len=n)
    origion.v <- seq(vmin, vmax, len=n)

    if (add==FALSE) rgl.clear()
        
    off1 <- c(0, 0, 1, 1)
    off2 <- c(0, 1, 1, 0)
    ppm1 <- n - 1
    gridu <- rep(1 : ppm1, rep(4 * (n - 1), n - 1)) + off1
    gridv <- rep(rep(1 : (n - 1), n - 1), rep(4, (n - 1) ^ 2)) + off2
    u <- origion.u[gridu]
    v <- origion.v[gridv]
      
    x <- fx(u,v)
    y <- fy(u,v)
    z <- fz(u,v)

    nf <- findNonFinite3d(x, y, z, 4)
    if (any(nf)) {
      x <- x[! nf]
      y <- y[! nf]
      z <- z[! nf]
    }
    rgl.quads(x, z, -y, ...)
}
