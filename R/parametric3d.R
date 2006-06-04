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

expandTriangleGrid <- function(x, y) {
    nx <- length(x) - 1
    ny <- length(y) - 1
    A <- c(0, 0)
    B <- c(1, 0)
    C <- c(1, 1)
    D <- c(0, 1)
    g <- expand.grid(x = 1 : nx, y = 1 : ny)
    even <- (g$x + g$y) %% 2 == 0
    gx11 <- ifelse(even, g$x + A[1], g$x + A[1])
    gy11 <- ifelse(even, g$y + A[2], g$y + A[2])
    gx12 <- ifelse(even, g$x + A[1], g$x + B[1])
    gy12 <- ifelse(even, g$y + A[2], g$y + B[2])
    i1 <- rbind(cbind(gx11, gy11), cbind(gx12, gy12))
    gx21 <- ifelse(even, g$x + B[1], g$x + B[1])
    gy21 <- ifelse(even, g$y + B[2], g$y + B[2])
    gx22 <- ifelse(even, g$x + C[1], g$x + C[1])
    gy22 <- ifelse(even, g$y + C[2], g$y + C[2])
    i2 <- rbind(cbind(gx21, gy21), cbind(gx22, gy22))
    gx31 <- ifelse(even, g$x + C[1], g$x + D[1])
    gy31 <- ifelse(even, g$y + C[2], g$y + D[2])
    gx32 <- ifelse(even, g$x + D[1], g$x + D[1])
    gy32 <- ifelse(even, g$y + D[2], g$y + D[2])
    i3 <- rbind(cbind(gx31, gy31), cbind(gx32, gy32))
    v1 <- cbind(x[i1[,1]], y[i1[,2]])
    v2 <- cbind(x[i2[,1]], y[i2[,2]])
    v3 <- cbind(x[i3[,1]], y[i3[,2]])
    list(v1 = v1, v2 = v2, v3 = v3)
}

parametric3d <- function(fx, fy, fz, u, v, umin, umax, vmin, vmax, n=100,
                         color = "white", alpha = 1, fill = TRUE,
                         col.mesh = if (fill) NA else color,
                         add = FALSE, draw = TRUE, engine = "rgl", ...){
    ##**** handle other args
    ##**** quads would be better
    if (missing(u)) u <- seq(umin, umax, len=n)
    if (missing(v)) v <- seq(vmin, vmax, len=n)
    tg <- expandTriangleGrid(u, v)
    f <- function(uv)
        cbind(fx(uv[,1], uv[,2]), fy(uv[,1], uv[,2]), fz(uv[,1], uv[,2]))
    v1 <- f(tg$v1)
    v2 <- f(tg$v2)
    v3 <- f(tg$v3)
    na1 <- is.na(v1[,1]) | is.na(v1[,2]) | is.na(v1[,3])
    na2 <- is.na(v2[,1]) | is.na(v2[,2]) | is.na(v2[,3])
    na3 <- is.na(v3[,1]) | is.na(v3[,2]) | is.na(v3[,3])
    nna <- ! (na1 | na2 | na3)
    tris <- makeTriangles(v1[nna,], v2[nna,], v3[nna,], color = color,
                          alpha = alpha, fill = fill, col.mesh = col.mesh)
    if (! draw || engine == "none")
        tris
    else {
        tris <- colorScene(tris)
        if (engine == "rgl")
            drawScene.rgl(tris, add = add, ...)
        else stop(paste("unknown rendering engine:", engine))
    }
}
