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

parametric3d <- function(fx, fy, fz, u, v, umin, umax, vmin, vmax, n=100,
                         color = "white", color2 = NA, smooth = 0,
                         material = "default", alpha = 1, fill = TRUE,
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
                          color2 = color2, smooth = smooth,
                          material = material, alpha = alpha,
                          fill = fill, col.mesh = col.mesh)
    if (! draw || engine == "none")
        tris
    else {
        tris <- colorScene(tris)
        if (engine == "rgl")
            drawScene.rgl(tris, add = add, ...)
        else if (engine %in% c("standard", "grid"))
            drawScene(tris, add = add, engine = engine, ...)
        else stop(paste("unknown rendering engine:", engine))
    }
}
