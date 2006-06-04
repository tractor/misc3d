makeTriangles <- function(v1, v2, v3, 
                          color = "red", color2 = NA, 
                          fill = TRUE, material = "default", alpha = 1,
                          col.mesh = if (fill) NA else "black",
	                  phong = 0) {
    if (missing(v2) || missing(v3)) {
        if (missing(v2) && missing(v3))
  	    v <- unzipTriangleMatrix(v1)
        else if (missing(v3))
            v <- ve2t(list(vb = v1, ib = v2))
        else stop("unknown form of triangle specification")
	v1 <- v$v1
	v2 <- v$v2
	v3 <- v$v3
    }
    structure(list(v1 = v1, v2 = v2, v3 = v3,
                   color = color, color2 = color2, fill = fill,
                   material = material, col.mesh = col.mesh, alpha = alpha,
                   phong = phong),
              class = "Triangles3D")
}

is.Triangles3D <- function(x) identical(class(x), "Triangles3D")

ve2t <- function(ve) {
    list (v1 = t(ve$vb[,ve$ib[1,]]),
          v2 = t(ve$vb[,ve$ib[2,]]),
          v3 = t(ve$vb[,ve$ib[3,]]))
}

unzipTriangleMatrix <- function(tris) {
    if (ncol(tris) != 3)
        stop("triangle matrix must have three columns.")
    if (nrow(tris) %% 3 != 0)
        stop("number of rows in triangle matrix must be divisible by 3")
    n <- nrow(tris) / 3
    list(v1 = tris[3 * (1 : n) - 2,],
         v2 = tris[3 * (1 : n) - 1,],
         v3 = tris[3 * (1 : n),])
}

zipTriangles <- function(tris) {
    n <- nrow(tris$v1)
    if (nrow(tris$v2) != n || nrow(tris$v3) != n)
        stop("vertex arrays must have the same number of rows")
    v <- matrix(0, nrow = 3 * n, ncol = 3)
    v[3 * (1 : n) - 2,] <- tris$v1
    v[3 * (1 : n) - 1,] <- tris$v2
    v[3 * (1 : n),] <- tris$v3
    v
}

colorTriangles <- function(triangles) {
    if (is.function(triangles$color) || is.function(triangles$color2)) {
        v <- (triangles$v1 + triangles$v2 + triangles$v3) / 3
        if (is.function(triangles$color))
            triangles$color <- triangles$color(v[,1], v[,2], v[,3])
        if (is.function(triangles$color2))
            triangles$color2 <- triangles$color2(v[,1], v[,2], v[,3])
        if (is.function(triangles$col.mesh))
            triangles$col.mesh <- triangles$col.mesh(v[,1], v[,2], v[,3])
    }
    triangles
}

colorScene <- function(scene) {
    if (is.Triangles3D(scene))
        colorTriangles(scene)
    else lapply(scene, colorTriangles)
}

## **** better to make new triangles including only requested components?
canonicalizeAndMergeScene <- function(scene, ...) {
    which <- list(...)
    if (is.Triangles3D(scene)) {
        n.tri <- nrow(scene$v1)
        for (n in which)
            if (length(scene[[n]]) != n.tri)
                scene[[n]] <- rep(scene[[n]], length = n.tri)
        scene
    }
    else {
        scene <- lapply(scene, canonicalizeAndMergeScene, ...)
        x <- scene[[1]]
        x$v1 <- do.call(rbind, lapply(scene, function(x) x$v1))
        x$v2 <- do.call(rbind, lapply(scene, function(x) x$v2))
        x$v3 <- do.call(rbind, lapply(scene, function(x) x$v3))
        for (n in which)
            x[[n]] <- do.call(c, lapply(scene, function(x) x[[n]]))
        x
    }
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
