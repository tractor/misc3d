drawScene.rgl <- function(scene, add = FALSE, ...) {
    if (! rgl.cur())
        open3d()
    if (!add)
        rgl.clear()

    triangles <- canonicalizeAndMergeScene(scene, "color", "alpha",
                                           "col.mesh", "fill")
    col <- rep(triangles$color, each = 3)
    alpha <- rep(triangles$alpha, each = 3)
    fill <- rep(triangles$fill, each = 3)
    col.mesh <- rep(triangles$col.mesh, each = 3)
    data <- zipTriangles(triangles)
    if (all(fill)) {
        front <- "filled"
        back <- "filled"
    }
    else if (any(fill))
        ##**** handle these by splitting; OK if no alpha < 1
        stop(paste("for now rgl engine cannot handle mixed fill/wire",
                   "frame contours"))
    else {
        front <- "lines"
        back <- "lines"
        col <- col.mesh
    }
    oldstyle = FALSE #*** eventually make this a settable option.
    if (oldstyle) {
        data <- data[,c(1, 3, 2)]
        data[,3] <- -data[,3]
    }
    if (nrow(data) > 0) # to avoid a segfault in rgl
        rgl.triangles(data[,1], data[,2], data[,3],
                      col = col, alpha = alpha,
                      front = front, back = back, ...)
}
