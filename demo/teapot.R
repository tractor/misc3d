library(misc3d)

local({
    data(teapot)

    ttri <- misc3d:::makeTriangles(teapot$vertices, teapot$edges,
                                   color = "red", color2 = "green")

    misc3d:::drawScene.rgl(ttri)

#    drawScene.rgl(list(ttri,translateTriangles(ttri,z=4)))

#    drawScene.rgl(list(updateTriangles(ttri,color=heat.colors(ncol(edges))),
#              translateTriangles(ttri,z=4)))

#    drawScene.rgl(list(updateTriangles(ttri,alpha = 0.5,color="blue"),
#              scaleTriangles(ttriMetal, 0.6)))
})
