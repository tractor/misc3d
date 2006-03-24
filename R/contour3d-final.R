#Try to shatter the contour
contour3d <- function (f, level, x = 1:dim(f)[1], y = 1:dim(f)[2], z = 1:dim(f)[3],
                       effect, markx=0,marky=0,markz=0)
 
{
 
    PreProcessing <- local({
        implode <- function(x) sum(2^(0:7) * x) + 1
        explode <- function(x) floor(((x - 1)%%2^(1:8))/2^(0:7))
        flip <- function(x) implode(ifelse(explode(x) == 1, 0, 
            1))
        BasicRotation <- matrix(data = c(1, 2, 3, 4, 5, 6, 7, 
            8, 5, 6, 2, 1, 8, 7, 3, 4, 8, 7, 6, 5, 4, 3, 2, 1, 
            4, 3, 7, 8, 1, 2, 6, 5, 2, 6, 7, 3, 1, 5, 8, 4, 6, 
            5, 8, 7, 2, 1, 4, 3, 5, 1, 4, 8, 6, 2, 3, 7, 4, 1, 
            2, 3, 8, 5, 6, 7, 3, 4, 1, 2, 7, 8, 5, 6, 2, 3, 4, 
            1, 6, 7, 8, 5, 6, 7, 3, 2, 5, 8, 4, 1, 7, 8, 4, 3, 
            6, 5, 1, 2, 8, 5, 1, 4, 7, 6, 2, 3, 7, 3, 2, 6, 8, 
            4, 1, 5, 4, 8, 5, 1, 3, 7, 6, 2, 3, 2, 6, 7, 4, 1, 
            5, 8, 2, 1, 5, 6, 3, 4, 8, 7, 1, 4, 8, 5, 2, 3, 7, 
            6, 1, 5, 6, 2, 4, 8, 7, 3, 5, 8, 7, 6, 1, 4, 3, 2, 
            8, 4, 3, 7, 5, 1, 2, 6, 3, 7, 8, 4, 2, 6, 5, 1, 7, 
            6, 5, 8, 3, 2, 1, 4, 6, 2, 1, 5, 7, 3, 4, 8), ncol = 8, 
            byrow = TRUE)
        CaseRotation <- matrix(data = c(1, 24, 2, 19, 2, 17, 
            3, 17, 2, 24, 4, 24, 3, 24, 6, 10, 2, 15, 3, 19, 
            4, 17, 6, 9, 3, 9, 6, 8, 6, 1, 9, 23, 2, 20, 3, 18, 
            4, 7, 6, 16, 5, 24, 7, 5, 7, 24, 12, 9, 4, 20, 6, 
            22, 8, 24, 10, 24, 7, 9, 15, 24, 13, 20, 6, 20, 2, 
            21, 4, 6, 3, 16, 6, 4, 4, 16, 8, 23, 6, 14, 10, 23, 
            5, 21, 7, 10, 7, 16, 15, 9, 7, 2, 13, 8, 12, 23, 
            6, 6, 3, 6, 6, 17, 6, 18, 9, 18, 7, 4, 13, 17, 15, 
            18, 6, 13, 7, 6, 12, 16, 13, 18, 6, 2, 11, 24, 7, 
            3, 7, 12, 3, 12, 2, 23, 5, 23, 4, 23, 7, 1, 3, 14, 
            7, 14, 6, 21, 15, 23, 4, 15, 7, 19, 8, 19, 13, 23, 
            6, 11, 12, 17, 10, 19, 6, 23, 4, 12, 7, 18, 8, 22, 
            13, 16, 7, 13, 11, 23, 13, 21, 7, 15, 8, 21, 13, 
            22, 14, 24, 8, 15, 13, 11, 7, 7, 8, 12, 4, 22, 3, 
            23, 7, 23, 6, 24, 12, 18, 6, 7, 13, 19, 9, 24, 6, 
            19, 7, 21, 11, 18, 13, 24, 7, 20, 15, 16, 7, 22, 
            6, 15, 3, 22, 6, 3, 15, 17, 10, 22, 6, 12, 12, 24, 
            7, 11, 6, 5, 3, 15, 13, 10, 7, 8, 8, 20, 4, 9, 7, 
            17, 5, 22, 4, 18, 2, 22, 2, 22, 4, 18, 5, 22, 7, 
            17, 4, 9, 8, 20, 7, 8, 13, 10, 3, 15, 6, 5, 7, 11, 
            12, 24, 6, 12, 10, 22, 15, 17, 6, 3, 3, 22, 6, 15, 
            7, 22, 15, 16, 7, 20, 13, 24, 11, 18, 7, 21, 6, 19, 
            9, 24, 13, 19, 6, 7, 12, 18, 6, 24, 7, 23, 3, 23, 
            4, 22, 8, 12, 7, 7, 13, 11, 8, 15, 14, 24, 13, 22, 
            8, 21, 7, 15, 13, 21, 11, 23, 7, 13, 13, 16, 8, 22, 
            7, 18, 4, 12, 6, 23, 10, 19, 12, 17, 6, 11, 13, 23, 
            8, 19, 7, 19, 4, 15, 15, 23, 6, 21, 7, 14, 3, 14, 
            7, 1, 4, 23, 5, 23, 2, 23, 3, 12, 7, 12, 7, 3, 11, 
            24, 6, 2, 13, 18, 12, 16, 7, 6, 6, 13, 15, 18, 13, 
            17, 7, 4, 9, 18, 6, 18, 6, 17, 3, 6, 6, 6, 12, 23, 
            13, 8, 7, 2, 15, 9, 7, 16, 7, 10, 5, 21, 10, 23, 
            6, 14, 8, 23, 4, 16, 6, 4, 3, 16, 4, 6, 2, 21, 6, 
            20, 13, 20, 15, 24, 7, 9, 10, 24, 8, 24, 6, 22, 4, 
            20, 12, 9, 7, 24, 7, 5, 5, 24, 6, 16, 4, 7, 3, 18, 
            2, 20, 9, 23, 6, 1, 6, 8, 3, 9, 6, 9, 4, 17, 3, 19, 
            2, 15, 6, 10, 3, 24, 4, 24, 2, 24, 3, 17, 2, 17, 
            2, 19, 1, 24), ncol = 2, byrow = TRUE)
        CaseRotationFlip <- cbind(CaseRotation, c(1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, -1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, -1, 1, -1, -1, -1, 1, 1, 1, 
            1, 1, 1, 1, -1, 1, 1, 1, 1, 1, 1, -1, -1, 1, 1, 1, 
            1, 1, 1, 1, -1, 1, 1, 1, -1, 1, -1, -1, -1, 1, 1, 
            1, 1, 1, 1, 1, -1, 1, 1, 1, -1, 1, -1, -1, -1, 1, 
            1, 1, 1, 1, 1, 1, -1, 1, 1, -1, -1, 1, -1, -1, -1, 
            1, 1, 1, 1, 1, -1, 1, -1, 1, 1, 1, -1, -1, -1, -1, 
            -1, 1, 1, -1, -1, 1, -1, -1, -1, -1, -1, -1, -1, 
            -1, -1, -1, -1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            -1, 1, 1, -1, -1, 1, 1, 1, 1, 1, -1, -1, -1, 1, -1, 
            1, -1, -1, -1, -1, -1, 1, 1, 1, -1, 1, 1, -1, -1, 
            1, -1, -1, -1, -1, -1, -1, -1, 1, 1, 1, -1, 1, -1, 
            -1, -1, 1, -1, -1, -1, -1, -1, -1, -1, 1, 1, 1, -1, 
            1, -1, -1, -1, 1, -1, -1, -1, -1, -1, -1, -1, 1, 
            1, -1, -1, -1, -1, -1, -1, 1, -1, -1, -1, -1, -1, 
            -1, -1, 1, 1, 1, -1, 1, -1, -1, -1, -1, -1, -1, -1, 
            -1, -1, -1, -1, 1, -1, -1, -1, -1, -1, -1, -1, -1, 
            -1, -1, -1, -1, -1, -1, -1))
        EdgePoints <- matrix(data = c(1, 1, 2, 2, 2, 3, 3, 3, 
            4, 4, 4, 1, 5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8, 5, 9, 
            1, 5, 10, 2, 6, 11, 3, 7, 12, 4, 8, 13, 9, 9), ncol = 3, 
            byrow = TRUE)
        BasicEdges <- list(c(1, 4, 9), c(2, 4, 9, 10), c(1, 4, 
            5, 6, 9, 10), c(1, 4, 6, 7, 9, 11), c(1, 4, 10, 11, 
            12), c(2, 4, 6, 7, 9, 10, 11), c(1, 2, 5, 6, 7, 8, 
            9, 10, 11, 13), c(9, 10, 11, 12), c(1, 2, 7, 8, 9, 
            11), c(1, 2, 3, 4, 5, 6, 7, 8, 13), c(1, 2, 6, 7, 
            9, 12), c(1, 4, 5, 8, 9, 10, 11, 12, 13), c(1, 2, 
            3, 4, 5, 6, 7, 8, 9, 10, 11, 12,13), c(1, 4, 7, 8, 10, 
            11))

        EdgeSequence1 <- list(c(1, 2, 3), c(1, 2, 4, 2, 3, 4), list(c(1, 2, 5, 3, 4, 6), c(1, 2, 6, 2, 4, 6, 2, 
                3, 4, 2, 5, 3)), list(c(1, 2, 5, 3, 4, 6), c(1,3, 5, 3, 4, 5, 2, 5, 4, 1, 2, 6, 1, 6, 3, 2, 
                4, 6)), c(1, 3, 2, 2, 3, 5, 3, 4, 5), list(c(1,2, 6, 2, 5, 6, 3, 4, 7), c(1, 2, 7, 2, 4, 7, 
                2, 5, 4, 3, 4, 5, 3, 5, 6, 1, 6, 3, 1, 3, 7, 7, 3, 4), c(1, 2, 7, 2, 4, 7, 2, 5, 4, 3, 4, 
                5, 3, 5, 6)), list(c(1, 8, 2, 3, 7, 6, 4, 5, 9), c(1, 8, 2, 3, 9, 4, 3, 7, 9, 5, 9, 7, 5, 
                7, 6), c(3, 7, 6, 1, 8, 4, 1, 4, 5, 1, 9, 2, 1, 5, 9), c(4, 5, 9, 1, 7, 6, 1, 6, 2, 2, 6, 
                3, 2, 3, 8), c(1, 10, 2, 2, 10, 9, 5, 9, 10, 5, 10, 6, 6, 10, 7, 3, 7, 10, 3, 10, 4, 4, 10, 
                8, 1, 8, 10), c(1, 10, 2, 1, 10, 7, 6, 10, 7,5, 10, 6, 5, 9, 10, 4, 9, 10, 3, 10, 4, 3, 8, 
                10, 2, 10, 8), c(5, 9, 10, 2, 10, 9, 1, 10, 2, 1, 7, 10, 6, 10, 7, 3, 10, 6, 3, 8, 10, 4, 10, 
                8, 4, 5, 10), c(1, 7, 6, 1, 6, 9, 1, 9, 2, 5, 9, 6, 3, 8, 4), c(1, 7, 8, 3, 8, 7, 3, 7, 6, 
                3, 6, 5, 3, 5, 4, 4, 5, 9, 4, 9, 8, 2, 8, 9, 1, 8, 2)), c(1, 2, 3, 1, 3, 4), c(1, 2, 6, 1, 
                3, 5, 1, 6, 3, 3, 4, 5), list(c(1, 4, 5, 2, 6, 3, 3, 6, 7, 4, 8, 5), c(1, 4, 3, 1, 3, 2, 3, 
                4, 8, 3, 8, 7, 5, 8, 6, 6, 8, 7, 1, 2, 6, 1, 6, 5), c(1, 2, 9, 1, 9, 5, 5, 9, 8, 4, 8, 9, 
                3, 4, 9, 3, 9, 7, 6, 7, 9, 2, 6, 9), c(5, 9, 6, 1, 9, 5, 1, 4, 9, 4, 8, 9, 7, 9, 8, 3, 9, 
                7, 2, 9, 3, 2, 6, 9), c(1, 2, 5, 2, 6, 5, 3, 4, 8, 3, 8, 7)), c(1, 2, 3, 1, 3, 6, 1, 6, 5, 
                3, 4, 6), list(c(1, 6, 2, 2, 6, 8, 3, 5, 4, 6, 7, 8), c(1, 5, 3, 1, 3, 6, 3, 6, 7, 3, 7, 4, 
                2, 4, 8, 2, 5, 4, 1, 5, 2, 4, 7, 8), c(1, 9, 2, 2, 9, 5, 3, 5, 9, 4, 9, 8, 3, 9, 4, 7, 8, 
                9, 6, 7, 9, 1, 9, 6), c(4, 9, 5, 1, 5, 9, 1, 9, 2, 2, 9, 8, 7, 8, 9, 6, 9, 7, 3, 6, 9, 3, 
                9, 4), c(1, 5, 2, 3, 8, 4, 3, 6, 8, 6, 7, 8)),
                list(
                #13.1
		c(7,8,12,2,3,11,1,4,9,5,6,10),
		#13.2
		c(2,  3, 11,  7,  8, 12,  9,  5,  4,  5,  4,  6,  4,  6,  1,  6,  1, 10),
		c(1,  4,  9,  7,  8, 12, 10,  2,  5,  2,  5,  3,  5,  3,  6,  3,  6, 11),
		c(5,  6, 10,  1,  4,  9, 11,  7,  2,  7,  2,  8,  2,  8,  3,  8,  3, 12),
		c(5,  6, 10,  2,  3, 11, 12,  4,  7,  4,  7,  1,  7,  1,  8,  1,  8,  9),
		c(5,  6, 10,  7,  8, 12,  2, 11,  1, 11,  1,  9, 11,  9,  3,  9,  3,  4),
		c(2,  3, 11,  4,  1,  9,  5, 10,  8, 10,  8, 12, 10, 12,  6, 12,  6,  7),
		#13.3
		c(7, 8, 12, 13, 3, 11, 13, 11, 6, 13,  6, 5, 13, 5,  9, 13,  9,  4, 13,  4,  1, 13, 1,10,13,10, 2,13,2,3),
		c(2, 3, 11, 13, 6, 10, 13, 10, 1, 13,  1, 4, 13, 4, 12, 13, 12,  7, 13,  7,  8, 13, 8, 9,13,9, 5, 13,5,6),
		c(7, 8, 12, 13, 6,  5, 13,  5, 9, 13,  9, 4, 13, 4,  3, 13,  3, 11, 13, 11,  2, 13, 2, 1,13,1,10,13,10,6),
		c(2, 3, 11, 13,  4,  1, 13,  1, 10, 13, 10, 6, 13, 6, 7,13,  7, 12, 13, 12,  8, 13, 8, 5,13,5, 9,13, 9,4),
		c(1, 4, 9,  13,  8, 12, 13, 12,  3, 13,  3, 2, 13, 2, 10, 13, 10, 5, 13,  5, 6, 13, 6,11,13,11,7,13, 7,8),
		c(7, 8, 12, 13,  5,  6, 13,  6, 11, 13, 11, 3, 13, 3,  4, 13,  4, 9, 13,  9, 1, 13, 1, 2,13,2,10,13,10,5),
		c(1, 4,  9, 13,  3,  2, 13,  2, 10, 13, 10, 5, 13, 5,  8, 13,  8, 12,13, 12, 7, 13, 7, 6,13,6,11,13,11,3),
	c(5, 6, 10, 13,  1,  9, 13,  9,  8, 13,  8, 7, 13, 7, 11, 13, 11,  2,13,  2,  3, 13, 3, 12, 13, 12, 4,13,  4, 1),
	c(5, 6, 10, 13,  8,  7, 13,  7, 11, 13, 11, 2, 13, 2,  1, 13,  1,  9,13,  9,  4, 13, 4,  3, 13, 3, 12,13, 12, 8),
	c(1, 4,  9, 13,  2,  3, 13,  3, 12, 13, 12, 8, 13, 8,  5, 13,  5, 10, 13, 10, 6, 13, 6,  7, 13, 7, 11,13, 11, 2),
	c(5, 6, 10, 13,  7,  8, 13,  8,  9, 13,  9, 1, 13, 1,  2, 13,  2, 11, 13, 11, 3, 13, 3,  4, 13, 4, 12,13, 12, 7),
	c(2, 3, 11, 13,  1,  4, 13,  4, 12, 13, 12, 7, 13, 7,  6, 13,  6, 10, 13, 10, 5, 13, 5,  8, 13, 8, 9, 13,  9, 1),
	#13.4
	c(13, 3, 11, 13, 11, 6, 13, 6, 7, 13, 7, 12, 13, 12, 8, 13, 8, 5, 13, 5, 9, 13, 9, 4, 
  	13, 4, 1,  13, 1, 10, 13,10, 2, 13, 2,  3),
	c(13, 4, 12, 13, 12, 7, 13, 7, 8, 13, 8,  9, 13,  9, 5, 13, 5, 6, 13, 6, 10, 13, 10, 1, 13,
  	1,  2, 13,  2, 11, 13, 11,  3, 13,  3,  4),
	c(13, 2, 10, 13, 10, 5, 13, 5, 6, 13, 6, 11, 13, 11, 7, 13, 7, 8, 13, 8, 12, 13,12, 3, 13, 
  	3,  4, 13,  4,  9, 13,  9,  1, 13,  1,  2),
	c(13, 1,  9, 13,  9, 8, 13, 8, 5, 13, 5, 10, 13, 10, 6, 13, 6, 7, 13, 7, 11, 13,11, 2, 13,
  	2,  3, 13,  3, 12, 13, 12,  4, 13,  4,  1),
	#13.5.1
	c(7,  8, 12,  2,  1, 10,  3,  4, 11,  4, 11,  6,  4,  6,  9,  6,  9,  5),
	c(3,  2, 11,  8,  5,  9,  4,  1, 12,  1, 12,  7,  1,  7, 10,  7, 10,  6),
	c(1,  4,  9,  6,  7, 11,  2,  3, 10,  3, 10,  5,  3,  5, 12,  5, 12,  8),
	c(5,  6, 10,  4,  3, 12,  1,  2,  9,  2,  9,  8,  2,  8, 11,  8, 11,  7),
	#13.5.2
	c(1, 2, 10, 8, 5, 9,  8, 9,  4, 8, 4,12, 4,12, 3,12, 3,11,12,11, 7,11, 7, 6, 7, 6, 8, 6, 8, 5),
	c(8, 5,  9, 3, 4, 12, 3, 12, 7, 3, 7,11, 7,11, 6,11, 6,10,11,10, 2,10, 2, 1, 2, 1, 3, 1, 3, 4),
	c(6, 7, 11, 1, 2, 10, 1, 10, 5, 1, 5, 9, 5, 9, 8, 9, 8,12, 9,12, 4,12, 4, 3, 4, 3, 1, 3, 1, 2),
	c(3, 4, 12, 6, 7, 11, 6, 11, 2, 6, 2, 10,2, 10, 1,10,1, 9, 10,9, 5, 5, 9, 8, 5, 8, 6, 8, 6,  7)),
        c(1, 4, 2, 1, 6, 4, 1, 5, 6, 3, 6, 4))
	
	switch23 <- function(x){
		num <- length(x) / 3
		temp <- x[c(0: (num-1))*3 + 2]
		x[c(0: (num-1))*3 + 2] <- x[c(0: (num-1))*3+ 3]
      		x[c(0: (num-1))*3 + 3] <- temp
		x
	}

	SwitchSeq <- function(ed){
		for (i in 1:length(ed)){
      			ver <- ed[[i]]
			if (is.list(ver)){
				for(j in 1:length(ver)){
                  			ed[[i]][[j]] <- switch23(ver[[j]])
				}
      			}
      			else
			ed[[i]] <- switch23(ver)
		}
		ed
              }
	
	EdgeSequence2 <- SwitchSeq(EdgeSequence1)
       
      
       
        GetEdges <- local({
            Edges <- apply(CaseRotationFlip[-c(1, 256), ], 1, 
                function(x) {
                  case <- x[1]
                  rotation <- x[2]
                  map <- rep(0, 8)
                  for (i in 1:8) {
                    temp <- as.integer(BasicRotation[rotation, 
                      ][i])
                    map[temp] <- i
                  }
                  sapply(BasicEdges[[case - 1]], function(x) {
                    if (x != 13) {
                      EndP1 <- EdgePoints[x, 2]
                      EndP2 <- EdgePoints[x, 3]
                      newEdge <- EdgePoints[(EdgePoints[, 2] == 
                        map[EndP1] & EdgePoints[, 3] == map[EndP2]) | 
                        (EdgePoints[, 3] == map[EndP1] & EdgePoints[, 
                          2] == map[EndP2]), ][1]
                    }
                    else newEdge <- 13
                    newEdge
                  })
                })
            Case <- cbind(seq(1:256), CaseRotationFlip[, c(1, 
                3)])
            Edges <- apply(Case[-c(1, 256), ], 1, function(x) {
                case <- x[2] - 1
                EdgeNum <- x[1] - 1
                if (x[3] == 1) 
                  sapply(EdgeSequence1[[case]], function(x) Edges[[EdgeNum]][x])
                else sapply(EdgeSequence2[[case]], function(x) Edges[[EdgeNum]][x])
            })
            Edges
        })
        BasicFace <- list(c(0), c(0), c(1), c(7), c(0), c(2, 
            7), c(1, 2, 6, 7), c(0), c(0), c(5, 6, 7), c(0), 
            c(1, 4, 7), c(1,2,3,4,5,6,7), c(0))
        FacePoints <- matrix(data = c(seq(1, 6), 1, 2, 4, 1, 
            1, 5, 6, 7, 7, 8, 3, 7, 2, 3, 3, 4, 2, 6, 5, 6, 8, 
            5, 4, 8), ncol = 5)
        FacePoints <- cbind(FacePoints, apply(FacePoints[, 2:5], 
            1, prod))
        GetFaces <- local({
            Faces <- apply(CaseRotationFlip[-c(1, 256), ], 1, 
                function(x) {
                  case <- x[1]
                  rotation <- x[2]
                  map <- rep(0, 8)
                  for (i in 1:8) {
                    temp <- as.integer(BasicRotation[rotation, 
                      ][i])
                    map[temp] <- i
                  }
                  sapply(BasicFace[[case - 1]], function(x) {
                    EndP <- rep(0, 4)
                    if (x == 0) 
                      newFace <- 0
                    else if (x == 7) 
                      newFace <- 7
                    else {
                      for (i in 1:4) {
                        point <- FacePoints[x, i + 1]
                        EndP[i] <- map[point]
                      }
                      newFace <- FacePoints[FacePoints[, 6] == 
                        prod(EndP[1:4]), ][1]
                    }
                    newFace
                  })
                })
            for (i in 1:254) {
                for (j in 1:length(Faces[[i]])) {
                  x <- Faces[[i]][j]
                  if (x != 0) {
                    index <- explode(i + 1)
                    if (sum(index) > 4) 
                      index <- ifelse(index == 0, 1, 0)
                    if (x != 7 && index[FacePoints[x, 2]] == 
                      0) 
                      Faces[[i]][j] <- -Faces[[i]][j]
                    if (x == 7) {
                      tcase <- CaseRotationFlip[i + 1, 1] - 1
                      if ((tcase == 4 || tcase == 6 || tcase == 10 || tcase == 
                        12) && !(index[1] + index[7] == 2) && 
                        !(index[3] + index[5] == 2)) 
                        Faces[[i]][j] <- -Faces[[i]][j]
                      if (tcase == 7 && !(index[1] + index[7] == 
                        0) && !(index[3] + index[5] == 0)) 
                        Faces[[i]][j] <- -Faces[[i]][j]
                    }
                  }
                }
            }
            Faces
        })
        list(Edges = GetEdges, Faces = GetFaces, EdgePoints = EdgePoints, 
            FacePoints = FacePoints, CRF = CaseRotationFlip)
    })

 GetTriangles <- function (f, level, x = 1:dim(f)[1], y = 1:dim(f)[2], z = 1:dim(f)[3],
                           effect,markx,marky,markz){

    implode.vec <- function(x1, x2, x3, x4, x5, x6, x7, x8) x1 + 
        2 * x2 + 2^2 * x3 + 2^3 * x4 + 2^4 * x5 + 2^5 * x6 + 
        2^6 * x7 + 2^7 * x8 + 1
    fgrid <- function(fun, x, y, z) {
        g <- expand.grid(x = x, y = y, z = z)
        array(fun(g$x, g$y, g$z), c(length(x), length(y), length(z)))
    }
    levCells <- function(v, level) {
        nx <- dim(v)[1]
        ny <- dim(v)[2]
        nz <- dim(v)[3]
        val <- vector("list", nz - 1)
        type <- vector("list", nz - 1)
        i <- 1:(nx - 1)
        j <- 1:(ny - 1)
        v1 <- v[, , 1, drop = TRUE]
        vv1 <- ifelse(v1 > level, 1, 0)
        ttt1 <- vv1[i, j] + 2 * vv1[i + 1, j] + 4 * vv1[i + 1, 
            j + 1] + 8 * vv1[i, j + 1]
        for (k in 1:(nz - 1)) {
            v2 <- v[, , k + 1, drop = TRUE]
            vv2 <- ifelse(v2 > level, 1, 0)
            ttt2 <- vv2[i, j] + 2 * vv2[i + 1, j] + 4 * vv2[i + 
                1, j + 1] + 8 * vv2[i, j + 1]
            ttt <- ttt1 + 16 * ttt2
            iii <- ttt > 0 & ttt < 255
            val[[k]] <- which(iii) + (nx - 1) * (ny - 1) * (k - 
                1)
            type[[k]] <- as.integer(ttt[iii])
            v1 <- v2
            vv1 <- vv2
            ttt1 <- ttt2
        }
        v <- unlist(val)
        i <- as.integer((v - 1)%%(nx - 1) + 1)
        j <- as.integer(((v - 1)%/%(nx - 1))%%(ny - 1) + 1)
        k <- as.integer((v - 1)%/%((nx - 1) * (ny - 1)) + 1)
        t <- unlist(type)
        NAS <- which(is.na(t))
        if (length(NAS) > 0) 
            t <- t[-NAS]
        list(i = i, j = j, k = k, t = t)
    }
    GetInfo <- function(cube.1) {
        index <- matrix(c(0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 
            0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1), nrow = 8)
        if (interface == 1) 
            ax.inc <- c(x[2] - x[1], y[2] - y[1], z[2] - z[1])
        else ax.inc <- c(1, 1, 1)
        ver.inc <- t(apply(index, 1, function(x) x * ax.inc))
        cube.co <- kronecker(rep(1, nrow(cube.1)), ver.inc) + 
            kronecker(cube.1, rep(1, 8))
        if (interface == 1) 
            value <- f(cube.co[, 1], cube.co[, 2], cube.co[, 
                3]) - level
        else value <- apply(cube.co, 1, function(x) vol[x[1], 
            x[2], x[3]]) - level
        if (interface == 1) 
            information <- cbind(cube.co, value)
        else information <- cbind(x[cube.co[, 1]], y[cube.co[, 
            2]], z[cube.co[, 3]], value)
        information
    }
    GetPoints <- function(edge, p1) {
        x1 <- EP[edge, 2]
        x2 <- EP[edge, 3]
        c((1 - floor(x1/9)) * information[p1 + x1 - 1, 1] + floor(x1/9) * 
            information[p1, 1], (1 - floor(x1/9)) * information[p1 + 
            x2 - 1, 1] + floor(x1/9) * information[p1 + 1, 1], 
            (1 - floor(x1/9)) * information[p1 + x1 - 1, 2] + 
                floor(x1/9) * information[p1 + 1, 2], (1 - floor(x1/9)) * 
                information[p1 + x2 - 1, 2] + floor(x1/9) * information[p1 + 
                2, 2], (1 - floor(x1/9)) * information[p1 + x1 - 
                1, 3] + floor(x1/9) * information[p1 + 1, 3], 
            (1 - floor(x1/9)) * information[p1 + x2 - 1, 3] + 
                floor(x1/9) * information[p1 + 5, 3], (1 - floor(x1/9)) * 
                information[p1 + x1 - 1, 4] + floor(x1/9) * (0 * 
                information[p1 + 1, 3] + 1), (1 - floor(x1/9)) * 
                information[p1 + x2 - 1, 4] + floor(x1/9) * (0 * 
                information[p1 + 1, 3] - 1))
    }
    CalPoint <- function(x1, x2, y1, y2, z1, z2, v1, v2) {
        s <- v1/(v1 - v2)
        x <- x1 + s * (x2 - x1)
        y <- y1 + s * (y2 - y1)
        z <- z1 + s * (z2 - z1)
        c(x, y, z)
    }
    FaceNo7 <- function(faces, p1) {
        index <- ifelse(faces > 0, 1, -1)
        faces <- abs(faces)
        e1 <- FP[faces, 2]
        e2 <- FP[faces, 3]
        e3 <- FP[faces, 4]
        e4 <- FP[faces, 5]
        A <- information[p1 + e1 - 1, 4]
        B <- information[p1 + e2 - 1, 4]
        C <- information[p1 + e3 - 1, 4]
        D <- information[p1 + e4 - 1, 4]
        index <- index * ifelse(A * B - C * D > 0, 1, -1)
        index <- ifelse(index == 1, 1, 0)
        index
    }
    Face7 <- function(faces, p1) {
        index <- ifelse(faces > 0, 1, -1)
        A0 <- information[p1, 4]
        B0 <- information[p1 + 3, 4]
        C0 <- information[p1 + 2, 4]
        D0 <- information[p1 + 1, 4]
        A1 <- information[p1 + 4, 4]
        B1 <- information[p1 + 7, 4]
        C1 <- information[p1 + 6, 4]
        D1 <- information[p1 + 5, 4]
        a <- (A1 - A0) * (C1 - C0) - (B1 - B0) * (D1 - D0)
        b <- C0 * (A1 - A0) + A0 * (C1 - C0) - D0 * (B1 - B0) - 
            B0 * (D1 - D0)
        c <- A0 * C0 - B0 * D0
        tmax <- -b/(2 * a)
        maximum <- a * tmax^2 + b * tmax + c
        maximum <- ifelse(maximum == "NaN", -1, maximum)
        cond1 <- ifelse(a < 0, 1, 0)
        cond2 <- ifelse(tmax > 0, 1, 0)
        cond3 <- ifelse(tmax < 1, 1, 0)
        cond4 <- ifelse(maximum > 0, 1, 0)
        totalcond <- cond1 * cond2 * cond3 * cond4
        index <- index * ifelse(totalcond == 1, 1, -1)
        index <- ifelse(index == 1, 1, 0)
    }
    PreRender <- function(edges, p1, type) {
     if(type==1){
        if (typeof(edges) == "list") {
            count <- sapply(edges, function(x) length(x))
            edges <- cbind(unlist(edges), rep(p1, count))
        }
        else {
            count <- nrow(edges)
            edges <- cbind(as.vector(t(edges)), rep(p1, each = count))
        }
     }

     else{
	if (is.vector(edges)) 
            edges <- matrix(edges, ncol = length(edges))
        p1 <- edges[, 1]
        count <- ncol(edges) - 1
        edges <- cbind(as.vector(t(edges[, -1])), rep(p1, each = count))
     }

 
        information <- GetPoints(edges[, 1], edges[, 2])
        information <- matrix(information, ncol = 8)
        information <- CalPoint(information[, 1], information[, 
            2], information[, 3], information[, 4], information[, 
            5], information[, 6], information[, 7], information[, 
            8])
        information <- matrix(information, ncol = 3)
        information
   }
    

    Faces <- PreProcessing$Faces
    Edges <- PreProcessing$Edges
    EP <- PreProcessing$EdgePoints
    FP <- PreProcessing$FacePoints
    CR <- PreProcessing$CRF
    
    if (interface == 1) {
        NAS <- unique(c(which(is.na(x)), which(is.na(y)), which(is.na(z))))
        if (length(NAS) > 0) {
            x <- x[-NAS]
            y <- y[-NAS]
            z <- z[-NAS]
        }
        vol <- fgrid(f, x, y, z)
        
    }
    else vol <- f

    if (interface == 2) {
        nx <- dim(vol)[1]
        ny <- dim(vol)[2]
        nz <- dim(vol)[3]
                
        if (length(x) != nx || length(y) != ny || length(z) != 
            nz) 
            stop("dimensions of f do not match x, y, or z")
    }
    

    GetBasic <- function(R){
     if (interface == 1) 
            cube.1 <- cbind(x[v$i[R]], y[v$j[R]], z[v$k[R]])
        else cube.1 <- cbind(v$i[R], v$j[R], v$k[R])
        information <- GetInfo(cube.1)
        information <- rbind(information, rep(0, 4))
        p1 <- (1:length(R) - 1) * 8 + 1
        cases <- v$t[R]
        list(information=information, p1 = p1, cases=cases)
   }
    
    v <- levCells(vol, level)
    if (effect!= "none"){	
       	if (effect=="bandx")
    	  d <- which(v$i %% 2==0)
	else if (effect=="bandy")
    	  d <- which(v$j %% 2==0)
	else if (effect=="bandz")
    	  d <- which(v$k %% 2==0)
       	else if (effect=="shatter")
          d <- unique(c(which(v$i %% 2==0),which(v$j %% 2==0),which(v$k %% 2==0)))
       	else if (effect=="corner"){
          if (markx==0) dnox <- 1
          else dnox <- 2
          
          if (marky==0) dnoy <- 1
          else dnoy <- 2

          if (markz==0) dnoz <- 1
          else dnoz <- 2
          
	  chopx <- floor(max(v$i)/dnox)
          if (markx > 0 || markx==0) dx <- which(v$i > (max(v$i)-chopx))
          else dx <- which(v$i <= chopx)

          chopy <- floor(max(v$j)/dnoy)
          if (marky > 0 || marky==0) dy <- dx[which(v$j[dx] > (max(v$j)-chopy))]
          else dy <- dx[which(v$j[dx] <= chopy)]
          
          chopz <- floor(max(v$k)/dnoz)
          if (markz > 0 || markz==0)  dz <- dy[which(v$k[dy] > (max(v$k)-chopz))]
          else dz <- dy[which(v$k[dy] <= chopz)]

          d <- dz
          #browser()
        }
	#else if (effect=="corner"){
	#  chopx <- floor(max(v$i)/1);  dx <- which(v$i > (max(v$i)-chopx))
	#  chopy <- floor(max(v$j)/2);  dy <- dx[which(v$j[dx] > (max(v$j)-chopy))]
        #  chopz <- floor(max(v$k)/2);  dz <- dy[which(v$k[dy] > (max(v$k)-chopz))]
        #  d <- dz
          #browser()
        #}
       	v$i <- v$i[d]
       	v$j <- v$j[d]
       	v$k <- v$k[d]
       	v$t <- v$t[d]
    }	
    
    tcase <- CR[v$t + 1, 1] - 1
    R <- which(tcase %in% c(1, 2, 5, 8, 9, 11,14))
    if (length(R) > 0) {
        Basics <- GetBasic(R)
        information <- Basics$information
        p1 <- Basics$p1
        cases <- Basics$cases
        edges <- Edges[cases]
        triangles <- PreRender(edges, p1,type=1)
         
    }
    #special
    #name : name of the case
    #nface: how many cases need to be checked
    #sev: whether face 7 need to be checked
    #nedge: total number of edges in the lookuptable
    #ind: the index needed to check the lookuptable
    #position: the corresponding positions in the lookuptable.
    special <- list(name =c(3, 4, 6, 7,  10,12,13),
                    nface=c(1, 1, 2, 4,  3, 3, 7),
                    sev = c(0, 1, 1, 1,  1, 1, 1),
                    nedge=c(18,24,48,177,96,96,816),
                    ind=list(c(0,1), c(0,1), c(0,2,1,3),c(0,8,4,12,2,10,1,9,6,14,5,13,3,11,15,7),
                             c(0,4,1,5,2,6,3,7),c(0,4,2,6,1,5,3,7),
                             c(0,1,2,4,8,16,32,3,9,17,33,6,18,34,12,20,36,24,40,35,25,22,44,19,41,38,28,83,105,102,92)),
                    position=list(list(c(1:6),c(7:18)),
                                  list(c(1:6),c(7:24)),
                                  list(c(1:9),c(10:33),c(34:48),c(34:48)),
                                  list(c(1:9),c(1:9),c(10:24),c(10:24),c(25:39),c(25:39),c(40:54), c(40:54),
                                       c(55:81), c(55:81),c(82:108),c(82:108),c(109:135),c(109:135),c(136:150),c(151:177)),
                                  list(c(1:12),c(13:36),c(37:60),c(37:60), c(61:84), c(61:84),c(85:96),c(85:96)),
                                  list(c(1:12), c(13:36), c(37:60), c(37:60),c(61:84), c(61:84),c(85:96),c(85:96)),
                                  list(
                                       c(1:12),
                                       c(13:30),c(31:48),c(49:66),c(67:84),c(85:102),c(103:120),
                                       c(121:150),c(151:180),c(181:210),c(211:240),c(241:270),c(271:300),c(301:330),
                                       c(331:360),c(361:390),c(391:420),c(421:450),c(451:480),
                                       c(481:516),c(517:552),c(553:588),c(589:624),
                                       c(625:642),c(643:660),c(661:678),c(679:696),
                                       c(697:726),c(727:756),c(757:786),c(787:816))
                                 )
                    )


    for (i in 1:length(special$name)){
      R <- which(tcase == special$name[i])
      if (length(R) > 0) {
       
        Basics <- GetBasic(R)
        information <- Basics$information
        p1 <- Basics$p1
        cases <- Basics$cases

        nface <- special$nface[i]
        nedge <- special$nedge[i]
        faces <- matrix(unlist(Faces[cases]), ncol = nface, byrow = TRUE)

        if (i==1)
          index <- FaceNo7(faces[, 1], p1)
        else if (i==2)
          index <- Face7(faces[, 1], p1)
        else{ 
          index <-  Face7(faces[, nface], p1)*2^(nface-1)
          for(j in 1:(nface-1)){
            temp <-  FaceNo7(faces[, j], p1)
            index <- index + temp * 2^(j-1) 
          }
        }
        edges <- matrix(unlist(Edges[cases]), ncol = nedge, byrow = TRUE)
        edges <- cbind(edges, p1, index)
        ind <- special$ind[[i]]
        position <- special$position[[i]]
        
        for (j in 1:length(ind)){
          ed <- edges[which(index == ind[j]), c(nedge+1, position[[j]])]
          if (length(ed) > 0) 
                      triangles <- rbind(triangles, PreRender(ed,nedge+1,type=2))
        }
      }
    }
 
   
  
   triangles
  }
	 if (typeof(f) == "closure") 
      		interface <- 1
	 else if (is.array(f) && length(dim(f)) == 3) {
        	interface <- 2
    	}
	 else stop("vol has to be a function or a 3-dimensional array")
 	
	 triangles <- matrix(0, ncol=3)
         l <- rep(0,length(level))
         for (i in seq(along = level)) {
                
                temp <- GetTriangles(f,level[i],x,y,z,effect[i],markx[i],marky[i],markz[i]) 
                l[i] <- nrow(temp)
                triangles <- rbind(triangles, temp)
                
         }
         triangles <- triangles[-1,]
        
    list(triangles=triangles, l=l)
}

plotMesh.gridpoly<-function(triangles, rot.mat, fill, alpha) 

    ## rot.mat: 4x4 transformation matrix
    ## dist: controls perspective, 0 = none
 {  
    #rot.mat <- ltransform3dMatrix(list(y = 10, x = 40, z=-10)) 
    dist = 0.1
    x <- ltransform3dto3d(triangles, rot.mat, dist = dist) 
    id <- seq(length = ncol(x) / 3) 
    ord <- order(x[3, id * 3] + x[3, id * 3 - 1] + 
                 x[3, id * 3 - 2])
    
    xscale <- range(x[1,]) 
    yscale <- range(x[2,]) 
    md <- max(diff(xscale), diff(yscale)) 
    pushViewport(viewport(w = 0.9 * diff(xscale) / md, 
                          h = 0.9 * diff(yscale) / md, 
                          xscale = xscale,
                         yscale = yscale))
    id <-
        as.vector(outer(1:3, (id[ord]-1) * 3, "+"))

    grid.polygon(x = x[1,id], 
                 y = x[2,id],
                 default.units = "native",
                 gp = gpar(fill = fill[ord], alpha=alpha[ord]), 
                 id = rep(id[ord], each = 3)) 
    
   
 }

## These functions work with collections of n triangles.  A collection of
## triangles is a list with components v1, v2, v3 representing the
## coordinates of the thre vertices; each of these components is an n by
## 3 matrix.

## triangleNormals computes the normal vectors to a collection of
## triangles as the vector crossprocuct of the direction from v1 to v2
## and the direction from v2 to v3.  The result is an n by 3 matrix of
## unit representing the n unit normal vectors.

triangleNormals <- function(triangles) {
   x <- triangles$v2 - triangles$v1
   y <- triangles$v3 - triangles$v2
   z <- cbind(x[,2]*y[,3] - x[,3]*y[,2],
              x[,3]*y[,1] - x[,1]*y[,3],
              x[,1]*y[,2] - x[,2]*y[,1])
   z / sqrt(rowSums(z^2))
}

## Lighting functions are functions of the form
##
##     lighting(normals, view, light, color1, color2, ...)
##
## with
##
##    normals   a matrix of unit normal vectors as computed by triangleNormals
##    view      vector pointing to the viewer
##    light     vector pointing to the light source
##    color1    color or vector of n colors for the sides of the triangles
##              in the direction of to the normal vectors
##    color2    color or vector of n colors for the sides of the triangles
##              in the opposite direction of to the normal vectors
##    ...       additional parameters needed by the lighting function
##
## lightning functions return a vector of n rgb colors corresponding
## to the sides of the trinagles facing the viewer and the lighting
## algorithm.

## phongLighting implements a simple version of the Phong lighting
## model (not shading--that would involve interpolation within the
## triangles). It incorporates ambient and diffuse light, which are
## the same color as the object, and specular light, which is a convex
## combination of the obkect color and the (white) light color.  This
## is based roughly on the description in Foley and Van Dam.

phongLighting <- function(normals, view, light, color1, color2,
                          ambient = 0.3, diffuse = 0.7, specular = 0.1,
                          exponent = 10, sr = 0) {
    V <- view / sqrt(sum(view^2))
    L <- light / sqrt(sum(light^2))
    H <- (L + V) / sqrt(sum((L + V)^2))
    sgn <- as.vector(normals %*% V) > 0
    N <- ifelse(sgn,1, -1) * normals
    Is <- as.vector(specular * abs(N %*% H) ^ exponent)
    Id <-  as.vector(diffuse * pmax(N %*% L,0))
    rgbcol <- t(col2rgb(ifelse(sgn, color1, color2)) / 255)
    Lrgbcol <- pmin((ambient + Id + sr * Is) * rgbcol + (1 - sr) * Is, 1)
    rgb(Lrgbcol[,1], Lrgbcol[,2], Lrgbcol[,3])
}

## The following implement variants with different default values
## based on Based on Matlab's values

phongShiny <- function(normals, view, light, color1, color2,
                       ambient = 0.3, diffuse = 0.6, specular = 0.9,
                       exponent = 20, sr = 0)
    phongLighting(normals, view, light, color1, color2, ambient, diffuse,
                  specular, exponent, sr)

phongDull <- function(normals, view, light, color1, color2,
                      ambient = 0.3, diffuse = 0.8, specular = 0.0,
                      exponent = 10, sr = 0)
    phongLighting(normals, view, light, color1, color2, ambient, diffuse,
                  specular, exponent, sr)

phongMetal <- function(normals, view, light, color1, color2,
                       ambient = 0.3, diffuse = 0.3, specular = 1.0,
                       exponent = 25, sr = 0.5)
    phongLighting(normals, view, light, color1, color2, ambient, diffuse,
                  specular, exponent, sr)

## perspLighting is an implementation of the lighting algorithm
## described in the help page for persp().  It _looks_ like the shade
## parameter used in persp may be closer to twice the value supplied
## to persp; that is, perspLighting with shade = x seems to be
## comparable to persp() with shade = x / 2.

perspLighting <- function(normals, view, light, color1, color2, shade = 1.5) {
    V <- view / sqrt(sum(view^2))
    L <- light / sqrt(sum(light^2))
    sgn <- as.vector(normals %*% V) > 0
    N <- ifelse(sgn,1, -1) * normals
    I <-  ((1 + as.vector(pmax(N %*% L, 0))) / 2) ^ shade
    Lrgbcol <- I * t(col2rgb(ifelse(sgn, color1, color2)) / 255)
    rgb(Lrgbcol[,1], Lrgbcol[,2], Lrgbcol[,3])
}

## ptri is a simple function for plotting triangles.  The viewer is
## looking down the positive Z axis.

ptri <- function(triangles,lighting = phongLighting, 
                 view = c(0, 0, 1), light = c(1, 1, 1),
                 color1 = "red", color2 = "green", ...
                 ) {
    x <- triangles$v1
    y <- triangles$v2
    z <- triangles$v3
    mr <- function(x) mean(range(x))
    m <- (apply(x,2,mr) + apply(y,2,mr) + apply(z,2,mr)) / 3
    x <- sweep(x, 2, m)
    y <- sweep(y, 2, m)
    z <- sweep(z, 2, m)
    r <- max(abs(x), abs(y), abs(z))
    normals <- triangleNormals(triangles)
    col <- lighting(normals, view, light, color1, color2, ...)
    r <- range(x,y,z)
    plot(c(-r, r), c(-r, r),type="n", axes = FALSE, ann = FALSE)
    i <- order(x[,3])
    xx <- as.vector(rbind(x[i,1], y[i,1], z[i,1], NA))
    yy <- as.vector(rbind(x[i,2], y[i,2], z[i,2], NA))
    polygon(xx, yy, col=col[i], border=NA)
}

## makeTriangles creates a set of triangles for a grid specified by x,
## y and function falues computed with f if f is a function or taken
## from f if f is a matrix.

makeTriangles <- function(x, y, f) {
    if (is.function(f))
        ff <- function(ix, iy) f(x[ix], y[iy])
    else
        ff <- function(ix, iy) f[ix + length(x) * (iy - 1)]
    nx <- length(x) - 1
    ny <- length(y) - 1
    x1 <- c(0, 0)
    y1 <- c(1, 0)
    z1 <- c(1, 1)
    x2 <- x1
    y2 <- z1
    z2 <- c(0, 1)
    g <- expand.grid(x = 1 : nx, y = 1 : ny)
    i1 <- rbind(cbind(g$x + x1[1], g$y + x1[2]),
                cbind(g$x + x2[1], g$y + x2[2]))
    i2 <- rbind(cbind(g$x + y1[1], g$y + y1[2]),
                cbind(g$x + y2[1], g$y + y2[2]))
    i3 <- rbind(cbind(g$x + z1[1], g$y + z1[2]),
                cbind(g$x + z2[1], g$y + z2[2]))
    v1 <- cbind(x[i1[,1]], y[i1[,2]], ff(i1[,1], i1[,2]))
    v2 <- cbind(x[i2[,1]], y[i2[,2]], ff(i2[,1], i2[,2]))
    v3 <- cbind(x[i3[,1]], y[i3[,2]], ff(i3[,1], i3[,2]))
    list(v1 = v1, v2 = v2, v3 = v3)
}

## transformTriangles uses the lattice utilities to transform
## triangles as in the teapot example.

library(lattice) # to get 3D transfromation functions

transformTriangles <- function(triangles, ...) {
    rot.mat <- ltransform3dMatrix(list(...))
    lapply(triangles,function(v) t(ltransform3dto3d(t(v),rot.mat,0)))
}


Inter <- function(f, level, x = 1:dim(f)[1], y = 1:dim(f)[2], z = 1:dim(f)[3]){
    effect <- tclVar("none")
    engine  <- tclVar(4)
    alo  <- tclVar(1)
    ahi  <- tclVar(1)	
    colors <- tclVar("heat.colors")
    markx <- tclVar(0)
    marky <- tclVar(0)
    markz <- tclVar(0)
    lightingbb    <- tclVar("phongDull")
    nl <- length(level)
    effectv <- rep("none",nl)
    markxv<- markyv <- markzv <- rep(0,nl)
    replot <- function(...) {
        layer <- parse(text=tclvalue(layervar))[[1]]
        effectv[layer] <<- ef; markxv[layer] <<- mx;  markyv[layer] <<- my;  markzv[layer] <<- mz 
        result <- eval(substitute(contour3d(f, level, x=x, y=x, z=x,
                                            effect=effectv, markx=markxv, marky=markyv, markz=markzv)))
        zzz <- result$triangles
        l <- result$l
        
        if(al > ah) stop("The minimum of alpha has to be less than or equal to the maximum")
               
        if (cl=="heat.colors") cl <- heat.colors
        else if (cl=="rainbow") cl <- rainbow  
        else if (cl=="terrain.colors") cl <- terrain.colors  
        else if (cl=="topo.colors") cl <- topo.colors
        else if (cl=="cm.colors") cl <- cm.colors
        else if (cl=="gray.colors") cl <- gray.colors

 	#Get the color and alpha right
        color <- rev(cl(nl))
        alpha <- seq(al,ah,len=nl)
        if(en==1){
             color <- rep(color,l)
             alpha <- rep(alpha,l)
           }
        #else if (en==2){
        else{
             color <- rep(color,l/3)
             alpha <- rep(alpha,l/3)
           }
 
       
        
        #Render isosurfaces
        if (en==1){
           rgl.clear()	
           eval(substitute(
                rgl.triangles(x=zzz[,1], y=zzz[,2], z=zzz[,3], 
                color=color, alpha=alpha)))
          rgl.bg(col="white")
	}
	else if (en==2){
           rx <- parse(text=tclvalue(rxvar))[[1]]
           ry <- parse(text=tclvalue(ryvar))[[1]]
           rz <- parse(text=tclvalue(rzvar))[[1]]
           rot.mat <- eval(substitute(ltransform3dMatrix(list(y = ry, x = rx, z=rz))))
           if(nl > 1){
		pdf(file=tclvalue(pathvar),version = "1.4", width = 4, height = 4)
               	par(mar = c(5, 4, 2, 2))
           	#rot.mat <- ltransform3dMatrix(list(y = 10, x = 40, z=-10)) 
	   	grid.newpage()  
           	eval(substitute(plotMesh.gridpoly(triangles=t(zzz), rot.mat=rot.mat, fill=color, alpha=alpha)))
           	dev.off()
           }
           else{
		grid.newpage()  
           	eval(substitute(plotMesh.gridpoly(triangles=t(zzz), rot.mat=rot.mat, fill=color, alpha=alpha)))
	   } 
	}
        else if (en==3){
           #if(nl >1) stop("Engine 3 could not render one isosurface!")
	   if (li=="phongShiny") li <- phongShiny	      
	   else if (li=="phongDull") li <- phongDull
           else if (li=="phongMetal") li <- phongMetal
           else if (li=="perspLighting") li <- perspLighting
           nzzz <- nrow(zzz)/3
	   x <- zzz[c((1:nzzz-1)*3+1),]	
           y <- zzz[c((1:nzzz-1)*3+2),]
           z <- zzz[c((1:nzzz)*3),]
           zzz <- list(v1=x,v2=y,v3=z)
           rx <- parse(text=tclvalue(rxvar))[[1]]
           ry <- parse(text=tclvalue(ryvar))[[1]]
           rz <- parse(text=tclvalue(rzvar))[[1]]
           eval(substitute(ptri(transformTriangles(zzz,z=rz,y=ry,x=rx),lighting=li,
                                color1=color, color2 = color)))
        }
        
    }

  
    regen <- function(...) {
        ef <<- as.character(tclObj(effect))
	en <<- as.numeric(tclObj(engine))
	li <<- as.character(tclObj(lightingbb))
        cl <<- as.character(tclObj(colors))
        al <<- as.numeric(tclObj(alo))
        ah <<- as.numeric(tclObj(ahi))
        mx <<- as.numeric(tclObj(markx))
        my <<- as.numeric(tclObj(marky))
        mz <<- as.numeric(tclObj(markz))
          
    }



    base <- tktoplevel()
    tkwm.title(base, "Render")

    spec.frm <- tkframe(base,borderwidth=2)
    left.frm <- tkframe(spec.frm)
    right.frm <- tkframe(spec.frm)

    ## Five left frames:

    #frame1 is used to set up the object for selecting engines	
    frame1 <- tkframe(left.frm, relief="groove", borderwidth=2)
    tkpack(tklabel(frame1, text="Choose Engine"))
    for ( i in c(1, 2, 3) ) {
        tmp <- tkradiobutton(frame1, command=regen,
                             text=i,value=i,variable=engine)
        tkpack(tmp, anchor="w")

    }
    tkpack(tklabel(frame1, text="Where to you want to save the image for Engine 2?"))
    pathvar <- tclVar(0)
    tkpack(tkentry(frame1, textvariable = pathvar))

    #frame2 is used to input alpha
    frame2 <- tkframe(left.frm, relief="groove", borderwidth=2)
    tkpack(tklabel(frame2, text="Minimum Value of Alpha"))
    tkpack(tkscale(frame2, command=regen, from=0.00, to=1.00,
                   showvalue=T, variable=alo,
                   resolution=0.05, orient="horiz"))
    tkpack(tklabel(frame2, text="Maximum Value of Alpha"))
    tkpack(tkscale(frame2, command=regen, from=0.00, to=1.00,
                   showvalue=T, variable=ahi,
                   resolution=0.05, orient="horiz"))

    #frame3 is used to input colors
    frame3 <- tkframe(left.frm, relief="groove", borderwidth=2)
    tkpack(tklabel(frame3, text="Colors"))
    for ( i in c("rainbow","heat.colors", "terrain.colors", "topo.colors", "cm.colors", "gray.colors") ) {
        tmp <- tkradiobutton(frame3, command=regen,
                             text=i,value=i,variable=colors)
        tkpack(tmp, anchor="w")

    }

    #frame4 is used to input rotation angles
    frame4 <- tkframe(left.frm, relief="groove", borderwidth=2)
    tkpack(tklabel(frame4, text="Rotation Angles for Engine 2 and 3"))
    tkpack(tklabel(frame4, text="direction x"))
    rxvar <- tclVar(10)
    tkpack(tkentry(frame4, textvariable = rxvar))
    tkpack(tklabel(frame4, text="direction y"))
    ryvar <- tclVar(40)
    tkpack(tkentry(frame4, textvariable = ryvar))
    tkpack(tklabel(frame4, text="direction z"))
    rzvar <- tclVar(-10)
    tkpack(tkentry(frame4, textvariable = rzvar))
    

    #frame5 is used to select the lighting for engine 3.      
    frame5 <- tkframe(left.frm, relief="groove", borderwidth=2)
    tkpack(tklabel(frame5, text="Lighting for Engine 3"))
    for ( i in c("phongShiny", "phongDull", "phongMetal", "perspLighting") ) {
        tmp <- tkradiobutton(frame5, command=regen,
                             text=i,value=i,variable=lightingbb)
        tkpack(tmp, anchor="w")

    }


    ## One right frames:
    frame6 <- tkframe(right.frm, relief="groove", borderwidth=2)
    tkpack(tklabel(frame6, text="Special Effect"))
    tkpack(tklabel(frame6, text="Special effect on layer:"))
    layervar <- tclVar(1)
    tkpack(tkentry(frame6, textvariable = layervar))

    for ( i in c("none", "bandx", "bandy", "bandz", "shatter") ) {
        tmp <- tkradiobutton(frame6, command=regen,
                             text=i,value=i,variable=effect)
        tkpack(tmp, anchor="w")

    }

    frame61 <- tkframe(frame6, relief="groove", borderwidth=2)
    tkpack(tkradiobutton(frame61, command=regen, text="corner",value="corner",variable=effect),anchor="w")
    tkpack(tklabel(frame61, text="direction x"))
    for ( i in c(-1, 0, 1) ) {
        tmp <- tkradiobutton(frame61, command=regen,
                             text=i,value=i,variable=markx)
        tkpack(tmp, anchor="w")

    }
    tkpack(tklabel(frame61, text="direction y"))
    for ( i in c(-1,0,1) ) {
        tmp <- tkradiobutton(frame61, command=regen,
                             text=i,value=i,variable=marky)
        tkpack(tmp, anchor="w")

    }
    tkpack(tklabel(frame61, text="direction z"))
    for ( i in c(-1,0,1) ) {
        tmp <- tkradiobutton(frame61, command=regen,
                             text=i,value=i,variable=markz)
        tkpack(tmp, anchor="w")

    }

    tkpack(frame61, fill="x")
    
    tkpack(frame1, frame2, frame3, fill="x")
    tkpack(frame4, frame5, frame6, fill="x")
   

    tkpack(left.frm, right.frm,side="left", anchor="n")


    ## `Bottom frame' (on base):
    ini.but <- tkbutton(base,text="Render",
                      command=replot)

    

    q.but <- tkbutton(base,text="Quit",
                      command=function()tkdestroy(base))

    tkpack(spec.frm, ini.but, q.but)

}

