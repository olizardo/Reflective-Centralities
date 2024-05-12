bonacich <- function(A, e = 0.000001) {
     n <- nrow(A)
     c <- rep(1, n)
     m <- c
     w <- 1
     k <- 0
     while (w > e) {
          o.c <- c
          c <- A %*% c
          c <- c/norm(c, type = "E")
          m <- cbind(m, c)
          w <- abs(sum(abs(c) - abs(o.c)))
          k <- k + 1
     }
return(list(k = k, m = m))
}