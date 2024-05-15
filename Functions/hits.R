hits <- function(A, e = 0.000001) {
     n <- nrow(A)
     c.h <- 1
     a <- rep(1, n)/sqrt(n)  #initializing authority scores
     m.h <- a
     m.a <- a
     k <- 0 #initializing counter
     while (c.h > e) {
          if (k > 0) {
                o.h <- h
                }
          o.a <- a
          h <- A %*% o.a #hub scores a function of previous authority scores
          h <- h/norm(h, type = "E")
          a <- t(A) %*% h #authority scores a function of current hub scores
          a <- a/norm(a, type = "E")
          if (k > 0) {
                c.h <- abs(sum(abs(o.h) - abs(h))) #diff. between old and new scores
                }
          m.a <- cbind(m.a, a) #building authority score matrix
          m.h <- cbind(m.h, h) #building hub score matrix
          k <- k + 1 #incrementing counter
     }
return(list(k = k, m.a = round(m.a, 4), m.h = round(m.h, 4)))
}