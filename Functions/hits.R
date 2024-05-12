hits <- function(A, e = 0.000001) { #From Fouss et al. 2016, p. 226
     n <- nrow(A)
     c.h <- 1
     c.a <- 1
     h <- rep(1, 12)  #initializing hub scores
     a <- rep(1, 12)  #initializing authority scores
     m.h <- h
     m.a <- a
     k <- 0 #initializing counter
     while (c.h > e & c.a > e) {
          o.h <- h
          o.a <- a
          h <- A %*% a #hub scores a function of authority scores
          h <- h/norm(h, type = "E")
          a <- t(A) %*% h #authority scores a function of hub scores
          a <- a/norm(a, type = "E")
          c.h <- abs(sum(abs(o.h) - abs(h)))
          c.a <- abs(sum(abs(o.a) - abs(a)))
          m.a <- cbind(m.a, a) #building authority score matrix
          m.h <- cbind(m.h, h) #building hub score matrix
          k <- k + 1 #incrementing counter
     }
return(list(k = k, m.a = round(m.a, 4), m.h = round(m.h, 4)))
}