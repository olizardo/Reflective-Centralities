salsa <- function(A, e = 0.000001) { #From Fouss et al. 2016, p. 226
     n <- nrow(A)
     c.h <- 1
     c.a <- 1
     h <- rep(1, n)/sqrt(n)  #initializing hub scores
     a <- rep(1, n)/sqrt(n)  #initializing authority scores
     m.h <- h
     m.a <- a
     Wr <- A/rowSums(A)
     Wc <- A/colSums(A)
     k <- 0 #initializing counter
     while (c.h > e) {
          o.h <- h
          o.a <- a
          a <- t(Wc) %*% o.h #authority scores a function of previous hub scores
          a <- a/norm(a, type = "E")
          h <- Wr %*% o.a #hub scores a function of previous authority scores
          h <- h/norm(h, type = "E")
          m.a <- cbind(m.a, a) #building authority score matrix
          m.h <- cbind(m.h, h) #building hub score matrix
          if (k >= 5) {
                  c.a <- abs(sum(abs(m.a[,k-2]) - abs(m.a[,k])))
                  c.h <- abs(sum(abs(m.h[,k-2]) - abs(m.h[,k])))
                }
          k <- k + 1 #incrementing counter
        }
     return(list(k = k, 
                 m.a = round(m.a, 4), 
                 m.h = round(m.h, 4),
                 Wr = Wr,
                 Wc = Wc))
}