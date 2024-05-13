salsa2 <- function(A, e = 0.000001) { 
     n <- nrow(A)
     c.h <- 1
     c.a <- 1
     X <- A/rowSums(A)  
     Z <- t(A)/colSums(A)  
     h <- rep(1, 12)
     a <- rep(1, 12)
     m.h <- h
     m.a <- a
     k <- 0 #initializing counter
     while (c.h > e & c.a > e) {
          o.h <- h
          o.a <- a
          h <- Z %*% a
          h <- h/norm(h, type = "E")
          a <- t(X) %*% h
          a <- a/norm(a, type = "E")
          c.h <- abs(sum(abs(o.h) - abs(h)))
          c.a <- abs(sum(abs(o.a) - abs(a)))
          m.a <- cbind(m.a, a) #building authority score matrix
          m.h <- cbind(m.h, h) #building hub score matrix
          k <- k + 1 #incrementing counter
     }
     return(list(k = k, m.a = round(m.a, 4), m.h = round(m.h, 4)))
}