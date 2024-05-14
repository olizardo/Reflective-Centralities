salsa <- function(A, e = 0.000001) { 
     n <- nrow(A)
     o.d <- rowSums(A)
     i.d <- colSums(A)
     c.h <- 1
     h <- rep(1/n, n)  #initializing hub scores
     m.h <- h
     k <- 0 #initializing counter
     while (c.h > e) {
        o.h <- h
        t <- rep(0, n)
        for (i in 1:n) {
          i.out <- which(A[i, ] == 1) 
          for (j in i.out) {
             j.in <- which(A[, j] == 1) 
                for (k in j.in) {
                   t[i] <- t[i] + (h[k] / (i.d[j] * o.d[k]))
                }
            }
        }
        h <- t
        h <- h/norm(as.matrix(h), type = "E")
        c.h <- abs(sum(abs(o.h) - abs(h)))
        m.h <- cbind(m.h, h) #building hub score matrix
        k <- k + 1 #incrementing counter
     }
     
     c.a <- 1
     a <- rep(1/n, n)  #initializing hub scores
     m.a <- a
     k <- 0 #initializing counter
     while (c.a > e) {
             o.a <- a
             t <- rep(0, n)
             for (i in 1:n) {
                     i.in <- which(A[, i] == 1) 
                     for (j in i.in) {
                             j.out <- which(A[j, ] == 1) 
                             for (k in j.out) {
                                     t[i] <- t[i] + (a[k] / (o.d[j] * i.d[k]))
                             }
                     }
             }
             a <- t
             a <- a/norm(as.matrix(a), type = "E")
             c.a <- abs(sum(abs(o.a) - abs(a)))
             m.a <- cbind(m.a, a) #building authhority score matrix
             k <- k + 1 #incrementing counter
     }
return(list(k = k, m.h = round(m.h, 4), m.a = round(m.a, 4)))
}