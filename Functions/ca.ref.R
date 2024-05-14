ca.ref <- function(A, e = 0.000001) { 
     n <- nrow(A)
     c.h <- 1
     c.a <- 1
     h <- rowSums(A)  #initializing hub scores to outdegrees
     a <- colSums(A)  #initializing authority scores to indegrees
     m.h <- h
     m.a <- a
     k <- 0 #initializing counter
     while (c.h > e & c.a > e) {
          o.h <- h
          o.a <- a
          h <- (A %*% a)/rowSums(A)  #at step 1 h equals the sum of the indegrees of each of i's out-neighbors divided by the i's outdegree
          h <- scale(h) #standardizing h
          a <- (t(A) %*% h)/colSums(A)  #at step 1 a equals the sum of the outdegrees of each of i's in-neighbors divided by the i's indegree
          a <- scale(a) #standardizing a
          c.h <- abs(sum(abs(o.h) - abs(h)))
          c.a <- abs(sum(abs(o.a) - abs(a)))
          m.a <- cbind(m.a, a) #building authority score matrix
          m.h <- cbind(m.h, h) #building hub score matrix
          k <- k + 1 #incrementing counter
     }
return(list(k = k, m.a = round(m.a, 4), m.h = round(m.h, 4)))
}