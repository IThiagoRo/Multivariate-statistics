#PartA

#1)
mu = c(1, 2, 4, 3, 5)
sigma = rbind(c(1, 4, 6, 1, 6), 
              c(4, 2, 9, 7, 3),
              c(6, 8, 4, 10, 5),
              c(1, 7, 10, 7, 8),
              c(6, 3, 5, 8, 9))
m = rbind(c(1, 0, 0, 0, -2), c(0, 1, 1, 3, 0))
d = c(1, 2)


mu_y = m %*% (mu) + (d)

sigma_y = m %*% sigma %*% t(m)

x_1 = rbind(c(0,1,0,0,0),
            c(0,0,0,1,0),
            c(0,0,0,0,1))
mu_1 = x_1 %*% mu
sigma_1 = x_1 %*% sigma %*% t(x_1)


x_2 = rbind(c(1,0,0,0,0),
            c(0,0,1,0,0))
mu_2 = x_2 %*% mu
sigma_2 = x_2 %*% sigma %*% t(x_2)

x2 = c(1,2)
sigma12 = rbind(c(4, 1, 6),
                c(9, 10, 5))

mu_1 + (t(sigma12) %*% solve(sigma_2) %*%(x2-mu_2))


sigma_1 - t(sigma12) %*% solve(sigma_2) %*% sigma12
        