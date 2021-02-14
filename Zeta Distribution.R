# AUTHOR:	Ali Alimohammadi
#   DATE:	January 10th, 2021.
draw.zeta <- function(nrep, alpha)
{
    if (alpha <= 1) { stop("Alpha must be greater than 1!\n") }
    zeta <- numeric(nrep)
    for (i in 1:nrep)
    {
        index <- 0;
        while (index < 1)
        {
            u1 <- runif(1)
            u2 <- runif(1)
            x <- floor(u1 ^ (-1 / (alpha - 1)))
            t <- (1 + 1 / x) ^ (alpha - 1)
            w <- x < (t / (t - 1)) * (2 ^ (alpha - 1) - 1) / (2 ^ (alpha - 1) * u2)
            zeta[i] <- x
            index <- sum(w)
        }
    }
    zeta
}