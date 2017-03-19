# IS curve equation
# y = output; c = marginal propensity to consume; alpha = 1/(1-c) A = autonomous component;
# b = Senstivity to interest rates; i = interest rates
# I = I.0 - b*i (investment equation)
# y = alpha*A - alpha*b*i

IS.curve <- function(c, A, b, i) {
    y = (1 / (1 - c)) * A - (1 / (1 - c)) * b * i
    return(y)
}


# LM curve equation
# Usual definitions follow from above; k = sensitivity of transactions demand for money to change in income
# h = senstivity of speculative demand for money to change in income; MS = real money supply in the economy 
# ((nominal money)/(prices)
# y = MS/k - (h/k)*i

LM.curve <- function(ms, h, k, i) {
    y = ms / k + (h / k) * i
    return(y)
}

# Function to calculate the point of intersection of the IS and LM curves

Intersect <- function(c, A, b, ms, h, k, i) # Using cramers rule to solve a system of simultaneous equations
    {
    a1 <- (1 / (1 - c)) * b
    b1 <- 1
    c1 <- (1 / (1 - c)) * A
    a2 <- -(h / k)
    b2 <- 1
    c2 <- ms / k
    stopifnot(a1 * b2 - b1 * a2 != 0) # We need to make sure that the determinant is non-zero.
    return(list(
            x = (b2 * c1 - b1 * c2) / (a1 * b2 - b1 * a2),
            y = (a1 * c2 - a2 * c1) / (a1 * b2 - b1 * a2)
            )
        )
}

# IS curve plotting

autonomous.component <- 100
mpc <- 0.5
b <- 0.75
i <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
y.is <- IS.curve(mpc, autonomous.component, b, i)

# LM curve plotting

ms <- 143
h <- 1.8
k <- 0.8
y.lm <- LM.curve(ms, h, k, i)

# Effect of government spending or tax cut of any other form of fiscal policy

autonomous.component.gov <- 102 # Say the government spending increased by 2 units
y.is.gov <- IS.curve(mpc, autonomous.component.gov, b, i)

# Effect of increasing money supply by the central bank (or monetary policy)

ms.mon <- 145 # money supply increased by 2 units
y.lm.mon <- LM.curve(ms.mon, h, k, i)

# Finding the point of intersection of the IS-LM curves

intersect <- Intersect(mpc, autonomous.component, b, ms, h, k, i)
intersect.gov <- Intersect(mpc, autonomous.component.gov, b, ms, h, k, i)
intersect.mon <- Intersect(mpc, autonomous.component, b, ms.mon, h, k, i)
intersect.mon.gov <- Intersect(mpc, autonomous.component.gov, b, ms.mon, h, k, i)


# IS-LM framework

plot(y.is, i, xlim = c(180, 205), ylim = c(-1, 10), type = "l",
main = "Fiscal and Monetary policy together", xlab = "Output(Y)",
ylab = "Interest rates(I)", col = "red")

lines(y.lm, i, type = "l", col = "green")
lines(y.is.gov, i, type = "l", lty = 2, col = "red")
lines(y.lm.mon, i, type = "l", lty = 2, col = "green")
legend("bottomright", c("IS", "LM"), cex = 0.5, col = c("red", "green"), lwd = 2, bty = "n")

lines(c(-1, intersect$y, intersect$y), c(intersect$x, intersect$x, -1), lty = 2, col = 'black')
lines(c(-1, intersect.gov$y, intersect.gov$y), c(intersect.gov$x, intersect.gov$x, -1), lty = 2, col = 'black')
lines(c(-1, intersect.mon$y, intersect.mon$y), c(intersect.mon$x, intersect.mon$x, -1), lty = 2, col = 'black')
lines(c(-1, intersect.mon.gov$y, intersect.mon.gov$y),
c(intersect.mon.gov$x, intersect.mon.gov$x, -1), lty = 2, col = 'black')