# this is a script to run simple simulation with balanced group

#' Calculate the likelihood of Bernoulli distribution
bernoulliLikeli <- function(theta, y, n) {
  exp(y * log(theta) + (n - y) * log1p(-theta))
}

bernoulliLikeliSingle <- function(theta, y) {
  ifelse(y == 1, theta, 1 - theta)
}

#' Calculate the GROW e-value for balanced design
calculateOptimalEVal <- function(ya, yb, groupSize = 1, thetaTrue) {
  # unpack parameters
  thetaA <- thetaTrue[["thetaA"]]
  thetaB <- thetaTrue[["thetaB"]]
  theta0 <- (thetaA + thetaB) / 2

  naLag <- nbLag <- seq_len(length(ya)) - 1

  # ensure lag still work when nbObs = 1
  yaLag <- c(0, cumsum(ya)[-length(ya)])
  ybLag <- c(0, cumsum(yb)[-length(yb)])

  likeliA <- bernoulliLikeliSingle(thetaA, ya)
  likeliB <- bernoulliLikeliSingle(thetaB, yb)
  likeli0a <- bernoulliLikeliSingle(theta0, ya)
  likeli0b <- bernoulliLikeliSingle(theta0, yb)

  eVec <- (likeliA * likeliB) / (likeli0a * likeli0b)

  return(prod(eVec))
}



#' Assumed the Beta prior hyper parameters are same across group a and b
#'
#' This method only assumes a single point for the prior distribution for
#' both group a and group b. betaA1 = betaA2 = betaB1 = betaB2 = betaPrior
calculateEValSingle <- function(ya, yb, groupSize = 1, betaPrior = 0.18) {
  # update from j = 1 to m, theta is updated looking back at Y_{(j-1), a/b}
  # (j - 1) * groupSize = 0 to (m - 1) * groupSize
  # groupSize = 2, starts with ZERO, and proceeds to final (j - 1) data block
  naLag <- nbLag <- seq_len(length(ya)) - 1

  # ensure lag still work when nbObs = 1
  yaLag <- c(0, cumsum(ya)[-length(ya)])
  ybLag <- c(0, cumsum(yb)[-length(yb)])

  thetaA <- (yaLag + betaPrior) / (naLag + 2 * betaPrior)
  thetaB <- (ybLag + betaPrior) / (nbLag + 2 * betaPrior)
  theta0 <- (thetaA + thetaB) / 2

  likeliA <- bernoulliLikeliSingle(thetaA, ya)
  likeliB <- bernoulliLikeliSingle(thetaB, yb)
  likeli0a <- bernoulliLikeliSingle(theta0, ya)
  likeli0b <- bernoulliLikeliSingle(theta0, yb)

  eVec <- (likeliA * likeliB) / (likeli0a * likeli0b)

  return(prod(eVec))
}

calculateEValLoop <- function(ya, yb, groupSize, betaPrior = 0.18) {
  # Initialize values
  thetaA <- thetaB <- theta0 <- 0.5
  eVal <- 1
  naLag <- nbLag <- 0
  yaLag <- ybLag <- 0

  for (i in seq_along(ya)) {
    # Calculate E-value
    eCur <- (bernoulliLikeliSingle(thetaA, ya[i]) * bernoulliLikeliSingle(thetaB, yb[i])) /
      (bernoulliLikeliSingle(theta0, ya[i]) * bernoulliLikeliSingle(theta0, yb[i]))

    eVal <- eVal * eCur

    # Update cumulative counts
    naLag <- naLag + 1
    nbLag <- nbLag + 1
    yaLag <- yaLag + ya[i]
    ybLag <- ybLag + yb[i]

    # Update theta estimates using Bayesian updating
    thetaA <- (yaLag + betaPrior) / (naLag + 2 * betaPrior)
    thetaB <- (ybLag + betaPrior) / (nbLag + 2 * betaPrior)
    theta0 <- (thetaA + thetaB) / 2
  }

  return(eVal)
}

set.seed(123)
thetaTrue <- list(
  "thetaA" = 0.3,
  "thetaB" = 0.7
)

m <- 2000

ya <- rbinom(m, 1, thetaTrue[["thetaA"]])
yb <- rbinom(m, 1, thetaTrue[["thetaB"]])

# ya <- c(0, 0, 0, 0, 1)
# yb <- c(1, 1, 1, 1, 1)

print(calculateEValSingle(ya, yb, 1, 0.18))
print(calculateOptimalEVal(ya, yb, 1, thetaTrue))


# calculate the difference between the GRO e and learnt e in (2.7) in Turner's
# paper
calculateEValOverBeta <- function(ya, yb, groupSize, betaPriorList, thetaTrue) {
  # Initialize vectors to store results
  eGRO <- log(sapply(seq_along(ya), function(i) {
    yaTmp <- ya[1:i]
    ybTmp <- yb[1:i]
    calculateOptimalEVal(yaTmp, ybTmp, groupSize, thetaTrue)
  }))

  # Initialize a list to store eBayes for each betaPrior
  eBayesList <- list()

  # Loop over the list of betaPriorList
  for (betaPrior in betaPriorList) {
    eBayesList[[as.character(betaPrior)]] <- eGRO - log(sapply(seq_along(ya), function(i) {
      yaTmp <- ya[1:i]
      ybTmp <- yb[1:i]
      calculateEValSingle(yaTmp, ybTmp, groupSize, betaPrior)
    }))
  }

  # Return the results as a list
  return(eBayesList)
}

# Function to plot the results of eGRO and eBayes
plotEBayesList <- function(eBayesList, betaPriorList) {
  colors <- rainbow(length(eBayesList))
  
  
  # Plot the first eBayes (Red line for the first betaPrior)
  plot(seq_along(eBayesList[[1]]), eBayesList[[1]],
    type = "l", col = colors[1],
    xlab = "m", ylab = "log",
    main = expression("eGRO - eBayes"), 
    ylim = range(unlist(eBayesList))
  )

  # Plot the remaining eBayes for each betaPrior with different colors
  for (i in seq_along(eBayesList)[-1]) {
    lines(seq_along(eBayesList[[i]]), eBayesList[[i]], col = colors[i], lty = 1)
  }

  # Add a legend
  legend("bottomright",
    legend = paste("Beta =", betaPriorList),
    col = colors,
    lty = 1, cex = 0.7
  )
}

# Example usage
# betaPriorList <- c(0.18, seq(from = 0.02, to = 0.4, by = 0.02))
betaPriorList <- seq(from = 0.05, to = 0.9, by = 0.12)
eBayesList <- calculateEValOverBeta(ya, yb, groupSize = 1, betaPriorList, thetaTrue = thetaTrue)

# Plot the results
plotEBayesList(eBayesList, betaPriorList)
