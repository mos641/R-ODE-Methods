# Mostapha A
# CST8233
# Assignment 3

# function that prints the first menu and validates input
menuOne <- function(){
  # set variable for loop
  menuOption <- "3"
  
  # print menu
  writeLines("\nChoose the method for solving the ODE:\n1. Euler's Method\n2. Runge-Kutta 4th Order Method\n")
  # take input
  menuOption <- readline(prompt = "Please enter your choice: ")
  # if it is not 1 or 2 ask again
  while (menuOption != "1" && menuOption != "2"){
    writeLines("You must enter \"1\" or \"2\", try again.")
    menuOption <- readline(prompt = "Please enter your choice: ")
  }
  
  # return validated input
  return(menuOption)
}

# function that prints the second menu and validates input
menuTwo <- function(){
  # set variable for loop
  menuOption <- "3"
  
  # print menu
  writeLines("\nChoose step size \"h\" (0.8, 0.2, 0.05)")
  # take input
  menuOption <- readline(prompt = "Please enter your choice: ")
  # if it is not 1 or 2 ask again
  while (menuOption != "0.8" && menuOption != "0.2" && menuOption != "0.05" && menuOption != ".8" && menuOption != ".2" && menuOption != ".05"){
    writeLines("You must enter \"0.8\" or \"0.2\" or \"0.05\", try again.")
    menuOption <- readline(prompt = "Please enter your choice: ")
  }
  
  # return validated input
  return(menuOption)
}

# function for solving ODE using euler method
euler <- function(xVec, ynot, stepSize){
  # create a vector for y values
  yVec <- vector(mode = "numeric", length = length(xVec))
  # define ynot as provided value
  yVec[1] <- ynot
  
  # loop through to calculate each y value
  for(i in 1:length(xVec)){
    # f(x,y) = cos4x - 2y     y(i+1) = yi + f(x,y)h
    yVec[i+1] <- yVec[i] + ( (cos(4*xVec[i]) - 2*yVec[i]) * stepSize )
  }
  
  # return our y values
  return(yVec)
}

# function for solving ODE using runge-kutta 4th order
rungekutta <- function(xVec, ynot, stepSize){
  # create a vector for y values
  yVec <- vector(mode = "numeric", length = length(xVec))
  # define ynot as provided value
  yVec[1] <- ynot
  
  # loop through to calculate each y value
  for(i in 1:length(xVec)){
    # f(x,y) = cos4x - 2y     y(i+1) = yi + f(x,y)h
    # calculate k1,k2,k3,k4
    k1 <- (cos(4*xVec[i]) - 2*yVec[i])
    k2 <- (cos(4*(xVec[i] + 0.5*stepSize)) - 2*(yVec[i] + 0.5*k1*stepSize))
    k3 <- (cos(4*(xVec[i] + 0.5*stepSize)) - 2*(yVec[i] + 0.5*k2*stepSize))
    k4 <- (cos(4*(xVec[i] + stepSize)) - 2*(yVec[i] + k3*stepSize))
    
    # caclulate yi
    yVec[i+1] <- yVec[i] + ( ((1/6)*(k1 + 2*k2 + 2*k3 + k4)) * stepSize )
  }
  
  # return our y values
  return(yVec)
}

# function for calculating the solution
solution <- function(xVec, stepSize){
  # create a vector for y values
  yVec <- vector(mode = "numeric", length = length(xVec))
  
  # loop through to calculate each y value
  for(i in 1:length(xVec)){
    # f(x) = 0.1cos4x + 0.2sin4x + 2.9e^(-2x)
    yVec[i] <- 0.1*cos(4*xVec[i]) + 0.2*sin(4*xVec[i]) + 2.9*exp(-2*xVec[i])
  }
  
  # return our y values
  return(yVec)
}

# function for calculating relative error
errors <- function(yEstimate, yExact){
  yVec <- vector(mode = "numeric", length = length(yExact))
  # loop to calculate error
  for(i in 1:length(yExact)){
    absError <- abs(yEstimate[i] - yExact[i])
    yVec[i] <- abs(absError / yExact[i]) * 100
  }
  
  # return vector
  return(yVec)
}

# main function to loop through menus
main <- function(){
  menuOption <- "0"
  start <- 0
  end <- 2
  ynot <- 3
  
  # loop through menu indefinitely
  while(1){
    # print first menu
    menuOption <- menuOne()
    
    # print the second menu
    stepSize <- as.numeric(menuTwo())
    # create x vector
    xVec <- seq(from = start, to = end, by = stepSize)
    
    # if they chose eulers or runge-kutta call the relevant function
    if (menuOption == "1"){
      yEstimate <- euler(xVec, ynot, stepSize)
    } else {
      yEstimate <- rungekutta(xVec, ynot, stepSize)
    }
    
    # calculate the exact values
    yExact <- solution(xVec, stepSize)
    
    # caclulate the percent error
    errors <- errors(yEstimate, yExact)
    
    # print table
    writeLines("")
    table <- matrix(c(xVec[2:length(xVec)], yExact[2:length(yExact)], yEstimate[2:length(yExact)], errors[2:length(yExact)]), ncol = 4)
    colnames(table) <- c("Time(seconds)", "  Exact Temp (C)", "  Estimated Temp (C)", "  Percentage Error (%)")
    prmatrix(table, rowlab=rep("",length(yExact)))
    
  } # end of loop
}

# call function to run program
main()

