#######################################################
######Returns hospital with designated ranking for certain state and outcome

rankhospital <- function(state, outcome, num = "best"){
  options(warn=-1)  			##suppress warnings
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  all_states <- unique(data$State)	
  if (state %in% data$State){
    y <- data[which(data[, "State"] == state),]
  } else {
    stop("invalid state")
  }
  if (outcome == "heart attack"){
    dat_sub <- data.frame(y[, c(2, 11)])
  } else {
    if (outcome == "heart failure"){
      dat_sub <- data.frame(y[, c(2, 17)])
    } else {
      if (outcome == "pneumonia"){
        dat_sub <- data.frame(y[, c(2, 23)])
      } else {
        stop("invalid outcome")
      }
    }
  }
  colnames(dat_sub) <- c("Hospital.Name", "Mortality")
  Data_sub <- transform(dat_sub, Mortality = as.numeric(Mortality))
  Data_sub <- Data_sub[order(Data_sub$Hospital.Name),]       ##Sorts alphabetically
  ndx <- order(Data_sub$Mortality, na.last = NA)             ##Sorts by rank
  Z <- Data_sub[ndx,]
  Rank <- vector(mode = "numeric")
  Rank <- c(Rank, 1:nrow(Z))
  ZZ <- cbind(Z, Rank)
  if (num %in% "best"){
    Output <- ZZ[1, 1]
  } else {
    if (num %in% "worst"){
      Out <- tail(ZZ, n = 1)
      Output <- Out[1, 1]
    } else {
      Out <- ZZ[which(ZZ[, "Rank"] %in% num),]
      Output <- Out[1, 1]
    }
  }
  Output
}
