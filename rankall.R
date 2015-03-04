############################################################################
##Takes outcome and ranking and generates data frame of hopsital for each state with that ranking

rankall <- function(outcome, num = "best"){
  options(warn=-1)
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- data[order(data$State),]
  all_states <- unique(data$State) 
  if (outcome == "heart attack"){
    dat_sub <- data.frame(data[, c(2, 7, 11)])
  } else {
    if (outcome == "heart failure"){
      dat_sub <- data.frame(data[, c(2, 7, 17)])
    } else {
      if (outcome == "pneumonia"){
        dat_sub <- data.frame(data[, c(2, 7, 23)])
      } else {
        stop("invalid outcome")
      }
    }
  }
  colnames(dat_sub) <- c("Hospital.Name", "State", "Mortality")
  dat_sub <- transform(dat_sub, Mortality = as.numeric(Mortality))
  Final <- data.frame()
  for(i in seq_along(all_states)){
    Data_sub <- dat_sub[which(dat_sub$State == all_states[i]),]
    colnames(Data_sub) <- c("Hospital.Name", "State", "Mortality")
    Data_sub <- transform(Data_sub, Mortality = as.numeric(Mortality))
    Data_sub <- Data_sub[order(Data_sub$Hospital.Name),]
    ndx <- order(Data_sub$Mortality, na.last = NA)  
    Z <- Data_sub[ndx,]
    Rank <- vector(mode = "numeric")
    Rank <- c(Rank, 1:nrow(Z))
    ZZ <- cbind(Z, Rank)
    if (num %in% "best"){
      Output <- ZZ[1, 1]
      Output <- cbind(Output, all_states[i])
    } else {
      if (num %in% "worst"){
        Out <- tail(ZZ, n = 1)
        Output <- Out[1, 1]
        Output <- csubind(Output, all_states[i])
      } else {
        Out <- ZZ[which(ZZ[, "Rank"] %in% num),]
        Output <- Out[1, 1]
        Output <- cbind(Output, all_states[i])
      }
    }
    Final <- rbind(Final, Output)
  }
  colnames(Final) <- c("hospital", "state")
  Final
}