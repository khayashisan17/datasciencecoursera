#################################################################
#### Function takes outcome input and determines best ("lowest") hospital for that outcome

best <- function(state, outcome){
	options(warn=-1)				##suppress warnings
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	all_states <- unique(data$State)	
	if (state %in% data$State){
		y <- data[which(data[, "State"] == state),]
	} else {
		stop("invalid state")
	}
	if (outcome == "heart attack"){
		x <- as.numeric(y[, 11])
		dat_sub <- data.frame(y[, c(2, 11)])
	} else {
		if (outcome == "heart failure"){
			x <- as.numeric(y[, 17])
			dat_sub <- data.frame(y[, c(2, 17)])
		} else {
			if (outcome == "pneumonia"){
				x <- as.numeric(y[, 23])
				dat_sub <- data.frame(y[, c(2, 23)])
			} else {
				stop("invalid outcome")
			}
		}
	}
	min_value <- min(x[!is.na(x)])
	colnames(dat_sub) <- c("Hospital.Name", "Mortality")
	Data_sub <- transform(dat_sub, Mortality = as.numeric(Mortality))
	Output <- Data_sub[which(Data_sub[, 2] == min_value), ]
	Output[1, 1]	
}
