
corr <- function(directory, threshold = 0){
	files_full <- list.files(directory, full.names = TRUE)
	tmp <- lapply(files_full, read.csv)
	dat_complete <- lapply(tmp, na.omit)
	n.complete <- sapply(dat_complete, nrow)
	corr_vector <- vector(mode = "numeric")
	for (i in seq_along(n.complete)){
		if (n.complete[i] > threshold){
			x <- data.frame(dat_complete[i])   		
			corr_value <- cor(x$nitrate, x$sulfate)
			corr_vector <- c(corr_vector, corr_value)
		}
	}
	corr_vector
}



