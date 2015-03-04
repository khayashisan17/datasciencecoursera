
complete <- function(directory, id = 1:332) {
	full_files <- list.files(directory, full.names = TRUE)
	dat_subset <- full_files[id]
	tmp <- lapply(dat_subset, read.csv)
	ok <- lapply(tmp, complete.cases)
	nobs <- sapply(ok, sum)
	output <- data.frame(id = id, nobs = nobs)
	output
}

