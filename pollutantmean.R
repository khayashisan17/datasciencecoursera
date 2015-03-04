
pollutantmean <- function(directory, pollutant, id = 1:332) {
  full_files <- list.files(directory, full.names = TRUE)
  tmp <- lapply(full_files, read.csv)
  output <- do.call(rbind, tmp)
  dat_subset <- output[which(output[, "ID"] %in% id),]
  if(pollutant == "nitrate"){
    pollutant_mean <- mean(dat_subset$nitrate, na.rm = TRUE)
  } else {
        pollutant_mean <- mean(dat_subset$sulfate, na.rm = TRUE)
      }
  pollutant_mean
}

