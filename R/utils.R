


sox_trim <- function(file,
                     start,
                     dur,
                     verbose = FALSE,
                     padding_beginning = 0.35,
                     padding_end = 0.4,
                     cuts_dir = "cuts/") {
  # Convert to mp3 as well
  new_name <- paste0(cuts_dir, tools::file_path_sans_ext(basename(file)),
                     '_', start, '_', dur, '.wav')

  start <- start-padding_beginning

  if(start < 0) {
    start <- 0
  }

  cmd <- paste0('sox ', file,  ' ', new_name, ' trim ',
                start, ' ', dur + padding_end, " rate 16000 channels 1") # This is the homebrew version with mp3 support
  if(verbose) {
    print(cmd)
  }
  system(cmd)
  return(new_name)
}
