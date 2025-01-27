

#' Extract syllables from an audio file
#'
#' @param audio_file
#'
#' @returns
#' @export
#'
#' @examples
extract_syllables <- function(audio_file) {

  trimmed_files <- pyin::pyin(audio_file, normalise = TRUE) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(trimmed_audio_file = sox_trim(file_name, onset, dur)) %>%
    dplyr::ungroup()

  audio_features <- trimmed_files %>%
    dplyr::pull(trimmed_audio_file) %>%
    purrr::map_dfr(function(f) {
      tryCatch({

        suppressMessages(suppressWarnings({
          musicassessr::extract_audio_features(f)
        }))

      }, error = function(err) {
        logging::logerror(err)
        tibble::tibble(file_key = NA, V1_mean = NA, V1_sd = NA, V2_mean = NA,
                       V2_sd = NA, V3_mean = NA, V3_sd = NA, V4_mean = NA, V4_sd = NA,
                       V5_mean = NA, V5_sd = NA, V6_mean = NA, V6_sd = NA, V7_mean = NA,
                       V7_sd = NA, V8_mean = NA, V8_sd = NA, V9_mean = NA, V9_sd = NA,
                       V10_mean = NA, V10_sd = NA, V11_mean = NA, V11_sd = NA, V12_mean = NA,
                       V12_sd = NA, mean = NA, sd = NA, median = NA, sem = NA, mode = NA,
                       Q25 = NA, Q75 = NA, IQR = NA, cent = NA, skewness = NA, kurtosis = NA,
                       sfm = NA, sh = NA, prec = NA, duration = NA, sample_rate = NA,
                       bit_depth = NA, rms = NA, zero_crossing_rate = NA, shannon_entropy = NA,
                       acoustic_complexity_index = NA, acoustic_diversity_index = NA,
                       acoustic_diversity_index2 = NA, acoustic_diversity_index3 = NA,
                       acoustic_entropy_index = NA, acoustic_evenness_index = NA,
                       bioacoustic_index = NA, frequency_peaks_number = NA, amplitude_index = NA,
                       normalized_difference_soundscape_index = NA, spectral_entropy_ndsi = NA,
                       spectral_entropy_anthrophony = NA, spectral_entropy_biophony = NA,
                       spectral_entropy2 = NA, temporal_entropy = NA)
      })
    }, .progress = TRUE)



  pred_data <- trimmed_files %>%
    cbind(audio_features) %>%
    dplyr::rename(midi_note = note) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.list), ~ purrr::map_dbl(., as.numeric)))

  loadNamespace('workflows')


  preds <- lyricassessr::xgb_wflow_fit_bundled %>%
    bundle::unbundle() %>%
    predict(new_data = pred_data) %>%
    dplyr::rename(SyllablePrediction = .pred_class)

  cbind(pred_data, preds) %>%
    dplyr::select(SyllablePrediction) # Maybe return others in the future

}

# t <- extract_syllables('~/lyricassessr/data-raw/NicoleVocals/sequenz510.WAV')
