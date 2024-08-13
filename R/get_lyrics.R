

get_lyrics <- function(audio_file,
                       prompt = NULL,
                       temperature = NULL) {

  logging::loginfo("Getting lyrics for %s", audio_file)

  api_key <- Sys.getenv("OPENAI_API_KEY")

  logging::loginfo("prompt %s: ", prompt)

  tryCatch({

    response <- httr::POST(
      "https://api.openai.com/v1/audio/transcriptions",
      httr::add_headers(
        `Authorization` = paste("Bearer", api_key),
        `Content-Type` = "multipart/form-data"
      ),
      body = list(
        file = httr::upload_file(audio_file),
        model = "whisper-1",
        prompt = prompt,
        temperature = temperature
      )
    )

    logging::loginfo("response: %s", response)

    result <- httr::content(response, "text",
                  encoding = "UTF-8") %>%
      rjson::fromJSON() %>%
      purrr::pluck('text')
  }, error = function(err) {

    logging::logerror(err)

    NA
  })

}


