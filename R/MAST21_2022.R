

#' Run the MAST21 2022 protocol
#'
#' @param musicassessr_state
#'
#' @return
#' @export
#'
#' @examples
deploy_MAST21_2022 <- function(musicassessr_state = "test", dinosaur_instructions = "Please press the “record” button and read the sentence below out loud: ") {

  psychTestR::make_test(
    psychTestR::join(
      psychTestR::new_timeline(
        psychTestR::join(

          musicassessr::musicassessr_init(),

          upei_intro(musicassessr_state),

          musicassessr::setup_pages(input = "microphone", absolute_url = "https://musicog.ca/"),

          psychTestR::one_button_page('Although the next short test involves singing,
                                      we would like to start off by asking you to read out loud four short sentences all beginning with the phrase
                                      "The hungry purple dinosaur".  The sentences may sound silly, but together,
                                      they cover all the sounds of the English language. '),

          musicassessr::record_audio_page(label = "say_pd1",
                                          page_text = shiny::tags$div(
                                                        shiny::tags$p(dinosaur_instructions),
                                                        shiny::tags$p(shiny::tags$strong("The hungry purple dinosaur ate the kind, zingy fox."))),
                                          auto_next_page = TRUE),

          musicassessr::record_audio_page(label = "say_pd2",
                                          page_text = shiny::tags$div(
                                              shiny::tags$p(dinosaur_instructions),
                                              shiny::tags$p(shiny::tags$strong("The hungry purple dinosaur ate the jabbering toy crab."))),
                                          auto_next_page = TRUE),

          musicassessr::record_audio_page(label = "say_pd3",
                                          page_text = shiny::tags$div(
                                                        shiny::tags$p(dinosaur_instructions),
                                                        shiny::tags$p(shiny::tags$strong("The hungry purple dinosaur ate the low mad whale. "))),
                                          auto_next_page = TRUE),

          musicassessr::record_audio_page(label = "say_pd4",
                                          page_text = shiny::tags$div(
                                                        shiny::tags$p(dinosaur_instructions),
                                                        shiny::tags$p(shiny::tags$strong("The hungry purple dinosaur now started vending and quacking."))),
                                          auto_next_page = TRUE),

          musicassessr::long_tone_trials(num_items = 6),

          MAST21_wav(),

          psychTestR::elt_save_results_to_disk(complete = FALSE)

          ), dict  = musicassessr::dict(NULL)), # end timeline (it's not needed from here onwards, and the SAA is embedded in UPEI_extra_questions, so to avoid nesting)

          psyquest::GMS(),

          UPEI_extra_questions(),

          psychTestR::final_page(
              shiny::tags$div(
                shiny::tags$p("Please click on the following link to go to the final test of this session: ", shiny::tags$a(" click here",
                                                           href = "https://www.google.com/intl/en_uk/chrome/", target = "_blank"), ".")
            ))
          ),
    opt = upei_test_options(musicassessr_state)
  )
}

# deploy_MAST21_2022('test')

# install_github('sebsilas/MAST21')

