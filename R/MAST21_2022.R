
daa_low <- function() {
  psychTestR::conditional(
    test = function(state, ...) {
      range <- psychTestR::get_global("range", state)
      range %in% c("Baritone", "Bass", "Tenor")
    },
    logic = MAST_wav(trial_type = "daa", high_or_low = "low")
  )
}

daa_high <- function() {
  psychTestR::conditional(
    test = function(state, ...) {
      range <- psychTestR::get_global("range", state)
      range %in% c("Soprano", "Alto")
    },
    logic = MAST_wav(trial_type = "daa", high_or_low = "high")
  )
}

doo_low <- function() {
  psychTestR::conditional(
    test = function(state, ...) {
      print('doo_low...')
      range <- psychTestR::get_global("range", state)
      print(range)
      range %in% c("Baritone", "Bass", "Tenor")
    },
    logic = MAST_wav(trial_type = "doo", high_or_low = "low")
  )
}

doo_high <- function() {
  psychTestR::conditional(
    test = function(state, ...) {
      print('doo_high...')
      range <- psychTestR::get_global("range", state)
      print(range)
      range %in% c("Soprano", "Alto")
    },
    logic = MAST_wav(trial_type = "doo", high_or_low = "high")
  )
}

condition_one <- function() {
  # # daa then doo
  psychTestR::conditional(test = function(state, ...) {
    psychTestR::get_global("snap", state) == 1
    }, logic = psychTestR::join(
      psychTestR::one_button_page("In the following trials, you will sing back melodies. Please sing with a \"Daah\" sound."),
      daa_low(),
      # OR
      daa_high(),
      psychTestR::elt_save_results_to_disk(complete = FALSE),

      musicassessr::sing_happy_birthday_page(feedback = FALSE, label = "sing_hbd2", text = "Please sing Happy Birthday."),

      psychTestR::elt_save_results_to_disk(complete = FALSE),

      psychTestR::one_button_page("In the following trials, you will sing back melodies. Please sing with a \"Dooo\" sound."),

      doo_low(),
      # OR
      doo_high()

      ))
}

condition_two <- function() {
  # # doo then daa
  psychTestR::conditional(test = function(state, ...) {
    psychTestR::get_global("snap", state) == 2
  }, logic = psychTestR::join(
    psychTestR::one_button_page("In the following trials, you will sing back melodies. Please sing with a \"Dooo\" sound."),
    doo_low(),
    # OR
    doo_high(),
    psychTestR::elt_save_results_to_disk(complete = FALSE),

    musicassessr::sing_happy_birthday_page(feedback = FALSE, label = "sing_hbd2", text = "Please sing Happy Birthday."),

    psychTestR::elt_save_results_to_disk(complete = FALSE),

    psychTestR::one_button_page("In the following trials, you will sing back melodies. Please sing with a \"Daah\" sound."),

    daa_low(),
    # OR
    daa_high()

  ))
}


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
                                      they cover all the sounds of the English language.'),

          psychTestR::module('say_pd',

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
                                            auto_next_page = TRUE)
            ),

          psychTestR::elt_save_results_to_disk(complete = FALSE),


          musicassessr::long_tone_trials(num_items = 6)

          ), dict  = musicassessr::dict(NULL)), # end timeline (it's not needed from here onwards, and the SAA is embedded in UPEI_extra_questions, so to avoid nesting)

          psychTestR::module("MAST21",

            psychTestR::one_button_page("You will now have another test of short singing examples.
                    There are 2 sets of 21 questions. The first 20 are very short.
                    Like the previous test, you will hear a melody and be asked to imitate.
                    Unlike the previous test, in which you sang along with the example, now you will listen and then sing: you will hear the example and then sing the imitation after it.
                    You will be asked to sing each of the two sets of 21 examples on a different syllable:  one set on /da/ (“Daah”) and the other on /du/ (“Dooo”).
                    The instructions before each set of 21 examples will let you know which syllable to use.
                    You will also be asked to sing “Happy birthday” on four occasions."),

            musicassessr::get_voice_range_page(with_examples = FALSE),


            psychTestR::elt_save_results_to_disk(complete = FALSE),


            psychTestR::code_block(function(state, ...) {
              snap <- sample(1:2, 1)
              psychTestR::set_global("snap", snap, state)
            }),

            musicassessr::sing_happy_birthday_page(feedback = FALSE, label = "sing_hbd1", text = "Please sing Happy Birthday."),

            psychTestR::elt_save_results_to_disk(complete = FALSE),

            condition_one(),
            # OR
            condition_two(),

            musicassessr::sing_happy_birthday_page(feedback = FALSE, label = "sing_hbd3", text = "Please sing Happy Birthday."),

            psychTestR::elt_save_results_to_disk(complete = FALSE)),

          psyquest::GMS(),

          UPEI_extra_questions(),

          musicassessr::sing_happy_birthday_page(feedback = FALSE, label = "sing_hbd4", text = "Please sing Happy Birthday."),


          psychTestR::elt_save_results_to_disk(complete = TRUE),

          psychTestR::reactive_page(function(state, ... ) {
            p_id <- psychTestR::get_global('p_id', state)
            url <- paste0('https://upeiairs.qualtrics.com/jfe/form/SV_5vDAjJhxLqZw7Km?participant=', p_id)
            psychTestR::final_page(shiny::tags$div(shiny::tags$p("Please click on the following link to go to the final test of this session: ",
                                  shiny::tags$a(" click here", href = url, target = "_blank"), ".")))
          })

          ),
    opt = upei_test_options(musicassessr_state)
  )
}

# deploy_MAST21_2022('test')

# install_github('sebsilas/MAST21')
