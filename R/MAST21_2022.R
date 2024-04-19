
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

after_setup <- function(page_type = "record_midi_page",
                        setup_pages = TRUE,
                        data_collection_method = c("midi", "audio", "key_presses"),
                        get_p_id = TRUE,
                        language,
                        musicassessr_state,
                        absolute_url = "https://musicog.ca/",
                        final_qualtrics_url = 'https://upeiairs.qualtrics.com/jfe/form/SV_5vDAjJhxLqZw7Km?participant=') {

  data_collection_method <- match.arg(data_collection_method)


  stopifnot(
    page_type %in% c("record_midi_page", "record_audio_page", "record_key_presses_page"),
    is.scalar.logical(setup_pages),
    is.scalar.character(data_collection_method),
    is.scalar.logical(get_p_id)
  )

  function() {
  psychTestR::make_test(
    psychTestR::join(
      psychTestR::new_timeline(
        psychTestR::join(



          musicassessr::musicassessr_init(),

          upei_intro(musicassessr_state),

          musicassessr::setup_pages(input = "microphone", absolute_url = absolute_url),


          say_pd(dinosaur_instructions = "Please press the “record” button and read the sentence below out loud: ",
                 body_instructions = 'Although the next short test involves singing,
                                      we would like to start off by asking you to read out loud four short sentences all beginning with the phrase
                                      "The hungry purple dinosaur".  The sentences may sound silly, but together,
                                      they cover all the sounds of the English language.'),

          psychTestR::elt_save_results_to_disk(complete = FALSE),


          musicassessr::long_tone_trials(num_items = 6)

        ),

        dict  = musicassessr::dict(NULL),
        default_lang = language

      ), # end timeline (it's not needed from here onwards, and the SAA is embedded in UPEI_extra_questions, so to avoid nesting)



      mast_21(mast_inst = "You will now have another test of short singing examples.
                    There are 2 sets of 21 questions. The first 20 are very short.
                    Like the previous test, you will hear a melody and be asked to imitate.
                    Unlike the previous test, in which you sang along with the example, now you will listen and then sing: you will hear the example and then sing the imitation after it.
                    You will be asked to sing each of the two sets of 21 examples on a different syllable:  one set on /da/ (“Daah”) and the other on /du/ (“Dooo”).
                    The instructions before each set of 21 examples will let you know which syllable to use.
                    You will also be asked to sing “Happy birthday” on four occasions."),

      psyquest::GMS(),

      UPEI_extra_questions(with_compensation_question = FALSE),

      musicassessr::sing_happy_birthday_page(feedback = FALSE, label = "sing_hbd4", text = "Please sing Happy Birthday."),


      psychTestR::elt_save_results_to_disk(complete = TRUE),

      psychTestR::reactive_page(function(state, ... ) {
        p_id <- psychTestR::get_global('p_id', state)
        url <- paste0(final_qualtrics_url, p_id)
        psychTestR::final_page(shiny::tags$div(shiny::tags$p("Please click on the following link to go to the final test of this session: ",
                                                             shiny::tags$a(" click here", href = url, target = "_blank"), ".")))
      })

    )
    ,
    opt = upei_test_options(musicassessr_state)
    )
  }
}



#' Run the MAST21 2024 protocol
#'
#' @param musicassessr_state
#' @param language
#' @param final_qualtrics_url
#' @param data_collection_method

#'
#' @return
#' @export
#'
#' @examples

deploy_MAST21_Old_2024 <- function(
    app_name = paste("UPEI ", format(Sys.Date(), "%Y"), " Study"),
    language,
    musicassessr_state = "test",
    absolute_url = "https://musicog.ca/",
    final_qualtrics_url = 'https://upeiairs.qualtrics.com/jfe/form/SV_5vDAjJhxLqZw7Km?participant=',
    opening_and_final_image = "https://img.freepik.com/free-vector/headphone-concept-illustration_114360-2132.jpg?w=826&t=st=1708543460~exp=1708544060~hmac=7a9cba8f7104f82422a55f30f5120598e4ce64dd222a1247c8da79825dddacb6"
) {


    psychTestR::make_test(
      psychTestR::join(
        psychTestR::new_timeline(
          psychTestR::join(



            musicassessr::musicassessr_init(),

            upei_intro(musicassessr_state),

            musicassessr::setup_pages(input = "microphone", absolute_url = absolute_url),


            say_pd(dinosaur_instructions = "Please press the “record” button and read the sentence below out loud: ",
                   body_instructions = 'Although the next short test involves singing,
                                      we would like to start off by asking you to read out loud four short sentences all beginning with the phrase
                                      "The hungry purple dinosaur".  The sentences may sound silly, but together,
                                      they cover all the sounds of the English language.'),

            psychTestR::elt_save_results_to_disk(complete = FALSE),


            musicassessr::long_tone_trials(num_items = 6)

          ),

          dict  = musicassessr::dict(NULL),
          default_lang = language

        ), # end timeline (it's not needed from here onwards, and the SAA is embedded in UPEI_extra_questions, so to avoid nesting)



        mast_21(mast_inst = "You will now have another test of short singing examples.
                    There are 2 sets of 21 questions. The first 20 are very short.
                    Like the previous test, you will hear a melody and be asked to imitate.
                    Unlike the previous test, in which you sang along with the example, now you will listen and then sing: you will hear the example and then sing the imitation after it.
                    You will be asked to sing each of the two sets of 21 examples on a different syllable:  one set on /da/ (“Daah”) and the other on /du/ (“Dooo”).
                    The instructions before each set of 21 examples will let you know which syllable to use.
                    You will also be asked to sing “Happy birthday” on four occasions."),

        psyquest::GMS(),

        UPEI_extra_questions(with_compensation_question = FALSE),

        musicassessr::sing_happy_birthday_page(feedback = FALSE, label = "sing_hbd4", text = "Please sing Happy Birthday."),


        psychTestR::elt_save_results_to_disk(complete = TRUE),

        psychTestR::reactive_page(function(state, ... ) {
          p_id <- psychTestR::get_global('p_id', state)
          url <- paste0(final_qualtrics_url, p_id)
          psychTestR::final_page(shiny::tags$div(shiny::tags$p("Please click on the following link to go to the final test of this session: ",
                                                               shiny::tags$a(" click here", href = url, target = "_blank"), ".")))
        })

      )
      ,
      opt = upei_test_options(musicassessr_state)
    )


}





# deploy_MAST21_2022('test')

# install_github('sebsilas/MAST21')
