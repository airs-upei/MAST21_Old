
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

          # musicassessr::setup_pages(input = "microphone", absolute_url = absolute_url),


          musicassessr::musicassessr_init(),

          upei_intro(musicassessr_state),

          musicassessr::setup_pages(input = "microphone", absolute_url = "https://musicog.ca/"),

          #
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



deploy_MAST21_2024 <- function(
    app_name = paste("UPEI ", format(Sys.Date(), "%Y"), " Study"),
    language,
    musicassessr_aws = FALSE,
    # absolute_url = "https://musicog.ca/",
    data_collection_method = "audio",
    get_p_id = FALSE,
    final_qualtrics_url = 'https://upeiairs.qualtrics.com/jfe/form/SV_5vDAjJhxLqZw7Km?participant=',
    opening_and_final_image = "https://img.freepik.com/free-vector/headphone-concept-illustration_114360-2132.jpg?w=826&t=st=1708543460~exp=1708544060~hmac=7a9cba8f7104f82422a55f30f5120598e4ce64dd222a1247c8da79825dddacb6"
) {



  after_tl <- after_setup(
    get_p_id = get_p_id,
    # absolute_url = absolute_url,
    data_collection_method = data_collection_method,
    final_qualtrics_url = final_qualtrics_url,
    language = language
  )


  welcome_pg <- psychTestR::one_button_page(shiny::tags$div(shiny::tags$h2(paste("Welcome to the UPEI ", 	format(Sys.Date(), "%Y"), " Singing Test")),
                                                            shiny::tags$img(src = opening_and_final_image, height = 200, width = 200)))


  before_tl <- upei_intro(musicassessr_state)


  musicassessr::make_musicassessr_test(
    welcome_page = welcome_pg,
    elts = after_tl,
    # elts = aft,
    elts_before_setup_pages = before_tl,
    title = paste("UPEI ", format(Sys.Date(), "%Y"), " Singing Test"),
    admin_password = "demo",
    languages = language,
    opt = musicassessr::musicassessr_opt(app_name = app_name,
                                         get_p_id = get_p_id,
                                         midi_input = data_collection_method == "midi",
                                         record_audio = data_collection_method == "audio",
                                         musicassessr_aws = musicassessr_aws,
                                         setup_options = musicassessr::setup_pages_options(input_type = if(data_collection_method == "midi") "midi_keyboard" else if(data_collection_method == "audio") "microphone" else "key_presses",
                                                                                           headphones = TRUE,
                                                                                           get_instrument_range = FALSE,
                                                                                           SNR_test = TRUE,
                                                                                           absolute_url = character(),
                                                                                           concise_wording = TRUE)
    )
  )


}


#' Run the MAST21 2022 protocol
#'
#' @param musicassessr_state
#' @param dinosaur_instructions
#' @param final_qualtrics_url
#'
#' @return
#' @export
#'
#' @examples
deploy_MAST21_2022 <- function(musicassessr_state = "test",
                               dinosaur_instructions = "Please press the “record” button and read the sentence below out loud: ",
                               final_qualtrics_url = 'https://upeiairs.qualtrics.com/jfe/form/SV_5vDAjJhxLqZw7Km?participant=') {

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
            url <- paste0(final_qualtrics_url, p_id)
            psychTestR::final_page(shiny::tags$div(shiny::tags$p("Please click on the following link to go to the final test of this session: ",
                                  shiny::tags$a(" click here", href = url, target = "_blank"), ".")))
          })

          ),
    opt = upei_test_options(musicassessr_state)
  )
}

deploy_MAST21_2024 <- function(
    app_name = paste("UPEI ", format(Sys.Date(), "%Y"), " Study"),
    language,
    musicassessr_aws = FALSE,
    # absolute_url = "https://musicog.ca/",
    data_collection_method = "audio",
    get_p_id = FALSE,
    final_qualtrics_url = 'https://upeiairs.qualtrics.com/jfe/form/SV_5vDAjJhxLqZw7Km?participant=',
    opening_and_final_image = "https://img.freepik.com/free-vector/headphone-concept-illustration_114360-2132.jpg?w=826&t=st=1708543460~exp=1708544060~hmac=7a9cba8f7104f82422a55f30f5120598e4ce64dd222a1247c8da79825dddacb6"
) {



  after_tl <- after_setup(
    get_p_id = get_p_id,
    # absolute_url = absolute_url,
    data_collection_method = data_collection_method,
    final_qualtrics_url = final_qualtrics_url,
    language = language
  )


  welcome_pg <- psychTestR::one_button_page(shiny::tags$div(shiny::tags$h2(paste("Welcome to the UPEI ", 	format(Sys.Date(), "%Y"), " Singing Test")),
                                                            shiny::tags$img(src = opening_and_final_image, height = 200, width = 200)))


  before_tl <- upei_intro(musicassessr_state)


  musicassessr::make_musicassessr_test(
    welcome_page = welcome_pg,
    elts = after_tl,
    # elts = aft,
    elts_before_setup_pages = before_tl,
    title = paste("UPEI ", format(Sys.Date(), "%Y"), " Singing Test"),
    admin_password = "demo",
    languages = language,
    opt = musicassessr::musicassessr_opt(app_name = app_name,
                                         get_p_id = get_p_id,
                                         midi_input = data_collection_method == "midi",
                                         record_audio = data_collection_method == "audio",
                                         musicassessr_aws = musicassessr_aws,
                                         setup_options = musicassessr::setup_pages_options(input_type = if(data_collection_method == "midi") "midi_keyboard" else if(data_collection_method == "audio") "microphone" else "key_presses",
                                                                                           headphones = TRUE,
                                                                                           get_instrument_range = FALSE,
                                                                                           SNR_test = TRUE,
                                                                                           absolute_url = character(),
                                                                                           concise_wording = TRUE)
    )
  )


}

# deploy_MAST21_2022('test')

# install_github('sebsilas/MAST21')
