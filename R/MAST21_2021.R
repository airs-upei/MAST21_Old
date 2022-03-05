MAST21 <- function(state = "production",
                   include_microphone_calibration_page = FALSE,
                   set_musicassessr_state = FALSE) {

  psychTestR::new_timeline(psychTestR::join(

    psychTestR::one_button_page(shiny::tags$div(
      shiny::tags$p("You will now have another test of short singing examples.
                      There are 2 sets of 21 questions.
                      The first 20 are very short. Like the previous test, you will hear a melody and be asked to imitate. Unlike the previous test, there is only one chance with each imitation.
                      You will be asked to sing the two sets of questions on two different syllables /da/ and /du/. ")
    )),

    if(include_microphone_calibration_page) microphone_calibration_page(),

    musicassessr::get_voice_range_page(with_examples = FALSE),


    psychTestR::code_block(function(state, ...) {
      snap <- sample(1:2, 1)
      psychTestR::set_global("snap", snap, state)
    }),

    musicassessr::sing_happy_birthday_page(feedback = FALSE, label = "sing_hbd1"),


    psychTestR::conditional(test = function(state, ...) {
      psychTestR::get_global("snap", state) == 1
    }, logic = psychTestR::join (
      psychTestR::one_button_page("In the following trials, you will sing back melodies. Please sing with a \"Daah\" sound."),

      MAST21_daah,

      musicassessr::sing_happy_birthday_page(feedback = FALSE, label = "sing_hbd2"),


      psychTestR::one_button_page(shiny::tags$div(
        shiny::tags$p("In the following trials, you will sing back melodies. Please sing with a \"Dooo\" sound."))),

      MAST21_dooo

    )),

    psychTestR::conditional(test = function(state, ...) {
      psychTestR::get_global("snap", state) == 2
    }, logic = psychTestR::join(
      psychTestR::one_button_page("In the following trials, you will sing back melodies. Please sing with a \"Dooo\" sound."),

      MAST21_dooo,

      psychTestR::elt_save_results_to_disk(complete = FALSE),

      musicassessr::sing_happy_birthday_page(feedback = FALSE, label = "sing_hbd3"),

      psychTestR::one_button_page("In the following trials, you will sing back melodies. Please sing with a \"Daah\" sound."),

      MAST21_daah

    )),

    psychTestR::elt_save_results_to_disk(complete = FALSE),

    musicassessr::sing_happy_birthday_page(feedback = FALSE, label = "sing_hbd4")
  ), dict = musicassessr::dict(NULL))
}



get_dob_page <- function(text = "When is your date of birth?") {
  psychTestR::page(
    label = "dob",
    ui = shiny::tags$div(
      shiny::tags$p(text),
      shiny::selectInput(inputId = "day", label = "Day", choices = as.character(1:31), width = "40%"),
      shiny::selectInput(inputId = "month", label = "Month", choices = month.name, width = "40%"),
      shiny::selectInput(inputId = "year", label = "Year", choices = as.character(1900:2021), width = "40%"),
      psychTestR::trigger_button("next", "Next")
    ),
    get_answer = function(input, ...) {
      list(day = input$day,
           month = input$month,
           year = input$year)
    })
}





return_questions <- function(append = NULL) {

  if(is.null(append)) {
    setup_questions()
  } else {
    psychTestR::code_block(function(state, ...) { })
  }
}



#' UPEI Battery
#'
#' @param state
#'
#' @return
#' @export
#'
#' @examples
UPEI_2021_battery <- function(state = "production") {

  psychTestR::make_test(
    psychTestR::join(

      upei_intro(state),

      SAA::SAA(num_items = list(
        long_tones = 6L, arrhythmic = 12L, rhythmic = 0L
      ),
      examples = 2L,
      final_results = FALSE,
      state = NULL,
      absolute_url = "https://musicog.ca",
      SNR_test = TRUE,
      get_range = TRUE,
      gold_msi = FALSE,
      demographics = FALSE,
      with_final_page = FALSE,
      melody_sound = "piano"),

      psychTestR::elt_save_results_to_disk(complete = FALSE),

      MAST21(),

      PDT::PDT(with_final_page = FALSE,
               headphones_page = FALSE,
               import_musicassessr_js_scripts = FALSE),

      psychTestR::elt_save_results_to_disk(complete = FALSE),

      mpt::mpt(num_items = 20L),

      psychTestR::elt_save_results_to_disk(complete = FALSE),

      mdt::mdt(num_items = 18L),

      psychTestR::elt_save_results_to_disk(complete = FALSE),

      psyquest::GMS(),

      psychTestR::one_button_page('test'),

      psychTestR::elt_save_results_to_disk(complete = FALSE),

      psychTestR::one_button_page(shiny::tags$div(
        shiny::tags$p("On the next page you will sing Happy Birthday again."))),

      musicassessr::sing_happy_birthday_page(feedback = FALSE),

      psychTestR::elt_save_results_to_disk(complete = FALSE),

      UPEI_extra_questions(),

      psychTestR::elt_save_results_to_disk(complete = TRUE),

      psychTestR::final_page(body = shiny::tags$div(style = "text-align: left;",
                                                    shiny::tags$p("You have now completed all the questions in this survey.  If you are interested in knowing more about the study, relevant information is provided in the following debriefing statement: "),
                                                    shiny::tags$h1("Learning and Memory for Popular Music and Imitation of Brief Melodies"),
                                                    shiny::tags$h2("Debriefing Statement: Session 2"),
                                                    shiny::tags$p("We would like to express our thanks and appreciation for your participation in Session 2 of this research project. Your contribution helps to advance our understanding of the knowledge acquired about popular music over the lifetime, and to specifically address the question of whether there is a time of life when such knowledge is easier to obtain than at other times. "),
                                                    shiny::tags$p("Session 2 focused on imitation of tones and brief melodies.  The study consisted of the presentation of long notes and short melodies that you were asked to imitate."),
                                                    shiny::tags$p("This research is part of a larger study looking into the relationship between adolescence and musical knowledge acquisition.  Part of musical knowledge is singing.  Singing is a musical behavior acquired naturally early in life, just like language is acquired.  In comparison to language acquisition, relatively little attention has been paid to singing development.  The study in which you participated aimed to obtain some basic data regarding singing accuracy (expecting performance to become less accurate with increasing numbers of notes to remember) and comparing males and females (where males have greater challenges to singing, due to issues of voice change in adolescence). "),
                                                    shiny::tags$p("This study is the first in which information on singing (from Session 2) will be related to information on knowledge and memory for popular music (from Session 1).  The overall interest of the study is whether there is a time during adolescence when it is easiest to acquire musical information.  The specific question that can be answered by the vocal imitation study is whether better vocal accuracy is associated with better memory for music. Also, for the first time, this experiment was conducted using the participant’s own computer in the participant’s own quiet environment.  In the past the study has been conducted in a laboratory environment.  We are interested in determining the extent to which the individual differences in equipment and setting will affect the variability of the data."),
                                                    shiny::tags$p("Your participation has helped students in the laboratory gain experience relevant to their honours degrees, and it has contributed to the exploration of important new research questions about how the developmental period of adolescence is related to musical knowledge. It also will add to our understanding of basic vocal abilities in older adolescents and early adulthood, for which very little information has been available."),
                                                    shiny::tags$p("If you have any further questions regarding this research study please feel free to contact  Kristen Gallant, at kbgallant5470@upei.ca (902-566-6023 – laboratory phone); Dr. Amy Simon, 902-566-6023; Dr. Annabel Cohen at acohen@upei.ca, 902-628-4325  (office phone)."),
                                                    shiny::tags$p("If you have indicated your interest in receiving a summary of the results of the study, you will be provided a link to this information by April 30, 2022."),
                                                    shiny::tags$p("Thank you for all your help!  Your contribution to this research is very much appreciated.")))

    ),
    opt = upei_test_options(state)
  )

}


upei_test_options <- function(state) {
  psychTestR::test_options(title = "UPEI",
                           admin_password = "@irs@irs2021#",
                           enable_admin_panel = FALSE,
                           display = psychTestR::display_options(
                             left_margin = 1L,
                             right_margin = 1L,
                             css = system.file('www/css/musicassessr.css', package = "musicassessr")
                           ),
                           additional_scripts = musicassessr::musicassessr_js(state),
                           languages = c("en"))
}


#' Title
#'
#' @param state
#'
#' @return
#' @export
#'
#' @examples
SAA_only <- function(state = "production") {
  upei_intro(state,
             SAA::SAA(num_items = list(
               long_tones = 6L, arrhythmic = 12L, rhythmic = 0L
             ),
             examples = 2L,
             final_results = FALSE,
             state = NULL,
             absolute_url = "https://musicog.ca",
             SNR_test = TRUE,
             get_range = TRUE,
             gold_msi = FALSE,
             demographics = FALSE,
             with_final_page = FALSE,
             melody_sound = "piano")
  )
}


#' Title
#'
#' @param state
#'
#' @return
#' @export
#'
#' @examples
MAST21_only <- function(state = "production") {
  upei_intro(state,
             MAST21(include_microphone_calibration_page = TRUE,
                    set_musicassessr_state = FALSE))
}


#' Title
#'
#' @param state
#'
#' @return
#' @export
#'
#' @examples
PDT_only <- function(state = "production") {
  upei_intro(state,
             PDT::PDT(with_final_page = FALSE,
                      headphones_page = FALSE,
                      import_musicassessr_js_scripts = FALSE))
}


#' Title
#'
#' @param state
#'
#' @return
#' @export
#'
#' @examples
MPT_only <- function(state = "production") {
  upei_intro(state,
             mpt::mpt(num_items = 20L))
}


#' Title
#'
#' @param state
#'
#' @return
#' @export
#'
#' @examples
MDT_only <- function(state = "production") {
  upei_intro(state, mdt::mdt(num_items = 18L))
}


#' Title
#'
#' @param state
#'
#' @return
#' @export
#'
#' @examples
end_only <- function(state = "production") {

  end <- psychTestR::join(
    psyquest::GMS(),

    microphone_calibration_page(),

    psychTestR::one_button_page(shiny::tags$div(
      shiny::tags$p("On the next page you will sing Happy Birthday again."))),

    musicassessr::sing_happy_birthday_page(feedback = FALSE),

    psychTestR::elt_save_results_to_disk(complete = FALSE),

    UPEI_extra_questions(),

    psychTestR::elt_save_results_to_disk(complete = TRUE),

    psychTestR::final_page(body = shiny::tags$div(style = "text-align: left;",
                                                  shiny::tags$p("You have now completed all the questions in this survey.  If you are interested in knowing more about the study, relevant information is provided in the following debriefing statement: "),
                                                  shiny::tags$h1("Learning and Memory for Popular Music and Imitation of Brief Melodies"),
                                                  shiny::tags$h2("Debriefing Statement: Session 2"),
                                                  shiny::tags$p("We would like to express our thanks and appreciation for your participation in Session 2 of this research project. Your contribution helps to advance our understanding of the knowledge acquired about popular music over the lifetime, and to specifically address the question of whether there is a time of life when such knowledge is easier to obtain than at other times. "),
                                                  shiny::tags$p("Session 2 focused on imitation of tones and brief melodies.  The study consisted of the presentation of long notes and short melodies that you were asked to imitate."),
                                                  shiny::tags$p("This research is part of a larger study looking into the relationship between adolescence and musical knowledge acquisition.  Part of musical knowledge is singing.  Singing is a musical behavior acquired naturally early in life, just like language is acquired.  In comparison to language acquisition, relatively little attention has been paid to singing development.  The study in which you participated aimed to obtain some basic data regarding singing accuracy (expecting performance to become less accurate with increasing numbers of notes to remember) and comparing males and females (where males have greater challenges to singing, due to issues of voice change in adolescence). "),
                                                  shiny::tags$p("This study is the first in which information on singing (from Session 2) will be related to information on knowledge and memory for popular music (from Session 1).  The overall interest of the study is whether there is a time during adolescence when it is easiest to acquire musical information.  The specific question that can be answered by the vocal imitation study is whether better vocal accuracy is associated with better memory for music. Also, for the first time, this experiment was conducted using the participant’s own computer in the participant’s own quiet environment.  In the past the study has been conducted in a laboratory environment.  We are interested in determining the extent to which the individual differences in equipment and setting will affect the variability of the data."),
                                                  shiny::tags$p("Your participation has helped students in the laboratory gain experience relevant to their honours degrees, and it has contributed to the exploration of important new research questions about how the developmental period of adolescence is related to musical knowledge. It also will add to our understanding of basic vocal abilities in older adolescents and early adulthood, for which very little information has been available."),
                                                  shiny::tags$p("If you have any further questions regarding this research study please feel free to contact  Kristen Gallant, at kbgallant5470@upei.ca (902-566-6023 – laboratory phone); Dr. Amy Simon, 902-566-6023; Dr. Annabel Cohen at acohen@upei.ca, 902-628-4325  (office phone)."),
                                                  shiny::tags$p("If you have indicated your interest in receiving a summary of the results of the study, you will be provided a link to this information by April 30, 2022."),
                                                  shiny::tags$p("Thank you for all your help!  Your contribution to this research is very much appreciated.")))
  )


  upei_intro(state, end)
}



# SAA_only('test')
# MAST21_only('test')
# PDT_only('test')
# MPT_only('test')
# MDT_only('test')
# end_only('test')


# SAA_only()
# MAST21_only()
# PDT_only()
# MPT_only()
# MDT_only()
# end_only()
# devtools::install_github('syntheso/musicassessr', ref = 'script-import')

# library(PDT)
# library(mpt)
# library(mdt)
# library(psyquest)
# library(SAA)
# library(musicassessr)
