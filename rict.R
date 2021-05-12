

# remotes::install_github("ropensci/sofa")
# install.packages(c("sofa", "jsonlite", "tidyverse"))
library(sofa)
library("jsonlite")
library(purrr)
library(dplyr)
library(tidyr)
library(readr)
library(devtools)
devtools::install_github("aquaMetrics/rict")
devtools::install_github("aquaMetrics/macroinvertebratesMetrics")
library(rict)
library(macroinvertebrateMetrics)


credentials <- readLines("credentials.txt", n = 2)
(x <-
    Cushion$new(
      user = credentials[1],
      pwd = credentials[2],
      transport = "http"
    ))


# x$ping()
# db_list(x)
i <- 0

since <- readLines("since.txt", n = 1)
if (length(since) == 0) {
  since <- NULL
}
# since <- NULL

body <-
  list(index = list(fields = I(c(
    "title", "task", "form", "question"
  ))),
  name = "test-index",
  type = "json")
db_index_create(x, "kelpie", body = body)



while (TRUE) {
  res <- db_changes(
    x,
    dbname = "kelpie",
    since = since,
    limit = 1,
    include_docs = "true"
  )
  i <- i + 1
  print(i)

  if (length(res$results) != 0) {
    since <- res$results[[1]]$seq
    write_lines(since, file = "since.txt")
    if (!is.null(res$results[[1]]$doc$data$question) &&
        res$results[[1]]$doc$data$question == "Taxon name" &&
        res$results[[1]]$doc$data$response != "") {
      # Do work - check if has doc$data (question/response?)
      data <- res$results[[1]]$doc$data

      # Conditional / Test - testing checked
      form <- doc_get(
        docid = paste0("form_2_", data$form),
        cushion = x,
        deleted = TRUE,
        dbname = "kelpie"
      )

      formTemplateId <- form$data$formTemplateId

      all_forms <- db_query(
        cushion = x,
        dbname = "kelpie",
        selector = list(data.task = list(`$eq` = form$data$task))
      )


      # find only required forms (not archived etc)
      passing <- map(all_forms$docs, function(form) {
        if (form$data$formTemplateId == formTemplateId &&
            form$data$archive != TRUE) {
          return(form$`_id`)
        } else {
          return()
        }
      })

      # Query required responses from forms
      responses <- map(unlist(passing), function(id) {
        response <- db_query(
          as = "json",
          cushion = x,
          dbname = "kelpie",
          selector = list(data.form = list(`$eq` = gsub(
            "form_2_", "", id
          )))
        )
      })

      # flatten responses

      data <- map_df(responses, function(response) {
        fromJSON(response, flatten = T)$docs
      })
      # pivot/ tidy responses
      if (nrow(data) > 0) {
        table <- as_tibble(data, .name_repair = "check_unique") %>%
          filter(data.response != "") %>%
          select(data.form, data.response, data.question)
        testdata <-
          pivot_wider(table, names_from = data.question, values_from = data.response)
        testdata <- type.convert(testdata)

        if ("Count" %in% colnames(testdata)) {
          testdata <- select(testdata,  data.form, `Taxon name`, Count)
          names(testdata) <- c("SAMPLE_ID", "TAXON", "RESULT")

          # calc WHPT
          testdata$SAMPLE_ID <- 1
          # Kelpie doesn't have commas in the Taxon names so adding them back
          # in for metric/invert table lookup
          testdata$TAXON <- gsub(pattern = " \\+ ",
                                 replacement = ", ",
                                 x = testdata$TAXON)

          metricResults <- calcWhpt(testdata[!is.na(testdata$RESULT), ])

          # get task
          task <- db_query(
            cushion = x,
            dbname = "kelpie",
            selector = list(`_id` = list(
              `$eq` = paste0("task_2_", form$data$task)
            ))
          )

          # get task physical attributes form
          physical <- db_query(
            cushion = x,
            dbname = "kelpie",
            selector = list(
              data.taskTemplate = list(`$eq` = task$docs[[1]]$data$taskTemplateId),
              data.title = list(`$eq` = "Location Characteristics"),
              data.archive = list(`$eq` = FALSE)
            )
          )
          if (length(physical$docs) != 0) {
            # get task physical attributes questions
            physical <- db_query(
              as = "json",
              cushion = x,
              dbname = "kelpie",
              selector = list(data.form = list(
                `$eq` = gsub("form_2_", "", physical$docs[[1]]$`_id`)
              ))
            )

            # flatten responses
            data <- map_df(physical, function(response) {
              fromJSON(response, flatten = T)$docs
            })

            table <- as_tibble(data, .name_repair = "check_unique") %>%
              filter(data.response != "") %>%
              select(data.form, data.response, data.question)
            testdata <-
              pivot_wider(table,
                          names_from = data.question,
                          values_from = data.response)
            testdata <- type.convert(testdata)
            physical_characteristics <- testdata

            # get substrate etc
            all_forms_json <- db_query(
              cushion = x,
              dbname = "kelpie",
              selector = list(data.task = list(`$eq` = form$data$task))
            )

            passing <- map(all_forms_json$docs, function(form) {
              if (form$data$archive != TRUE) {
                return(form$`_id`)
              } else {
                return()
              }
            })

            responses <- map(unlist(passing), function(id) {
              response <- db_query(
                as = "json",
                cushion = x,
                dbname = "kelpie",
                selector = list(data.form = list(`$eq` = gsub(
                  "form_2_", "", id
                )))
              )
            })


            data <- map_df(responses, function(response) {
              response <- fromJSON(response, flatten = T)$docs
              response$data.response <-
                as.character(response$data.response)
              return(response)
            })

            # pivot/ tidy responses
            table <- as_tibble(data, .name_repair = "check_unique") %>%
              filter(data.response != "") %>%
              select(`_id`, data.response, data.question)

            depth <- suppressWarnings(mean(as.numeric(table$data.response[table$data.question %in% c("Left Channel Depth",
                                                                                    "Right Channel Depth",
                                                                                    "Mid-channel Depth")]), na.rm = TRUE))

            testdata <-  table %>%
              pivot_wider(names_from = data.question, values_from = data.response)

            testdata <- type.convert(testdata)
            substrate <-
              testdata %>%  select(-c(`_id`, `Taxon name`, Count)) %>%  summarise_all(mean, na.rm = TRUE)
            substrate$Mean_Depth <- depth
            substrate$Mean_Width <- substrate$Width
            substrate$Date <-  table$data.response[table$data.question == "Date"]
            substrate$Season <-  as.Date(substrate$Date, "%Y-%m-%d")

            calcSeason <- function(dates,
                                   winter = "2012-12-1",
                                   spring = "2012-3-1",
                                   summer = "2012-6-1",
                                   autumn = "2012-9-1", output = "numeric") {
              WS <- as.Date(winter, format = "%Y-%m-%d") # Winter Solstice
              SE <- as.Date(spring, format = "%Y-%m-%d") # Spring Equinox
              SS <- as.Date(summer, format = "%Y-%m-%d") # Summer Solstice
              FE <- as.Date(autumn, format = "%Y-%m-%d") # Fall Equinox


              d <- as.Date(strftime(dates, format = "2012-%m-%d"))
              # Convert dates from any year to 2012 dates
              if (output == "numeric") {
                return(ifelse(d >= WS | d < SE, "4",
                              ifelse(d >= SE & d < SS, "1",
                                     ifelse(d >= SS & d < FE, "2", "3")
                              )
                ))
              }

              if (output == "shortname") {
                return(ifelse(d >= WS | d < SE, "WIN",
                              ifelse(d >= SE & d < SS, "SPR",
                                     ifelse(d >= SS & d < FE, "SUM", "AUT")
                              )
                ))
              }

              if (output == "fullname") {
                return(ifelse(d >= WS | d < SE, "Winter",
                              ifelse(d >= SE & d < SS, "Spring",
                                     ifelse(d >= SS & d < FE, "Summer", "Autumn")
                              )
                ))
              }
            }


            substrate$Season <-  calcSeason(substrate$Date)
            # Join WHPT, substrate, location characteristics into rict input
            metrics <-
              metricResults %>%  pivot_wider(names_from = DETERMINAND, values_from = RESULT)

            rict_input <-
              cbind(substrate, physical_characteristics, metrics)
            rict_input$dist_from_source <- rict_input$`Distance from source`
            rict_input$location <- 1
            # Sort out season columns...

            columns_required <- c("location",
              "Season",
              "WHPT NTAXA", "WHPT ASPT"
            )
            spring <- as_tibble(rict_input[
              rict_input$Season == 1,
              c(columns_required)
            ])

            names(spring) <- c(
              "location",
              "Spr_Season_ID",
              "Spr_TL2_WHPT_NTaxa (AbW,DistFam)",
              "Spr_TL2_WHPT_ASPT (AbW,DistFam)"
            )

            summer <- as_tibble(rict_input[
              rict_input$Season == 2,
              c(columns_required)
            ])

            names(summer) <- c(
              "location",
              "Sum_Season_ID",
              "Sum_TL2_WHPT_NTaxa (AbW,DistFam)",
              "Sum_TL2_WHPT_ASPT (AbW,DistFam)"
            )

            autumn <- as_tibble(rict_input[
              rict_input$Season == 3,
              c(columns_required)
            ])

            names(autumn) <- c(
              "location",
              "Aut_Season_ID",
              "Aut_TL2_WHPT_NTaxa (AbW,DistFam)",
              "Aut_TL2_WHPT_ASPT (AbW,DistFam)"
            )


            rict_input <- dplyr::full_join(rict_input, spring, by = c(
              "location" = "location"
            ))

            rict_input <- dplyr::full_join(rict_input, summer, by = c(
              "location" = "location"
            ))
            rict_input <- dplyr::full_join(rict_input, autumn, by = c(
              "location" = "location"
            ))


          # Get required form to populate with calculated responses
            rict_form <- db_query(
              cushion = x,
              dbname = "kelpie",
              selector = list(
                data.task = list(`$eq` = form$data$task),
                data.title = list(`$eq` = "RICT")
              )
            )

            if (length(rict_form$docs) != 0) {
             rict_questions <- db_query(
                cushion = x,
                dbname = "kelpie",
                selector = list(
                  data.form = list(`$eq` = gsub(
                    "form_2_", "", rict_form$docs[[1]]$`_id`
                  ))
                )
              )

              # test_rict <-
              #   rict(demo_observed_values[1, ], year_type = "single")
              rict_input$waterbody <- 1
              rict_input$SITE <- "Test"
              rict_input$Year <- as.integer(format.Date(rict_input$Date, "%Y"))
              rict_input$velocity <- NA
              rict_input$hardness <- NA
              rict_input$calcium <- NA
              rict_input$conductivity <- NA
              rict_input$AUT_NTAXA_BIAS <- NA
              rict_input$SUM_NTAXA_BIAS <- NA
              rict_input$SPR_NTAXA_BIAS <- NA

             rict_class <- rict(rict_input, year_type = "single")
             rict_pred <- rict_predict(rict_input)
             overall <- rict_class[, paste0("mintawhpt_",
                   tolower(calcSeason(rict_input$Date, output = "shortname")),
                   "_mostProb")]

             aspt <- rict_class[, paste0("mostProb_s_ASPT_",
                                            tolower(
                                              calcSeason(rict_input$Date,
                                                         output = "shortname")))]

             ntaxa <- rict_class[,
                                 paste0("mostProb_NTAXA_",
                                            tolower(
                                              calcSeason(rict_input$Date,
                                                         output = "shortname")))]

             pred_ntaxa <- rict_pred[, paste0("TL2_WHPT_NTAXA_AbW_DistFam_",  tolower(
               calcSeason(rict_input$Date,
                          output = "shortname")))]
             pred_aspt <- rict_pred[, paste0("TL2_WHPT_ASPT_AbW_DistFam_",  tolower(
               calcSeason(rict_input$Date,
                          output = "shortname")))]

            answers <- list(overall, aspt, ntaxa, pred_aspt, pred_ntaxa)
            names(answers) <- c("Overall Class", "ASPT Class","NTAXA Class",
                                "Predicted ASPT","Predicted NTAXA")
            n <- 0
            rict_docs <- lapply(rict_questions$docs, function(question) {
              n <<- n + 1
              rict_response <- paste0(
                '{"data": {
                  "question": "', rict_questions$docs[[n]]$data$question,'",
                  "response": "',
                   answers[rict_questions$docs[[n]]$data$question],
                  '",
                  "multiEntry": false,
                  "edit": false,
                  "type": "text",
                  "archive": false,
                  "pos": 1,
                  "form": "',
                  gsub("form_2_", "", rict_form$docs[[1]]$`_id`),
                  '"
                  }
               }'
              )
              return(rict_response)
            })

            n <- 0
            lapply(rict_docs, function(doc){
              n <<- n + 1
              doc_update(
                cushion = x,
                dbname = "kelpie",
                doc,
                docid = rict_questions$docs[[n]]$`_id`,
                rev = rict_questions$docs[[n]]$`_rev`
              )
            })

              whpt_form <- db_query(
                cushion = x,
                dbname = "kelpie",
                selector = list(
                  data.task = list(`$eq` = form$data$task),
                  data.title = list(`$eq` = "WHPT Scores"),
                  data.archive = list(`$eq` = FALSE)
                )
              )

              questions <- db_query(
                cushion = x,
                dbname = "kelpie",
                selector = list(
                  data.form = list(`$eq` = gsub(
                    "form_2_", "", whpt_form$docs[[1]]$`_id`
                  ))))
              # ,
              #     data.question = list(`$eq` = "WHPT ASPT")
              #   )
              # )
              # questions <- map_df(aspt_question, function(response) {
              #   fromJSON(response, flatten = T)$docs
              # })
              # metricResults$RESULT
              n <- 0
              docs1 <- lapply(questions$docs, function(question) {

                n <<- n + 1
                # question$data$response <- metricResults$RESULT[n]

                question <- paste0(
                    '{"data": {
                    "question": "',question$data$question,'",
                    "response": "',
                    metricResults$RESULT[metricResults$DETERMINAND == toupper(gsub(" ", "_", question$data$question))],
                    '",
                    "multiEntry": false,
                    "edit": false,
                    "type": "number",
                    "archive": false,
                    "pos": 1,
                    "form": "',
                    #gsub("form_2_", "", whpt_form$`_id`),
                    gsub("form_2_", "",   whpt_form$docs[[1]]$`_id`),
                    '"
                    }
                 }'
                  )

                return(question)
              })

                 n <- 0
              lapply(docs1, function(doc){
                n <<- n + 1

              doc_update(
                cushion = x,
                dbname = "kelpie",
                doc = doc,
                as = "json",
                docid = questions$docs[[n]]$`_id`,
                rev = questions$docs[[n]]$`_rev`
              )
              })

            }
          }
        }
      }
    }
    Sys.sleep(0.1)
  } else {
    Sys.sleep(3)
  }
}
