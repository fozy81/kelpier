
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
(x <- Cushion$new(user = credentials[1], pwd = credentials[2], transport = "http"))

# x$ping()
# db_list(x)
i <- 0

since <- readLines("since.txt", n = 1)
if (length(since) == 0) {
  since <- NULL
}
# since <- NULL

body <- list(index = list(fields = I(c("title","task","form","question"))), name = "test-index", type = "json")
db_index_create(x, "kelpie", body = body)



while (TRUE) {
  res <- db_changes(x,
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
        response <- db_query(as = "json",
          cushion = x,
          dbname = "kelpie",
          selector = list(
            data.form = list(`$eq` = gsub("form_2_", "", id))
          )
        )
      })

     # flatten responses
     data <- map_df(responses, function(response) {
        fromJSON(response, flatten = T)$docs
      })
     # pivot/ tidy responses
     table <- as_tibble(data, .name_repair = "check_unique") %>%
       filter(data.response != "") %>%
       select(data.form, data.response, data.question)
     testdata <- pivot_wider(table, names_from = data.question, values_from = data.response)
     testdata <- type.convert(testdata)
     testdata <- select(testdata,  data.form, Species, Count)
     names(testdata) <- c("SAMPLE_ID","TAXON", "RESULT")

     # calc WHPT
     testdata$SAMPLE_ID <- 1

     metricResults <- calcWhpt(testdata[!is.na(testdata$RESULT),])

     # get task
     task <- db_query(
                          cushion = x,
                          dbname = "kelpie",
                          selector = list(
                            `_id` = list(`$eq` = paste0("task_2_", form$data$task))
                          )
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

     # get task physical attributes questions
     physical <- db_query(as = "json",
       cushion = x,
       dbname = "kelpie",
       selector = list(
           data.form = list(`$eq` = gsub("form_2_", "", physical$docs[[1]]$`_id`))

       )
     )

     # flatten responses
     data <- map_df(physical, function(response) {
       fromJSON(response, flatten = T)$docs
     })

     table <- as_tibble(data, .name_repair = "check_unique") %>%
       filter(data.response != "") %>%
       select(data.form, data.response, data.question)
     testdata <- pivot_wider(table, names_from = data.question, values_from = data.response)
     testdata <- type.convert(testdata)
     physical_characteristics <- testdata

     # get substrate etc
     all_forms_json <- db_query(
       cushion = x,
       dbname = "kelpie",
       selector = list(data.task = list(`$eq` = form$data$task))
     )

     passing <- map(all_forms_json$docs, function(form) {
       if (
           form$data$archive != TRUE) {
         return(form$`_id`)
       } else {
         return()
       }
     })

     responses <- map(unlist(passing), function(id) {
       response <- db_query(as = "json",
                            cushion = x,
                            dbname = "kelpie",
                            selector = list(
                              data.form = list(`$eq` = gsub("form_2_", "", id))
                            )
       )
     })


     data <- map_df(responses, function(response) {
       response <- fromJSON(response, flatten = T)$docs
       response$data.response <- as.character(response$data.response)
       return(response)
     })

     # pivot/ tidy responses
     table <- as_tibble(data, .name_repair = "check_unique") %>%
       filter(data.response != "") %>%
       select( `_id`, data.response, data.question)

     depth <- mean(as.numeric(table$data.response[
       table$data.question %in% c("Left channel depth", "Right channel depth","Mid-channel depth")]), na.rm = TRUE)

     testdata <-  table %>%
       pivot_wider(names_from = data.question, values_from = data.response)

     testdata <- type.convert(testdata)
     substrate <- testdata %>%  select(-c(`_id`,Species,Count)) %>%  summarise_all(mean, na.rm = TRUE)
     substrate$Mean_Depth <- depth
     substrate$Mean_Width <- substrate$Width
     # Join WHPT, substrate, location characteristics into rict input
     metrics <- metricResults %>%  pivot_wider(names_from = DETERMINAND, values_from = RESULT)

     rict_input <- cbind(substrate, physical_characteristics, metrics)

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
        ntaxa_question <- db_query(
          cushion = x,
          dbname = "kelpie",
          selector = list(
            data.form = list(`$eq` = gsub("form_2_", "", rict_form$docs[[1]]$`_id`)),
            data.question = list(`$eq` = "ntaxa")
          )
        )

        test_rict <- rict(demo_observed_values[1,], year_type = "single")

        docs1 <- paste0('{"data": {
        "question": "Overall Class",
        "response": "', test_rict$mostProb_MINTA, '",
        "multiEntry": false,
        "edit": false,
        "type": "text",
        "archive": false,
        "pos": 1,
        "form": "', gsub("form_2_", "", rict_form$docs[[1]]$`_id`), '"
     }
      }')


        doc_update(
          cushion = x, dbname = "kelpie", docs1,
          docid = ntaxa_question$docs[[1]]$`_id`,
          rev = ntaxa_question$docs[[1]]$`_rev`
        )
      }
    }
    Sys.sleep(0.1)
  } else {
    Sys.sleep(5)
  }
}
