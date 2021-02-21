
# remotes::install_github("ropensci/sofa")
# install.packages(c("sofa", "jsonlite", "tidyverse"))
library(sofa)
library("jsonlite")
library(purrr)


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
      res$results[[1]]$doc$data$question == "Species" &&
      res$results[[1]]$doc$data$response != "") {
      # Do work - check if has doc$data (question/response?)
      data <- res$results[[1]]$doc$data

      # Conditional / Test - testing checked
      form <- doc_get(docid = paste0("form_2_",  data$form),
                      cushion = x,
                      dbname = "kelpie")

      formTemplateId <- form$data$formTemplateId

      all_forms <- db_query( cushion = x,
                dbname = "kelpie",
                selector = list(data.task = list(`$eq` = form$data$task)))

      passing <- map(all_forms$docs, function(form) {
        if(form$data$formTemplateId == formTemplateId &&
           form$data$archive != TRUE) {
          return(form$data$title)
        } else {
          return()
        }
      })

      rict_form <- db_query( cushion = x,
                             dbname = "kelpie",
                             selector = list(data.task = list(`$eq` = form$data$task),
                             data.title = list(`$eq` = "rict")))
      if(length(rict_form$docs) != 0) {

      ntaxa_question <- db_query( cushion = x,
                              dbname = "kelpie",
                              selector = list(data.form = list(`$eq` = gsub("form_2_", "", rict_form$docs[[1]]$`_id`)),
                                              data.question = list(`$eq` = "ntaxa")))

     docs1 <- paste0('{"data": {
        "question": "ntaxa",
        "response": ',length(unlist(passing)) ,',
        "multiEntry": false,
        "edit": false,
        "type": "text",
        "archive": false,
        "pos": 1,
        "form": "',gsub("form_2_", "", rict_form$docs[[1]]$`_id`),'"
     }
      }')


      doc_update(cushion = x, dbname = "kelpie", docs1,
                 docid = ntaxa_question$docs[[1]]$`_id`,
                 rev = ntaxa_question$docs[[1]]$`_rev`)
      }

    }
    Sys.sleep(0.1)
  } else {
    Sys.sleep(2)
  }
}
