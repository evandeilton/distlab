# Mock session that records input and custom messages sent to the client,
# so tests can check update*Input() and sendCustomMessage() calls.
recording_session <- function() {
  s <- shiny::MockShinySession$new()
  log <- new.env(parent = emptyenv())
  log$input <- list()
  log$custom <- list()
  s$sendInputMessage <- function(inputId, message) {
    log$input[[length(log$input) + 1L]] <- list(id = inputId, message = message)
  }
  s$sendCustomMessage <- function(type, message) {
    log$custom[[length(log$custom) + 1L]] <- list(type = type, message = message)
  }
  attr(s, "log") <- log
  s
}

# Messages recorded for one input id (the mock records ids without the
# module namespace).
input_messages <- function(session, id) {
  log <- attr(session, "log")
  Filter(function(m) identical(m$id, id), log$input)
}
