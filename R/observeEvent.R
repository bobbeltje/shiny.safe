
#' Safe version of observeEvent
#'
#' @param ...
#'
#' @export
observeEvent <- function(...){

  fc <- match.call(expand.dots = TRUE)
  expr <- fc[[3]]

  fc[[3]] <- quote({

    r <- try(expr=eval(expr), silent=TRUE)

    if (inherits(r, 'try-error')){

      showNotification(r[[1]], type='error')
    }
  })

  fc[[1]] <- shiny::observeEvent
  eval(fc, envir=list(expr=expr), enclos=parent.frame())
}
