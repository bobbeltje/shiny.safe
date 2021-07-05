
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

      fn <- function(){}
      body(fn) <- expr

      l <- list()
      for (i in rutils::findGlobals(fn)){
        if (!exists(i)) next
        x <- get(i)
        l[[i]] <- if (is.reactivevalues(x)) reactiveValuesToList(x) else x
      }

      message('saving state')
      saveRDS(l, 'tmp.rds')

      showNotification(r[[1]], type='error')
    }
  })

  fc[[1]] <- shiny::observeEvent
  eval(fc, envir=list(expr=expr), enclos=parent.frame())
}
