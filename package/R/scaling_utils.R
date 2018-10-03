#' Strip a vglm or gam model, only retaining those objects necessary for predicting
#'
#' @param model The vglm model
#'
#' @examples
#' \dontrun{
#' d.AD <- data.frame(treatment = gl(3, 3),outcome = gl(3, 1, 9),counts = c(18,17,15,20,10,20,25,13,12))
#' model <- VGAM::vglm(counts ~ outcome + treatment, family = VGAM::poissonff, data = d.AD, trace = TRUE)
#'
#' object.size(model)
#' object.size(strip_vglm(model))
#' 1/as.numeric(object.size(strip_vglm(model))/object.size(model))
#' predict(model, d.AD)
#' predict(strip_vglm(model), d.AD)
#' }
#' @rdname strip
#'
#' @export
strip_vglm = function(model) {
  # f <- list()
  # class(f)<- "vglmff"
  # model@family <- f
  model@qr <- list()
  model@x <- matrix()
  model@extra <- list()
  model@residuals <- matrix()
  model@predictors <- matrix()
  model@y <- matrix()
  model@fitted.values <- matrix()
  model@effects <- numeric()
  model@control <- list()
  model@call <- new("call")
  model@misc$Lower <- NULL
  model@misc$Upper <- NULL
  model@misc$earg <- NULL
  model@misc$formula <- NULL
  model@misc$orig.assign <- NULL

  model@family@weight  <- expression()
  model@family@initialize <- expression()
  model@family@deriv <- expression()
  model@family@last <- expression()
  model@family@linkinv <- function() {}
  environment(model@family@linkinv) <- new.env(parent = emptyenv())
  model@family@validparams <- function() {}
  environment(model@family@validparams)  <- new.env(parent = emptyenv())
  model@family@infos <- function() {}
  environment(model@family@infos) <- new.env(parent = emptyenv())
  model@family@loglikelihood <- function() {}
  environment(model@family@loglikelihood) <- new.env(parent = emptyenv())

  model
}

#' @examples
#' model <- scaling_results$models$model_time[[1]]
#' model2 <- strip_vglm(model)
#' model2@family %>% maps4(pryr::object_size) %>% unlist() %>% sort()
#' model2@family@linkinv %>% pryr::object_size()
















#' @export
#' @rdname strip
#'
strip_gam <-
  function(object)
  {
    object$residuals <- NULL
    # object$smooth <- NULL
    object$fitted.values <- NULL
    object$family <- NULL
    object$linear.predictors <- NULL
    object$weights <- NULL
    object$prior.weights <- NULL
    object$y <- NULL
    object$hat <- NULL
    object$formula <- NULL
    object$model <- NULL
    object$pred.formula <- NULL
    object$offset <- NULL
    attr(object$terms,".Environment") <- NULL
    attr(object$pterms,".Environment") <- NULL

    return(object)
  }
