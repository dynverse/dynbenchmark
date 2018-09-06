#' Strip a vglm model, removing components which are not necessary for predictions
#'
#' @param model The vglm model
#'
#' @examples
#' \dontrun{
#' d.AD <- data.frame(treatment = gl(3, 3),outcome = gl(3, 1, 9),counts = c(18,17,15,20,10,20,25,13,12))
#' model <- VGAM::vglm(counts ~ outcome + treatment, family = VGAM::poissonff, data = d.AD, trace = TRUE)
#'
#' pryr::object_size(model)
#' pryr::object_size(strip_vglm(model))
#' 1/as.numeric(pryr::object_size(strip_vglm(model))/pryr::object_size(model))
#' predict(model, d.AD)
#' predict(strip_vglm(model), d.AD)
#' }
#'
#' @export
strip_vglm = function(model) {
  f <- list()
  class(f)<- "vglmff"
  model@family <- f
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
  model
}
