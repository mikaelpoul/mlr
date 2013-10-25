#' @S3method makeRLearner regr.fnn
makeRLearner.regr.fnn = function() {
  makeRLearnerRegr(
    cl = "regr.fnn",
    package = "FNN",
    # l is for reject option. cannot be done with mlr atm
    par.set = makeParamSet(
      makeIntegerLearnerParam(id="k", default=1L, lower=1L),
      makeLogicalLearnerParam(id="use.all", default=TRUE, requires=expression(algorithm == "VR")),
      makeDiscreteLearnerParam(id="algorithm", default="cover_tree", values=list("cover_tree", "kd_tree", "VR"))
    ), 
   missings = FALSE,
   numerics = TRUE,
   factors = FALSE,
   se = FALSE,
   weights = FALSE
  )
}

#' @S3method trainLearner regr.fnn
trainLearner.regr.fnn = function(.learner, .task, .subset, .weights,  ...) {
  d = getTaskData(.task, .subset, target.extra=TRUE)
  list(train=d, parset=list(...))
}

#' @S3method predictLearner regr.fnn
predictLearner.regr.fnn = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  pars = list(train=m$train$data, test=.newdata, y=m$train$target)  
  pars = c(pars, m$parset, list(...))
  do.call(FNN::knn.reg, pars)$pred
}