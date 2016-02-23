#' @export
makeRLearner.classif.krls = function() {
  makeRLearnerClassif(
    cl = "classif.krls",
    package = "KRLS",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "whichkernel", values = c("gaussian", "linear", "poly2", "poly3", "poly4"), default = "gaussian"),
      makeNumericLearnerParam(id = "lambda"),
      makeNumericLearnerParam(id = "sigma", requires = quote(whichkernel == "gaussian")),
      makeLogicalLearnerParam(id = "derivative", default = TRUE, requires = quote(whichkernel == "gaussian" & vcov == TRUE)),
      makeLogicalLearnerParam(id = "binary", default = TRUE, requires = quote(derivative == TRUE)),
      makeLogicalLearnerParam(id = "vcov", default = TRUE),
      makeDiscreteLearnerParam(id = "print.level", default = 1L, values = c(0L, 1L, 2L), tunable = FALSE),
      makeNumericLearnerParam(id = "L"),
      makeNumericLearnerParam(id = "U"),
      makeNumericLearnerParam(id = "til"),
      makeNumericLearnerParam(id = "eigtrunc")
    ),
    properties = c("numerics", "factors", "prob", "twoclass"),
    name = "Kernel Regularized Least Squares",
    short.name = "krls",
    note = ""
  )
}

#' @export
trainLearner.classif.krls = function(.learner, .task, .subset, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE, recode.target = "01")
  info = getFixDataInfo(d$data, factors.to.dummies = TRUE, ordered.to.int = TRUE)
  args = c(list(X = as.matrix(fixDataForLearner(d$data, info)), y = d$target), list(...))
  rm(d)
  attachTrainingInfo(do.call(KRLS::krls, args), info)
}

#' @export
predictLearner.classif.krls = function(.learner, .model, .newdata, ...) {
  info = getTrainingInfo(.model)
  .newdata = as.matrix(fixDataForLearner(.newdata, info))
  ##levs = .model$task.desc$class.levels
  levs = c(.model$task.desc$negative, .model$task.desc$positive)
  p = KRLS::predict.krls(.model$learner.model, newdata = .newdata)
  if (.learner$predict.type == "prob") {
    p = propVectorToMatrix(as.numeric(p$fit), levs)
  } else {
    p = as.factor(ifelse(as.numeric(p$fit) > 0.5, levs[2L], levs[1L]))
  }
  return(p)
}
