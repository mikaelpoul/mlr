#' @export
makeRLearner.regr.krls = function() {
  makeRLearnerRegr(
    cl = "regr.krls",
    package = "KRLS",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "whichkernel", values = c("gaussian", "linear", "poly2", "poly3", "poly4"), default = "gaussian"),
      makeNumericLearnerParam(id = "lambda"),
      makeNumericLearnerParam(id = "sigma", requires = quote(whichkernel == "gaussian")),
      makeLogicalLearnerParam(id = "derivative", default = TRUE, requires = quote(whichkernel == "gaussian" & vcov == TRUE)),
      makeLogicalLearnerParam(id = "binary", default = TRUE, requires = quote(derivative == TRUE)),
      makeLogicalLearnerParam(id = "vcov", default = TRUE),
      makeDiscreteLearnerParam(id = "print.level", default = 1L, values = c(0, 1, 2), tunable = FALSE),
      makeNumericLearnerParam(id = "L"),
      makeNumericLearnerParam(id = "U"),
      makeNumericLearnerParam(id = "til"),
      makeNumericLearnerParam(id = "eigtrunc")
    ),
    properties = c("numerics", "factors", "se"),
    name = "Kernel Regularized Least Squares",
    short.name = "krls",
    note = ""
  )
}

#' @export
trainLearner.regr.krls = function(.learner, .task, .subset, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  info = getFixDataInfo(d$data, factors.to.dummies = TRUE, ordered.to.int = TRUE)
  args = c(list(X = as.matrix(fixDataForLearner(d$data, info)), y = d$target), list(...))
  rm(d)
  attachTrainingInfo(do.call(KRLS::krls, args), info)
}

#' @export
predictLearner.regr.krls = function(.learner, .model, .newdata, ...) {
  info = getTrainingInfo(.model)
  .newdata = as.matrix(fixDataForLearner(.newdata, info))
  se = (.learner$predict.type == "se")
  p = KRLS::predict.krls(.model$learner.model, newdata = .newdata, se.fit = se, ...)
  if (!se) {
    return(as.numeric(p$fit))
  } else {
    return(cbind(as.numeric(p$fit), as.numeric(p$se.fit)))
  }
}
