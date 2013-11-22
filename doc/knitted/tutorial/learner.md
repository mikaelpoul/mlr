Learners
========

These classes provide a unified interface to all
classification/regression methods in R. Some are already integrated,
others are not, but the package is specifically designed to make
extensions simple. If one of your favorite methods is missing, either
send the **mlr** maintainers an email or see the part of the tutorial
on [how to extend the package yourself](create_learner.md).
  
To see which learners are already implemented, have a look at the
[learners](http://berndbischl.github.io/mlr/learners.html) documentation page.

Generally, you have two options to access the learning algorithms in **mlr**. 
Either by passing the name as a string to the appropriate method, e.g. [train](http://berndbischl.github.io/mlr/train.html), 
or by constructing a [Learner](http://berndbischl.github.io/mlr/makeLearner.html) object explicitly by means of the
[makeLearner](http://berndbischl.github.io/mlr/makeLearner.html) factory method. 

Why construct a [Learner](http://berndbischl.github.io/mlr/makeLearner.html) object? 

* You have to create a [Learner](http://berndbischl.github.io/mlr/makeLearner.html) object if you want to pass hyperparameter values other than the default values.
* You have the opportunity to set an ID, which is quite useful if you conduct larger experiments and need to access results of a certain learning method.
* For classification tasks you have to create a [Learner](http://berndbischl.github.io/mlr/makeLearner.html) object if you want to control the type of [prediction](http://berndbischl.github.io/mlr/predict.WrappedModel.html) which is returned. Per default you get the class labels when making a [prediction](http://berndbischl.github.io/mlr/predict.WrappedModel.html). In order to get posterior probabilities you have to specify this when creating the learner.

The [makeLearner](http://berndbischl.github.io/mlr/makeLearner.html) factory method takes the names of the learning method as 
its first argument. The naming conventions are ``classif.<R_method_name>`` for 
classification methods and ``regr.<R_method_name>`` for regression methods.
The names of all learning methods are listed on the [learners](http://berndbischl.github.io/mlr/learners.html) documentation page.


Classification example
----------------------


```r
library("mlr")
lrn <- makeLearner("classif.rpart")
lrn
```

```
## Learner classif.rpart from package rpart
## Type: classif
## Class: classif.rpart
## Predict-Type: response
## Hyperparameters: xval=0
## 
## Supported features Numerics:TRUE Factors:TRUE
## Supports missings: TRUE
## Supports weights: TRUE
## Supports classes: two,multi
## Supports probabilities: TRUE
```


The generated learner is an object of class [Learner](http://berndbischl.github.io/mlr/makeLearner.html). It contains basic 
information about the learning method like the chosen hyperparameter values 
(if specified) and the type of [prediction](http://berndbischl.github.io/mlr/predict.WrappedModel.html) that will be made by this learner.
Moreover, it provides information about the types of classification problems 
and properties of the data the learning method can deal with. For example,
it states, if two- or multi-class problems are supported and if it can cope 
with case weights or missing values.

The tables in [IntegratedLearners](../integrated_learners.md) provide a survey about these properties for
all integrated learning methods.

Now let's create a [Learner](http://berndbischl.github.io/mlr/makeLearner.html) and additionally specify hyperparameter values and 
an ID.


```r
lrn <- makeLearner("classif.rpart", minsplit = 7, cp = 0,03, id = "myrpart")
```

```
## Error: Argument predict.type must be any of: response,prob!
```

```r
lrn
```

```
## Learner classif.rpart from package rpart
## Type: classif
## Class: classif.rpart
## Predict-Type: response
## Hyperparameters: xval=0
## 
## Supported features Numerics:TRUE Factors:TRUE
## Supports missings: TRUE
## Supports weights: TRUE
## Supports classes: two,multi
## Supports probabilities: TRUE
```


This is how to get posterior probabilities when making a [prediction](http://berndbischl.github.io/mlr/predict.WrappedModel.html).


```r
lrn <- makeLearner("classif.rpart", predict.type = "prob")
lrn
```

```
## Learner classif.rpart from package rpart
## Type: classif
## Class: classif.rpart
## Predict-Type: prob
## Hyperparameters: xval=0
## 
## Supported features Numerics:TRUE Factors:TRUE
## Supports missings: TRUE
## Supports weights: TRUE
## Supports classes: two,multi
## Supports probabilities: TRUE
```



Regression example
------------------

Create a rpart [Learner](http://berndbischl.github.io/mlr/makeLearner.html) for regression. 


```r
lrn <- makeLearner("regr.rpart")
lrn
```

```
## Learner regr.rpart from package rpart
## Type: regr
## Class: regr.rpart
## Predict-Type: response
## Hyperparameters: xval=0
## 
## Supported features Numerics:TRUE Factors:TRUE
## Supports missings: TRUE
## Supports weights: TRUE
## Supports standard errs: FALSE
```


Finally, let's create a Gradient Boosting Machine [Learner](http://berndbischl.github.io/mlr/makeLearner.html) and pass hyperparameters
and an ID.


```r
lrn <- makeLearner("regr.gbm", n.trees = 500, distribution = "laplace", interaction.depth = 3, 
    id = "mygbm")
lrn
```

```
## Learner mygbm from package gbm
## Type: regr
## Class: regr.gbm
## Predict-Type: response
## Hyperparameters: distribution=laplace,n.trees=500,interaction.depth=3
## 
## Supported features Numerics:TRUE Factors:TRUE
## Supports missings: TRUE
## Supports weights: TRUE
## Supports standard errs: FALSE
```

