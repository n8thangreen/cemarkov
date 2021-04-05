
# my_model <- markovModel$new()
# my_model$run()
#
markovModel <- R6Class("markovModel", list(
  init_pop = c(10,0,0),
  transmat = matrix(c(0.5, 0.25, 0.25,
                      0.4, 0.3, 0.3,
                      0.5, 0.1, 0.4),
                    nrow = 3,
                    byrow = TRUE),
  n = 10,

  run = function() {
    pop <- matrix(NA,
                  nrow = length(self$init_pop),
                  ncol = self$n)
    pop[, 1] <- self$init_pop

    for (i in 2:self$n) {
      pop[, i] <-
        t(pop[, i - 1]) %*% self$transmat
    }
    pop
  })
)


# cost_model <- markovModelVal$new()
# cost_model$run()
# cost_model$tot_vals()
#
markovModelVal <- R6Class("markovModelVal",
    inherit = markovModel,
    public = list(
      unit_vals = c(1,2,3),
      discount = 0,

      tot_vals = function() {
        counts <- super$run()
        disc <- (1 + self$discount)^(1:self$n - 1)
        tot_vals <- self$unit_vals %*% counts
        tot_vals <- tot_vals/disc
        tot_vals
      }
  )
)


# ce_model <- ceMarkov$new()
# ce_model$inmb(k = 10)
#
ceMarkov <- R6Class("ceMarkov", list(

    c_0 = markovModelVal$new(),
    h_0 = markovModelVal$new(),
    c_1 = markovModelVal$new(),
    h_1 = markovModelVal$new(),

    icer = function() {
      delta_c <-
        sum(self$c_1$tot_vals()) - sum(self$c_0$tot_vals())
      delta_h <-
        sum(self$h_1$tot_vals()) - sum(self$h_0$tot_vals())
      delta_c/delta_h
    },
    inmb = function(k) {
      delta_c <-
        sum(self$c_1$tot_vals()) - sum(self$c_0$tot_vals())
      delta_h <-
        sum(self$h_1$tot_vals()) - sum(self$h_0$tot_vals())
      delta_h*k - delta_c
    }
))

