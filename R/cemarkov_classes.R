
# my_model <- markovModel$new()
# my_model$run()
#
markovModel <- R6::R6Class("markovModel",
  public = list(
    init_pop = NULL,
    transmat = NULL,
    n = NULL,
    pop = NULL,

    #' @param pop
    #' @param init_pop
    #' @param transmat
    #' @return A new markovModel object
    initialize = function(n = NA,
                          init_pop = NA,
                          transmat = NA) {

      self$n <- if (is.na(n)) 10 else n

      self$init_pop <-
        if (is.na(init_pop)) c(10,0,0) else init_pop

      self$transmat <-
        if (is.na(transmat)) {
          matrix(c(0.5, 0.25, 0.25,
                   0.4, 0.3, 0.3,
                   0.5, 0.1, 0.4),
                 nrow = 3,
                 byrow = TRUE)
        } else {transmat}

      self$pop <- matrix(NA,
                         nrow = length(self$init_pop),
                         ncol = self$n)
      self$pop[, 1] <- self$init_pop
    },

    run = function() {

      for (i in 2:self$n) {
        self$pop[, i] <-
          t(self$pop[, i - 1]) %*% self$transmat
      }
      self$pop
    })
)


# cost_model <- markovModelVal$new()
# cost_model$run()
# cost_model$tot_vals()
#
markovModelVal <- R6::R6Class("markovModelVal",
    inherit = markovModel,

    public = list(
      unit_vals = NULL,
      discount = NULL,

      #' @param unit_vals
      #' @param discount
      initialize = function(unit_vals = NA,
                            discount = NA,
                            n = NA,
                            init_pop = NA,
                            transmat = NA) {

        super$initialize(n, init_pop, transmat)

        self$unit_vals <-
          if (is.na(unit_vals)) {c(1,2,3)
          } else {unit_vals}

        self$discount <-
          if (is.na(discount)) 0 else discount
      },

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
ceMarkov <- R6::R6Class("ceMarkov", list(

    transmat = NULL,
    init_pop = NULL,
    n = NULL,

    discount = NULL,
    unit_c0 = NULL,
    unit_c1 = NULL,
    unit_h0 = NULL,
    unit_h1 = NULL,

    c_0 = NULL,
    c_1 = NULL,
    h_0 = NULL,
    h_1 = NULL,

    initialize = function(unit_c0 = NA,
                          unit_c1 = NA,
                          unit_h0 = NA,
                          unit_h1 = NA,
                          discount = NA) {

      self$c_0 <- markovModelVal$new(
        unit_c0, discount, n, init_pop, transmat)
      self$h_0 <- markovModelVal$new(
        unit_c1, discount, n, init_pop, transmat)
      self$c_1 <- markovModelVal$new(
        unit_h0, discount, n, init_pop, transmat)
      self$h_1 <- markovModelVal$new(
        unit_h1, discount, n, init_pop, transmat)
    }

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

