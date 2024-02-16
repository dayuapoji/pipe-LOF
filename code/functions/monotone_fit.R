monotone_fit = function(TBF_inverse, failure_age) {
  to_minimize = function(TBF_inverse, failure_age, par) {
    return(sum((TBF_inverse - par[1] * par[2] * failure_age^(par[2] - 1))^2))
  }
  ml = optim(c(0, 2), to_minimize, TBF_inverse = TBF_inverse, 
             failure_age = failure_age, lower = c(0, 2))
  return(ml)
}

# Example
# y = c(0.01, 1, 3, 4, 20)    # 1/TBF for one pipe
# x = c(0.01, 10, 20, 30, 80)    # failure ages for one pipe
# monotone_fit(y, x)
# params = monotone_fit(y, x)$par    # these are the two estimated parameters you care about!
# 
# age = seq(0, 100, length = 1000)
# plot(x, y)	# observed data
# lines(age, params[1] * params[2] * age^(params[2] - 1))  # estimates
