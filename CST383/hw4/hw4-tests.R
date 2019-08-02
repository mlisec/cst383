
#source("../hw4.R")
source("../hw4-with-answers.R")
#
# test the PMF objects in homework 4
#

# example used in the code
pmf = pmf_create(1:3, c(0.5, 0.1, 0.4))
pmf_sample(pmf, 4)
pmf_expect(pmf)
pmf_variance(pmf)
pmf_prob(pmf, 1)
pmf_prob(pmf, c(1,3))
pmf_plot(pmf)

# PMF for McDonald's example
pmf = pmf_create(c(250, 540, 750), c(0.2, 0.5, 0.3))

pmf_plot(pmf)
hist(pmf_sample(pmf, 1000))
pmf_expect(pmf)
sqrt(pmf_variance(pmf))
pmf_prob(pmf, c(250, 540))

#
# test the random variable objects in homework 4
#

# example used in the code
x = rv_create(data.frame(x1=c(0,0,1,1),x2=c(0,1,0,1)), probs=rep(0.25,4), vals=c(0,1,1,2))
y = rv_create(data.frame(x1=c(0,0,1,1),x2=c(0,1,0,1)), probs=rep(0.25,4), vals=c(0,1,1,0))
rv_sample(x, 5)
rv_expect(x)
rv_variance(x)
rv_plot(x)
rv_prob(x, 2)
rv_prob(x, 1:2)
rv_event(x, 1)
rv_event(x, 2)
rv_cond_prob(x, 1, c(TRUE, TRUE, TRUE, FALSE))
rv_cond_prob(x, 2, c(TRUE, TRUE, TRUE, FALSE))
rv_cond_prob(x, 1, rv_event(x, 1:2))
z = rv_add(x,y)
rv_variance(z)
x2 = rv_apply_fun(x, function(x) x^2)
rv_variance(x2)

# random variable x is the sum of two dice rolls
n = 6
outcomes = expand.grid(1:n, 1:n)
probs = rep(1/(n^2), n^2)
vals = apply(outcomes, 1, function(x) x[1] + x[2])
x = rv_create(outcomes, probs, vals)

rv_plot(x)
hist(rv_sample(x, 10000))
rv_expect(x)
rv_variance(x)
rv_prob(x, 2)
rv_prob(x, 7)
rv_prob(x, 2:10)
rv_cond_prob(x, 2, rv_event(x, 1:3))

