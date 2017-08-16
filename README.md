# Linear Regression Lab

## Question 1

1. Create a function leastsquares_gd that takes y and X and returns the least squares estimate calculated with gradient descent.
2. Compare the outcomes of this function with the ones of the lm package and the ones from direct calculation. What is the influence of the stepsize? What is the influence of the number of gradient descent iterations? Are these two quantities connected?
3. Create a function leastsquares_gd_bt that complements leastsquares_gd with a backtracking rule.