# The Lasso 
lasso.mod = glmnet(x[train, ], y[train], alpha = 1, lambda = grid) 
plot(lasso.mod) 
set.seed(1) 
cv.out = cv.glmnet(x[train, ], y[train], alpha = 1) 
plot(cv.out) 
bestlam = cv.out$lambda.min 
lasso.pred = predict(lasso.mod, s = bestlam, newx = x[test, ]) 
mean((lasso.pred - y.test)^2)


out = glmnet(x, y, alpha = 1, lambda = grid) 
lasso.coef = predict(out, type = "coefficients", s = bestlam)
lasso.coef
