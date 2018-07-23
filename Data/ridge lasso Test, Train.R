install.packages("lars")
library(lars)

str(data)
lars<-lars(x,y,type="lasso")
summary(lars)
lars
plot(lars)
# 색이 있는 선들은 각각 독립변수의 regression coefficients를 뜻한다.
# x축은 s대신 shrinkage의 비율을 나타내고, 그 비율에 따라서 독립변수들의 coefficient가 달라진다.

# 물론 x축에 따라서 독립변수의 ±가 달라질 수도 있다.



# 결국 s에 따라서 coefficient가 변하거나, 혹은 값이 0이 되어 독립변수가 제거되어, subset selection이 되는 모습을 확인할 수 있다.


coef_lars <- coef(lars, s=4)
coef_lars

plot(lars, plottype="Cp")

round(lars$beta) 




model = lars(x, y)
coef(model)

cv = cv.glmnet(x, y)
model = glmnet(x, y, type.gaussian="covariance", lambda=cv$lambda.min)
predict(model, type="coefficients")
