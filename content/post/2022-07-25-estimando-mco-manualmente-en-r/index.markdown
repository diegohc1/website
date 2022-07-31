---
title: Estimando MCO 'manualmente' en R
author: ''
draft: TRUE
date: '2022-07-25'
slug: estimando-mco-manualmente-en-r
categories: [Econometría]
tags: []
subtitle: ''
summary: ''
authors: []
lastmod: '2022-07-25T18:39:09-05:00'
featured: no
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
---

El método más común para la estimación del modelo de regresión lineal es el de mínimos cuadrados. En esta entrada muestro como construir el estimador de mínimos cuadrados ordinarios paso a paso en R. En realidad, dados los supuestos del modelo clásico de regresión lineal, es un procedimiento bastante sencillo. La fórmula para estimar los parámetros es: `\(b = {(X^{\prime}X)}^{-1}X^{\prime}y\)`, donde `\(X\)` contiene las `\(n\)` observaciones de las `\(k\)` variables independientes, y `\(y\)` a las `\(n\)` observaciones de la variable dependiente. Pero... ¿de dónde sale está formula?


```r
# establecemos los parametros a estimar
set.seed(1)
N <- 100
b0 <- 2 
b1 <- 3 
b2 <- 5

# simulamos datos
x1 <- runif(N) 
x2 <- runif(N) 
u <- rnorm(N) 

# generamos y
y <- b0 + b1*x1 + b2*x2 + u

m1 <- lm(y ~ x1 + x2) #regresión
summary(m1)
```


Call:
lm(formula = y ~ x1 + x2)

Residuals:
    Min      1Q  Median      3Q     Max 
-1.8579 -0.6167 -0.1432  0.5352  2.3318 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   1.9906     0.2791   7.133 1.77e-10 ***
x1            2.9307     0.3634   8.064 1.96e-12 ***
x2            5.0144     0.3578  14.015  < 2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.9675 on 97 degrees of freedom
Multiple R-squared:  0.7323,	Adjusted R-squared:  0.7268 
F-statistic: 132.7 on 2 and 97 DF,  p-value: < 2.2e-16

```r
# definir la matrix X (variables explicativas)
X <- as.matrix(cbind(1, x1, x2)) #aumentando un vector de unos

#Estimacion de b = ( (X'X)^(-1) )X'y

# solve() --> genera la inversa de la matriz
# t()  --> genera la transpuesta de la matriz
# %*%  --> simbolo para multiplicar matrices

b <- solve( t(X) %*% X) %*% t(X) %*% y

# calcular los residuales / errores e = y-Xb
e <- y - X %*% b
colSums(e) #practicamente 0
```

[1] 2.926548e-13

```r
# numero de observaciones (n) y parametros (k)
n <- nrow(X);    k <- ncol(X)

# calcular la matriz de varianzas y covarianzas cov = ( 1/(n-k) )e'e(X'X)^(-1)
#  estimar la varianza de los errores: sigma^2 : s
# s = ( 1/(n-k) )e'e
s <- 1/(n-k) * as.numeric(t(e)%*%e) #as.numeric para que sea un valor

#  multiplicar por (X'X)^(-1)
cov <- s*solve( t(X) %*% X)

# desviaciones estandar de los coeficientes. se = raiz de las diagonales de cov
se <- sqrt(diag(cov))

# estadisticos t
t <- (b/se)

# p_value del estadistico t
p_val <- 2*pt(-abs(t), df = n-k-1)

#Tabla de la regresion
tabla <- round(cbind(b, se, t, p_val), 4)

#con función de R (lm)
a <- lm(y~x1+x2); summary(a)
```


Call:
lm(formula = y ~ x1 + x2)

Residuals:
    Min      1Q  Median      3Q     Max 
-1.8579 -0.6167 -0.1432  0.5352  2.3318 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   1.9906     0.2791   7.133 1.77e-10 ***
x1            2.9307     0.3634   8.064 1.96e-12 ***
x2            5.0144     0.3578  14.015  < 2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.9675 on 97 degrees of freedom
Multiple R-squared:  0.7323,	Adjusted R-squared:  0.7268 
F-statistic: 132.7 on 2 and 97 DF,  p-value: < 2.2e-16

```r
texreg::htmlreg(a)
```

<table class="texreg" style="margin: 10px auto;border-collapse: collapse;border-spacing: 0px;caption-side: bottom;color: #000000;border-top: 2px solid #000000;">
<caption>Statistical models</caption>
<thead>
<tr>
<th style="padding-left: 5px;padding-right: 5px;">&nbsp;</th>
<th style="padding-left: 5px;padding-right: 5px;">Model 1</th>
</tr>
</thead>
<tbody>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">(Intercept)</td>
<td style="padding-left: 5px;padding-right: 5px;">1.99<sup>***</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(0.28)</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">x1</td>
<td style="padding-left: 5px;padding-right: 5px;">2.93<sup>***</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(0.36)</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">x2</td>
<td style="padding-left: 5px;padding-right: 5px;">5.01<sup>***</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(0.36)</td>
</tr>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">R<sup>2</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.73</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Adj. R<sup>2</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.73</td>
</tr>
<tr style="border-bottom: 2px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">Num. obs.</td>
<td style="padding-left: 5px;padding-right: 5px;">100</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="font-size: 0.8em;" colspan="2"><sup>***</sup>p &lt; 0.001; <sup>**</sup>p &lt; 0.01; <sup>*</sup>p &lt; 0.05</td>
</tr>
</tfoot>
</table>

