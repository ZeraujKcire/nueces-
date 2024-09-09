
# === LIBRARIES === (((
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(lmtest))
suppressPackageStartupMessages(library(qqplotr))
suppressPackageStartupMessages(library(GGally))
# )))

# === DATABASE === (((
sprintf("--- DATABASE ---")
datos = read.table('merged_market_bombo.txt',header=TRUE,na.string="NA")
X1 = datos$EDAD
X2 = datos$VOL
Y = datos$PRECIO
sprintf("X1. EDAD")
sprintf("X2. VOL")
sprintf("Y. PRECIO")
# )))

# === GRAFICO DISPERSION === (((
sprintf("--- 2 GRAFICO DE DISPERSION ---")
ggpairs(datos,columns=2:4)
# )))

# === COMPRUEBACION DE SUPUESTOS === (((

# === HOMOCEDASTICIDAD === (((
sprintf("--- 3.1 HOMOCEDASTICIDAD ---")

# SUPERFICIE DE REGRESIÓN
regresion = lm(Y~X1+X2,data=datos,na.action="na.exclude")
# TEST DE BREUCHE PAGAN.
bptest(regresion)

# CÁLCULO DE RESIDUALES.
y_estimada        = fitted(regresion)
residuales = residuals(regresion)
residu_estandar   = rstudent(regresion)

# GRÁFICAS.
graf_1 = ggplot(data = NULL, aes(x=Y,y=residuales)) + geom_point() + ggtitle("Y vs Resiuales.")      + labs(x="Y",y="Residuales")     + geom_hline(yintercept=0)
graf_2 = ggplot(data = NULL, aes(x=Y,y=residu_estandar))   + geom_point() + ggtitle("Y vs Resid. Estand.")  + labs(x="Y",y="Resid. Estand.") + geom_hline(yintercept=0)

grid.arrange(graf_1,graf_2,ncol=2,nrow=1) 
# )))

# === INDEPENDENCIA === (((
sprintf("--- 3.2 INDEPENDIENCIA ---")
# TEST
dwtest(regresion)

# GRAFICA
graf_resid_estand = ggplot(data = NULL, aes(x=Y,y=residu_estandar)) + geom_point() + ggtitle("Y vs Residuales Estandarizados") + labs(x="Y",y="Residuales Estandarizados")  + geom_hline(yintercept=0,color="blue") + geom_line()
graf_resid_estand
# )))

# === NORMALIDAD === (((
sprintf("--- 3.3 NORMALIDAD ---")
histo_resid = ggplot(data = NULL, aes(x=residuales)) + geom_histogram() + ggtitle("Histograma de Residuales")
histo_resid

shapiro.test(residuales)
ks.test(residuales,"pnorm",0, sqrt(anova(regresion)[3,3]))
# )))

# )))

# === MODELO DE REGREION LINEAL === (((
sprintf("--- 4 ESTIMACION DE BETA_i ---")
regresion
# )))

# === DATOS ATIPICOS === (((
# sprintf("--- 5 DATOS ATÍPICOS ---")
# sprintf("Se usa la grafica de la sección 3.2")
# datos[datos[,4]>5,]
# )))

# === ANOVA === (((
sprintf("--- 6 TABLA ANOVA ---")
anova = anova(regresion)
anova
n = dim(anova)[1] # véase que se tienen 4 filas en la tabla anova (línea 64).

# CONSTRUIREMOS LA SIGUIENTE MATRIZ
# |----------------------+------------------------+----------------+------------------------------+-----------------------------------------|
# | Fuentes de variación | Suma Cuadrados         | Grados de Lib. | Cuadrados Medios             | F                                       |
# |----------------------+------------------------+----------------+------------------------------+-----------------------------------------|
# | Regresión            | SSR = sum_i anova[i,2] | n-1            | CMR = sum_i anova[i,3]/(n-1) | F = sum_i anova[i,3]/(n-1) / anova[n,3] |
# |----------------------+------------------------+----------------+------------------------------+-----------------------------------------|
# | Error                | SSE = anova[n,2]       | anova[n,1]     | CME = anova[n,3]             |                                         |
# |----------------------+------------------------+----------------+------------------------------+-----------------------------------------|
# | Totales              | suma                   | suma           | suma                         |                                         |
# |----------------------+------------------------+----------------+------------------------------+-----------------------------------------|

column.names = c("Suma Cuadrados","Grados Lib.","Cuadrados Medios","F")
row.names    = c("Regresión","Error","Totales")

# --- DATOS DE LA TABLA ---
# SUMA CUADRADOS REGRESIÓN
SSR = 0
for(i in 1:(n-1)) {SSR = SSR + anova[i,2]}
# GRADOS DE LIBERTAD RERGESIÓN
# n-1
# CUADRADOS MEDIOS REGRESIÓN.
suma = 0
for(i in 1:(n-1)) {suma = suma + anova[i,3]}
CMR = suma /(n-1)
# SUMA CUADRADOS ERRORES
SSE = anova[n,2]
# GRDOS DE LIBERTAD ERRORES
# anova[n,1]
# CUADRADOS MEDIOS ERRORES
CME = anova[n,3]
# F
F = CMR / CME
# TOTALES
total_1 = SSR + SSE
total_2 = (n-1) + anova[n,1]
total_3 = CMR + CME

columna_1 = c(SSR,SSE,total_1)
columna_2 = c(n-1,anova[n,1],total_2)
columna_3 = c(CMR,CME,total_3)
columna_4 = c(F,0,0)

nueva_anova = array(c(columna_1,columna_2,columna_3,columna_4),dim=c(3,4), dimnames=list(row.names,column.names))
sprintf("TABLA ANOVA. NUEVA")
nueva_anova
# )))

# === SIGNIFICANCIA === (((
sprintf("--- 7 SIGNIFICANCIA ---")
summary(regresion)
# )))

# === PRUEBAS HP DE COEF. === (((
sprintf(" --- PRUEBAS HP DE COEF. --- ")
n = dim(anova)[1] # véase que se tienen 4 filas en la tabla anova (línea 64).
qt(c(0.025,0.975),df=anova[n,1]) # se obtiene el percentil de la dsitribución T.
summary(regresion)$coefficients
# )))

# === OBERVACIONES INFLUYENTES === (((
sprintf(" --- 10 OBSERVACIONES INFLIYENTES --- ")
influence.measures(regresion)
# )))

# === COEF. DE DETERMINACION === (((
sprintf(" --- 11 COEFICIENTE DE DETERMINACIÓN --- ")
summary(regresion)$r.squared
# )))

# === SUSTITUCION === (((
sprintf(" --- SUSTITUCION --- ")
predict(regresion,data.frame(X1=c(5),X2=c(2.5) ))
# )))
