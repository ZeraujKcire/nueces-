
# === LIBRARIES === (((
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(lmtest))
suppressPackageStartupMessages(library(qqplotr))
suppressPackageStartupMessages(library(GGally))
# )))

# === DATABASE === (((
# LEER LOS DATOS Y GUARDARLOS.
datos = read.table('merged_market_compressor_tornillo.txt',header=TRUE)
X1 = datos$EDAD
X2 = datos$POT
Y = datos$PRECIO
# SE LE COLOCAN LOS NOMBRES A LA COLUMNAS CORRESPONDIENTES.
# colnames(datos) = c("NOMBRE","X1","X2","X3","Y")
# )))

# === GRAFICO DISPERSION === (((
sprintf("--- 2 GRAFICO DE DISPERSION ---")
# pairs(datos)
ggpairs(datos,columns=2:4)
# )))

# === COMPRUEBACION DE SUPUESTOS === (((

# === HOMOCEDASTICIDAD === (((
sprintf("--- 3.1 HOMOCEDASTICIDAD ---")

# SUPERFICIE DE REGRESIÓN
regresion = lm(Y~X1+X2,data=datos)
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

 # ========= 8.1 PH BETA_0  ======================== 
sprintf("--- 8.1 PH BETA_0 ---")
n = dim(anova)[1] # véase que se tienen 4 filas en la tabla anova (línea 64).
qt(c(0.025,0.975),df=anova[n,1]) # se obtiene el percentil de la dsitribución T.
summary(regresion)$coefficients
# ========= 9 REDUCCIÓN DEL MODELO ========================
# sprintf(" --- 9 REDUCCIÓN DE MODELO --- ")
# sprintf("Se reduce el modelo para el que el valor p sea mayor, i.e. para beta_0")
# Coefficients:
             # Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.652906   1.214868  -1.361   0.1925    <--
# X1           0.019021   0.013666   1.392   0.1830    
# X2           0.589858   0.316129   1.866   0.0805 .  
# X3           0.047516   0.006788   7.000    3e-06 ***
# regresion = lm(Y~X1+X2+X3-1,data=datos)
# summary(regresion)$coefficients
# sprintf("Se reduce el modelo para el que el valor p sea mayor, i.e. para beta_1")
      # Estimate  Std. Error   t value     Pr(>|t|)
# X1 0.001597949 0.004890316 0.3267577 7.478398e-01 <--
# X2 0.215013557 0.158860231 1.3534763 1.936274e-01
# X3 0.045829780 0.006839207 6.7010371 3.723601e-06
# regresion = lm(Y~X2+X3-1,data=datos)
# summary(regresion)$coefficients
# sprintf("Se reduce el modelo para el que el valor p sea mayor, i.e. para beta_2")
     # Estimate Std. Error   t value     Pr(>|t|)
# X2 0.21201033  0.1546090  1.371267 1.871463e-01 <--
# X3 0.04775641  0.0033784 14.135808 3.464345e-11
# regresion = lm(Y~X3-1,data=datos)
# summary(regresion)
     # Estimate  Std. Error  t value     Pr(>|t|)
# X3 0.05107362 0.002412323 21.17196 1.128277e-14

# ========= 10 OBERVACIONES INFLUYENTES ========================
sprintf(" --- 10 OBSERVACIONES INFLIYENTES --- ")
influence.measures(regresion)
# 9   0.13968  0.13968 0.999 1.92e-02 0.01480    
# 10  1.06647  1.06647 0.863 8.51e-01 0.13318   * <--
# 11 -0.26365 -0.26365 1.116 7.06e-02 0.09003    

# ========= 11 COEFICIENTE DE DETERMINACIÓN ========================
sprintf(" --- 11 COEFICIENTE DE DETERMINACIÓN --- ")
summary(regresion)$r.squared

# === SUSTITUCION === (((
sprintf(" --- SUSTITUCION --- ")
predict(regresion,data.frame(X1=c(2),X2=c(75) ))
# )))
