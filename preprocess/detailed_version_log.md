1. Agrupación de características binarias en dos subconjuntos (has_secondary_use y has_superstructure). De esta manera no se pierde información alguna y mantenemos la misma expresividad en los árboles. Puesto que se cuentan con muchas instancias, separación de un 20% para evaluación local.
   - Algunas variables categóricas cuentan con demasiadas categorías para CART.
   - Coste computacional alto en CART.

2. Se reducen características correladas (categorizando num_floors) y aquellas con demasiadas categorías (geo2, geo3, has_secondary_use). Se aumenta el conjunto de validación al 30%. Se eliminan algunas variables demasiado desbalanceadas (plan_configuration, legal_ownership_status). Se quitan los duplicados del dataset resultante.
   - Coste computacional alto. El dataset resultante sigue contando con unas 125.000 instancias.

3. Reducción de características a 10 con cálculo de scores en base a chi2 y reducción de instancias con ENN (one-hot-encoding aplicado previamente para el úso de estos métodos). Se mantienen fuera las variables eliminadas anteriormente, pero se mantienen los duplicados.
   
   Por un lado, queremos mantener los duplicados pues afecta a las medidas de GINI y entropía, de esta manera la información repetida se ve reflejada en los árboles pues se favorece la correcta etiquetación de estos. Por otro lado, al tener tan cantidad de instancias duplicadas podemos perder expresividad en árboles al contar con tantas columnas (y en unas pruebas iniciales se veía que sí, no se etiquetaba nunca la clase 1). Este problema es más frecuente en CART que en C4.5. También pueden generar overfitting en C4.5 si el método de poda coge umbrales bajos.

    El mantenerlos nos obliga a tener en cuenta además que al seleccionar características aumenta el número de duplicados en el dataset, y que se pueden generar instancias contradictorias (mismos valores, distinta clase de salida).

   - Resultados aceptables en CART, aunque se genera un árbol demasiado pequeño (por tanto no se aplica poda). 
   - Posible overfitting en C4.5, se produce un muy grande tras la poda.
   - Perdemos la proporción de etiquetas en nuestros datos, y podemos considerarlo tanto como algo bueno como algo malo. Para CART se da posiblemente bastante preferencia a la etiqueta 1.

4. Se parte del punto 3 y se aumenta el número de características a 15.
   - Se considera utilizar geo1 entero, pero no somos el experto, por lo que tomar la decisión en base a nuestra suposición a lo mejor no es buena idea.
    - Resultados malísimos en C4.5 probablemente causando overfitting por estos duplicados.
    - Se decide llevar preprocesamientos diferentes para cada algoritmo. Cuando obtenemos demasiadas instancias, el método "tree" de R falla.

5. Idem. pero con mutual_info_classif
   - Se prueba con diferentes tamaños de particiones, nos quedamos con 80%.
     - train 15%  = CV accuracy 62.61%, 0.6571 F1
     - train 50%  = CV accuracy 63.68%, 0.6638 F1
     - train 80%  = CV accuracy 64.34%, 0.6686 F1
     - train 100% = CV accuracy 64.67%
   - Parece que C4.5 funciona mejor con más datos y variables, probablemente porque hace proda tras conseguir el árbol. CART por el contrario no, se queda con un árbol pequeño así que la selección de instancias/características debe ser buena.
 
6. Punto 5 aumentando el conjunto de entrenamiento al 95%
   - Posible problema: Vienen de un one hot por lo que la reducción es enorme

7. Oversampling con método SMOTE seguido de un undersampling con ENN
   - Se desbalancea la clase mayoritaria, sobregeneralización en CART -> __Problema importante__
   - Buenos resultados en validación (del 5%), pobres en test.
   - Posible sobrepredicción las clases originalmente minoritarias.

8. Intentamos paliar el desbalanceo usando solo oversampling con SMOTE, aplicando one-hot. 300.000 instancias en total, separación del 35% para validación.

9.  Solo oversampling con SMOTE, sin aplicar one-hot
    - Se predice de manera excesiva la etiqueta 3.
    - Concluímos que el balanceo no funciona bien para el método C4.5, no descarta un efecto beneficioso en CART, puesto que genera árboles muy pequeños.

10. Pensando en v6: Discretización de variables continuas. Algo bueno en los árboles es que pueden trabajar con variables continuas, de manera que las medidas de GINI/entropía ayuden a elegir los puntos de separación en la partición de las ramas. Aún así, quizás ayudando previamente haciendo particiones de manera inteligente puede mejorar la calidad del árbol.
    - Sin reducción de características CART tarda mucho y agota máxima profundidad 
    - C4.5 va bien con todas, intentamos reducirle variables innecesarias

11. v10 con selección de características a 15, imitando a v6
   - Con 15 salen árboles muy pequeños, el uso las variables continuas es lo que iba bien. Pues también nos da errores de validación y CV muy malos
   - Se sube a 40, sigue con valores reguleros (CV 60.8112, f1 val 0.5742824, hojas 543, size 103180)
   - Se acaba evaluando en test con 80 características

12. v11 Reduciendo puntos con Tomek (sin eliminar duplicados, se lo dejamos a tomek)
   - No se puede, tarda mucho
