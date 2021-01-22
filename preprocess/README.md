# Versiones

1. Agrupación de características binarias en dos subconjuntos (has_secondary_use y has_superstructure).

2. Se reducen características correladas (categorizando num_floors) y aquellas con demasiadas categorías (geo2, geo3, has_secondary_use).Se eliminan algunas variables demasiado desbalanceadas (plan_configuration, legal_ownership_status). Se quitan los duplicados del dataset resultante.

3. Reducción de características a 10 con cálculo de scores en base a chi2 y reducción de instancias con ENN (one-hot-encoding aplicado previamente para el úso de estos métodos). Se mantienen fuera las variables eliminadas anteriormente, pero se mantienen los duplicados.   

4. Se parte del punto 3 y se aumenta el número de características a 15.
   
5. Idem. pero con mutual_info_classif   
 
6. Punto 5 aumentando el conjunto de entrenamiento al 95%   

7. Oversampling con método SMOTE seguido de un undersampling con ENN

8. Intentamos paliar el desbalanceo usando solo oversampling con SMOTE, aplicando one-hot.

9.  Solo oversampling con SMOTE, sin aplicar one-hot

10. Partiendo de v6, discretización de variables continuas.

11. v10 con selección de características a 15