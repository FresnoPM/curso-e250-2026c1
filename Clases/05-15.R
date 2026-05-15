# PPT: Clase 15
#
# Medimos el volumen de un set de datos en relación a la cantidad de conexiones que peude tener cada nodo
# igraph sirve para sets muy grandes
# network sirve para profundizar dsde lo estadístico pero no se banca sets de datos muy grandes
#
#
# ¿Qué es una densidad?
# dado un dato con N variables (N nodos)
# Entre esos nodos hay una cantidad de links L, determina la densidad
# El máximo posible de links
#   No Dirigida     L_max = combinatoria(N 2) = N*(N-1)/2   cuando es simétrica (no importa la direccionalidad de las conexiones)
#   Dirigida        L_max =                     N*(N-1)     cuando la red es dirigida
#
#
# Densidad   d=L/L_max
#   No dirigida     2L/(N*(N-1))
#   Dirigida        1L/(N*(N-1))
#
#
# Puedo averiguar el grado de conexiones para cada nodo y distribución de grado
# Para caracterizar globalmente esta red
#
# Tarea caps 1 a 4 https://github.com/kolaczyk/sand
# Acá se contruye la matriz
#
# https://cheatography.com/trvoldemort/cheat-sheets/igraph/ cheatsheet ded igraph
#
#
# Según el tipo de distribución nos es más relevante diferentes métricas, no sabemos a priori cuál distribución tienen nuestros datos, por ejemplo si fuera normal usaríamos la media, pero si es asimétrica nos conviene usar la mediana para identidicar el valor típico de la variable analizada
#
#



