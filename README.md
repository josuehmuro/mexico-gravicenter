# mexico-gravicenter
Lo que busca hacer el repositorio es dar un calculo aproximado del gravicentro de la republica mexicana.
Primero, se usa el método Montecarlo para obtener una cantidad fija de puntos aleatorios dentro de mexico
dado por su latitud y longitud. Con estos puntos se calcula el promedio de las latitudes y longitudes para
dar un punto medio aproximado.
Para aumentar la precision del punto obtenido, en vez de calcular un unico punto medio entre una poblacion
considerable, se obtienen varios puntos medios aproximados y se consideran como una muestra aleatoria.
Se hace estadistica inferencial para calcular un nuevo gravicentro aproximado. Dependiendo del tamaño de 
la muestra se calcula un intervalo de confianza con la distribucion T de Student o una normal, y el 
intervalo de confianza se grafica en el mapa. 

En el repositorio se encuentra un pdf con un mapa con el punto obtenido, notese que el intervalo es tan
pequeño que es mas pequeño que el propio punto. Además se agrega la informacion obtenida con una prueba
de 50 puntos medios cada uno obtenido de 1000 puntos aleatorios dentro de mexico.
