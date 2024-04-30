# Manual Técnico - Pixel Print Studio
## José Daniel Lorenzana Medina - 202206560

## Introducción
Este manual técnico proporciona una descripción detallada de todas las clases utilizadas en la realización de este proyecto, 
como por ejemplo, la realización de las rutas, tabla hash, árbol Merckle, entre otras. 
A continuación se empezará con la descripción de cada una mostrando fragmentos de código para las mismas.

## Módulo HashTable - Técnicos
Este módulo en Fortran define una tabla hash (HashTable) y algunas operaciones asociadas, como la inserción de elementos, la impresión de la tabla, la búsqueda de un elemento por su clave (key), la resolución de colisiones y la creación de un gráfico de la tabla hash. Aquí hay una explicación de las partes clave del código:

**Declaraciones y Parámetros Iniciales:**
- `implicit none`: Obliga a declarar explícitamente todas las variables.
- `private`: Oculta las variables y subrutinas internas fuera del módulo.
- `table_size`: Tamaño inicial de la tabla hash (por defecto 7).
- `R`: Constante utilizada en el cálculo de la función hash.
- `MAX_USED_PERCENTAGE`: Porcentaje máximo de ocupación de la tabla antes de realizar el rehashing.

**Tipos de Datos:**
- `tecnico`: Define una estructura que representa la información de un técnico, que incluye campos como key, telefono, nombre, apellido y direccion.
- `HashTable`: Define la estructura de la tabla hash que contiene un arreglo dinámico de tipo `tecnico` y un contador de elementos.

**Subrutina `insert`:**
- Inserta un nuevo elemento en la tabla hash.
- Maneja colisiones utilizando el método de doble dispersión.
- Realiza el rehashing si la tabla excede un cierto porcentaje de ocupación.

**Función `rehashing`:**
- Duplica el tamaño de la tabla y reinserta todos los elementos en la nueva tabla durante el rehashing.

**Subrutina `solve_collision`:**
- Resuelve las colisiones utilizando el método de doble dispersión para encontrar una nueva posición para insertar un elemento.

**Función `get_position`:**
- Calcula la posición inicial de un elemento en la tabla hash utilizando una función hash.

**Subrutina `print`:**
- Imprime la tabla hash en la consola, mostrando el DPI, nombre, apellido, dirección y teléfono de cada técnico.

**Subrutina `grafico`:**
- Crea un archivo DOT para representar la tabla hash como un gráfico.
- Utiliza el programa Graphviz (dot) para generar una imagen PNG del gráfico.

**Subrutina `search_tecnico`:**
- Busca un técnico en la tabla hash por su DPI (key) y muestra su información en la consola.

En resumen, este módulo proporciona una implementación básica de una tabla hash en Fortran, con operaciones para insertar, buscar, imprimir y visualizar la tabla.
```fortran
module hash_table
    implicit none
    private
    integer :: table_size = 7
    real, parameter :: R = 0.618034
    integer, parameter :: MAX_USED_PERCENTAGE = 70
    type tecnico
    integer(8) ::key,telefono
    character(:), allocatable::nombre,apellido,direccion
    end type tecnico
    type, public :: HashTable
        integer :: elements = 0
        type(tecnico), allocatable :: array(:)

        contains
        procedure :: insert
        procedure :: print
        procedure :: search_tecnico
        procedure, private :: solve_collision
        procedure :: grafico
    end type HashTable
```
## Módulo Árbol AVL - Sucursales
Este módulo en Fortran define una estructura de datos de árbol AVL, que es una variante de los árboles binarios de búsqueda que mantienen la propiedad de equilibrio. Aquí está una explicación de las partes clave del código:

**Declaraciones y Uso de Módulos Externos:**
- Se utilizan los módulos `uuid_module` y `hash_table`.
- Se define una serie de constantes para representar el estado del balance del árbol (`LEFT_HEAVY`, `BALANCED`, `RIGHT_HEAVY`).

**Tipos de Datos:**
- `Node_t`: Define un nodo del árbol AVL que contiene un identificador, factores de equilibrio, datos específicos del nodo y una tabla hash.
- `Tree_t`: Define la estructura del árbol AVL que contiene un puntero a la raíz del árbol.

**Subrutinas y Funciones:**
- `new_branch`: Crea un nuevo nodo del árbol con los datos proporcionados.
- `new_avl`: Inicializa un nuevo árbol AVL.
- Rotaciones (`rotationII`, `rotationDD`, `rotationDI`, `rotationID`): Implementan las rotaciones necesarias para mantener el equilibrio del árbol AVL.
- `insert_node`: Inserta un nuevo nodo en el árbol AVL, manteniendo el equilibrio.
- `recursive_insert`: Inserta recursivamente un nuevo nodo en el árbol AVL, ajustando los factores de equilibrio y realizando rotaciones si es necesario.
- `recursive_print`: Recorre el árbol recursivamente para generar la representación gráfica del árbol en formato DOT.
- `graph_avl_tree`: Genera un archivo DOT y utiliza Graphviz para crear una representación gráfica del árbol AVL.
- `searchNode`, `searchNodeRecursive`: Realizan una búsqueda recursiva de un nodo por su ID y contraseña.
- `searchNode_hash`, `searchNodeRecursive_hash`: Realizan una búsqueda recursiva de un nodo por su ID y agregan una tabla hash al nodo encontrado.
- `searchNode_listar`, `searchNodeRecursive_listar`: Realizan una búsqueda recursiva de un nodo por su ID y listan los técnicos asociados a ese nodo.
  
En resumen, este módulo proporciona una implementación de un árbol AVL en Fortran, con operaciones para insertar nodos, buscar nodos por ID y contraseña, y generar una representación gráfica del árbol. También permite asociar una tabla hash a los nodos del árbol para una funcionalidad adicional.
```fortran
module Avl_Tree
    !use abb_m
    use uuid_module
    use hash_table
    implicit none
  
    ! Cons
    integer, parameter :: LEFT_HEAVY = -1
    integer, parameter :: BALANCED = 0
    integer, parameter :: RIGHT_HEAVY = +1
  
    type Node_t
        integer :: id,Factor
        character(:), allocatable ::dept,direccion,password
       type(HashTable):: tablahash
        type(Node_t), pointer :: Left => null()
        type(Node_t), pointer :: Right => null()
    end type Node_t
  
    type Tree_t
        type(Node_t), pointer :: root => null()
        contains
        procedure :: new_avl
        procedure :: insert_node
        procedure :: graph_avl_tree
        procedure :: searchNode
        procedure :: searchNode_hash
        procedure :: searchNode_listar
        procedure :: searchBranch

    end type Tree_t
```
## Módulo de Rutas
Este módulo en Fortran define varias estructuras de datos y procedimientos relacionados con grafos y análisis de rutas. Aquí tienes un resumen de las partes principales del código:

**Definición de Tipos de Datos:**
- `arista`: Representa una arista en un grafo, con campos como id, weight (peso), parent_id (id del nodo padre), y printers (número de impresoras).
- `arista_list`: Representa una lista de aristas en un grafo, con punteros al primer y último elemento.
- `result`: Representa un resultado, con campos como id, weight y printers.
- `result_list`: Representa una lista de resultados, con punteros al primer y último elemento.
- `node`: Representa un nodo en un grafo, con un id y una lista de aristas vecinas.
- `graph`: Representa un grafo, con un número de nodos y un puntero al primer nodo.
- `analyzer`: Representa un analizador que contiene datos de un grafo.

**Subrutinas y Funciones:**
- `ordenar_distancias` y `ordenar_distancias_2`: Insertan una arista en una lista de aristas de forma ordenada según su peso o su id.
- `pop`: Extrae la primera arista de una lista de aristas.
- `unir_listas` y `unir_listas_2`: Unen dos listas de aristas.
- `vacio_ver`: Verifica si una lista de aristas está vacía.
- `anadir_peso_impresoras`: Añade un peso a todas las aristas de una lista y actualiza el número de impresoras.
- `sumar_resultado`: Agrega un resultado a una lista de resultados.
- `insertar_info` y `insertar_info_2`: Insertan información de una arista en un grafo.
- `print`: Imprime una lista de resultados.
- `insertar_nodo`: Inserta un nodo en un grafo.
- `insertar_arista` y `insertar_arista_2`: Insertan una arista en un grafo.
- `obtener_nodo`: Obtiene un nodo por su id.
- `show_graph`: Muestra la estructura del grafo.
- `setear_grafo`: Establece el grafo en un analizador.
- `shortest_path` y `longest_path`: Encuentran la ruta más corta o más larga entre dos nodos en el grafo.
- `graficar`: Genera una representación gráfica del grafo.
- `reset_grafo`: Reinicia el grafo, liberando la memoria utilizada por sus nodos y aristas.

En resumen, este módulo proporciona una variedad de herramientas para trabajar con grafos, incluyendo la creación de grafos, la inserción de nodos y aristas, la búsqueda de rutas más cortas o largas, y la generación de representaciones gráficas de los grafos.
```fortran
module routes
    implicit none
    type arista
        integer :: id
        integer :: weight
        integer :: parent_id
        integer :: printers
        type(arista), pointer :: next => null()
        type(arista), pointer :: prev => null()
    end type arista
    type arista_list
        type(arista), pointer :: head => null()
        type(arista), pointer :: tail => null()
    contains
        procedure :: ordenar_distancias ! Agregar una arista a la lista de aristas de forma ordenada
        procedure :: ordenar_distancias_2 ! Agregar una arista a la lista de aristas de forma ordenada
        procedure :: pop ! Sacar la primera arista de la lista
        procedure :: vacio_ver ! Verificar si la lista de aristas está vacía
        procedure :: unir_listas ! unir_listas two arista lists
        procedure :: unir_listas_2 ! unir_listas two arista lists
        procedure :: anadir_peso_impresoras ! Añadir peso o las impresoras
    end type arista_list
    type result
        integer :: id
        integer :: weight
        integer :: printers
        type(result), pointer :: next => null()
    end type result
    type result_list
        integer :: total_weight, total_printers
        type(result), pointer :: head => null()
        type(result), pointer :: tail => null()
    contains
        procedure :: sumar_resultado
        procedure :: print
    end type result_list
    type node
        integer :: id 
        type(arista_list) :: neighbors
        type(node), pointer :: next => null()
    end type node
    type graph
        integer :: n_nodes
        type(node), pointer :: head => null()
    contains
  end type graph
      type analyzer
          type(graph):: graph_data 
      contains
          procedure :: setear_grafo
          procedure :: shortest_path
          procedure :: longest_path
    end type analyzer
```

## Módulo Árbol de Merckle
Este módulo en Fortran implementa una estructura de datos de árbol de Merkle, que es una estructura de datos en árbol usada principalmente en criptografía y sistemas distribuidos para verificar la integridad y autenticidad de los datos.

Aquí está una descripción de las partes principales del código:

**Declaraciones y Uso de Módulos Externos:**
- Se utiliza el módulo `sha256_module` para calcular los valores hash SHA-256.
- Se declara una variable `uid` para asignar identificadores únicos a los nodos.

**Tipos de Datos:**
- `data`: Define un tipo de datos para almacenar la información de los nodos de datos, incluidos los identificadores, las direcciones y los valores hash.
- `hash_node`: Define un tipo de datos para los nodos del árbol de hash, que almacenan valores hash y referencias a los datos.
- `merkle`: Define la estructura del árbol de Merkle, que contiene punteros a la parte superior del árbol de hash y a la lista de datos, así como un contador de posición.

**Subrutinas y Funciones:**
- `agregar_data`: Agrega un nuevo nodo de datos a la lista de datos en el árbol de Merkle.
- `largo_info`: Calcula la cantidad de nodos de datos en el árbol.
- `get_data`: Obtiene el nodo de datos en una posición específica de la lista.
- `crear_merckle`: Crea la estructura del árbol de hash de Merkle con nodos vacíos.
- `hashear`: Recorre el árbol de hash para calcular los valores hash en cada nivel.
- `primero_hasheo`: Obtiene el valor hash de la raíz del árbol de Merkle.
- `generar_merckle`: Genera el árbol de hash de Merkle completo, rellenando los datos faltantes y calculando los valores hash.
- `dot_merckle`, `dot_merckle_rec`: Generan un archivo DOT y una representación gráfica del árbol de Merkle utilizando Graphviz.

En resumen, este módulo proporciona una implementación de un árbol de Merkle en Fortran, con operaciones para agregar datos, generar el árbol de Merkle, calcular valores hash y visualizar el árbol en formato DOT.
```fortran
module merkle_tree
    use sha256_module
    implicit none
    integer :: uid = 1
    type data
        integer :: uid
        character(:), allocatable :: id_origin, address_origin                
        character(:), allocatable :: id_destination, address_destination
        character(:), allocatable :: cost_between, hash_value
        type(data), pointer :: next => null()
    end type data

    type hash_node
        integer :: uid
        character(:), allocatable :: hash        
        type(hash_node), pointer :: left => null()
        type(hash_node), pointer :: right => null()
        type(data), pointer :: dataref => null()
    end type hash_node
    
    type merkle 
        type(hash_node), pointer :: top_hash => null()
        type(data), pointer :: data_head => null()
        type(data), pointer :: data_tail => null()
        integer :: pos = 0
    contains 
        procedure :: agregar_data
        procedure :: get_data
        procedure :: largo_info
        procedure :: crear_merckle
        procedure :: hashear
        procedure :: generar_merckle
        procedure :: dot_merckle 
        procedure :: dot_merckle_rec
        procedure :: primero_hasheo
    end type merkle
```

## Módulo de BlockChain
Este módulo en Fortran implementa una estructura de cadena de bloques que utiliza un árbol de Merkle para mantener la integridad de los datos almacenados en los bloques. Aquí está una descripción de las partes principales del código:

**Declaraciones y Uso de Módulos Externos:**
- Se utiliza el módulo `routes` para acceder a las rutas de los nodos.
- Se utiliza el módulo `merkle_tree` para manejar el árbol de Merkle.
- Se utiliza el módulo `Avl_Tree` para manejar la estructura de árbol AVL.

**Tipos de Datos:**
- `block`: Define un tipo de datos para representar un bloque en la cadena de bloques. Contiene información como el índice del bloque, el nonce, la marca de tiempo, los datos almacenados en el bloque, los hashes previos, el hash de la raíz del árbol de Merkle y el hash del bloque.
- `chainer`: Define un tipo de datos para la cadena de bloques, que contiene punteros al primer y último bloque de la cadena.

**Subrutinas y Funciones:**
- `generar_bloque`: Genera un nuevo bloque en la cadena de bloques. Calcula el hash de la raíz del árbol de Merkle para los datos almacenados en el bloque.
- `add_block`: Agrega un nuevo bloque a la cadena de bloques. Calcula el hash del bloque basado en su índice, nonce, hash previo y hash de la raíz del árbol de Merkle.
- `imprimir_info`: Imprime la información de un bloque, incluyendo su índice, marca de tiempo, nonce, datos, hashes previos, de la raíz del árbol de Merkle y del bloque.
- `imprimir_cad`: Imprime toda la cadena de bloques, llamando a la función `imprimir_info` para cada bloque.

En resumen, este módulo proporciona una implementación básica de una cadena de bloques que utiliza un árbol de Merkle para garantizar la integridad de los datos almacenados en los bloques. Cada bloque contiene un conjunto de datos y un hash de la raíz del árbol de Merkle que representa la integridad de esos datos.
```fortran
module block_chain
    use routes, only: result_list, result
    use merkle_tree
    use Avl_Tree
    implicit none
    integer :: block_id = 0
    type block 
        integer :: index
        integer :: nonce
        character(:), allocatable :: timestamp
        type(result_list) :: data
        character(len=256) :: previous_hash = '0000'
        character(len=256) :: root_merkle = '0000'
        character(len=256) :: hash = '0000'
        type(block), pointer :: next => null()
        type(Tree_t) :: branches
    contains
        procedure :: generar_bloque
        procedure :: imprimir_info
    end type block
    type chainer
        type(block), pointer :: head => null()
        type(block), pointer :: tail => null()
    contains 
        procedure :: add_block
        procedure :: imprimir_cad
    end type chainer
```

## Conclusión

En resumen, este manual técnico proporciona una visión exhaustiva de los diferentes módulos y estructuras de datos implementados en el proyecto Pixel Print Studio. 
Desde la tabla hash hasta la cadena de bloques, cada componente está diseñado para cumplir funciones específicas dentro del sistema, como la gestión de técnicos, 
sucursales, rutas y la garantía de integridad de los datos.

Se ha empleado el lenguaje de programación Fortran para desarrollar estos módulos, aprovechando sus características y capacidades para manejar eficientemente grandes 
conjuntos de datos y estructuras complejas. La documentación detallada de cada módulo facilita su comprensión y uso por parte de otros desarrolladores, lo que contribuye 
a la escalabilidad y mantenibilidad del proyecto.
