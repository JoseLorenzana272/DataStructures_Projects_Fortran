# Manual Técnico - PixelPrint Studio
## José Daniel Lorenzana Medina - 202206560
### Estructura de Datos

# Inicio de Sesión:

- El código comienza con un bucle principal que solicita al usuario ingresar su nombre de usuario y contraseña.
- Se proporciona una interfaz de bienvenida a través de impresiones de texto.
- Si las credenciales coinciden con las predeterminadas, se inicia sesión como administrador y se muestra un menú de opciones administrativas.
- Si las credenciales no coinciden con las predeterminadas, se verifica su existencia en la estructura de datos. Si son válidas, se inicia sesión como usuario normal y se muestra un menú de opciones para ellos.
- Se manejan los casos de credenciales incorrectas con la opción de volver a intentar o salir del programa.

## Menú Principal:

### Para el usuario administrador:
- Se muestra un menú con opciones para ingresar clientes, registrar clientes, eliminar clientes, visualizar un árbol B de clientes, buscar clientes, o cerrar sesión.
- Cada opción ejecuta una lógica específica que involucra la interacción con estructuras de datos como árboles y la manipulación de datos de clientes.
```fortran
if (is_admin) then
                    do
                        print *, 'Seleccione una opción:'
                        print *, '1. Ingresar clientes'
                        print *, '2. Registrar Clientes'
                        print *, '3. Eliminar Clientes'
                        print *, '4. Visualizar Arbol B de Clientes'
                        print *, '5. Busqueda de Clientes'
                        print *, '6. Cerrar Sesión'
                        print *, ''

                        read(*, *) option

                        select case(option)
                            case(1)
                                ! Lógica para la tarea 1 (admin)
                                print *, 'Ingresar clientes'
                                print *, 'Ingrese la ruta del archivo JSON de clientes: '
```


### Para el usuario normal:
- Se muestra un menú con opciones para cargar capas, cargar imágenes, cargar álbumes, visualizar estructuras de datos, crear y eliminar imágenes, generar reportes o cerrar sesión.
- Cada opción ejecuta lógica relacionada con el manejo de imágenes y capas, utilizando estructuras de datos como árboles y matrices dispersas para almacenar información relevante.
- Se manejan las opciones inválidas con mensajes adecuados para informar al usuario sobre su elección incorrecta.
```fortran
else if (btree_cosa%search_user(username, password)) then
            temporal_client = btree_cosa%search_username(username)
                print *, 'Bienvenido ', temporal_client%user_name
                bst_tree = temporal_client%bst_user
                avl_tree = temporal_client%avl_user
                albumes = temporal_client%album_user

                do
                    print *, 'Seleccione una opción:'
                    print *, '1. Cargar capas'
                    print *, '2. Cargar Imágenes'
                    print *, '3. Cargar Álbumes'
                    print *, '4. Visualizar Estructuras de Datos'
                    print *, '5. Crear y eliminar imagenes'
                    print *, '6. Generar reportes'
                    print *, '7. Cerrar Sesión'
                    print *, ''

                    read(*, *) option

                    select case(option)
```


## Estructuras de Datos Utilizadas:

- El código hace uso de estructuras de datos como árboles binarios de búsqueda (BST), árboles AVL y listas para gestionar clientes, imágenes, capas y álbumes.
- Se emplea una matriz dispersa para almacenar información sobre los píxeles de las capas de las imágenes.
- Cada operación en el menú principal involucra manipulación de estas estructuras, como agregar, eliminar, buscar o visualizar datos.

## Manejo de Errores:

- Se incluye manejo de errores para credenciales incorrectas y opciones inválidas.
- Se proporciona al usuario la oportunidad de volver a intentar ingresar credenciales en caso de error.

## Cierre de Sesión:

- Se ofrece la opción de cerrar sesión en cualquier momento, permitiendo una salida ordenada del programa.

En resumen, el código implementa un sistema completo de inicio de sesión y menú principal para una aplicación de gestión de imágenes, con capacidades administrativas y de usuario normal, utilizando diversas estructuras de datos y proporcionando una experiencia de usuario intuitiva y segura.

# Módulo de Matrices Dispersas

### Definición de tipos de datos:

- Se define un tipo `node_val` para representar los valores de los nodos de la matriz dispersa, que incluye un indicador de existencia y un campo para el color.
- Se define un tipo `node_matrix` para representar los nodos individuales de la matriz dispersa, que incluye coordenadas (i, j), color y punteros a los nodos vecinos.
- Se define un tipo `matrix` que contiene un puntero a la raíz de la matriz dispersa y dimensiones (ancho y alto), además de procedimientos asociados para manipular la matriz.

```fortran
module matrix_m_sparse
    implicit none
    private

    type :: node_val
        private
        logical :: exists = .false.
        character(len=7) :: color
    end type node_val

    type, public :: node_matrix

        integer :: i, j
        character(len=7) :: color
        type(node_matrix), pointer :: up => null()
        type(node_matrix), pointer :: down => null()
        type(node_matrix), pointer :: right => null()
        type(node_matrix), pointer :: left => null()
    end type node_matrix

    type, public :: matrix
        
        type(node_matrix), pointer :: root => null()
        integer :: width = 0
        integer :: height = 0
```

### Operaciones sobre la matriz:

- Se proporcionan procedimientos para insertar elementos en la matriz, tanto individualmente como por fila o columna.
- Se implementan funciones para buscar filas y columnas específicas en la matriz.
- Se incluye una función para verificar si un nodo ya existe en la matriz dispersa.

### Impresión y visualización:

- Se implementa un procedimiento para imprimir la matriz dispersa en la consola, mostrando las filas y columnas con sus respectivos valores.
- Se proporciona un procedimiento para imprimir los encabezados de columna.
- Se incluyen procedimientos para generar visualizaciones de la matriz en formato DOT (Graphviz) y HTML, con colores para representar los valores de los nodos.

### Limpieza y gestión de memoria:

- Se incluye un procedimiento para limpiar la matriz dispersa, liberando la memoria asignada a los nodos y reiniciando las dimensiones de la matriz.

En resumen, el módulo de matriz dispersa proporciona una estructura de datos eficiente para representar matrices con valores dispersos, junto con una variedad de operaciones para insertar, buscar y visualizar elementos en la matriz. Además, se ofrece la capacidad de generar representaciones visuales de la matriz en formato DOT y HTML para facilitar la comprensión y el análisis de los datos.

# Módulo de Árbol BST para Capas

### Tipos de Datos:
- **node:** Representa un nodo del árbol. Contiene un identificador único (uid), una matriz dispersa (newMatrix), y punteros a los nodos hijo izquierdo y derecho.
- **bst:** Representa el árbol binario de búsqueda. Contiene un puntero a la raíz del árbol y procedimientos para agregar nodos, recorrer el árbol en diferentes órdenes, generar representaciones gráficas del árbol, entre otros.

### Procedimientos Principales:
- **add:** Agrega un nuevo nodo al árbol.
- **add_matrix:** Agrega un nuevo nodo al árbol junto con una matriz dispersa.
- **preorder, inorder, postorder:** Realizan recorridos en preorden, inorden y postorden del árbol, respectivamente.
- **preorder_limit, inorder_limit, postorder_limit:** Realizan recorridos limitados en preorden, inorden y postorden hasta cierto límite de profundidad.
- **dotgen:** Genera una representación gráfica del árbol en formato DOT y crea un archivo de imagen.
- **buscarNodo_r:** Busca un nodo con un identificador específico y devuelve su matriz dispersa asociada.
- **search_amplitude:** Realiza un recorrido por niveles en el árbol.
- **calcularProfundidad:** Calcula la profundidad del árbol.
- **imprimirHojas:** Imprime las hojas del árbol.
- **graficarCapa:** Genera una representación gráfica de una capa específica del árbol.

### Procedimientos Auxiliares:
Además, hay otros procedimientos auxiliares para realizar operaciones específicas en el árbol, como la inicialización del árbol, la búsqueda de nodos de manera recursiva, y la generación de imágenes de capas específicas.

En resumen, este módulo proporciona una implementación completa de un árbol binario de búsqueda en Fortran, junto con varias funcionalidades para trabajar con él, incluyendo la búsqueda, la impresión de nodos, la generación de representaciones gráficas, entre otros.

# Módulo de Árbol AVL de imágenes

### Definición de tipos:

- **node_avl:** Define un nodo de un árbol AVL que contiene un valor entero, una referencia a un árbol binario de búsqueda (BST), la altura del nodo y punteros a los nodos hijo izquierdo y derecho.
- **avl:** Define el tipo de estructura AVL que contiene un puntero a la raíz del árbol AVL y procedimientos asociados.
```fortran
module avldef_tree
    use bstdef_t
    implicit none
    
    ! Definición de tipos
    type :: node_avl
        integer :: value
        type(bst) :: bst_tree
        integer :: height
        type(node_avl), pointer :: left => null()
        type(node_avl), pointer :: right => null()
    end type node_avl
    
    type :: avl
        type(node_avl), pointer :: root => null()
```

### Procedimientos Principales:

- **add_avl:** Agrega un valor al árbol AVL.
- **add_avl_tree:** Agrega un valor al árbol AVL junto con un árbol binario de búsqueda asociado.
- **add_rec_avl:** Procedimiento recursivo para agregar un valor al árbol AVL.
- **add_rec_avl_bst:** Procedimiento recursivo para agregar un valor al árbol AVL junto con un árbol binario de búsqueda asociado.
- **getheight:** Función para obtener la altura de un nodo AVL.

### Rotaciones AVL:

- **srl:** Rotación simple a la izquierda.
- **srr:** Rotación simple a la derecha.
- **drl:** Rotación doble a la izquierda.
- **drr:** Rotación doble a la derecha.

### Otros Procedimientos de Recorrido:

- **preorder_avl, inorder_avl, postorder_avl:** Realizan recorridos en preorden, inorden y postorden del árbol AVL, respectivamente.
- **dotgen_avl:** Genera un archivo DOT y una imagen PNG del árbol AVL.

### Procedimientos Relacionados con Imágenes:

- **add_layer_to_image:** Agrega una capa a una imagen asociada en el árbol AVL.
- **delete_image:** Elimina una imagen del árbol AVL.
- **find_image_node:** Encuentra un nodo de imagen en el árbol AVL.
- **search_image_node:** Busca un nodo de imagen en el árbol AVL de manera recursiva.

### Procedimientos Relacionados con la Generación de Gráficos:

- **dotgen_avl_bst:** Genera un archivo DOT y una imagen PNG del árbol AVL junto con un árbol binario de búsqueda asociado.
- **dotgen_rec_avl_bst:** Procedimiento recursivo para generar un archivo DOT del árbol AVL junto con un árbol binario de búsqueda asociado.

Este módulo proporciona una implementación completa de un árbol AVL en Fortran, con todas las operaciones básicas necesarias, como inserción, eliminación y búsqueda, así como procedimientos adicionales para visualización y manipulación de árboles AVL con árboles binarios de búsqueda asociados.

# Módulo de Álbumes


### Definición de tipos:

- **node_image:** Define un nodo que contiene la identificación de una imagen y un puntero al siguiente nodo.
- **node_album:** Define un nodo que representa un álbum, con un nombre, el número de imágenes en el álbum, punteros al siguiente y al anterior álbum, y un puntero a la lista de imágenes en el álbum.
- **album:** Define la estructura del álbum que contiene punteros a la cabeza y a la cola de la lista de álbumes.
```fortran
module AlbumModule
    implicit none

    type node_image
        integer :: id
        type(node_image), pointer :: next => null()
    end type node_image

    type node_album
        character(len=:), allocatable :: name
        integer :: images
        type(node_album), pointer :: next => null()
        type(node_image), pointer :: image_list => null()
        type(node_album), pointer :: prev => null()
    end type node_album

    type album
        type(node_album), pointer :: head => null()
        type(node_album), pointer :: tail => null()
    contains
        procedure :: add_album
        procedure :: add_image
        procedure :: generate_dot_file
        procedure :: count_albums
    end type album
```

### Procedimientos Principales:

- **add_album:** Agrega un nuevo álbum a la lista de álbumes.
- **add_image:** Agrega una imagen a un álbum específico.
- **generate_dot_file:** Genera un archivo DOT y una imagen PNG que representan los álbumes y sus imágenes en un formato gráfico.
- **count_albums:** Cuenta el número total de álbumes en la lista.
- **int2str:** Convierte un entero en una cadena de caracteres.

El módulo proporciona una manera de organizar álbumes y sus imágenes en una estructura de datos enlazada y también ofrece la capacidad de visualizar esta estructura utilizando la herramienta Graphviz para generar gráficos.

# Módulo de árbol B de usuarios

### Definición de tipos:

- **nodeptr:** Define un tipo que contiene un puntero a un nodo del árbol B.
- **clientes:** Define una estructura para almacenar la información del cliente, incluyendo DPI, nombre de usuario, contraseña, así como punteros a las estructuras de árbol binario de búsqueda (BST), árbol AVL y álbumes definidos en el módulo AlbumModule.
- **BTreeNode:** Define un nodo del árbol B que contiene una matriz de valores de clientes, un número de elementos en el nodo y una matriz de enlaces a otros nodos.
- **Btree_class:** Define la clase principal que contiene un puntero a la raíz del árbol B.
```fortran
module BTree
    use bstdef_t
    use avldef_tree
    use AlbumModule
    implicit none

      ! Order 5
    integer, parameter :: MAXI = 4, MINI = 2

    type nodeptr
        type (BTreeNode), pointer :: ptr => null()
    end type nodeptr

    type clientes
        integer(8) :: dpi
        character(len=:), allocatable :: user_name
        character(len=:), allocatable :: password_client
        type(bst) :: bst_user
        type(avl) :: avl_user
        type(album) :: album_user
        integer :: layer_counter = 0
        integer :: image_counter = 0
    end type clientes

    type BTreeNode
        type(clientes) :: val(0:MAXI+1)
        integer :: num = 0
        type(nodeptr) :: link(0:MAXI+1)

    end type BTreeNode

    type Btree_class
        type(BTreeNode), pointer :: root => null()
```

### Procedimientos Principales:

- **insert:** Inserta un nuevo cliente en el árbol B.
- **setValue:** Función recursiva para establecer el valor de un cliente en un nodo del árbol B.
- **insertNode:** Inserta un cliente en un nodo del árbol B.
- **splitNode:** Divide un nodo del árbol B si está lleno.
- **createNode:** Crea un nuevo nodo del árbol B.
- **traversal:** Recorre el árbol B e imprime sus valores.
- **dotgen_btree:** Genera un archivo DOT y una imagen PNG que representan el árbol B en un formato gráfico.
- **search_user:** Busca un cliente por nombre de usuario y contraseña.
- **register_clients:** Registra nuevos clientes en el árbol B.
- **delete_client:** Elimina un cliente del árbol B por su DPI.
- **search_client_by_dpi:** Busca un cliente por su DPI y muestra su información, incluyendo el número de capas y el número de imágenes en sus álbumes.
- **search_username:** Busca un cliente por su nombre de usuario.
- **search_trees:** Actualiza las estructuras asociadas con un cliente específico, como el BST, AVL y álbumes.

Este módulo proporciona una implementación completa de un árbol B para gestionar clientes, incluyendo operaciones de inserción, búsqueda y eliminación, así como la capacidad de generar visualizaciones gráficas del árbol.

De esta manera finaliza el Manual Técnico para el programa de PixelPrint studio.
