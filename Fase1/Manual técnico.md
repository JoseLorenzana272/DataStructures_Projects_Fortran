# Manual Técnico: Sistema de Simulación para "Pixel Print Studio"
##### José Daniel Lorenzana Medina - 202206560 - FASE 1 Estructura de Datos
## 1. Introducción
Este manual técnico proporciona una guía detallada sobre el diseño, la implementación y el funcionamiento del sistema de simulación desarrollado para "Pixel Print Studio". El sistema está diseñado para optimizar los procesos de recepción y producción de impresiones de pixel art, utilizando estructuras de datos y algoritmos eficientes.

## 2. Descripción General
El sistema simula el proceso completo, desde que los clientes realizan sus solicitudes de impresión hasta que reciben sus impresiones. Utiliza una interfaz de línea de comandos (CLI) para interactuar con el usuario y ofrece varias funcionalidades clave, como la carga masiva de clientes, la gestión de ventanillas de atención, la producción de impresiones y la generación de informes.


## 3. Estructuras de Datos Utilizadas
El sistema utiliza las siguientes estructuras de datos para gestionar el flujo de clientes y la producción de impresiones:

### 3.1. Cola de Recepción
- **Descripción:** Almacena a los clientes que llegan a la empresa y esperan ser atendidos en una estación de recepción.
- **Implementación:** Implementada como una cola simple.
- **Funcionalidades:** Permite la carga masiva de clientes y la recepción de clientes aleatorios en cada paso de simulación.
```fortran
type :: cola_recepcion
    type(nodo_cola_recepcion), pointer :: frente => null()
    type(nodo_cola_recepcion), pointer :: final => null()
end type cola_recepcion
```
### 3.2. Lista de Ventanillas
- **Descripción:** Representa las estaciones de atención donde se atienden a los clientes.
- **Implementación:** Implementada como una lista simplemente enlazada.
- **Funcionalidades:** Permite definir y gestionar el número de ventanillas disponibles.
```fortran
type :: lista_ventanillas
    type(ventanilla), pointer :: head => null()
contains
    procedure :: crear_ventanillas
    ! Otros procedimientos...
end type lista_ventanillas
```
### 3.3. Pila de Imágenes
- **Descripción:** Almacena las imágenes que un cliente desea imprimir en una ventanilla.
- **Implementación:** Implementada como una pila en cada nodo de la lista de ventanillas.
- **Funcionalidades:** Permite recibir y almacenar imágenes durante el proceso de atención al cliente.
```fortran
type :: pila_imagenes
    type(imagen), pointer :: head => null()
contains
    procedure :: push
    ! Otros procedimientos...
end type pila_imagenes

```

### 3.4. Lista de Clientes Atendidos
- **Descripción:** Registra a los clientes que han sido atendidos por las ventanillas.
- **Implementación:** Implementada como una lista simplemente enlazada.
- **Funcionalidades:** Almacena información sobre el cliente, la ventanilla que lo atendió, el número de imágenes impresas y el tiempo total en el sistema.
``` fortran
type :: lista_clientes_atendidos
    type(cliente_atendido), pointer :: head => null()
contains
    procedure :: agregar_cliente
    ! Otros procedimientos...
end type lista_clientes_atendidos

```

### 3.5. Cola de Impresión
- **Descripción:** Gestiona la producción de impresiones, clasificando las imágenes según su tipo (pequeñas o grandes).
- **Implementación:** Implementada como una cola simple para cada tipo de imagen.
- **Funcionalidades:** Recibe imágenes de las ventanillas y las encola para su impresión en las impresoras correspondientes.
``` fortran
type :: cola_impresion
    type(nodo_cola_impresion), pointer :: frente => null()
    type(nodo_cola_impresion), pointer :: final => null()
end type cola_impresion

```

### 3.6. Lista de Clientes en Espera
- **Descripción:** Almacena a los clientes que han sido atendidos en las ventanillas y esperan la producción de sus impresiones.
- **Implementación:** Implementada como una lista circular doblemente enlazada, donde cada nodo contiene una lista de imágenes impresas.
- **Funcionalidades:** Administra la espera de los clientes y almacena las imágenes impresas asociadas a cada cliente.
``` fortran
type :: lista_clientes_espera
    type(cliente_espera), pointer :: head => null()
contains
    procedure :: append_cliente_espera
    ! Otros procedimientos...
end type lista_clientes_espera

```

## 4. Funcionamiento del Sistema
El sistema sigue un flujo de operaciones definido para simular el proceso de atención a los clientes y la producción de impresiones. A continuación se describe el funcionamiento general:

### 4.1. Carga Masiva de Clientes
- El usuario tiene la opción de cargar clientes desde un archivo JSON al iniciar la aplicación.
- Los clientes cargados inicialmente se añaden a la cola de recepción.
- Además, se generan clientes aleatorios en cada paso de simulación, con cantidades aleatorias de imágenes y nombres aleatorios.

### 4.2. Ejecución de Pasos
- El usuario puede avanzar un paso de simulación, durante el cual se atienden clientes y se producen impresiones.
- En cada paso, se asignan clientes a las ventanillas disponibles y se realizan las operaciones de impresión correspondientes.
- Se registran los clientes atendidos y se actualizan las estructuras de datos según sea necesario.

### 4.3. Estado en Memoria de las Estructuras
- El usuario puede visualizar el estado actual de las estructuras de datos en memoria mediante gráficos generados con Graphviz.
- Los gráficos muestran la distribución y relación entre las diferentes estructuras, como las ventanillas, las colas de impresión y las listas de clientes.

### 4.4. Generación de Reportes
- El usuario puede generar informes sobre los clientes atendidos, como los clientes con más imágenes grandes o pequeñas.
- Se generan gráficos que muestran estadísticas relevantes sobre la producción de impresiones y el tiempo de espera de los clientes.

## 5. Flujo de la Aplicación
El sistema sigue un flujo de ejecución definido, donde se realizan las siguientes acciones en cada paso de simulación:

1. Los clientes llegan a la empresa y se colocan en la cola de recepción.
2. Los clientes son asignados a las ventanillas disponibles para su atención.
3. Las ventanillas atienden a los clientes, producen las impresiones y las envían a las colas de impresión correspondientes.
4. Los clientes esperan en la lista de espera hasta que todas sus impresiones estén listas.
5. Una vez completadas las impresiones, los clientes salen de la empresa y se registra su tiempo total en el sistema.

## 6. Conclusiones
El sistema de simulación desarrollado para "Pixel Print Studio" ofrece una solución eficiente para gestionar el flujo de clientes y la producción de impresiones. Utilizando estructuras de datos adecuadas y algoritmos eficientes, el sistema optimiza los procesos operativos y mejora la eficiencia global de la empresa.
