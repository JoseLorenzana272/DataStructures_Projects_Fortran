# Manual de Usuario: Sistema de Simulación para "Pixel Print Studio"

## 1. Introducción

Bienvenido al sistema de simulación para "Pixel Print Studio". Este manual proporciona instrucciones detalladas sobre cómo utilizar la aplicación para gestionar el proceso de recepción y producción de impresiones de pixel art.

## 2. Instalación

El sistema de simulación viene preinstalado y listo para usar. No se requiere ninguna instalación adicional.

## 3. Interfaz de Usuario

La interfaz de usuario se presenta como una interfaz de línea de comandos (CLI), donde el usuario puede interactuar con el sistema a través de comandos simples.

## 4. Funcionalidades Principales

El sistema ofrece las siguientes funcionalidades principales:

### 4.1. Carga Masiva de Clientes

Comando: `cargar_clientes archivo.json`

Descripción: Permite cargar clientes desde un archivo JSON para su procesamiento en el sistema.

### 4.2. Ejecución de Pasos

Comando: `avanzar_paso`

Descripción: Avanza un paso en la simulación, lo que implica atender clientes, producir impresiones y actualizar el estado del sistema.

### 4.3. Estado en Memoria de las Estructuras

Comando: `ver_estado`

Descripción: Muestra el estado actual de las estructuras de datos en memoria, permitiendo al usuario visualizar la distribución de clientes, ventanillas y colas de impresión.

### 4.4. Generación de Reportes

Comando: `generar_reporte`

Descripción: Genera informes detallados sobre los clientes atendidos, las estadísticas de producción de impresiones y otros datos relevantes.

## 5. Uso Básico

Para utilizar el sistema, sigue estos pasos:

- Carga de Clientes: Utiliza el comando `cargar_clientes` seguido del nombre del archivo JSON que contiene la información de los clientes.
- Avance de Pasos: Utiliza el comando `avanzar_paso` para simular el avance del proceso de recepción y producción.
- Ver Estado: Utiliza el comando `ver_estado` para visualizar el estado actual del sistema en memoria.
- Generación de Reportes: Utiliza el comando `generar_reporte` para generar informes sobre el proceso de simulación.

## 6. Ejemplo de Uso

A continuación, se muestra un ejemplo de cómo usar el sistema:

- Carga masiva de clientes desde un archivo JSON:

```fortran
cargar_clientes clientes.json

Avanzar un paso en la simulación:
avanzar_paso

Ver el estado actual del sistema en memoria:
ver_estado

Generar un informe sobre los clientes atendidos:
generar_reporte
```
## 7. Soporte y Contacto
José Daniel Lorenzana Medina, 202206560
