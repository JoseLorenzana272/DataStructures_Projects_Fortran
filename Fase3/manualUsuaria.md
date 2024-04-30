# Manual de Usuario - Pixel print Studio
## José Daniel Lorenzana Medina - 202206560

## Introducción
Este manual proporciona una guía detallada sobre cómo utilizar el programa menu_program. Este programa es una herramienta de gestión diseñada para administrar sucursales, rutas y generar reportes dentro de una organización.

## Requisitos del Sistema
- Este programa está diseñado para ser ejecutado en un entorno que admita el lenguaje de programación Fortran.
- Se requiere un compilador de Fortran compatible para compilar y ejecutar el programa.

## Inicio de Sesión
1. Al iniciar el programa, se solicitarán las credenciales de usuario para iniciar sesión.
2. El usuario debe ingresar un nombre de usuario y una contraseña.
3. La contraseña se encriptará antes de ser verificada.
4. Después de tres intentos fallidos, el programa se cerrará automáticamente por razones de seguridad.

![image](https://github.com/JoseLorenzana272/EDD_PROYECTO1_202206560/assets/122989930/fa328560-1b07-45c7-9a54-aa5d690c48d0)


## Menú Principal
Una vez iniciada sesión con éxito, se mostrará el menú principal con las siguientes opciones:

1. **Carga de archivos:** Permite cargar archivos de sucursales y rutas.
2. **Administrar Sucursales:** Proporciona opciones para administrar las sucursales, como cargar técnicos, generar recorridos óptimos, obtener información sobre técnicos y listar técnicos.
3. **Generar Reportes:** Permite generar diferentes tipos de reportes.
0. **Salir:** Cierra el programa.
```fortran
DO
    WRITE(*,*) "===================================================="
    WRITE(*,*) "                 MENÚ PRINCIPAL"
    WRITE(*,*) "===================================================="
    WRITE(*,*) "1. Carga de archivos"
    WRITE(*,*) "   1.1 Cargar Sucursales"
    WRITE(*,*) "   1.2 Cargar Rutas"
    WRITE(*,*) "2. Administrar Sucursales"
    WRITE(*,*) "3. Generar Reportes"
    WRITE(*,*) "0. Salir"
    WRITE(*,*) "----------------------------------------------------"
    WRITE(*,*) "Ingrese el número correspondiente a su opción: "
    READ(*,*) opcion
            
    SELECT CASE (opcion)
        CASE (1)
            ! Opciones de carga de archivos
        CASE (2)
            ! Opciones de administrar sucursales
        CASE (3)
            ! Opciones de generar reportes
        CASE (0)
            WRITE(*,*) "Saliendo del programa..."
            EXIT
        CASE DEFAULT
            WRITE(*,*) "Opción inválida. Intente nuevamente."
    END SELECT
END DO

```

## Carga de Archivos
1.1 **Cargar Sucursales:** Permite cargar un archivo JSON que contiene información sobre las sucursales, incluyendo ID, departamento, dirección y contraseña encriptada.
1.2 **Cargar Rutas:** Permite cargar un archivo JSON que describe las rutas entre sucursales, incluyendo la distancia y el número de impresoras en mantenimiento en cada ruta.
```fortran
SUBROUTINE subMenu_cargaArchivos()
    INTEGER :: opcion

    WRITE(*,*) "1.1 Sucursales"
    WRITE(*,*) "1.2 Rutas"
    WRITE(*,*) "Ingrese una opción: "
    READ(*,*) opcion

    SELECT CASE (opcion)
        CASE (1)
            ! Cargar Sucursales
        CASE (2)
            ! Cargar Rutas
        CASE DEFAULT
            WRITE(*,*) "Opción inválida."
    END SELECT
END SUBROUTINE subMenu_cargaArchivos

```
## Administrar Sucursales
1. **Carga de técnicos:** Permite cargar un archivo JSON que contiene información sobre los técnicos de una sucursal.
2. **Generar recorrido más óptimo:** Calcula la ruta más eficiente entre dos sucursales, minimizando la distancia recorrida y maximizando la cantidad de impresoras atendidas.
3. **Información Técnico en Específico:** Permite buscar información detallada sobre un técnico específico.
4. **Listar Técnicos:** Muestra una lista de todos los técnicos de la sucursal.
5. **Generar Reportes:** Permite generar diferentes tipos de reportes.
```fortran
WRITE(*,*) "Sucursales seleccionado."
print *, "Ingrese el id de la sucursal: "
read (*,*) id_sucursal
print *, "Ingrese la contraseña de la sucursal: "
read (*,*) sucur_encrypted
contrasena_encriptada = hashPassword(trim(sucur_encrypted))
call avl_sucursales%searchNode(id_sucursal, contrasena_encriptada)
print *, "Bienvenido a la sucursal ", id_sucursal
print *, "Ingrese una opción: "
print *, "1. Carga de tecnicos"
print *, "2. Generar recorrido más óptimo"
print *, "3. Información Técnico en Específico"
print *, "4. Listar Tecnicos"
print *, "5. Generar Reportes"
print *, "6. Regresar"
read (*,*) opcion
SELECT CASE (opcion)
CASE (1)
  ! Código para la opción "Carga de tecnicos"
  print *, "Ingrese la ruta del archivo de tecnicos: "
  read (*,*) filename_json
  call json%initialize()    ! Se inicializa el módulo JSON
  call json%load(filename=filename_json)  ! Se carga el archivo JSON llamado 'config.json'
  !call json%print()         ! Se imprime el contenido del archivo JSON (opcional)
  call json%info('',n_children=size)
  call json%get_core(jsonc)               ! Se obtiene el núcleo JSON para acceder a sus funciones básicas
  call json%get('', tech_pointer, found)
```

## Generar Reportes
En esta opción se generarán diferentes reportes, tales como la gráfica de la ruta, de la Tabla Hash, Árbol de Merckle, entre otros.
*Ejemplo de una de las estructuras que se graficaran para el reporte:

![image](https://github.com/JoseLorenzana272/EDD_PROYECTO1_202206560/assets/122989930/a1f30b86-4e6d-4938-a73c-e25a3baf8c6e)


## Salir
Permite salir del programa y finalizar la sesión actual.
```fortran
CASE (0)
    WRITE(*,*) "Saliendo del programa..."
    EXIT
```
