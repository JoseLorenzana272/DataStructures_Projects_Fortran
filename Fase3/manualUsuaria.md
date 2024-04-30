# Manual de Usuario - Pixel Print Studio
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

## Menú Principal
Una vez iniciada sesión con éxito, se mostrará el menú principal con las siguientes opciones:
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


1. **Carga de archivos:** Permite cargar archivos de sucursales y rutas.
## Carga de Archivos
1.1 **Cargar Sucursales:** Para cargar las sucursales, el usuario debe proporcionar un archivo JSON que contenga la información relevante de cada sucursal. Este archivo debe incluir campos como ID, departamento, dirección y contraseña encriptada. El programa leerá este archivo y almacenará la información en la estructura de datos correspondiente para su posterior manipulación.
```fortran
WRITE(*,*) "Carga de sucursales seleccionada."
            print *, "Ingrese la ruta del archivo de sucursales: "
                read (*,*) filename_json
                call json%initialize()
                call json%load(filename=filename_json)
                !call json%print()
                call json%info('', n_children=size)
                call json%get_core(jsonc)
                call json%get('', sucur_pointer, found)

                do i = 1, size
                    call jsonc%get_child(sucur_pointer, i, deptPointer, found=found)


                    call jsonc%get_child(deptPointer, 'id', partPointer, found=found)
                    if (found) then
                        call jsonc%get(partPointer, id_sucursal)
                        print *, 'ID: ', id_sucursal
                    end if
                    
                    call jsonc%get_child(deptPointer, 'departamento', partPointer, found=found)
                    if (found) then
                        call jsonc%get(partPointer, departamento)
                        print *, 'Departamento: ', departamento
                    end if
                    
                    call jsonc%get_child(deptPointer, 'direccion', partPointer, found=found)
                    if (found) then
                        call jsonc%get(partPointer, direccion)
                        print *, 'Dirección: ', direccion
                    end if
                    
                    call jsonc%get_child(deptPointer, 'password', partPointer, found=found)
                    if (found) then
                        
                        call jsonc%get(partPointer, password)
                        print *, 'Contraseña: ', password
                        contrasena_encriptada = hashPassword(password)
                    end if
                    call avl_sucursales%insert_node(id_sucursal, departamento, direccion, contrasena_encriptada)
                end do
                call avl_sucursales%graph_avl_tree()
                call json%destroy()

```

1.2 **Cargar Rutas:** Para cargar las rutas entre sucursales, el usuario debe proporcionar un archivo JSON que describa las conexiones entre las sucursales, incluyendo la distancia y el número de impresoras en mantenimiento en cada ruta. Este archivo se procesará para generar un grafo de rutas que se utilizará para calcular recorridos óptimos.
```fortran
WRITE(*,*) "Carga de rutas seleccionada."
            print *, "Ingrese la ruta del archivo de rutas: "
            read (*,*) filename_const

            ! Inicializar y cargar el JSON desde el archivo
            call json%initialize()
            call json%load(filename=filename_const)
            !call json%print()
            call json%info('grafo', n_children=size) ! Aquí especificamos 'grafo' como la clave del arreglo
            call json%get('grafo', grafo_pointer, found) ! Obtenemos el puntero al arreglo 'grafo'

            do i = 1, size
                call jsonc%get_child(grafo_pointer, i, edgePointer, found=found) ! Obtenemos el i-ésimo elemento del arreglo 'grafo'

                call jsonc%get_child(edgePointer, 's1', partPointer, found=found)
                if (found) then
                    call jsonc%get(partPointer, s1)
                    print *, 'Sucursal 1: ', s1
                end if

                call jsonc%get_child(edgePointer, 's2', partPointer, found=found)
                if (found) then
                    call jsonc%get(partPointer, s2)
                    print *, 'Sucursal 2: ', s2
                end if

                call jsonc%get_child(edgePointer, 'distancia', partPointer, found=found)
                if (found) then
                    call jsonc%get(partPointer, distancia)
                    print *, 'Distancia: ', distancia
                end if

                call jsonc%get_child(edgePointer, 'imp_mantenimiento', partPointer, found=found)
                if (found) then
                    call jsonc%get(partPointer, imp_mantenimiento)
                    print *, 'Impresoras en Mantenimiento: ', imp_mantenimiento
                    print *, "--------------------------------------------------"
                end if
                call graphRutas%insert_data(s1,s2,distancia, imp_mantenimiento)
                call graphImpresoras%insert_data_2(s1,s2,imp_mantenimiento, distancia)
            end do
            print *, "MOSTRANDO RUTAS ---------------------"
            call graphRutas%show()
            print *, "-------------------------------"
            call analizador%set_graph(graphRutas)
            call analizador_impresoras%set_graph(graphImpresoras)
            call graphRutas%graficar('grafo_route.dot', "Grafo de Rutas")
            call json%destroy()
```

2. **Administrar Sucursales:** Proporciona opciones para administrar las sucursales, como cargar técnicos, generar recorridos óptimos, obtener información sobre técnicos y listar técnicos.
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
```
3. **Generar Reportes:** Permite generar diferentes tipos de reportes.

## Carga de Archivos
1.1 **Cargar Sucursales:** Permite cargar un archivo JSON que contiene información sobre las sucursales, incluyendo ID, departamento, dirección y contraseña encriptada.
1.2 **Cargar Rutas:** Permite cargar un archivo JSON que describe las rutas entre sucursales, incluyendo la distancia y el número de impresoras en mantenimiento en cada ruta.

## Administrar Sucursales
1. **Carga de técnicos:** Permite cargar un archivo JSON que contiene información sobre los técnicos de una sucursal.
2. **Generar recorrido más óptimo:** Calcula la ruta más eficiente entre dos sucursales, minimizando la distancia recorrida y maximizando la cantidad de impresoras atendidas.
3. **Información Técnico en Específico:** Permite buscar información detallada sobre un técnico específico.
4. **Listar Técnicos:** Muestra una lista de todos los técnicos de la sucursal.
5. **Generar Reportes:** Permite generar diferentes tipos de reportes.
```fortran
WRITE(*,*) ".............................................................."
                                WRITE(*,*) "          Ruta que Minimiza la Distancia Recorrida"
                                WRITE(*,*) "(con cantidad de impresoras a las que se le dio mantenimiento)"
                                WRITE(*,*) ".............................................................."
                                print *, "El peso total (minimizar distancia) del recorrido es: ", resultado_grafo !perdida
                                print *, "El total de las impresas en mantenimiento es: ", resultado_nodos%total_printers !Ganancia
                                diferencia_ruta = (resultado_nodos%total_printers*100)-resultado_grafo
                                print *, 'Diferencia entre ganancia y perdida (Minimizacion): ', diferencia_ruta
                                print *, '----------------------'
                                call resultado_impresoras%print()
                                resultado_impresorasMantenimiento = resultado_impresoras%total_weight*100
                                WRITE(*,*) ".............................................................."
                                WRITE(*,*) "Ruta que Maximice la Cantidad de Impresoras a las que se le dio Mantenimiento"
                                WRITE(*,*) "           (con la distancia que se recorrió al seguir esa ruta)             "
                                WRITE(*,*) ".............................................................."
                                print *, "El costo total del recorrido con impresoras es: ", &
                                resultado_impresorasMantenimiento
                                print *, "La la distancia que se recorrió al seguir esa ruta: ", &
                                resultado_impresoras%total_printers
                                diferencia_impresoras = resultado_impresorasMantenimiento&
                                -(resultado_impresoras%total_printers*80)
                                print *, 'Diferencia entre ganancia y perdida (Maximizacion): ', diferencia_impresoras
                                print *, '------------------------------------------------------------------'
```
## Generar Reportes
Permite graficar diferentes estructuras de datos, como la tabla Hash, árbol Merckle, Grafo durigido de rutas, etc.
Ejemplo de tabla Hash:

![image](https://github.com/JoseLorenzana272/EDD_PROYECTO1_202206560/assets/122989930/a92cdec0-f131-4642-9985-ef86500fd476)

## Salir
Permite salir del programa y finalizar la sesión actual.
```fortran
CASE (0)
    WRITE(*,*) "Saliendo del programa..."
    EXIT
```
