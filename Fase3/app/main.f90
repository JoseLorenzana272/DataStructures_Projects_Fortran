PROGRAM menu_program
    use json_module
    use hash_table
    use Avl_Tree
    use PasswordEncryptor
    use routes
    use block_chain
    IMPLICIT NONE
    INTEGER :: opcion
    type(HashTable) :: table
    integer, parameter :: long = selected_int_kind(4)
    character(len=200) :: filename_json, filename_const
    character(:), allocatable :: name, lastname, genre
    character(:), allocatable :: address, departamento, direccion, password, contrasena_encriptada
    integer ::  i, size, id_sucursal, s1, s2, distancia, imp_mantenimiento, destiny_brach
    integer*8 ::  dpi, phone
    character(:), allocatable ::  dpi_value, phone_value
    type(json_value), pointer :: tech_pointer, personPointer, attributePointer, &
    sucur_pointer, deptPointer, partPointer, grafo_pointer, edgePointer
    type(json_file) :: json
    type(json_core) :: jsonc
    logical :: found
    type(Tree_t) :: avl_sucursales
    type(graph) :: graphRutas, graphImpresoras
    type(result_list) :: resultado_nodos, resultado_impresoras
    type(analyzer) :: analizador, analizador_impresoras
    integer :: resultado_grafo, resultado_impresorasMantenimiento, diferencia_ruta, diferencia_impresoras
    character(len=20) :: username, contrasena_inicio
    character(len=32) :: sucur_encrypted
    integer :: login_attempts
    logical :: login_success
    type(block) :: blockChain_Merckle

    username = 'hola'
    contrasena_inicio = '1234'
    login_attempts = 0
    login_success = .FALSE.

    DO WHILE (.NOT. login_success)
        ! Solicitar inicio de sesión
        PRINT *, "Por favor, ingrese sus credenciales para iniciar sesión."
        PRINT *, "Nombre de usuario: "
        READ(*,*) username
        PRINT *, "Contraseña: "
        READ(*,*) contrasena_inicio
        contrasena_encriptada = hashPassword(contrasena_inicio)
        PRINT *, "Contraseña encriptada: ", contrasena_encriptada

        ! Verificar credenciales
        IF (username == 'hola' .AND. contrasena_inicio == '123') THEN
            login_success = .TRUE.
            PRINT *, "Inicio de sesión exitoso."
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
                        WRITE(*,*) "Carga de archivos seleccionada."
                        CALL subMenu_cargaArchivos()
                    CASE (2)
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
            
                            do i=1, size
                                call jsonc%get_child(tech_pointer, i, personPointer, found=found)
            
                                call jsonc%get_child(personPointer, 'dpi', attributePointer, found=found)
                                if (found) then
                                    call jsonc%get(attributePointer, dpi_value)
                                    print *, 'DPI: ', dpi_value
                                    read(dpi_value,'(I13)') dpi
                                    
                                end if
            
                                call jsonc%get_child(personPointer, 'nombre', attributePointer, found=found)
                                if (found) then
                                    call jsonc%get(attributePointer, name)
                                    print *, 'Nombre: ', name
                                end if
            
                                call jsonc%get_child(personPointer, 'apellido', attributePointer, found=found)
                                if (found) then
                                    call jsonc%get(attributePointer, lastname)
                                    print *, 'Apellido: ', lastname
                                end if
            
                                call jsonc%get_child(personPointer, 'genero', attributePointer, found=found)
                                if (found) then
                                    call jsonc%get(attributePointer, genre)
                                    print *, 'Genero: ', genre
                                end if
            
                                call jsonc%get_child(personPointer, 'direccion', attributePointer, found=found)
                                if (found) then
                                    call jsonc%get(attributePointer, address)
                                    print *, 'Direccion: ', address
                                end if
            
                                call jsonc%get_child(personPointer, 'telefono', attributePointer, found=found)
                                if (found) then
                                    call jsonc%get(attributePointer, phone_value)
                                    print *, 'Telefono: ', phone_value
                                    read(phone_value,'(I13)') phone
                                end if
                                call table%insert(dpi, name, lastname, address, phone)
                            end do
                                call table%print()
                                call avl_sucursales%searchNode_hash(id_sucursal, table)
                                call table%grafico("hash.dot")
            
                                call json%destroy()
                            CASE (2)
                                ! Código para la opción "Generar recorrido más óptimo"
                                print *, "Generar recorrido más óptimo"
                                ! Sucursal de inicio es cuando se inicia sesión con Sucursal
                                print *, "Ingrese la sucursal de destino: "
                                read (*,*) destiny_brach
                                print *, '------------------------------------------------------------------'
                                resultado_nodos = analizador%shortest_path(id_sucursal, destiny_brach)
                                resultado_impresoras = analizador_impresoras%longest_path(id_sucursal, destiny_brach)
                                call resultado_nodos%print()
                                resultado_grafo = resultado_nodos%total_weight*80
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
                                if (diferencia_ruta > diferencia_impresoras) then
                                    print *, 'RUTA SUGERIDA'
                                    print *, "La mejor opción es minimizar la distancia recorrida"
                                    print *, 'El costo total de la ruta es: ', resultado_grafo
                                    print *, 'La ganancia total de la ruta (impresoras atendidas) es: ', &
                                    resultado_nodos%total_printers
                                    call blockChain_Merckle%generar_bloque(resultado_nodos, avl_sucursales, .false.)
                                    print *, '------------------------------------------------------------------'
            
                                else if (diferencia_ruta < diferencia_impresoras) then
                                    print *, 'RUTA SUGERIDA'
                                    print *, "La mejor opción es maximizar la cantidad de impresoras a las que &
                                    se les dio mantenimiento"
                                    print *, 'El costo total de la ruta es: ', resultado_impresorasMantenimiento
                                    print *, 'La ganancia total de la ruta (impresoras atendidas) es: ', &
                                    resultado_impresoras%total_printers
                                    call blockChain_Merckle%generar_bloque(resultado_impresoras, avl_sucursales, .true.)
                                    print *, '------------------------------------------------------------------'
                                else
                                    print *, 'RUTA SUGERIDA'
                                    print *, "Ambas opciones son igual de buenas (Tanto en minimizar la &
                                    distancia como en maximizar la &
                                    cantidad de impresoras a las que se les dio mantenimiento)"
                                    print *, 'El costo total de la ruta es: ', resultado_grafo
                                    print *, 'La ganancia total de la ruta (impresoras atendidas) es: ', &
                                    resultado_nodos%total_printers
                                    call blockChain_Merckle%generar_bloque(resultado_nodos, avl_sucursales, .false.)
                                    print *, '------------------------------------------------------------------'
                                end if
                                print *, "Ingrese el DPI del técnico a quien se le asignará la ruta: "
                                read (*,*) dpi
                                print *, "El tecnico asignado es: "
                                call table%search_tecnico(dpi)
                                call graphRutas%reset_grafo()
                                call graphImpresoras%reset_grafo()
                                call recargar_rutas()
                            CASE (3)
                                ! Código para la opción "Información Técnico en Específico"
                                print *, "Ingrese el DPI del técnico: "
                                read (*,*) dpi
                                call table%search_tecnico(dpi)
                            CASE (4)
                                ! Código para la opción "Listar Tecnicos"
                                print *, "Listar Tecnicos"
                                call avl_sucursales%searchNode_listar(id_sucursal)
                            CASE (5)
                                ! Código para la opción "Generar Reporte"
                                print *, "Generar Reporte"
                            CASE (6)
                                ! Código para la opción "Regresar al menu inicial"
                                print *, "Regresar al menu inicial"
            
                            CASE DEFAULT
                                PRINT *, "Opción no válida"
                            END SELECT
            
                    CASE (3)
                        WRITE(*,*) "Reportes seleccionado."
                    CASE (0)
                        WRITE(*,*) "Saliendo del programa..."
                        EXIT
                    CASE DEFAULT
                        WRITE(*,*) "Opción inválida. Intente nuevamente."
                END SELECT
            END DO
        ELSE
            login_attempts = login_attempts + 1
            PRINT *, "Credenciales incorrectas. Intente nuevamente."
            IF (login_attempts >= 3) THEN
                PRINT *, "Demasiados intentos fallidos. Saliendo del programa."
                EXIT
            END IF
        END IF
    END DO

CONTAINS

SUBROUTINE subMenu_cargaArchivos()
    IMPLICIT NONE
    INTEGER :: opcion

    WRITE(*,*) "1.1 Sucursales"
    WRITE(*,*) "1.2 Rutas"
    WRITE(*,*) "Ingrese una opción: "
    READ(*,*) opcion

    SELECT CASE (opcion)
        CASE (1)
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

        CASE (2)
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
                call graphRutas%insertar_info(s1,s2,distancia, imp_mantenimiento)
                call graphImpresoras%insertar_info_2(s1,s2,imp_mantenimiento, distancia)
            end do
            print *, "MOSTRANDO RUTAS ---------------------"
            call graphRutas%show_graph()
            print *, "-------------------------------"
            call analizador%setear_grafo(graphRutas)
            call analizador_impresoras%setear_grafo(graphImpresoras)
            call graphRutas%graficar('grafo_route.dot', "Grafo de Rutas")
            call json%destroy()

            
        CASE DEFAULT
            WRITE(*,*) "Opción inválida."
    END SELECT
END SUBROUTINE subMenu_cargaArchivos

SUBROUTINE LimpiarPantalla()
    PRINT *, CHAR(27)//"[2J"
END SUBROUTINE LimpiarPantalla

subroutine recargar_rutas()
            call json%initialize()
            call json%load(filename=filename_const)
            call json%info('grafo', n_children=size) ! Aquí especificamos 'grafo' como la clave del arreglo
            call json%get('grafo', grafo_pointer, found) ! Obtenemos el puntero al arreglo 'grafo'

            do i = 1, size
                call jsonc%get_child(grafo_pointer, i, edgePointer, found=found) ! Obtenemos el i-ésimo elemento del arreglo 'grafo'

                call jsonc%get_child(edgePointer, 's1', partPointer, found=found)
                if (found) then
                    call jsonc%get(partPointer, s1)

                end if

                call jsonc%get_child(edgePointer, 's2', partPointer, found=found)
                if (found) then
                    call jsonc%get(partPointer, s2)

                end if

                call jsonc%get_child(edgePointer, 'distancia', partPointer, found=found)
                if (found) then
                    call jsonc%get(partPointer, distancia)

                end if

                call jsonc%get_child(edgePointer, 'imp_mantenimiento', partPointer, found=found)
                if (found) then
                    call jsonc%get(partPointer, imp_mantenimiento)
                end if
                call graphRutas%insertar_info(s1,s2,distancia, imp_mantenimiento)
                call graphImpresoras%insertar_info_2(s1,s2,imp_mantenimiento, distancia)
            end do
            call analizador%setear_grafo(graphRutas)
            call analizador_impresoras%setear_grafo(graphImpresoras)
            call json%destroy()

end subroutine recargar_rutas

END PROGRAM menu_program