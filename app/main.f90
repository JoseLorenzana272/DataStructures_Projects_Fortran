program menu
    use json_module
    use linked_list_m
    use lista_ventanillas_module
    use clientes_atendidos_module
    use image_stack_module

    use lista_clientes_espera_module
    implicit none
    integer :: opcion, numero_ventanillas
    integer :: id_value, imagen_g, imagen_p
    character(:), allocatable ::  name_value

    type(linked_list) :: lista_clientes
    type(lista_ventanillas) :: ventanillas
    type(lista_clientes_espera) :: clientes_espera
    !type(image):: new_image
    type(json_file) :: json   ! Se declara una variable del tipo json_file
    type(json_value), pointer :: listPointer, personPointer, attributePointer  ! Se declaran punteros a variables del tipo json_value
    type(json_core) :: jsonc  ! Se declara una variable del tipo json_core para acceder a las funciones básicas de JSON
    character(:), allocatable :: nombre  ! Se declara una cadena de caracteres que se asignará dinámicamente
    integer :: i, size, id        ! Se declaran variables enteras
    logical :: found
    integer :: paso = 1
    ! Variables random
    integer :: num_clientes, num_imagenes_g, num_imagenes_p
    character(len=20) :: nombres(20) = ['Carlos ', 'Luisa  ', 'Pedro  ', 'Marina ', 'Laura  ', &
                                    'David  ', 'Ana    ', 'Sandra ', 'Pablo  ', 'Elena  ', &
                                    'Mario  ', 'Carmen ', 'Sergio ', 'Andrea ', 'Javier ', &
                                    'Miguel ', 'Raquel ', 'Ruben  ', 'Natalia', 'Oscar  ']

character(len=20) :: apellidos(20) = ['Garcia ', 'Martin ', 'Lopez  ', 'Fernan ', 'Perez  ', &
                                    'Gomez  ', 'Sanchez', 'Diaz   ', 'Leon   ', 'Romero ', &
                                    'Alonso ', 'Gutierz', 'Torres ', 'Ruiz   ', 'Jimenez', &
                                    'Silva  ', 'Serrano', 'Molina ', 'Paz    ', 'Nunez  ']
    integer :: indice_nombre, indice_apellido
    integer :: last_id_used
    real :: rand_num

    !variables reportes
    integer :: total_clients
    total_clients = lista_clientes%count_list()
    
    

    do
        print *, 'MENU :D:'
        print *, '1. Carga masiva de clientes'
        print *, '2. Cantidad de ventanillas'
        print *, '3. Ejecutar paso'
        print *, '4. Estado en memoria de las estructuras'
        print *, '5. Reportes'
        print *, '6. Datos del estudiante'
        print *, '7. Salir'
        print *, 'Seleccione una opcion:'
        read(*, *) opcion

        call limpiar_pantalla()

        select case (opcion)
        case (1)
            print *, 'Cargando clientes desde el archivo "config.json"'
        
            call json%initialize()    ! Se inicializa el módulo JSON
            call json%load(filename='config.json')  ! Se carga el archivo JSON llamado 'config.json'
            !call json%print()         ! Se imprime el contenido del archivo JSON (opcional)
            call json%info('',n_children=size)
        
            call json%get_core(jsonc)               ! Se obtiene el núcleo JSON para acceder a sus funciones básicas
            call json%get('', listPointer, found)
        
            do i = 1, size                          ! Se inicia un bucle sobre el número de elementos en el JSON
                call jsonc%get_child(listPointer, i, personPointer, found = found)  ! Se obtiene el i-ésimo hijo de listPointer
        
                ! Obtenemos tanto el ID como el nombre del objeto JSON
                call jsonc%get_child(personPointer, 'id', attributePointer, found = found)
                if (found) then
                    call jsonc%get(attributePointer, id_value)
                end if
        
                call jsonc%get_child(personPointer, 'nombre', attributePointer, found = found)
                if (found) then
                    call jsonc%get(attributePointer, name_value)
                end if

                call jsonc%get_child(personPointer, 'img_g', attributePointer, found = found)
                if (found) then
                    call jsonc%get(attributePointer, imagen_g)
                end if

                call jsonc%get_child(personPointer, 'img_p', attributePointer, found = found)
                if (found) then
                    call jsonc%get(attributePointer, imagen_p)
                end if
        
                ! Insertamos el ID y el nombre en la lista enlazada
                call lista_clientes%append(id_value, name_value, imagen_g, imagen_p)
            end do
        
            call json%destroy()
            call lista_clientes%print()
        
        case (2)
            print *, 'Ingrese el número de ventanillas:'
            read(*, *) numero_ventanillas
            call ventanillas%crear_ventanillas(numero_ventanillas)
            print *, 'Se han asignado ', numero_ventanillas, ' ventanillas.'
        case (3)
            print *, 'Ha seleccionado la opción 3: Ejecutar paso.'
            
            last_id_used = lista_clientes%obtener_ultimo_id()
            
            call random_number(rand_num)
            num_clientes = floor(rand_num * 4)
            
            do i = 1, num_clientes
                call random_number(rand_num)
                indice_nombre = floor(rand_num * 20) + 1
                call random_number(rand_num)
                indice_apellido = floor(rand_num * 20) + 1
        
                call random_number(rand_num)
                num_imagenes_g = floor(rand_num * 5)
                call random_number(rand_num)
                num_imagenes_p = floor(rand_num * 5)
        
                last_id_used = last_id_used + 1
                
                call lista_clientes%append( &
                    last_id_used, &
                    trim(nombres(indice_nombre)) // ' ' // trim(apellidos(indice_apellido)), &
                    num_imagenes_g, &
                    num_imagenes_p &
                )
                
                ! Imprimir detalles del cliente generado (depuración)
                print *, '-------------------------------------------------'
                print *, 'Cliente generado:', last_id_used, &
                        trim(nombres(indice_nombre)) // ' ' // trim(apellidos(indice_apellido)), &
                        num_imagenes_g, num_imagenes_p
            end do
            
            print *, '-------------------------------------------------'
            print *, 'Se han generado ', num_clientes, ' clientes aleatorios.'
            call ejecutar_paso(lista_clientes, ventanillas, paso)
        
            !print *, 'Lista de clientes después de ejecutar el paso:'
            !call lista_clientes%print()
            print *, '-------------------------------------------------------------'
            print *, 'Lista de clientes después de generar clientes aleatorios:'
            call lista_clientes%print()
        
            !new_image%id = id
            
            !call ventanillas%head%imagenes%push(new_image)
            
            !print *, 'Se ha agregado una imagen a la ventanilla ', ventanillas%last_assigned_id
            call clientes_espera%print_clientes_espera()
            
        case (4)
            print *, 'Ha seleccionado la opcion 4: Estado en memoria de las estructuras.'
            call lista_clientes%graficar('lista.dot')
            call ventanillas%graficar_ventanillas()
            call clientes_espera%graficar_clientes_espera()
        case (5)
            print *, 'Ha seleccionado la opcion 5: Reportes.'
            
            call lista_clientes%top_clientes_img_grande()
        
            call lista_clientes%top_clientes_img_peque()

            call lista_clientes%graficar_tops_clientes('tops_clientes.dot')
            !call manejar_lista_clientes_en_espera(lista_espera, ventanillas)

            

        case (6)
            print *, 'Ha seleccionado la opcion 6: Datos del estudiante.'
            print *, 'Nombre: José Daniel Lorenzana Medina'
            print *, 'Carnet: 202206560'
            print *, 'Curso: Estructura de Datos'
            print *, 'Seccion: C'  
        case (7)
            print *, 'Saliendo del programa.'
            exit
        case default
            print *, 'Opcion incorrecta. Por favor, seleccione una opcion válida.'
        end select
    end do

contains
    subroutine limpiar_pantalla()
        character(len=30) :: escape_sequence
        escape_sequence = ACHAR(27) // '[2J'
        print *, escape_sequence
    end subroutine limpiar_pantalla

    subroutine ejecutar_paso(lista_clientes, ventanillas, paso)
        use lista_ventanillas_module
        implicit none
        type(linked_list) :: lista_clientes
        type(lista_ventanillas) :: ventanillas
        integer, intent(inout) :: paso
        integer :: id, img_grande, img_peque
        character(len=:), allocatable :: name
        integer :: file_unit
        character(len=100) :: filename
    
        ! Abre el archivo de registro
        filename = 'registro_pasos.txt'
        open(newunit=file_unit, file=filename, status='unknown', action='write', position='append')
    
        if (.not. associated(ventanillas%head%next)) then
            print *, 'No hay ventanillas disponibles.'
            return
        end if
    
        call lista_clientes%obtener_y_eliminar_primer_cliente(id, name, img_grande, img_peque)
    
        ! Asignar la ventanilla
        call ventanillas%asignar_ventanilla(id, img_grande, img_peque)
    
        ! Agregar una imagen a la pila de la ventanilla asignada
        call ventanillas%head%imagenes%push(paso)
    
        print *, '------PASO ', paso, '------'
        print *, 'Cliente ', id, ' (', trim(name), ') asignado a la ventanilla ', ventanillas%last_assigned_id
        print *, 'Imagen ', paso, ' agregada a la ventanilla ', ventanillas%last_assigned_id
    
        write(file_unit, *) '------PASO ', paso, '------'
        write(file_unit, *) 'Cliente ', id, ' (', trim(name), ') asignado a la ventanilla ', ventanillas%last_assigned_id
        write(file_unit, *) 'Imagen ', paso, ' agregada a la ventanilla ', ventanillas%last_assigned_id
    
        paso = paso + 1

        call clientes_espera%append_cliente_espera(id, name, img_grande, img_peque)
    
        close(file_unit)
    end subroutine ejecutar_paso

    
    
end program menu
