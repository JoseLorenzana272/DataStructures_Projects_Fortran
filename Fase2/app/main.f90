module authentication_module
implicit none
private

public :: login_user

contains

subroutine login_user(default_user, default_password, username, &
    password, valid_login, is_admin, &
    normal_user, normal_password)

    use BTree
    character(len=30), intent(in) :: default_user
    character(len=30), intent(in) :: default_password
    character(:), allocatable, intent(inout) :: username
    character(:), allocatable, intent(inout) :: password
    character(len=30), intent(in) :: normal_user
    character(len=30), intent(in) :: normal_password
    logical, intent(out) :: valid_login
    logical, intent(out) :: is_admin
    type(Btree_class) :: clients_info
    character(len=1) :: choice

    do
    print *, ''
    print *, 'Ingrese su nombre de usuario:'
    read(*, *) username
    
    print *, 'Ingrese su contraseña:'
    read(*, *) password
    
    if (username == default_user .and. password == default_password) then
        valid_login = .true.
        is_admin = .true.
        exit
    else if (username == normal_user .and. password == normal_password) then
        valid_login = .true.
        is_admin = .false.
        exit
    else
        valid_login = .false.
        print *, ''
        print *, 'Credenciales incorrectas.'
        print *, ''
        print *, 'Desea intentarlo de nuevo? (S/N)'
        read(*, *) choice
        if (choice /= 'S' .and. choice /= 's') exit
    end if
    end do
end subroutine login_user

end module authentication_module

program login_program
    use json_module
    use matrix_m_sparse
    use bstdef_t
    use BTree
    use avldef_tree
    use AlbumModule
    use authentication_module
    implicit none
    character(len=30) :: username
    character(len=30) :: password
    character(len=30) :: default_user = 'Goku'
    character(len=30) :: default_password = 'kakaroto'
    character(len=1) :: choice
    logical :: valid_login
    logical :: is_admin
    integer :: option
    integer :: i, size, id, j, pixels_size       ! Se declaran variables enteras
    logical :: found
    integer(8) :: dpi_to_delete, dpi_found
    character(len=200) :: filename_json
    ! PROFUNDIDAD
    integer :: depth
    integer :: image_id
    ! Json usuarios
    character(:), allocatable ::  name_value, name_album
    integer(8) ::  dpi
    character(:), allocatable ::  dpi_value
    character(:), allocatable ::  password_value
    !Json Capas
    integer :: id_layer
    integer :: row
    integer :: column, nCapas, valor, nImages
    integer :: num_layers
    character(:), allocatable ::  color
    type(matrix) :: sparse_matrix
    type(matrix) :: temporal_matrix
    type(bst) :: bst_tree
    type(avl) :: avl_tree
    type(node_avl), pointer :: temporal_avl
    type(clientes) :: temporal_client
    ! LO CREE
    type(bst) :: bst_tree_new
    type(matrix) :: temporal_node
    type(album) :: albumes
    type(Btree_class) :: btree_cosa
    type(Btree_class) :: carajo
    type(clientes) :: build_client 
    type(json_file) :: json   ! Se declara una variable del tipo json_file
    type(json_value), pointer :: listPointer, personPointer, attributePointer, pixelPointer, layerPointer, pixelsPointer  ! Se declaran punteros a variables del tipo json_value
    type(json_value), pointer :: imagePointer, imagesArray, valueImage, imgValue, layerArray, valueLayer, albumValue
    type(json_core) :: jsonc  ! Se declara una variable del tipo json_core para acceder a las funciones básicas de JSON
    ! Contadores
    
    ! Agregar un usuario normal para probar
    !character(len=30) :: normal_user = 'jose'
    !character(len=30) :: normal_password = '123'

    ! Para recorrido limitado
    integer :: limit
    integer :: contador

    ! Para crear imagenes
    integer :: id_img


    
    
        do
            print '(a)', '***********************************'
            print '(a)', '*        BIENVENIDO A             *'
            print '(a)', '*         PIXELPRINT              *'
            print '(a)', '*            :D                   *'
            print '(a)', '***********************************'
            print '(a)', '    /\_/\'
            print '(a)', ' _\/ o o \/_______'
            print '(a)', '  /\__^__/\     _ \'
            print '(a)', '      \ _/ ___ (  \ \'
            print '(a)', '       (__/  (__ /  \|'
            print *, 'Ingrese su nombre de usuario:'
            read(*, '(A)') username
            print *, 'Ingrese su contraseña:'
            read(*, '(A)') password
            !call login_user(default_user, default_password, username, password, valid_login, is_admin, &
            !normal_user, normal_password)

            if (username == default_user .and. password == default_password) then
                
                is_admin = .true.
                print *, ''
                print '(a)', '***********************************'
                print '(a)', '*    INICIO DE SESION EXITOSO     *'
                print '(a)', '***********************************'

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
                                read(*, *) filename_json
                                call json%initialize()    ! Se inicializa el módulo JSON
                                    call json%load(filename=filename_json)  ! Se carga el archivo JSON llamado 'config.json'
                                    call json%print()         ! Se imprime el contenido del archivo JSON (opcional)
                                    call json%info('',n_children=size)
                                
                                    call json%get_core(jsonc)               ! Se obtiene el núcleo JSON para acceder a sus funciones básicas
                                    call json%get('', listPointer, found)
                                
                                    do i = 1, size                          ! Se inicia un bucle sobre el número de elementos en el JSON
                                        call jsonc%get_child(listPointer, i, personPointer, found = found)  ! Se obtiene el i-ésimo hijo de listPointer
                                
                                        ! Obtenemos tanto el ID como el nombre del objeto JSON
                                        call jsonc%get_child(personPointer, 'dpi', attributePointer, found = found)
                                        if (found) then
                                            call jsonc%get(attributePointer, dpi_value)
                                            print *, 'DPI: ', dpi_value
                                            read(dpi_value,'(I13)') dpi
                                            build_client%dpi = dpi
                                        end if
                                
                                        call jsonc%get_child(personPointer, 'nombre_cliente', attributePointer, found = found)
                                        if (found) then
                                            call jsonc%get(attributePointer, name_value)
                                            print *, 'Nombre: ', name_value
                                            build_client%user_name = name_value
                                        end if

                                        call jsonc%get_child(personPointer, 'password', attributePointer, found = found)
                                        if (found) then
                                            call jsonc%get(attributePointer, password_value)
                                            print *, 'Password: ', password_value
                                            build_client%password_client = password_value
                                        end if
                                        call btree_cosa%insert(build_client)
                                        print *, 'Cliente agregado: '
                                        !call btree_cosa%traversal(btree_cosa%root)
                                    end do
                            
                                    call json%destroy()
                            case(2)
                                print *, 'Registrar clientes'
                                call register_clients(btree_cosa)
                            case(3)
                                print *, 'Eliminar Clientes'
                                
                                print *, 'Ingrese DPI del cliente a eliminar:'
                                read(*, *) dpi_to_delete
                                call delete_client(btree_cosa, dpi_to_delete)

                            case (4)
                                print *, 'Visualizar Arbol B de Clientes'
                                call btree_cosa%traversal(btree_cosa%root)
                                call btree_cosa%dotgen_btree(btree_cosa%root, 'BTree.dot')
                                print *, 'Archivo generado: BTree.dot'
                            case (5)
                                print *, 'Busqueda de Clientes'
                                print *, 'Ingrese el DPI del cliente a buscar: '
                                read(*, *) dpi_found
                                call btree_cosa%search_client_by_dpi(dpi_found)
                                
                            case(6)
                                ! Cerrar sesión
                                print *, 'Cerrando Sesion...'
                                exit
                            case default
                                ! Opción no válida
                                print *, 'Opcion no valida.'
                        end select
                    end do
                end if
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
                        
                        case(1)
                            print *, 'Ingrese la ruta del archivo JSON de capas: '
                            read(*, *) filename_json
                            ! Lógica para la tarea 1 (usuario normal)
                            call json%initialize()    ! Se inicializa el módulo JSON
                            call json%load(filename=filename_json)  ! Se carga el archivo JSON llamado 'capas.json'
                            !call json%print()         ! Se imprime el contenido del archivo JSON (opcional)
                            call json%info('',n_children=size)

                            call json%get_core(jsonc)               ! Se obtiene el núcleo JSON para acceder a sus funciones básicas
                            call json%get('', listPointer, found)
                            ! LIMPIAR
                            
                            do i = 1, size                          ! Se inicia un bucle sobre el número de elementos en el JSON
                                call sparse_matrix%clear()
                                call jsonc%get_child(listPointer, i, layerPointer, found = found)  ! Se obtiene el i-ésimo hijo de listPointer

                                ! Obtenemos el ID de la capa
                                call jsonc%get_child(layerPointer, 'id_capa', attributePointer, found = found)
                                if (found) then
                                    call jsonc%get(attributePointer, id_layer)
                                    print *, 'ID de la capa: '
                                    print *, id_layer
                                    call bst_tree%add(id_layer)
                                    temporal_client%layer_counter = temporal_client%layer_counter + 1
                                end if

                                ! Obtenemos los pixeles
                                call jsonc%get_child(layerPointer, 'pixeles', pixelsPointer, found = found)
                                if (found) then
                                    call jsonc%info(pixelsPointer, n_children=pixels_size)
                                    !print *, 'Pixeles: '
                                    print *, pixels_size
                                    do j = 1, pixels_size
                                        
                                        
                                        call jsonc%get_child(pixelsPointer, j, pixelPointer, found = found)
                                        if (found) then
                                            call jsonc%get_child(pixelPointer, 'fila', attributePointer, found = found)
                                            if (found) then
                                                call jsonc%get(attributePointer, row)
                                                !print *, 'Fila: '
                                                print *, row
                                            end if
                                            call jsonc%get_child(pixelPointer, 'columna', attributePointer, found = found)
                                            if (found) then
                                                call jsonc%get(attributePointer, column)
                                                !print *, 'Columna: '
                                                print *, column
                                            end if
                                            call jsonc%get_child(pixelPointer, 'color', attributePointer, found = found)
                                            if (found) then
                                                call jsonc%get(attributePointer, color)
                                                !print *, 'Color: '
                                                print *, color
                                                ! Se insertan los valores a la matriz dispersa
                                                
                                                
                                            end if
                                            
                                            call bst_tree%buscarNodo(id_layer, row, column, color)
                                        end if
                                    end do
                                    !call sparse_matrix%print()
                                    !call bst_tree%imprimirEnOrden()
                                end if

                            end do
                            print *, "Matriz dispersa:"
                            
                            call json%destroy()

                        case(2)
                            ! Lógica para la tarea 2 (usuario normal)
                            print *, 'Cargar imágenes'
                            print *, 'Ingrese la ruta del archivo JSON de imagenes: '
                            read(*, *) filename_json
                            call json%initialize()    ! Se inicializa el módulo JSON
                            call json%load(filename=filename_json)  ! Se carga el archivo JSON llamado 'capas.json'
                            ! call json%print()         ! Se imprime el contenido del archivo JSON (opcional)
                            call json%info('',n_children=size)

                            call json%get_core(jsonc)               ! Se obtiene el núcleo JSON para acceder a sus funciones básicas
                            call json%get('', listPointer, found)
                            do i = 1, size
                                call jsonc%get_child(listPointer, i, imgValue, found=found)
                                if (found) then
                                    call jsonc%get_child(imgValue, 'id', attributePointer, found=found)
                                    if (found) then
                                        call jsonc%get(attributePointer, id)
                                        temporal_client%image_counter = temporal_client%image_counter + 1
                                    end if
                                    print *, "Leyendo imagen: ", id
                                    call jsonc%get_child(imgValue, 'capas', layerArray, found=found)
                                    if (found) then
                                        call jsonc%info(layerArray, n_children=nCapas)
                                        call bst_tree_new%inicializar_arbol_vacio()
                                        do j = 1, nCapas
                                            
                                            call jsonc%get_child(layerArray, j,  valueLayer, found=found)
                                            if (found) then
                                                call jsonc%get(valueLayer, valor)
                                                print *, "CAPA: ", valor
                                                temporal_node = bst_tree%buscarNodo_r(valor)
                                                call bst_tree_new%add_matrix(valor, temporal_node)
                                                
                                            end if
                                        end do
                                        
                                    end if
                                    
                                end if
                                
                                call avl_tree%add_avl_tree(id, bst_tree_new)
                                !call bst_tree_new%dotgen(bst_tree_new%root, 'prueba_tree_new.dot')
                                !call bst_tree%dotgen(bst_tree%root, 'BST_PRUEBA.dot')
                            end do

                            call json%destroy()
                            
                        case(3)
                            print *, 'Cargar Álbumes'
                            print *, 'Ingrese la ruta del archivo JSON de albumes: '
                            read(*, *) filename_json
                            call json%initialize()    ! Se inicializa el módulo JSON
                            call json%load(filename=filename_json)  ! Se carga el archivo JSON llamado 'capas.json'
                            ! call json%print()         ! Se imprime el contenido del archivo JSON (opcional)
                            call json%info('',n_children=size)

                            call json%get_core(jsonc)               ! Se obtiene el núcleo JSON para acceder a sus funciones básicas
                            call json%get('', listPointer, found)
                            do i = 1, size
                                call jsonc%get_child(listPointer, i, albumValue, found=found)
                                if (found) then
                                    call jsonc%get_child(albumValue, 'nombre_album', attributePointer, found=found)
                                    if (found) then
                                        call jsonc%get(attributePointer, name_album)
                                        call albumes%add_album(name_album)
                                    end if
                                    print *, "Nombre Album: ", name_album
                                    call jsonc%get_child(albumValue, 'imgs', imagesArray, found=found)
                                    if (found) then
                                        call jsonc%info(imagesArray, n_children=nImages)
                                        do j = 1, nImages
                                            call jsonc%get_child(imagesArray, j,  valueLayer, found=found)
                                            if (found) then
                                                call jsonc%get(valueLayer, valor)
                                                !print *, "Imagen: ", valor
                                                call albumes%add_image(name_album, valor)
                                            end if
                                        end do 
                                    end if
                                end if
                            end do
                            
                            call json%destroy()
                            
                        case (4)
                            
                            print *, '----------Seleccione una estructura para visualizar:----------'
                            print *, '1. Arbol Binario de Búsqueda (BST)'
                            print *, '2. Arbol AVL'
                            print *, '3. Lista de Albumes'
                            print *, '4. Recorrido limitado'
                            print *, '5. Generacion de imagen (amplitud)'
                            print *, '6. Generar imagen por capas'
                            print *, '7. Graficar una capa'
                            print *, '8. Graficar AVL con capas de una imagen'
                            print *, '9. Volver al menú principal'
                            print *, ''
                            
                            read(*, *) option
                            
                            select case(option)
                                case(1)
                                    print *, 'Visualizando Arbol Binario de Búsqueda (BST) de Capas...'
                                    call bst_tree%dotgen(bst_tree%root, 'BST_tree.dot')
                                    print *, 'Archivo generado: BST_tree.dot'
                                case(2)
                                    print *, 'Visualizando Arbol AVL de imagenes...'
                                    call avl_tree%dotgen_avl(avl_tree%root, 'AVL_tree.dot')
                                    print *, 'Archivo generado: AVL_tree.dot'
                                case(3)
                                    print *, 'Visualizando Lista de Albumes...'
                                    call albumes%generate_dot_file('Albumes.dot')
                                case(4)
                                    print *, '----------Recorrido limitado----------'
                                    print *, 'Ingrese el numero de capas a utilizar: '
                                    read(*, *) limit
                                    print *, '1. Recorrido por preorden'
                                    print *, '2. Recorrido por inorden'
                                    print *, '3. Recorrido por postorden'
                                    print *, 'Volver al menú'
                                    read(*, *) option
                                    call sparse_matrix%clear()
                                    select case(option)
                                        case(1)
                                            contador = 0
                                            print *, 'Recorrido por preorden'
                                            call bst_tree%preorder_limit(bst_tree%root, limit, contador, sparse_matrix)
                                            call sparse_matrix%dot_matrix('sparse_matrix.dot')
                                            call sparse_matrix%html_matrix('image_pre.html')
                                        case(2)
                                            contador = 0
                                            print *, 'Recorrido por inorden'
                                            call bst_tree%inorder_limit(bst_tree%root, limit, contador, sparse_matrix)
                                            call sparse_matrix%dot_matrix('sparse_matrix_in.dot')
                                            call sparse_matrix%html_matrix('image_in.html')
                                        case(3)
                                            contador = 0
                                            print *, 'Recorrido por postorden'
                                            call bst_tree%postorder_limit(bst_tree%root, limit, contador, sparse_matrix)
                                            call sparse_matrix%dot_matrix('sparse_matrix_post.dot')
                                            call sparse_matrix%html_matrix('image_post.html')
                                        case(4)
                                            ! Aqui se saldrá del menú :)
                                        case default
                                            
                                            print *, 'Opción no válida.'
                                    end select
                                case(5)
                                    call sparse_matrix%clear()
                                    print *, 'Generacion de imagen (amplitud)'
                                    print *, 'Ingrese el ID de la imagen a buscar: '
                                    read (*,*) image_id
                                    temporal_avl => avl_tree%search_image_node(avl_tree%root, image_id)
                                    call temporal_avl%bst_tree%dotgen(temporal_avl%bst_tree%root, 'BST_tree_amplitude.dot')
                                    call temporal_avl%bst_tree%search_amplitude(sparse_matrix)
                                    call sparse_matrix%html_matrix('image_amplitude.html')
                                case(6)
                                    call temporal_matrix%clear()
                                    do 
                                        print *, 'Generar imagen por capas'
                                        print *, '1. Agregar capa'
                                        print *, '2. Graficar imagen'
                                        print *, '3. Volver al menú principal'
                                        read(*, *) option
                                        select case(option)
                                            case(1)
                                                print *, 'Ingrese el ID de la capa a agregar: '
                                                read(*, *) id_layer
                                                ! Aqui se agrega el ID a la imagen
                                                call bst_tree%stack_layers(id_layer, temporal_matrix)
                                            case(2)
                                                ! Aqui se llamara a la funcion para graficar la imagen en HTML
                                                call temporal_matrix%html_matrix('image_layer.html')
                                            case(3)
                                                ! Aqui se saldrá del menú :)
                                                exit
                                            case default
                                                ! Opción no válida
                                                print *, 'Opción no válida.'
                                            

                                            end select
                                    end do
                                case(7)
                                    print *, 'Graficar una capa'
                                    print *, 'Ingrese el ID de la capa a graficar: '
                                    read(*, *) id_layer
                                    call bst_tree%graficarCapa(id_layer, 'layerImage.html')
                                case(8)
                                    print *, 'Graficar AVL con capas de una imagen'
                                    print *, 'Ingrese el ID de la imagen a graficar: '
                                    read(*, *) id_img
                                    call avl_tree%dotgen_avl_bst(avl_tree%root, id_img, 'AVL_BST_tree.dot')
                                case(9)
                                    ! Aqui se saldrá del menú :)
                                case default
                                    ! Opción no válida
                                    print *, 'Opción no válida.'
                            end select
                        case(5)
                            print *, 'Crear y eliminar imagenes'
                            print *, '1. Crear imagen'
                            print *, '2. Eliminar imagen'
                            print *, '3. Volver al menú principal'
                            read(*, *) option
                            
                            select case(option)
                            case (1)
                        
                                print *, 'Ingrese el nombre de la imagen a crear: '
                                read(*, *) id_img
                                call avl_tree%add_avl(id_img)
                                print *, 'Ingrese el número de capas que conformarán la imagen: '
                                read(*, *) nCapas
                                do j = 1, nCapas
                                    print *, 'Ingrese el ID de la capa ', j, ': '
                                    read(*, *) id_layer
                                    ! Aqui se agrega el ID a la imagen
                                    call avl_tree%add_layer_to_image(id_img, id_layer)
                                end do
                            case (2)
                                print *, 'Ingrese el ID de la imagen a eliminar: '
                                read(*, *) id_img
                                call avl_tree%delete_image(id_img)
                            case (3)
                                ! Aqui se saldrá del menú :)
                            case default
                                ! Opción no válida
                                print *, 'Opción no válida.'
                            end select

                        case(6)
                            print *, 'Generacion de reportes'
                            print *, 'Top 5 imagenes con mas capas: '
                            !call avl_tree%get_top_5()
                            print *, '-----------------------------'
                            print *, 'Todas las capas que son hojas: '
                            call bst_tree%imprimirHojas()
                            print *, '-----------------------------'
                            print *, 'Todos los recorridos de las capas: '
                            write(*,*) 'Recorrido en preorden: '
                            call bst_tree%preorder(bst_tree%root)
                            write(*,*)

                            write(*,*) 'Recorrido en inorden: '
                            call bst_tree%inorder(bst_tree%root)
                            write(*,*)

                            write(*,*) 'Recorrido en postorden: '
                            call bst_tree%postorder(bst_tree%root)
                            write(*,*)


                            print *, '-----------------------------'
                            print *, 'Profundidad del arbol de capas BST: '
                            depth = bst_tree%calcularProfundidad()
                            !print *, depth
                        case(7)
                            ! Cerrar sesión
                            print *, 'Cerrando Sesión...'
                            call btree_cosa%search_trees(username, bst_tree, avl_tree, albumes, &
                            temporal_client%layer_counter, temporal_client%image_counter)
                            exit
                        case default
                            ! Opción no válida
                            print *, 'Opción no válida.'
                    end select
                end do
            else
                print *, ''
                print '(a)', '***********************************'
                print '(a)', '*    INICIO DE SESION FALLIDO     *'
                print '(a)', '***********************************'
                print *, ''
                print *, 'Credenciales incorrectas.'
                print *, ''
                print *, 'Desea intentarlo de nuevo? (S/N)'
                read(*, *) choice
                if (choice /= 'S' .and. choice /= 's') exit
            end if

    end do
end program login_program
