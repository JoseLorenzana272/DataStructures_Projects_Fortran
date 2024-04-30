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
        procedure :: insertar_info
        procedure :: insertar_info_2
        procedure :: insertar_nodo
        procedure :: insertar_arista
        procedure :: insertar_arista_2
        procedure :: obtener_nodo
        procedure :: show_graph
        procedure :: graficar
        procedure :: reset_grafo
    end type graph
    type analyzer
        type(graph):: graph_data 
    contains
        procedure :: setear_grafo
        procedure :: shortest_path
        procedure :: longest_path
    end type analyzer
contains
    !! Subrutina para ordenar las aristas de forma ascendente
    subroutine ordenar_distancias(this, id, weight, parent_id, sort_by_id, impresoras)
        class(arista_list), intent(inout) :: this
        integer, intent(in) :: id, weight, parent_id, impresoras
        logical, intent(in) :: sort_by_id
        type(arista), pointer :: new_arista
        type(arista), pointer :: current
        type(arista), pointer :: previous
        allocate(new_arista)
        new_arista%id = id
        new_arista%weight = weight
        new_arista%parent_id = parent_id
        new_arista%printers = impresoras

        if (.not. associated(this%head)) then
            this%head => new_arista
            this%tail => new_arista
            return
        end if

        current => this%head
        previous => null()

        if (sort_by_id) then
            do while (associated(current))
                if (current%id > id) then
                    exit
                end if
                previous => current
                current => current%next
            end do
        else
            do while (associated(current))
                if (current%weight > weight) then
                    exit
                end if
                previous => current
                current => current%next
            end do
        end if

        if (.not. associated(previous)) then
            new_arista%next => this%head
            this%head%prev => new_arista
            this%head => new_arista
        else if (.not. associated(current)) then
            this%tail%next => new_arista
            new_arista%prev => this%tail
            this%tail => new_arista
        else
            previous%next => new_arista
            new_arista%prev => previous
            new_arista%next => current
            current%prev => new_arista
        end if
    end subroutine ordenar_distancias
    
    ! Subrutina para ordenar las aristas de forma ascendente
    subroutine ordenar_distancias_2(this, id, weight, parent_id, sort_by_id, printers)
        class(arista_list), intent(inout) :: this
        integer, intent(in) :: id, weight, parent_id, printers
        logical, intent(in) :: sort_by_id
        type(arista), pointer :: new_arista
        type(arista), pointer :: current
        type(arista), pointer :: previous
        allocate(new_arista)
        new_arista%id = id
        new_arista%weight = weight
        new_arista%parent_id = parent_id
        new_arista%printers = printers

        if (.not. associated(this%head)) then
            this%head => new_arista
            this%tail => new_arista
            return
        end if

        current => this%head
        previous => null()

        if (sort_by_id) then
            do while (associated(current))
                if (current%id > id) then
                    exit
                end if
                previous => current
                current => current%next
            end do
        else
            do while (associated(current))
                if (current%weight < weight) then
                    exit
                end if
                previous => current
                current => current%next
            end do
        end if

        if (.not. associated(previous)) then
            new_arista%next => this%head
            this%head%prev => new_arista
            this%head => new_arista
        else if (.not. associated(current)) then
            this%tail%next => new_arista
            new_arista%prev => this%tail
            this%tail => new_arista
        else
            previous%next => new_arista
            new_arista%prev => previous
            new_arista%next => current
            current%prev => new_arista
        end if
    end subroutine ordenar_distancias_2

    ! Función para sacar la primera arista de la lista
    function pop(this) result(arista_res)
        class(arista_list), intent(inout) :: this
        type(arista), pointer :: arista_res
        if (.not. associated(this%head)) then
            arista_res => null()
            return
        end if
        arista_res => this%head
        this%head => this%head%next
        if (associated(this%head)) then
            this%head%prev => null()
        else
            this%tail => null()
        end if
    end function pop


    ! Subrutina para unir dos listas de aristas
    subroutine unir_listas(this,  to_unir_listas)
        class(arista_list), intent(inout) :: this
        class(arista_list), intent(in) :: to_unir_listas
        type(arista), pointer :: current

        current => to_unir_listas%head
        do while (associated(current))
            call this%ordenar_distancias(current%id, current%weight, current%parent_id, .FALSE., current%printers)
            current => current%next
        end do
        
    end subroutine unir_listas

    ! Subrutina para unir dos listas de aristas
    subroutine unir_listas_2(this,  to_unir_listas)
        class(arista_list), intent(inout) :: this
        class(arista_list), intent(in) :: to_unir_listas
        type(arista), pointer :: current

        current => to_unir_listas%head
        do while (associated(current))
            call this%ordenar_distancias_2(current%id, current%weight, current%parent_id, .FALSE., current%printers)
            current => current%next
        end do
        
    end subroutine unir_listas_2

    function vacio_ver(this) result(res)
        class(arista_list), intent(in) :: this
        logical :: res
        res = .not. associated(this%head)
    end function vacio_ver

    subroutine anadir_peso_impresoras(this, weight, printers)
        class(arista_list), intent(inout) :: this
        integer, intent(in) :: weight, printers
        type(arista), pointer :: current
        current => this%head
        do while (associated(current))
            current%weight = current%weight + weight
            current%printers = current%printers + printers
            current => current%next
        end do        
    end subroutine anadir_peso_impresoras

    ! Subrutina para sumar un resultado a la lista de resultados
    subroutine sumar_resultado(this,  id, weight, printers)
        class(result_list), intent(inout) :: this
        integer, intent(in) :: id, weight, printers
        type(result), pointer :: new_result
        allocate(new_result)
        new_result%id = id
        new_result%weight = weight
        new_result%printers = printers
        if (.not. associated(this%head)) then
            this%head => new_result
            this%tail => new_result
            return
        end if
        this%tail%next => new_result
        this%tail => new_result  
        this%total_weight = this%tail%weight  
        this%total_printers = this%tail%printers
    end subroutine sumar_resultado

    subroutine insertar_info(this, id, neighbor_id, weight, impresoras)
        class(graph), intent(inout) :: this
        integer, intent(in) :: id, neighbor_id, weight, impresoras   
        type(node), pointer :: current

        current => this%obtener_nodo(id)
        if ( .NOT. associated(current) ) then
            call this%insertar_nodo(id)
            call this%insertar_arista(neighbor_id, weight, this%head, impresoras)
        else
            call this%insertar_arista(neighbor_id, weight, current, impresoras)
        end if
    end subroutine insertar_info

    subroutine insertar_info_2(this, id, neighbor_id, weight, printers)
        class(graph), intent(inout) :: this
        integer, intent(in) :: id, neighbor_id, weight, printers
        type(node), pointer :: current

        current => this%obtener_nodo(id)
        if ( .NOT. associated(current) ) then
            call this%insertar_nodo(id)
            call this%insertar_arista_2(neighbor_id, weight, this%head, printers)
        else
            call this%insertar_arista_2(neighbor_id, weight, current, printers)
        end if
    end subroutine insertar_info_2

    subroutine print(this)
        class(result_list), intent(in) :: this
        type(result), pointer :: current
        current => this%head
        do while (associated(current))
            write(*,'(A, I0, A, I0)') 'Nodo (Viaje a sucursal): ', current%id, ", Peso Acumulado: ", current%weight
            current => current%next
        end do
    end subroutine print

    subroutine insertar_nodo(this,  id)
        class(graph), intent(inout) :: this
        integer, intent(in) :: id
        type(node), pointer :: new_node

        allocate(new_node)
        new_node%id = id
        
        if (.not. associated(this%head)) then
            this%head => new_node
            return
        end if

        new_node%next => this%head
        this%head => new_node
    end subroutine insertar_nodo

    ! Subrutina para insertar una arista en el grafo
    ! Si el nodo no existe, se crea
    ! Si la arista ya existe, se actualiza el peso
    subroutine insertar_arista(this, id, weight, parent, impresoras)
        class(graph), intent(inout) :: this
        integer, intent(in) :: id, weight, impresoras
        type(node), pointer :: parent
        type(node), pointer :: arista_node 
        arista_node => this%obtener_nodo(id)
        if ( .NOT. associated(arista_node) ) then
            call this%insertar_nodo(id)
        end if
        call parent%neighbors%ordenar_distancias(id, weight, parent%id, .TRUE., impresoras)
        this%n_nodes = this%n_nodes + 1
    end subroutine insertar_arista

    ! Lo mismo que la de arriba pero con el orden de los pesos invertido (impresoras)
    subroutine insertar_arista_2(this, id, weight, parent, printers)
        class(graph), intent(inout) :: this
        integer, intent(in) :: id, weight, printers
        type(node), pointer :: parent
        type(node), pointer :: arista_node 
        arista_node => this%obtener_nodo(id)
        if ( .NOT. associated(arista_node) ) then
            call this%insertar_nodo(id)
        end if
        call parent%neighbors%ordenar_distancias_2(id, weight, parent%id, .TRUE., printers)
        this%n_nodes = this%n_nodes + 1
    end subroutine insertar_arista_2

    ! Función para obtener un nodo por su id
    function obtener_nodo(this, id) result(retval)
        class(graph), intent(in) :: this
        integer, intent(in) :: id
        type(node), pointer :: retval
        type(node), pointer :: current
        current => this%head
        do while (associated(current))
            if (current%id == id) then
                retval => current
                return
            end if
            current => current%next
        end do
        retval => null()  
    end function obtener_nodo

    subroutine show_graph(this)
        class(graph), intent(in) :: this
        type(node), pointer :: current
        type(arista), pointer :: current_arista
        current => this%head
        do while (associated(current))
            write(*,'(A, I0)') 'Node: ', current%id
            current_arista => current%neighbors%head
            do while (associated(current_arista))
                write(*,'(A, I0, A, I0, A)', advance='no') 'Arista: ', current_arista%id, ", " ,current_arista%weight, " "
                current_arista => current_arista%next
            end do
            write(*, *) ''
            current => current%next
        end do
    end subroutine show_graph

    subroutine setear_grafo(this,  graph_p)
        class(analyzer), intent(inout) :: this
        type(graph), intent(in) :: graph_p
        this%graph_data = graph_p
    end subroutine setear_grafo

    function shortest_path(this, id_origin, id_destination) result(retval)
        class(analyzer), intent(in) :: this
        integer, intent(in) :: id_origin, id_destination
        integer :: sub_total, sub_total_printers
        type(result_list), pointer :: retval
        type(arista_list), pointer :: queue
        type(node), pointer :: current_node
        type(arista), pointer :: current_arista
        print *, 'Ruta más corta desde sucursal ', id_origin, ' a la ', id_destination
        sub_total = 0
        sub_total_printers = 0
        allocate(retval)
        retval%total_weight = 0
        allocate(queue)
        current_node => this%graph_data%obtener_nodo(id_origin)
        if ( associated(current_node) ) then
            call queue%unir_listas(current_node%neighbors)
            call retval%sumar_resultado(id_origin, 0, 0)
        end if
        do while ( .NOT. queue%vacio_ver() )
            current_arista => queue%pop()
            sub_total = current_arista%weight
            sub_total_printers = current_arista%printers
            current_node => this%graph_data%obtener_nodo(current_arista%id)
            if ( .NOT. associated(current_node) ) then
                print *, 'No se encontró el nodo'
                exit
            end if
            if (current_node%id == id_destination) then
                print *, 'Se encontró la sucursal destino'
                call retval%sumar_resultado(current_node%id, sub_total, sub_total_printers)
                exit
            end if
            call current_node%neighbors%anadir_peso_impresoras(sub_total, sub_total_printers)
            call queue%unir_listas(current_node%neighbors)
            call retval%sumar_resultado(current_node%id, sub_total, sub_total_printers)
            current_node => current_node%next
        end do
    end function shortest_path

    function longest_path(this, id_origin, id_destination) result(retval)
        class(analyzer), intent(in) :: this
        integer, intent(in) :: id_origin, id_destination
        integer :: sub_total, sub_total_printers
        type(result_list), pointer :: retval
        type(arista_list), pointer :: queue
        type(node), pointer :: current_node
        type(arista), pointer :: current_arista
        integer, allocatable :: max_weights(:)
        logical, allocatable :: visited(:)
    
        allocate(retval)
        retval%total_weight = 0
        retval%total_printers = 0
        allocate(queue)
        allocate(max_weights(this%graph_data%n_nodes))
        allocate(visited(this%graph_data%n_nodes))
        max_weights = -HUGE(0)  ! Inicializar todos los pesos como negativos
        visited = .false.
    
        current_node => this%graph_data%obtener_nodo(id_origin)
        if ( associated(current_node) ) then
            call queue%unir_listas_2(current_node%neighbors)
            call retval%sumar_resultado(id_origin, 0, 0)
            max_weights(current_node%id) = 0
        end if
    
        do while ( .NOT. queue%vacio_ver() )
            current_arista => queue%pop()
            sub_total = current_arista%weight + max_weights(current_arista%parent_id)
            sub_total_printers = current_arista%printers
            if (sub_total > max_weights(current_arista%id)) then
                max_weights(current_arista%id) = sub_total
                current_node => this%graph_data%obtener_nodo(current_arista%id)
                if ( .NOT. associated(current_node) ) then
                    print *, 'No se encontró el nodo'
                    exit
                end if
                if (current_node%id == id_destination) then
                    print *, 'Se encontró la sucursal destino'
                    call retval%sumar_resultado(current_node%id, sub_total, sub_total_printers)
                    exit
                end if
                if (.not. visited(current_node%id)) then
                    call current_node%neighbors%anadir_peso_impresoras(sub_total - max_weights(current_node%id), sub_total_printers)
                    call queue%unir_listas_2(current_node%neighbors)
                    call retval%sumar_resultado(current_node%id, sub_total, sub_total_printers)
                    visited(current_node%id) = .true.
                end if
            end if
        end do
    end function longest_path
    
    subroutine graficar(this, filename, title)
        class(graph), intent(in) :: this
        character(len=*), intent(in) :: filename, title
        type(node), pointer :: current_node
        type(arista), pointer :: current_arista
        integer :: io_unit, io_stat
        
        open(newunit=io_unit, file=filename, status='replace', action='write', iostat=io_stat)
        if (io_stat /= 0) then
            print *, 'Error al abrir el archivo'
            return
        end if
        
        write(io_unit, *) 'digraph G {'
        write(io_unit, *) '    rankdir=LR;'
        write(io_unit, '(A, A)') '    labelloc="t"; label="', title, '";'
        
        write(io_unit, *) '    node [shape=circle, style=filled, color=lightblue, fontname="Arial"];'
        write(io_unit, *) '    edge [fontname="Arial"];'
        
        current_node => this%head
        do while (associated(current_node))
            current_arista => current_node%neighbors%head
            do while (associated(current_arista))
                if (current_node%id /= current_arista%id) then
                    write(io_unit, '(A, I0, A, I0, A, I0, A)') '    ', current_node%id, ' -> ', &
                    current_arista%id, ' [label="', current_arista%weight, '", color=darkgreen];'
                end if
                current_arista => current_arista%next
            end do
            current_node => current_node%next
        end do
        
        write(io_unit, *) '}'
        close(io_unit)
        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')     
        print *,"Arbol de Imagenes con Capas graficado con exito"   
        call system('start ' // trim(adjustl(filename)) // '.png')
    end subroutine graficar
    
    subroutine reset_grafo(this)
        class(graph), intent(inout) :: this
        type(node), pointer :: current_node
        type(node), pointer :: temp_node
        type(arista), pointer :: current_arista
        type(arista), pointer :: temp_arista
    
        current_node => this%head
        do while (associated(current_node))
            current_arista => current_node%neighbors%head
            do while (associated(current_arista))
                temp_arista => current_arista
                current_arista => current_arista%next
                nullify(temp_arista%prev)
                nullify(temp_arista%next)
                deallocate(temp_arista)
            end do
            nullify(current_node%neighbors%head)
            nullify(current_node%neighbors%tail)
    
            temp_node => current_node
            current_node => current_node%next
            deallocate(temp_node)
        end do
    
        nullify(this%head)
        this%n_nodes = 0
    
        print *, "El grafo ha sido reiniciado."
    end subroutine reset_grafo

end module routes