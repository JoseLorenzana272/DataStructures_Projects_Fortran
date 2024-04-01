module bstdef_t
    use matrix_m_sparse
    implicit none
    
    
    type :: node
        type(matrix) :: newMatrix
        integer :: uid 
        type(node), pointer :: left => null()
        type(node), pointer :: right => null()
    end type node

    type :: bst
        type(node), pointer :: root => null()
    contains
        procedure :: add
        procedure :: add_matrix
        procedure :: add_rec
        procedure :: preorder
        procedure :: inorder
        procedure :: postorder
        procedure :: preorder_limit
        procedure :: inorder_limit
        procedure :: postorder_limit
        procedure :: dotgen
        procedure :: dotgen_rec
        procedure :: imprimirEnOrden
        procedure :: buscarNodo
        procedure :: search_amplitude
        procedure :: calcularProfundidad
        procedure :: buscarNodo_r
        procedure :: inicializar_arbol_vacio
        procedure :: imprimirHojas
        procedure :: graficarCapa
        procedure :: stack_layers
        procedure :: dotgen_unit
    end type bst

contains

    subroutine add(this, uid)
        class(bst), intent(inout) :: this
        integer, intent(in) :: uid
        type(node), pointer :: tmp
        if(associated(this%root)) then
            call this%add_rec(uid, this%root)
        else
            allocate(tmp)
            tmp%uid = uid

            this%root => tmp
        end if
    end subroutine add

    subroutine add_matrix(this, uid, sparse_matrix)
        class(bst), intent(inout) :: this
        integer, intent(in) :: uid
        type(matrix), intent(in) :: sparse_matrix
        type(node), pointer :: tmp
        if(associated(this%root)) then
            call this%add_rec(uid, this%root)
        else
            allocate(tmp)
            tmp%uid = uid
            tmp%newMatrix = sparse_matrix
            this%root => tmp
        end if
    end subroutine add_matrix
    
    subroutine add_rec(this, uid, tmp)
        class(bst), intent(inout) :: this
        integer, intent(in) :: uid
        class(node), intent(inout) :: tmp
        if (uid < tmp%uid) then
            if (associated(tmp%left)) then
                call this%add_rec(uid, tmp%left)
            else
                allocate(tmp%left)
                tmp%left%uid = uid

            end if
        else
            if (associated(tmp%right)) then
                call this%add_rec(uid, tmp%right)
            else
                allocate(tmp%right)
                tmp%right%uid = uid

            end if
        end if
    end subroutine add_rec

    subroutine preorder(this, tmp)
        class(bst), intent(in) :: this
        class(node), intent(in), pointer :: tmp
        if( .not. associated(tmp)) then
            return
        end if
        write (*, '(1I3)', advance='no') (tmp%uid)
        call this%preorder(tmp%left)
        call this%preorder(tmp%right)
    end subroutine preorder

    subroutine preorder_limit(this, tmp, limit, counter, sparse_matrix)
        class(bst), intent(in) :: this
        class(node), intent(in), pointer :: tmp
        type(matrix), intent(inout) :: sparse_matrix
        integer, intent(in) :: limit
        integer, intent(inout) :: counter
        type(node_matrix), pointer :: actual_node, actual_row
        
        if (.not. associated(tmp)) then
            return
        end if
        if (counter /= limit) then
            counter = counter + 1
            actual_row => tmp%newMatrix%root%down
            do while (associated(actual_row))
                actual_node => actual_row%right
                do while (associated(actual_node))
                    call sparse_matrix%insert(actual_node%i, actual_node%j, actual_node%color)
                    actual_node => actual_node%right
                end do
                actual_row => actual_row%down
            end do
            print *, "Clave:", tmp%uid

            
            call this%preorder_limit(tmp%left, limit, counter, sparse_matrix)
            call this%preorder_limit(tmp%right, limit, counter, sparse_matrix)
        end if
    end subroutine preorder_limit

    subroutine inorder_limit(this, tmp, limit, counter, sparse_matrix)
        class(bst), intent(in) :: this
        class(node), intent(in), pointer :: tmp
        type(matrix), intent(inout) :: sparse_matrix
        integer, intent(in) :: limit
        integer, intent(inout) :: counter
        type(node_matrix), pointer :: actual_node, actual_row
    
        if (.not. associated(tmp)) then
            return
        end if
        if (counter /= limit) then
            counter = counter + 1
            call this%inorder_limit(tmp%left, limit, counter, sparse_matrix)
            
            
            actual_row => tmp%newMatrix%root%down
            do while (associated(actual_row))
                actual_node => actual_row%right
                do while (associated(actual_node))
                    call sparse_matrix%insert(actual_node%i, actual_node%j, actual_node%color)
                    actual_node => actual_node%right
                end do
                actual_row => actual_row%down
            end do
            print *, "Clave:", tmp%uid
    
            call this%inorder_limit(tmp%right, limit, counter, sparse_matrix)
        end if
    end subroutine inorder_limit
    

    subroutine postorder_limit(this, tmp, limit, counter, sparse_matrix)
        class(bst), intent(in) :: this
        class(node), intent(in), pointer :: tmp
        type(matrix), intent(inout) :: sparse_matrix
        integer, intent(in) :: limit
        integer, intent(inout) :: counter
        type(node_matrix), pointer :: actual_node, actual_row

    
        if (.not. associated(tmp) .or. counter == limit) then
            return
        end if
        if (counter /= limit) then
            counter = counter + 1
            call this%postorder_limit(tmp%left, limit, counter, sparse_matrix)
            call this%postorder_limit(tmp%right, limit, counter, sparse_matrix)
        
            
            actual_row => tmp%newMatrix%root%down
            do while (associated(actual_row))
                actual_node => actual_row%right
                do while (associated(actual_node))
                    call sparse_matrix%insert(actual_node%i, actual_node%j, actual_node%color)
                    actual_node => actual_node%right
                end do
                actual_row => actual_row%down
            end do
            print *, "Clave:", tmp%uid
            

        end if

    end subroutine postorder_limit
    
    

    subroutine inorder(this, tmp)
        class(bst), intent(in) :: this
        class(node), intent(in), pointer :: tmp
        if( .not. associated(tmp)) then
            return
        end if
        call this%inorder(tmp%left)
        write (*, '(1I3)', advance='no') (tmp%uid)
        call this%inorder(tmp%right)
    end subroutine inorder

    subroutine postorder(this, tmp)
        class(bst), intent(in) :: this
        class(node), intent(in), pointer :: tmp
        if( .not. associated(tmp)) then
            return
        end if
        call this%postorder(tmp%left)
        call this%postorder(tmp%right)
        write (*, '(1I3)', advance='no') (tmp%uid)
    end subroutine postorder

    subroutine dotgen(this, tmp, filename)
        class(bst), intent(in) :: this
        class(node), intent(in), pointer :: tmp
        character(len=*), intent(in) :: filename
        integer :: unit
        open(unit, file=filename, status='replace')
        write(unit, '(A)') 'graph{'
        call this%dotgen_rec(tmp, unit)
        write(unit, '(A)') '}'
        close(unit)
        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')
        call system('start ' // trim(adjustl(filename)) // '.png')
    end subroutine dotgen

    subroutine dotgen_rec(this, tmp, unit)
        class(bst), intent(in) :: this
        class(node), intent(in), pointer :: tmp
        integer, intent(in) :: unit
        if( .not. associated(tmp)) then
            return
        end if
        write (unit, '(A,I5,A,I5,A)') ' ', tmp%uid, ' [label="', tmp%uid, '"];'
        if(associated(tmp%left)) then
            write (unit, '(A,I5,A,I5,A)') ' ', tmp%uid, ' -- ', tmp%left%uid, ';'
        end if
        if(associated(tmp%right)) then
            write (unit, '(A,I5,A,I5,A)') ' ', tmp%uid, ' -- ', tmp%right%uid, ';'
        end if
        call this%dotgen_rec(tmp%left, unit)
        call this%dotgen_rec(tmp%right, unit)
    end subroutine dotgen_rec

    subroutine imprimirEnOrden(bst_c)
        class(bst), intent(in) :: bst_c
        ! Si el árbol no está vacío, imprimir recursivamente
        if (associated(bst_c%root)) then
            call imprimirRecursivo(bst_c%root)
        end if
    end subroutine imprimirEnOrden

    ! Subrutina recursiva para imprimir los nodos en orden
    recursive subroutine imprimirRecursivo(node_bst)
        type(node), pointer, intent(in) :: node_bst
        
        if (associated(node_bst)) then
            ! Imprimir subárbol izquierdo
            call imprimirRecursivo(node_bst%left)
            ! Imprimir clave actual del nodo
            !print *, "Clave (VEGETA SI):", node_bst%uid
            call node_bst%newMatrix%print()
            ! Imprimir subárbol derecho
            call imprimirRecursivo(node_bst%right)
        end if
    end subroutine imprimirRecursivo


    ! LO ACABO DE AGREGAR
    function buscarNodo_r(arbol, uid) result(capaEncontrada)
        class(bst), intent(in) :: arbol
        integer, intent(in) :: uid
        type(matrix) :: capaEncontrada
        type(node), pointer :: nodoEncontrado

        nodoEncontrado => buscarRecursivo_r(arbol%root, uid)
        if (associated(nodoEncontrado)) then
            capaEncontrada = nodoEncontrado%newMatrix
        end if

    end function buscarNodo_r

    ! Subrutina recursiva para buscar un nodo
    recursive function buscarRecursivo_r(nodo, key) result(nodoEncontrado)
        type(node), pointer, intent(in) :: nodo
        integer, intent(in) :: key
        type(node), pointer :: nodoEncontrado

        ! Si el nodo actual es nulo, no se encontró el nodo buscado
        if (.not. associated(nodo)) then
            nodoEncontrado => null()
            return
        end if

        ! Si la clave buscada es menor que la clave del nodo actual, buscar en el subárbol izquierdo
        if (key < nodo%uid) then
            nodoEncontrado => buscarRecursivo_r(nodo%left, key)
        ! Si la clave buscada es mayor que la clave del nodo actual, buscar en el subárbol derecho
        else if (key > nodo%uid) then
            nodoEncontrado => buscarRecursivo_r(nodo%right, key)
        ! Si la clave buscada es igual a la clave del nodo actual, hemos encontrado el nodo
        else
            nodoEncontrado => nodo
        end if
    end function buscarRecursivo_r

    subroutine buscarNodo(bst_c, uid, fila, columna, color)
        class(bst), intent(in) :: bst_c
        integer, intent(in) :: uid
        integer, intent(in) :: fila
        integer, intent(in) :: columna
        character(len=7), intent(in) :: color
    
        if (associated(bst_c%root)) then
            call buscarNodoRecursivo(bst_c%root, uid, fila, columna, color)
        else
            print *, "El árbol está vacío."
        end if
    end subroutine buscarNodo
    
    recursive subroutine buscarNodoRecursivo(node_bst, target_uid, fila, columna, color)
        type(node), pointer, intent(in) :: node_bst
        integer, intent(in) :: target_uid
        integer, intent(in) :: fila
        integer, intent(in) :: columna
        character(len=7), intent(in) :: color
    
        if (associated(node_bst)) then
            if (node_bst%uid == target_uid) then
                ! Nodo encontrado, imprimir información
                print *, "Nodo encontrado:"
                call node_bst%newMatrix%insert(fila, columna, color)
                !print *, "Clave (GOKU SI):", node_bst%uid
                ! call node_bst%newMatrix%print()
            elseif (target_uid < node_bst%uid) then
                ! Si la clave objetivo es menor, buscar en el subárbol izquierdo
                call buscarNodoRecursivo(node_bst%left, target_uid, fila, columna, color)
            else
                ! Si la clave objetivo es mayor, buscar en el subárbol derecho
                call buscarNodoRecursivo(node_bst%right, target_uid, fila, columna, color)
            end if
        else
            ! Nodo no encontrado
            print *, "Nodo no encontrado con la clave:", target_uid
        end if
    end subroutine buscarNodoRecursivo
    
    ! Subrutina para recorrer Amplitud
    subroutine search_amplitude(arbol,matriz)
        class(bst), intent(in) :: arbol 
        type(matrix), intent(inout) :: matriz 
        integer :: alturaArbol, nivel
        alturaArbol = arbol%calcularProfundidad()
        ! Aqui se recorre el arbol por niveles
        do nivel = 0, alturaArbol
        call seacrch_level(arbol%root, nivel, matriz)
        end do
    end subroutine search_amplitude

    ! Subrutina recursiva para recorrer el arbol por niveles
    recursive subroutine seacrch_level(tmp, nivel, sparse_matrix)
        
        class(node), intent(in), pointer :: tmp
        type(matrix), intent(inout) :: sparse_matrix
        type(node_matrix), pointer :: actual_node, actual_row
        integer, intent(in) :: nivel
        if (.not. associated (tmp)) then
        return
        end if
        if (nivel == 0) then 
            print *, "Clave:" , tmp%uid
            actual_row => tmp%newMatrix%root%down
            do while (associated (actual_row))
                actual_node => actual_row%right
                    do while(associated(actual_node))
                        call sparse_matrix%insert(actual_node%i, actual_node%j, actual_node%color)
                        actual_node => actual_node%right
                    end do
                    actual_row => actual_row%down
                end do
            else
            call seacrch_level(tmp%left, nivel-1, sparse_matrix)
            call seacrch_level(tmp%right, nivel-1, sparse_matrix)
        end if
    end subroutine seacrch_level

    subroutine inicializar_arbol_vacio(self)
        class(bst), intent(inout) :: self
        
        allocate(self%root)
        self%root => null()
        
    end subroutine inicializar_arbol_vacio

    function calcularProfundidad(this) result(height)
        class(bst) :: this
        integer :: height
        height = profundidadRecursiva(this%root)
        print *, "La profundidad del árbol es:", height
    end function calcularProfundidad

    recursive function profundidadRecursiva(nodo_m) result(profundidadNodo)
        type(node), pointer, intent(in) :: nodo_m
        integer :: profundidadIzquierda, profundidadDerecha, profundidadNodo
    
        if (.not. associated(nodo_m)) then
            profundidadNodo = 0
            return
        end if

        profundidadIzquierda = profundidadRecursiva(nodo_m%left)
        profundidadDerecha = profundidadRecursiva(nodo_m%right)
        profundidadNodo = 1 + max(profundidadIzquierda, profundidadDerecha)
    end function profundidadRecursiva

    subroutine imprimirHojas(bst_c)
        class(bst), intent(in) :: bst_c
    
        ! Comenzar la impresión de las hojas recursivamente
        call imprimirHojasRecursivo(bst_c%root)
    end subroutine imprimirHojas
    
    recursive subroutine imprimirHojasRecursivo(nodo)
        type(node), pointer :: nodo
    
        if (associated(nodo)) then
            ! Si el nodo es una hoja, imprimir su contenido
            if (.not. (associated(nodo%left) .or. associated(nodo%right))) then
                print *, nodo%uid
            else
                ! Si no es una hoja, continuar la búsqueda en los subárboles
                call imprimirHojasRecursivo(nodo%left)
                call imprimirHojasRecursivo(nodo%right)
            end if
        end if
    end subroutine imprimirHojasRecursivo
    
    ! Subrutina para agregar id de una o varias capas a graficar y se buscarán en el árbol binario 
    !correspondiente para luego generar su imagen apilando dichas capas

    subroutine stack_layers(this, id, matrix_s)
        class(bst), intent(inout) :: this
        integer, intent(in) :: id
        type(matrix), intent(inout) :: matrix_s
        type(node), pointer :: tmp
        if(associated(this%root)) then
            call stack_layers_rec(this%root, id, matrix_s)
        else
            print *, "El árbol está vacío."
        end if
    end subroutine stack_layers

    recursive subroutine stack_layers_rec(root_node, id, matrix_s)
        integer, intent(in) :: id
        type(node), pointer, intent(in) :: root_node
        type(matrix), intent(inout) :: matrix_s
        type(node_matrix), pointer :: actual_node, actual_row
        if (associated(root_node)) then
            if (root_node%uid == id) then
                ! Nodo encontrado, imprimir información
                print *, "Nodo encontrado:"
                actual_row => root_node%newMatrix%root%down
                do while (associated(actual_row))
                    actual_node => actual_row%right
                    do while (associated(actual_node))
                        call matrix_s%insert(actual_node%i, actual_node%j, actual_node%color)
                        actual_node => actual_node%right
                    end do
                    actual_row => actual_row%down
                end do
                !call matrix_s%html_matrix("pruebaMatriz.html")
            elseif (id < root_node%uid) then
                ! Si la clave objetivo es menor, buscar en el subárbol izquierdo
                call stack_layers_rec(root_node%left, id, matrix_s)
            else
                ! Si la clave objetivo es mayor, buscar en el subárbol derecho
                call stack_layers_rec(root_node%right, id, matrix_s)
            end if
        else
            ! Nodo no encontrado
            print *, "Nodo no encontrado con la clave:", id
        end if
    end subroutine stack_layers_rec

    ! subrutina para graficar una capa
    subroutine graficarCapa(arbol, id_layer, filename)
        class(bst), intent(in) :: arbol
        integer, intent(in) :: id_layer
        character(len=*), intent(in) :: filename
        type(matrix) :: capaEncontrada
        type(node), pointer :: nodoEncontrado

        ! Buscar la capa en el árbol
        capaEncontrada = arbol%buscarNodo_r(id_layer)
        ! Generar la imagen de la capa
        !call capaEncontrada%print()
        print *, "Graficando capa..."
        call capaEncontrada%html_matrix(filename)
        !call capaEncontrada%dot_matrix(filename)
    end subroutine graficarCapa

    ! grafica solo con unit
    subroutine dotgen_unit(this, tmp, unit)
        class(bst), intent(in) :: this
        class(node), intent(in), pointer :: tmp
        integer, intent(inout) :: unit
        write(unit, '(A)') 'subgraph Layer{'
        call this%dotgen_rec(tmp, unit)
        write(unit, '(A)') '}'
    end subroutine dotgen_unit


end module bstdef_t