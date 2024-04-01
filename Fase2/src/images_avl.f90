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
    contains
        procedure :: add_avl
        procedure :: add_rec_avl
        procedure :: preorder_avl
        procedure :: inorder_avl
        procedure :: postorder_avl
        procedure :: srl
        procedure :: srr
        procedure :: drl
        procedure :: drr
        procedure :: getheight
        procedure :: getmax
        procedure :: dotgen_avl
        procedure :: dotgen_rec_avl
        procedure :: buscarNodo_avl
        procedure :: add_layer_to_image
        procedure :: delete_image
        procedure :: delete_avl
        procedure :: search_image_node
        procedure :: add_avl_tree
        procedure :: dotgen_avl_bst
        procedure :: dotgen_rec_avl_bst
    end type avl

contains

    ! Subrutina para agregar un valor al árbol
    subroutine add_avl(this, value)
        class(avl), intent(inout) :: this
        integer, intent(in) :: value
        type(node_avl), pointer :: tmp
        if(associated(this%root)) then
            call this%add_rec_avl(value, this%root)
        else
            allocate(tmp)
            tmp%value = value
            tmp%height = 0
            this%root => tmp
        end if
    end subroutine add_avl

    subroutine add_avl_tree(this, value, tree_bst)
        class(avl), intent(inout) :: this
        type(bst), intent(in) :: tree_bst
        integer, intent(in) :: value
        type(node_avl), pointer :: tmp
        if(associated(this%root)) then
            call this%add_rec_avl(value, this%root)
        else
            allocate(tmp)
            tmp%value = value
            tmp%bst_tree = tree_bst
            tmp%height = 0
            this%root => tmp
        end if
    end subroutine add_avl_tree

    ! Subrutina recursiva para agregar un valor al árbol
    subroutine add_rec_avl(this, value, tmp)
        class(avl), intent(inout) :: this
        integer, intent(in) :: value
        type(node_avl), pointer, intent(inout) :: tmp
        integer :: r, l, m
        if (.not. associated(tmp)) then
            allocate(tmp)
            tmp%value = value
            tmp%height = 0
        else if (value < tmp%value) then
            call this%add_rec_avl(value, tmp%left)
            if ((this%getheight(tmp%left) - this%getheight(tmp%right))==2) then
                if (value < tmp%left%value) then
                    tmp => this%srl(tmp)
                else
                    tmp => this%drl(tmp)
                end if
            end if
        else
            call this%add_rec_avl(value, tmp%right)
            if ((this%getheight(tmp%right) - this%getheight(tmp%left))==2) then
                if (value > tmp%right%value) then
                    tmp => this%srr(tmp)
                else
                    tmp => this%drr(tmp)
                end if
            end if
        end if
        r = this%getheight(tmp%right)
        l = this%getheight(tmp%left)
        m = this%getmax(r, l)
        tmp%height = m + 1		
    end subroutine add_rec_avl

    ! Función para obtener la altura de un nodo
    integer function getheight(this, tmp)
        class(avl), intent(in) :: this
        type(node_avl), intent(in), pointer :: tmp
        if (.not. associated(tmp)) then
            getheight = -1
        else
            getheight = tmp%height 
        end if
    end function getheight

    ! Función para realizar una rotación simple a la izquierda
    function srl(this, t1) result(t2)
        class(avl), intent(in) :: this
        type(node_avl), intent(in), pointer :: t1
        type(node_avl), pointer :: t2 
        t2 => t1%left
        t1%left => t2%right
        t2%right => t1
        t1%height = this%getmax(this%getheight(t1%left), this%getheight(t1%right))+1
        t2%height = this%getmax(this%getheight(t2%left), t1%height)+1
    end function srl

    ! Función para realizar una rotación simple a la derecha
    function srr(this, t1) result(t2)
        class(avl), intent(in) :: this
        type(node_avl), intent(in), pointer :: t1
        type(node_avl), pointer :: t2 
        t2 => t1%right
        t1%right => t2%left
        t2%left => t1
        t1%height = this%getmax(this%getheight(t1%left), this%getheight(t1%right))+1
        t2%height = this%getmax(this%getheight(t2%right), t1%height)+1
    end function srr

    ! Función para realizar una doble rotación a la izquierda
    function drl(this, tmp) result(res)
        class(avl), intent(in) :: this
        type(node_avl), intent(in), pointer :: tmp
        type(node_avl), pointer :: res
        tmp%left => this%srr(tmp%left)
        res => this%srl(tmp)
    end function drl

    ! Función para realizar una doble rotación a la derecha
    function drr(this, tmp) result(res)
        class(avl), intent(in) :: this
        type(node_avl), intent(in), pointer :: tmp
        type(node_avl), pointer :: res
        tmp%right => this%srl(tmp%right)
        res => this%srr(tmp)
    end function drr

    ! Función para obtener el máximo entre dos valores
    integer function getmax(this, val1, val2)
        class(avl), intent(in) :: this
        integer, intent(in) :: val1, val2
        getmax = merge(val1, val2, val1 > val2)
    end function getmax

    ! Subrutina para recorrer el árbol en preorden
    subroutine preorder_avl(this, tmp)
        class(avl), intent(in) :: this
        type(node_avl), intent(in), pointer :: tmp
        if( .not. associated(tmp)) then
            return
        end if
        write (*, '(1I3)', advance='no') (tmp%value)
        call this%preorder_avl(tmp%left)
        call this%preorder_avl(tmp%right)
    end subroutine preorder_avl

    ! Subrutina para recorrer el árbol en orden
    subroutine inorder_avl(this, tmp)
        class(avl), intent(in) :: this
        type(node_avl), intent(in), pointer :: tmp
        if( .not. associated(tmp)) then
            return
        end if
        call this%inorder_avl(tmp%left)
        write (*, '(1I3)', advance='no') (tmp%value)
        call this%inorder_avl(tmp%right)
    end subroutine inorder_avl

    ! Subrutina para recorrer el árbol en postorden
    subroutine postorder_avl(this, tmp)
        class(avl), intent(in) :: this
        type(node_avl), intent(in), pointer :: tmp
        if( .not. associated(tmp)) then
            return
        end if
        call this%postorder_avl(tmp%left)
        call this%postorder_avl(tmp%right)
        write (*, '(1I3)', advance='no') (tmp%value)
    end subroutine postorder_avl

    ! Subrutina para generar un archivo DOT del árbol
    subroutine dotgen_avl(this, tmp, filename)
        class(avl), intent(in) :: this
        class(node_avl), intent(in), pointer :: tmp
        character(len=*), intent(in) :: filename
        integer :: unit
        open(unit, file=filename, status='replace')
        write(unit, '(A)') 'graph{'
        write(unit, '(A)') 'node [shape=box];'
        call this%dotgen_rec_avl(tmp, unit)
        write(unit, '(A)') '}'
        close(unit)
        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')
        call system('start ' // trim(adjustl(filename)) // '.png')
    end subroutine dotgen_avl

    ! Subrutina recursiva para generar un archivo DOT del árbol
    subroutine dotgen_rec_avl(this, tmp, unit)
        class(avl), intent(in) :: this
        class(node_avl), intent(in), pointer :: tmp
        integer, intent(in) :: unit
        if( .not. associated(tmp)) then
            return
        end if
        write (unit, '(A,I5,A,I5,A)') ' ', tmp%value, ' [label="', tmp%value, '"];'
        if(associated(tmp%left)) then
            write (unit, '(A,I5,A,I5,A)') ' ', tmp%value, ' -- ', tmp%left%value, ';'
        end if
        if(associated(tmp%right)) then
            write (unit, '(A,I5,A,I5,A)') ' ', tmp%value, ' -- ', tmp%right%value, ';'
        end if
        call this%dotgen_rec_avl(tmp%left, unit)
        call this%dotgen_rec_avl(tmp%right, unit)
    end subroutine dotgen_rec_avl

    subroutine buscarNodo_avl(avl_c, id, sparse_matrix)
        class(avl), intent(in) :: avl_c
        type(matrix), intent(in) :: sparse_matrix
        integer, intent(in) :: id
    
        if (associated(avl_c%root)) then
            call buscarNodoRecursivo_avl(avl_c%root, id, sparse_matrix)
        else
            print *, "El árbol está vacío."
        end if
    end subroutine buscarNodo_avl
    
    recursive subroutine buscarNodoRecursivo_avl(avl_node, id, sparse_matrix)
        type(matrix), intent(in) :: sparse_matrix
        integer, intent(in) :: id
        type(node_avl), pointer :: avl_node
    
        if (associated(avl_node)) then
            if (avl_node%value == id) then
                ! Nodo encontrado, imprimir información
                print *, "Nodo encontrado:"
                call avl_node%bst_tree%add_matrix(id, sparse_matrix)
                print *, "Clave (GOHAN SI):", avl_node%value
            elseif (id < avl_node%value) then
                ! Si la clave objetivo es menor, buscar en el subárbol izquierdo
                call buscarNodoRecursivo_avl(avl_node%left, id, sparse_matrix)
            else
                ! Si la clave objetivo es mayor, buscar en el subárbol derecho
                call buscarNodoRecursivo_avl(avl_node%right, id, sparse_matrix)
            end if
        else
            ! Nodo no encontrado
            print *, "Nodo no encontrado con la clave:", id
        end if
    end subroutine buscarNodoRecursivo_avl

    recursive function find_image_node(this, id_img) result(img_node)
    class(avl), intent(in) :: this
    integer, intent(in) :: id_img
    type(node_avl), pointer :: img_node
    
    if (associated(this%root)) then
        img_node => this%search_image_node(this%root, id_img)
    else
        img_node => null()
    end if
end function find_image_node

subroutine add_layer_to_image(this, id_img, id_layer)
    class(avl), intent(inout) :: this
    integer, intent(in) :: id_img
    integer, intent(in) :: id_layer
    type(node_avl), pointer :: img_node

    ! Buscar el nodo de la imagen con el ID correspondiente
    img_node => this%search_image_node(this%root, id_img)

    if (associated(img_node)) then
        ! Agregar la capa al árbol BST asociado con la imagen
        call img_node%bst_tree%add(id_layer)
    else
        print *, "Error: No se encontró la imagen con el ID:", id_img
    end if
end subroutine add_layer_to_image


recursive function search_image_node(this, avl_node, id_img) result(img_node)
    class(avl), intent(in) :: this
    type(node_avl), intent(in), pointer :: avl_node
    integer, intent(in) :: id_img
    type(node_avl), pointer :: img_node

    if (associated(avl_node)) then
        if (avl_node%value == id_img) then
            img_node => avl_node
            
        elseif (id_img < avl_node%value) then
            img_node => this%search_image_node(avl_node%left, id_img)
        else
            img_node => this%search_image_node(avl_node%right, id_img)
        end if
    else
        img_node => null()
    end if
end function search_image_node


subroutine delete_image(this, id_img)
    class(avl), intent(inout) :: this
    integer, intent(in) :: id_img

    ! Buscar y eliminar el nodo de la imagen con el ID correspondiente
    call this%delete_avl(this%root, id_img)
end subroutine delete_image

recursive subroutine delete_avl(this, tmp, id_img)
    class(avl), intent(inout) :: this
    type(node_avl), pointer, intent(inout) :: tmp
    integer, intent(in) :: id_img
    type(node_avl), pointer :: t

    if (.not. associated(tmp)) then
        print *, "Error: No se encontró la imagen con el ID:", id_img
    else if (id_img < tmp%value) then
        call this%delete_avl(tmp%left, id_img)
    else if (id_img > tmp%value) then
        call this%delete_avl(tmp%right, id_img)
    else if (associated(tmp%left) .and. associated(tmp%right)) then
        t => find_min(tmp%right)
        tmp%value = t%value
        call this%delete_avl(tmp%right, t%value)
    else
        t => tmp
        if (.not. associated(tmp%left)) then
            tmp => tmp%right
        else if (.not. associated(tmp%right)) then
            tmp => tmp%left
        end if
        deallocate(t)
    end if
end subroutine delete_avl

function find_min(tmp) result(min_node)
    type(node_avl), intent(in), pointer :: tmp
    type(node_avl), pointer :: min_node

    min_node => tmp
    if (associated(tmp)) then
        do while (associated(min_node%left))
            min_node => min_node%left
        end do
    end if
end function find_min


! Graficar el avl con una capa de imagen
subroutine dotgen_avl_bst(this, tmp, id ,filename)
    class(avl), intent(in) :: this
    class(node_avl), intent(in), pointer :: tmp
    character(len=*), intent(in) :: filename
    integer :: unit, id
    open(unit, file=filename, status='replace')
    write(unit, '(A)') 'graph Node{'
    write(unit, '(A)') 'node [shape=box];'
    call this%dotgen_rec_avl_bst(tmp, id, unit)
    write(unit, '(A)') '}'
    close(unit)
    call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')
    call system('start ' // trim(adjustl(filename)) // '.png')
end subroutine dotgen_avl_bst

! Subrutina recursiva para generar un archivo DOT del árbol
subroutine dotgen_rec_avl_bst(this, tmp, id, unit)
    class(avl), intent(in) :: this
    class(node_avl), intent(in), pointer :: tmp
    integer, intent(inout) :: unit, id

    if( .not. associated(tmp)) then

        return
    end if
    write (unit, '(A,I5,A,I5,A)') '"Cell ', tmp%value, '" [label="', tmp%value, '"];'
    if(tmp%value == id) then
        call tmp%bst_tree%dotgen_unit(tmp%bst_tree%root, unit)
    end if
    if(associated(tmp%left)) then
        write (unit, '(A,I5,A,A,I5,A)') '"Cell ', tmp%value, '" -- ', '"Cell ', tmp%left%value, '";'
    end if
    if(associated(tmp%right)) then
        write (unit, '(A,I5,A,A,I5,A)') '"Cell ', tmp%value, '" -- ', '"Cell ', tmp%right%value, '";'
    end if
    call this%dotgen_rec_avl_bst(tmp%left, id, unit)
    call this%dotgen_rec_avl_bst(tmp%right, id, unit)
end subroutine dotgen_rec_avl_bst


end module avldef_tree
