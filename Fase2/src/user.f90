
module BTree
    use bstdef_t
    use avldef_tree
    use AlbumModule
    implicit none

      ! Order 5
    integer, parameter :: MAXI = 4, MINI = 2

    type nodeptr
        type (BTreeNode), pointer :: ptr => null()
    end type nodeptr

    type clientes
        integer(8) :: dpi
        character(len=:), allocatable :: user_name
        character(len=:), allocatable :: password_client
        type(bst) :: bst_user
        type(avl) :: avl_user
        type(album) :: album_user
        integer :: layer_counter = 0
        integer :: image_counter = 0
    end type clientes

    type BTreeNode
        type(clientes) :: val(0:MAXI+1)
        integer :: num = 0
        type(nodeptr) :: link(0:MAXI+1)

    end type BTreeNode

    

    type Btree_class
        type(BTreeNode), pointer :: root => null()
    contains
        procedure :: insert
        procedure :: setValue
        procedure :: insertNode
        procedure :: splitNode
        procedure :: createNode
        procedure :: traversal
        procedure :: dotgen_btree
        procedure :: dotgen_rec_btree
        procedure :: search_user
        procedure :: register_clients
        procedure :: delete_client
        procedure :: search_client_by_dpi
        procedure :: search_username
        procedure :: search_trees
    end type Btree_class

    
contains

subroutine insert(this, val)
    class(Btree_class), intent(inout) :: this
    type(clientes), intent(in) :: val
    type(clientes) :: i
    type(BTreeNode), pointer :: child
    allocate(child)
    if (this%setValue(val, i, this%root, child)) then
            this%root => this%createNode(i, child)
    end if
end subroutine insert

recursive function setValue(this, val, pval, node, child) result(res)
    class(Btree_class), intent(in) :: this
    type(clientes), intent(in) :: val
    type(clientes), intent(inout) :: pval
    type(BTreeNode), pointer, intent(inout) :: node
    type(BTreeNode), pointer, intent(inout) :: child
    type(BTreeNode), pointer :: newnode        
    integer :: pos
    logical :: res
    allocate(newnode)
    if (.not. associated(node)) then            
            pval = val
            child => null()
            res = .true.
            return
    end if
    if (val%dpi < node%val(1)%dpi) then
            pos = 0
    else
            pos = node%num
            do while (val%dpi < node%val(pos)%dpi .and. pos > 1) 
            pos = pos - 1
            end do
            if (val%dpi == node%val(pos)%dpi) then
                print *, "Clientes duplicados no están permitidos"
                res = .false.
                return
            end if
    end if
    if (this%setValue(val, pval, node%link(pos)%ptr, child)) then
            if (node%num < MAXI) then
                call this%insertNode(pval, pos, node, child)
            else
                call this%splitNode(pval, pval, pos, node, child, newnode)
                child => newnode
                res = .true.
            return
        end if
    end if
    res = .false.
end function setValue

subroutine insertNode(this, val, pos, node, child)
    class(Btree_class), intent(in) :: this
    type(clientes), intent(in) :: val
    integer, intent(in) :: pos
    type(BTreeNode), pointer, intent(inout) :: node
    type(BTreeNode), pointer, intent(in) :: child
    integer :: j
    j = node%num
    do while (j > pos)
            node%val(j + 1) = node%val(j)
            node%link(j + 1)%ptr => node%link(j)%ptr
            j = j - 1
    end do
    node%val(j + 1) = val
    node%link(j + 1)%ptr => child
    node%num = node%num + 1
end subroutine insertNode

subroutine splitNode(this, val, pval, pos, node, child, newnode)
    class(Btree_class), intent(in) :: this
    type(clientes), intent(in) :: val
    integer, intent(in) :: pos
    type(clientes), intent(inout) :: pval
    type(BTreeNode), pointer, intent(inout) :: node,  newnode
    type(BTreeNode), pointer, intent(in) ::  child
    integer :: median, i, j
    if (pos > MINI) then
            median = MINI + 1
    else
            median = MINI
    end if
    if (.not. associated(newnode)) then
        allocate(newnode)
    do i = 0, MAXI
                newnode%link(i)%ptr => null()
        enddo
    end if
    j = median + 1
    do while (j <= MAXI)
            newnode%val(j - median) = node%val(j)
            newnode%link(j - median)%ptr => node%link(j)%ptr
            j = j + 1
    end do
    node%num = median
    newnode%num = MAXI - median
    if (pos <= MINI) then
            call this%insertNode(val, pos, node, child)
    else
            call this%insertNode(val, pos - median, newnode, child)
    end if        
    pval = node%val(node%num)        
    newnode%link(0)%ptr => node%link(node%num)%ptr
    node%num = node%num - 1
end subroutine splitNode

function createNode(this, val, child) result(newNode)
    class(Btree_class), intent(in) :: this
    type(clientes), intent(in) :: val
    type(BTreeNode), pointer, intent(in) :: child
    type(BTreeNode), pointer :: newNode
    integer :: i
    allocate(newNode)
    newNode%val(1) = val
    newNode%num = 1
    newNode%link(0)%ptr => this%root
    newNode%link(1)%ptr => child
    do i = 2, MAXI
            newNode%link(i)%ptr => null()
    end do
end function createNode

recursive subroutine traversal(this, myNode)
    class(Btree_class), intent(in) :: this
    type(BTreeNode), pointer, intent(in) :: myNode
    integer :: i
    if (associated(myNode)) then
            write (*, '(A)', advance='no') ' [ '
            i = 0
            do while (i < myNode%num)
                write (*,'(I0, A)', advance='no') myNode%val(i+1)%dpi, ' '
                write (*, '(A, A)', advance='no') myNode%val(i+1)%user_name, ' '
                write (*, '(A, A)', advance='no') myNode%val(i+1)%password_client, ' '
                i = i + 1
            end do
            do i = 0, myNode%num
                call this%traversal(myNode%link(i)%ptr)    
            end do
            write (*, '(A)', advance='no') ' ] '
    end if
end subroutine traversal

subroutine dotgen_btree(this, tmp, filename)
    class(Btree_class), intent(in) :: this
    type(BTreeNode), pointer, intent(in) :: tmp
    character(len=*), intent(in) :: filename
    integer :: unit
    integer :: node_counter = 0
    open(newunit=unit, file=filename, status='replace')
    write(unit, '(A)') 'digraph BTree {'
    write(unit, '(A)') 'node [shape = record,height=.1];'
    call this%dotgen_rec_btree(tmp, unit)
    write(unit, '(A)') '}'
    close(unit)
    call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')
    call system('start ' // trim(adjustl(filename)) // '.png')
end subroutine dotgen_btree

recursive subroutine dotgen_rec_btree(this, tmp, unit)
    class(Btree_class), intent(in) :: this
    type(BTreeNode), pointer, intent(in) :: tmp
    integer, intent(in) :: unit
    integer :: i
    integer :: node_counter = 0
    integer :: node_counter2 = 1
    character(len=32) :: node_name, child_name

    if (associated(tmp)) then
        node_counter = node_counter + 1
        write(node_name, '(I0)') node_counter
        write(unit, '(A, A, A)') node_name, ' [label = "<f0>', trim(tmp%val(1)%user_name)
        do i = 2, tmp%num
            write(unit, '(A, I0, A)') ' |<f', i-1, '>', trim(tmp%val(i)%user_name)
        end do
        write(unit, '(A)') '"];'
        do i = 0, tmp%num
            if (associated(tmp%link(i)%ptr)) then
                node_counter2 = node_counter2 + 1
                write(child_name, '(I0)') node_counter2
                write(unit, '(A, A, I0, A, A)') node_name, ':f', i, ' -> ', child_name, ';'
                call this%dotgen_rec_btree(tmp%link(i)%ptr, unit)
            end if
        end do
    end if
end subroutine dotgen_rec_btree


! función para buscar recursivamente por nombre de usuario y contraseña
recursive subroutine search_user_rec(node, user_name, password_client, found)
    type(BTreeNode), pointer, intent(in) :: node
    character(len=*), intent(in) :: user_name
    character(len=*), intent(in) :: password_client
    logical, intent(inout) :: found

    integer :: i

    if (.not. associated(node)) then
        found = .false.
        return
    end if

    do i = 1, node%num
        !print *, 'Comparando ', trim(node%val(i)%user_name), ' con ', user_name
        if (node%val(i)%user_name == user_name .and. node%val(i)%password_client == password_client) then
            print *, 'Encontrado'
            found = .true.
            exit
        end if
    end do
    if (found) then
        return
    end if

    do i = 0, node%num
        call search_user_rec(node%link(i)%ptr, user_name, password_client, found)
    end do


    ! Si no se encuentra en ninguno de los nodos hijos
    
end subroutine search_user_rec

! función para buscar por nombre de usuario y contraseña
function search_user(this, user_name, password_client) result(res)
    class(Btree_class), intent(in) :: this
    character(len=*), intent(in) :: user_name
    character(len=*), intent(in) :: password_client
    type(clientes) :: i
    type(BTreeNode), pointer :: tmp
    logical :: res
    tmp => this%root
    res = .false.
    
    if (associated(tmp)) then
        call search_user_rec(tmp, user_name, password_client, res)
    end if
end function search_user

subroutine register_clients(this)
    class(Btree_class), intent(inout) :: this
    
    type(clientes) :: new_client
    integer :: dpi_input
    character(len=100) :: name_input, password_input

    print *, 'Ingrese DPI del nuevo cliente:'
    read(*, *) dpi_input
    new_client%dpi = dpi_input

    print *, 'Ingrese nombre del nuevo cliente:'
    read(*, *) name_input
    new_client%user_name = name_input

    print *, 'Ingrese contraseña del nuevo cliente:'
    read(*, *) password_input
    new_client%password_client = password_input

    call this%insert(new_client)
end subroutine register_clients

subroutine delete_client(this, dpi)
    class(Btree_class), intent(inout) :: this
    integer(8), intent(in) :: dpi

    ! Search for the client with the given DPI
    logical :: found
    found = .false.
    call delete_client_rec(this%root, dpi, found)

    if (.not. found) then
        print *, 'Cliente no encontrado.'
    endif
end subroutine delete_client

! Recursive subroutine to delete a client with the given DPI
recursive subroutine delete_client_rec(node, dpi, found)
    type(BTreeNode), pointer, intent(inout) :: node
    integer(8), intent(in) :: dpi
    logical, intent(inout) :: found

    integer :: i, pos

    if (associated(node)) then
        ! Find the position of DPI in the node
        pos = 0
        do i = 1, node%num
            if (node%val(i)%dpi == dpi) then
                pos = i
                exit
            endif
        end do

        if (pos /= 0) then
            ! Found the DPI, delete it
            found = .true.
            do i = pos, node%num - 1
                node%val(i) = node%val(i + 1)
            end do
            node%num = node%num - 1
            if (found) then
                return
            end if
        else
            ! DPI not found, continue searching in child nodes
            do i = 1, node%num + 1
                call delete_client_rec(node%link(i)%ptr, dpi, found)
                if (found) then
                    return
                endif
            end do
        endif
    endif
end subroutine delete_client_rec

subroutine search_client_by_dpi(this, dpi)
    class(Btree_class), intent(in) :: this
    integer(8), intent(in) :: dpi
    integer :: total

    type(clientes) :: found_client
    logical :: found
    found = .false.

    call search_client_by_dpi_rec(this%root, dpi, found_client, found)

    if (found) then
        print *, 'Cliente encontrado:'
        print *, 'DPI:', found_client%dpi
        print *, 'Nombre:', found_client%user_name
        print *, 'Contrasena:', found_client%password_client
        print *, 'Cantidad Capas:' , found_client%layer_counter
        print *, 'Cantidad Imagenes:' , found_client%image_counter
        call found_client%album_user%count_albums(total)
        print *, 'Cantidad de albumes: ', total
    else
        print *, 'Cliente no encontrado.'
    endif
end subroutine search_client_by_dpi

recursive subroutine search_client_by_dpi_rec(node, dpi, found_client, found)
    type(BTreeNode), pointer, intent(in) :: node
    integer(8), intent(in) :: dpi
    type(clientes), intent(out) :: found_client
    logical, intent(inout) :: found

    integer :: i

    if (associated(node)) then
        ! Buscar el DPI en el nodo actual
        do i = 1, node%num
            if (node%val(i)%dpi == dpi) then
                found = .true.
                found_client = node%val(i)
                return
            endif
        end do

        ! Si no se encuentra en el nodo actual, continuar la búsqueda en los nodos hijos
        do i = 0, node%num
            call search_client_by_dpi_rec(node%link(i)%ptr, dpi, found_client, found)
            if (found) return
        end do
    endif
end subroutine search_client_by_dpi_rec

! Buscar cliente pero retornar su nombre

recursive subroutine search_username_rec(node, user_name, res)
    type(BTreeNode), pointer, intent(in) :: node
    character(len=*), intent(in) :: user_name
    type(clientes) :: res
    integer :: i

    if (.not. associated(node)) then
        return
    end if

    do i = 1, node%num
        !print *, 'Comparando ', trim(node%val(i)%user_name), ' con ', user_name
        if (node%val(i)%user_name == user_name) then
            print *, 'Encontrado'
            res = node%val(i)
            exit
        end if
    end do

    do i = 0, node%num
        call search_username_rec(node%link(i)%ptr, user_name, res)
    end do


    ! Si no se encuentra en ninguno de los nodos hijos
    
end subroutine search_username_rec

! función para buscar por nombre de usuario y contraseña
function search_username(this, user_name) result(res)
    class(Btree_class), intent(in) :: this
    character(len=*), intent(in) :: user_name
    type(clientes) :: i, res
    type(BTreeNode), pointer :: tmp
    tmp => this%root
    
    if (associated(tmp)) then
        call search_username_rec(tmp, user_name, res)
        print *, 'Nombre: ', res%user_name
    end if
end function search_username


! funcion para actualizar estructuras
subroutine search_trees(this, user_name, bst_t, avl_t, album_t, layer_counter, image_counter)
    class(Btree_class), intent(in) :: this
    character(len=*), intent(in) :: user_name
    type(clientes) :: i
    type(BTreeNode), pointer :: tmp
    type(bst), intent(in) :: bst_t
    type(avl), intent(in) :: avl_t
    type(album), intent(in) :: album_t
    integer, intent(in) :: layer_counter, image_counter
    tmp => this%root
    
    if (associated(tmp)) then
        call search_trees_rec(tmp, user_name, bst_t, avl_t, album_t, layer_counter, image_counter)
    end if
end subroutine search_trees

recursive subroutine search_trees_rec(node, user_name, bst_t, avl_t, album_t, layer_counter, image_counter)
    type(BTreeNode), pointer, intent(in) :: node
    character(len=*), intent(in) :: user_name
    integer :: i
    type(bst), intent(in) :: bst_t
    type(avl), intent(in) :: avl_t
    type(album), intent(in) :: album_t
    integer, intent(in) :: layer_counter, image_counter

    if (.not. associated(node)) then
        return
    end if

    do i = 1, node%num
        !print *, 'Comparando ', trim(node%val(i)%user_name), ' con ', user_name
        if (node%val(i)%user_name == user_name) then
            print *, 'Encontrado'
            node%val(i)%bst_user = bst_t
            node%val(i)%avl_user = avl_t
            node%val(i)%album_user = album_t
            node%val(i)%layer_counter = layer_counter
            node%val(i)%image_counter = image_counter
            exit
        end if
    end do

    do i = 0, node%num
        call search_trees_rec(node%link(i)%ptr, user_name, bst_t, avl_t, album_t, layer_counter, image_counter)
    end do


    ! Si no se encuentra en ninguno de los nodos hijos
    
end subroutine search_trees_rec

end module BTree