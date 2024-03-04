module linked_list_m
    implicit none
    private

    type, public :: node
        private
        integer :: id, img_grande, img_peque
        character(:), allocatable ::  name
        type(node), pointer :: next     
    end type node

    type, public :: linked_list
        private
        type(node), pointer :: head => null()
    contains
        procedure :: push
        procedure :: append
        procedure :: print
        procedure :: graficar
        procedure :: count_list
        procedure :: obtener_primer_cliente
        procedure :: eliminar_cliente
        procedure :: obtener_y_eliminar_primer_cliente
        procedure :: obtener_ultimo_id
        procedure :: pop
        procedure :: top_clientes_img_grande
        procedure :: top_clientes_img_peque
        procedure :: graficar_tops_clientes
    end type linked_list

contains

    subroutine push(this, id, name, img_grande, img_peque)
        class(linked_list), intent(inout) :: this
        integer, intent(in) :: id, img_grande, img_peque
        character(len=*), intent(in):: name
        

        type(node), pointer :: temp
        allocate(temp)
        temp%id = id
        temp%name = name
        temp%img_grande = img_grande
        temp%img_peque = img_peque
        temp%next => null()

        if (.not. associated(this%head)) then
            this%head => temp
        else
            temp%next => this%head
            this%head => temp
        end if

    end subroutine push

    subroutine append(this, id, name, img_grande, img_peque)
        class(linked_list), intent(inout) :: this
        integer, intent(in) :: id, img_grande, img_peque
        character(len=*), intent(in) :: name
    
        type(node), pointer :: temp
        type(node), pointer :: current
    
        ! Verificar si el cliente ya existe
        if (associated(this%head)) then
            current => this%head
            do while (associated(current%next))
                if (current%id == id) then
                    print *, 'El cliente con ID ', id, ' ya existe en la lista.'
                    return
                end if
                current => current%next
            end do
            if (current%id == id) then
                print *, 'El cliente con ID ', id, ' ya existe en la lista.'
                return
            end if
        end if
    
        ! Si el cliente no existe, se agrega a la lista
        allocate(temp)
        temp%id = id
        temp%name = name
        temp%img_grande = img_grande
        temp%img_peque = img_peque
        temp%next => null()
    
        if (.not. associated(this%head)) then
            this%head => temp
        else
            current => this%head
            do while (associated(current%next))
                current => current%next
            end do
            current%next => temp
        end if
    end subroutine append
    

    subroutine print(this)
        class(linked_list), intent(in) :: this
        type(node), pointer :: current
    
        current => this%head
    
        print '(A4, 2x, A20, 2x, A13, 2x, A13)', 'ID', 'Nombre', 'Imagen Grande', 'Imagen Pequena'
        print '(A)', repeat('-', 60)
        do while (associated(current))
            print '(I4, 2x, A20, 2x, I13, 2x, I13)', current%id, adjustl(current%name), current%img_grande, current%img_peque
            current => current%next
        end do 
    end subroutine print
    
    subroutine graficar(this, filename)
        class(linked_list), intent(in) :: this
        character(len=*), intent(in) :: filename
    
        integer :: unit
        type(node), pointer :: current
        integer :: count
    
        ! Abrir el archivo DOT
        open(unit, file=filename, status='replace')
        write(unit, *) 'digraph Pila {'
        write(unit, *) '    node [shape=box, style=filled, color=blue, fillcolor=pink];' ! Aplicar atributos a todos los nodos
        ! Escribir nodos y conexiones
        current => this%head
        count = 0
        do while (associated(current))
            count = count + 1
            write(unit, *) '    "Node', count, '" [label="', current%name, '"];'
            if (associated(current%next)) then
                write(unit, *) '    "Node', count, '" -> "Node', count+1, '";'
            end if
            current => current%next
        end do 
    
        ! Cerrar el archivo DOT
        write(unit, *) '}'
        close(unit)
    
        ! Generar el archivo PNG utilizando Graphviz
        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')
    
        print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '.png'
    end subroutine graficar

    function count_list(this) result(count)
        class(linked_list), intent(in) :: this
        type(node), pointer :: current
        integer :: count

        current => this%head
        count = 0

        do while (associated(current))
            count = count + 1
            current => current%next
        end do 
    end function count_list

    subroutine obtener_primer_cliente(this, id, name, img_grande, img_peque)
        class(linked_list), intent(inout) :: this
        integer, intent(out) :: id, img_grande, img_peque
        character(len=:), allocatable, intent(out) :: name
        type(node), pointer :: current
    
        if (.not. associated(this%head)) then
            print *, 'No hay clientes en la lista.'
            return
        end if
    
        current => this%head
        id = current%id
        name = current%name
        img_grande = current%img_grande
        img_peque = current%img_peque
    end subroutine obtener_primer_cliente
    
    subroutine eliminar_cliente(this, id)
        class(linked_list), intent(inout) :: this
        integer, intent(in) :: id
        type(node), pointer :: current, prev
    
        if (.not. associated(this%head)) then
            print *, 'La lista de clientes está vacía.'
            return
        end if
    
        current => this%head
        prev => null()
    
        do while (associated(current))
            if (current%id == id) then
                if (associated(prev)) then
                    prev%next => current%next
                else
                    this%head => current%next
                end if
                deallocate(current)
                print *, 'Cliente con ID ', id, ' eliminado de la lista.'
                return
            end if
            prev => current
            current => current%next
        end do
    
        print *, 'Cliente con ID ', id, ' no encontrado en la lista.'
    end subroutine eliminar_cliente

    subroutine obtener_y_eliminar_primer_cliente(this, id, name, img_grande, img_peque)
        class(linked_list), intent(inout) :: this
        integer, intent(out) :: id, img_grande, img_peque
        character(len=:), allocatable, intent(out) :: name
        
        call this%obtener_primer_cliente(id, name, img_grande, img_peque)
        
        print *, '-------------------------------------------------'
        print *, 'Cliente obtenido:', id, name, img_grande, img_peque
        
        call this%eliminar_cliente(id)
        
        print *, '-------------------------------------------------'
        print *, 'Cliente eliminado:', id
    end subroutine obtener_y_eliminar_primer_cliente
    

    subroutine pop(this, id, name, img_grande, img_peque)
        class(linked_list), intent(inout) :: this
        integer, intent(out) :: id, img_grande, img_peque
        character(len=:), allocatable, intent(out) :: name
        type(node), pointer :: temp

        if (.not. associated(this%head)) then
            print *, 'La cola de clientes está vacía.'
            return
        end if

        temp => this%head
        id = temp%id
        name = temp%name
        img_grande = temp%img_grande
        img_peque = temp%img_peque

        this%head => temp%next
        deallocate(temp)
    end subroutine pop

    function obtener_ultimo_id(this) result(ultimo_id)
        class(linked_list), intent(in) :: this
        type(node), pointer :: current
        integer :: ultimo_id
    
        ultimo_id = 0
        current => this%head
    
        do while (associated(current))
            if (current%id > ultimo_id) then
                ultimo_id = current%id
            end if
            current => current%next
        end do 
    end function obtener_ultimo_id
    
    subroutine top_clientes_img_grande(this)
    class(linked_list), intent(in) :: this
    type(node), pointer :: current
    integer :: max_img1, max_img2, max_img3, max_img4, max_img5
    integer :: max_id1, max_id2, max_id3, max_id4, max_id5
    character(len=:), allocatable :: max_name1, max_name2, max_name3, max_name4, max_name5
    integer :: id, img_grande
    integer :: num_clients, i

    max_id1 = 0
    max_id2 = 0
    max_id3 = 0
    max_id4 = 0
    max_id5 = 0
    max_name1 = ''
    max_name2 = ''
    max_name3 = ''
    max_name4 = ''
    max_name5 = ''
    max_img1 = 0
    max_img2 = 0
    max_img3 = 0
    max_img4 = 0
    max_img5 = 0

    num_clients = this%count_list()

    current => this%head
    do i = 1, num_clients
        if (associated(current)) then
            id = current%id
            img_grande = current%img_grande
            
            if (img_grande > max_img1) then
                max_name5 = max_name4
                max_id5 = max_id4
                max_img5 = max_img4
                max_name4 = max_name3
                max_id4 = max_id3
                max_img4 = max_img3
                max_name3 = max_name2
                max_id3 = max_id2
                max_img3 = max_img2
                max_name2 = max_name1
                max_id2 = max_id1
                max_img2 = max_img1
                max_name1 = current%name
                max_id1 = id
                max_img1 = img_grande
            else if (img_grande > max_img2) then
                max_name5 = max_name4
                max_id5 = max_id4
                max_img5 = max_img4
                max_name4 = max_name3
                max_id4 = max_id3
                max_img4 = max_img3
                max_name3 = max_name2
                max_id3 = max_id2
                max_img3 = max_img2
                max_name2 = current%name
                max_id2 = id
                max_img2 = img_grande
            else if (img_grande > max_img3) then
                max_name5 = max_name4
                max_id5 = max_id4
                max_img5 = max_img4
                max_name4 = max_name3
                max_id4 = max_id3
                max_img4 = max_img3
                max_name3 = current%name
                max_id3 = id
                max_img3 = img_grande
            else if (img_grande > max_img4) then
                max_name5 = max_name4
                max_id5 = max_id4
                max_img5 = max_img4
                max_name4 = current%name
                max_id4 = id
                max_img4 = img_grande
            else if (img_grande > max_img5) then
                max_name5 = current%name
                max_id5 = id
                max_img5 = img_grande
            end if
            
            current => current%next
        end if
    end do

    print *, 'Top 5 de clientes con mayor cantidad de imágenes grandes:'
    print *, 'Cliente ', max_id1, ': ', max_img1, ' imágenes grandes (', trim(max_name1), ')'
    print *, 'Cliente ', max_id2, ': ', max_img2, ' imágenes grandes (', trim(max_name2), ')'
    print *, 'Cliente ', max_id3, ': ', max_img3, ' imágenes grandes (', trim(max_name3), ')'
    print *, 'Cliente ', max_id4, ': ', max_img4, ' imágenes grandes (', trim(max_name4), ')'
    print *, 'Cliente ', max_id5, ': ', max_img5, ' imágenes grandes (', trim(max_name5), ')'
end subroutine top_clientes_img_grande

subroutine top_clientes_img_peque(this)
    class(linked_list), intent(in) :: this
    type(node), pointer :: current
    integer :: min_img1, min_img2, min_img3, min_img4, min_img5
    integer :: min_id1, min_id2, min_id3, min_id4, min_id5
    character(len=:), allocatable :: min_name1, min_name2, min_name3, min_name4, min_name5
    integer :: id, img_peque
    integer :: num_clients, i

    min_id1 = 0
    min_id2 = 0
    min_id3 = 0
    min_id4 = 0
    min_id5 = 0
    min_name1 = ''
    min_name2 = ''
    min_name3 = ''
    min_name4 = ''
    min_name5 = ''
    min_img1 = huge(min_img1) 
    min_img2 = huge(min_img2) 
    min_img3 = huge(min_img3) 
    min_img4 = huge(min_img4) 
    min_img5 = huge(min_img5) 

    num_clients = this%count_list()

    current => this%head
    do i = 1, num_clients
        if (associated(current)) then
            id = current%id
            img_peque = current%img_peque
            
            if (img_peque < min_img1) then
                min_name5 = min_name4
                min_id5 = min_id4
                min_img5 = min_img4
                min_name4 = min_name3
                min_id4 = min_id3
                min_img4 = min_img3
                min_name3 = min_name2
                min_id3 = min_id2
                min_img3 = min_img2
                min_name2 = min_name1
                min_id2 = min_id1
                min_img2 = min_img1
                min_name1 = current%name
                min_id1 = id
                min_img1 = img_peque
            else if (img_peque < min_img2) then
                min_name5 = min_name4
                min_id5 = min_id4
                min_img5 = min_img4
                min_name4 = min_name3
                min_id4 = min_id3
                min_img4 = min_img3
                min_name3 = min_name2
                min_id3 = min_id2
                min_img3 = min_img2
                min_name2 = current%name
                min_id2 = id
                min_img2 = img_peque
            else if (img_peque < min_img3) then
                min_name5 = min_name4
                min_id5 = min_id4
                min_img5 = min_img4
                min_name4 = min_name3
                min_id4 = min_id3
                min_img4 = min_img3
                min_name3 = current%name
                min_id3 = id
                min_img3 = img_peque
            else if (img_peque < min_img4) then
                min_name5 = min_name4
                min_id5 = min_id4
                min_img5 = min_img4
                min_name4 = current%name
                min_id4 = id
                min_img4 = img_peque
            else if (img_peque < min_img5) then
                min_name5 = current%name
                min_id5 = id
                min_img5 = img_peque
            end if
            
            current => current%next
        end if
    end do

    print *, 'Top 5 de clientes con menor cantidad de imágenes pequeñas:'
    print *, 'Cliente ', min_id1, ': ', min_img1, ' imágenes pequeñas (', trim(min_name1), ')'
    print *, 'Cliente ', min_id2, ': ', min_img2, ' imágenes pequeñas (', trim(min_name2), ')'
    print *, 'Cliente ', min_id3, ': ', min_img3, ' imágenes pequeñas (', trim(min_name3), ')'
    print *, 'Cliente ', min_id4, ': ', min_img4, ' imágenes pequeñas (', trim(min_name4), ')'
    print *, 'Cliente ', min_id5, ': ', min_img5, ' imágenes pequeñas (', trim(min_name5), ')'
end subroutine top_clientes_img_peque

subroutine graficar_tops_clientes(this, filename)
    class(linked_list), intent(in) :: this
    character(len=*), intent(in) :: filename
    
    integer :: unit

    
    ! Abrir el archivo DOT
    open(unit, file=filename, status='replace')
    write(unit, *) 'digraph TopClientes {'
    write(unit, *) '    node [shape=box, style=filled, color=blue, fillcolor=pink];' ! Aplicar atributos a todos los nodos
    
    ! Obtener top clientes con más imágenes grandes
    call this%top_clientes_img_grande()
    print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '_top_clientes_img_grande.png'

    ! Obtener top clientes con menos imágenes pequeñas
    call this%top_clientes_img_peque()
    print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '_top_clientes_img_peque.png'

    ! Cerrar el archivo DOT
    write(unit, *) '}'
    close(unit)
    
    ! Generar el archivo PNG utilizando Graphviz
    call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')

    print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '.png'
end subroutine graficar_tops_clientes


    
end module linked_list_m
