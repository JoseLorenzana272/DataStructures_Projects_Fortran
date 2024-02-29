module lista_ventanillas_module
    use image_stack_module
    implicit none
    private
    public :: lista_ventanillas, nodo_ventanilla
    
    type, public :: nodo_ventanilla
        integer :: id
        integer :: id_cliente
        type(nodo_ventanilla), pointer :: next => null()
        type(image_stack) :: imagenes
    end type nodo_ventanilla

    type, public :: lista_ventanillas
        type(nodo_ventanilla), pointer :: head => null()
        type(nodo_ventanilla), pointer :: next_available => null()
        integer :: last_assigned_id
    contains
        procedure :: append_ventanilla
        procedure :: print_ventanillas
        procedure :: asignar_ventanilla
        procedure :: crear_ventanillas
        !procedure :: append_image
        !procedure :: agregar_imagenes
    end type lista_ventanillas

contains

subroutine append_ventanilla(self, id)
    class(lista_ventanillas), intent(inout) :: self
    integer, intent(in) :: id
    type(nodo_ventanilla), pointer :: new_node

    allocate(new_node)
    new_node%id = id
    call new_node%imagenes%init_stack()

    new_node%next => null()

    if (associated(self%head)) then
        new_node%next => self%head
    end if
    self%head => new_node
end subroutine append_ventanilla


    subroutine print_ventanillas(self)
        class(lista_ventanillas), intent(in) :: self
        type(nodo_ventanilla), pointer :: current

        print *, 'Ventanillas:'
        current => self%head
        do while (associated(current))
            print *, 'Ventanilla ', current%id
            current => current%next
        end do
    end subroutine print_ventanillas

    subroutine asignar_ventanilla(self, id_cliente, img_grande, img_peque)
        class(lista_ventanillas), intent(inout) :: self
        integer, intent(in) :: id_cliente, img_grande, img_peque
        type(nodo_ventanilla), pointer :: current
        integer :: i
    
        if (.not. associated(self%next_available)) then
            print *, 'No hay ventanillas disponibles.'
            return
        end if
    
        current => self%next_available
        current%id_cliente = id_cliente
        self%next_available => current%next
        if (.not. associated(self%next_available)) then
            self%next_available => self%head  
        end if
        self%last_assigned_id = current%id  
    
        do i = 1, img_grande + img_peque
            call current%imagenes%push(i) 
            print *, '-----------------------------------------------------'
            print *, 'Imagen ', i, ' agregada a la ventanilla ', current%id
            print *, '-----------------------------------------------------'
        end do
    
        print *, 'Ventanilla ', current%id, ' asignada'
    end subroutine asignar_ventanilla
    

    ! ESTA SI SIRVE
    subroutine crear_ventanillas(self, n)
        class(lista_ventanillas), intent(inout) :: self
        integer, intent(in) :: n
        integer :: i
        type(nodo_ventanilla), pointer :: current, prev
    
        do i = 1, n
            allocate(current)
            current%id = i
            call current%imagenes%init_stack()
            current%next => null()
    
            if (.not. associated(self%head)) then
                self%head => current
            else
                prev%next => current
            end if
            prev => current
    
            print *, 'Ventanilla ', i, ' creada'
    
            ! Establecer la primera ventanilla disponible
            if (i == 1) then
                self%next_available => current
            end if
        end do
    end subroutine crear_ventanillas

    !subroutine append_image(self, id, image)
     !   class(lista_ventanillas), intent(inout) :: self
      !  integer, intent(in) :: id
       ! type(image_stack), intent(in) :: image
        !type(nodo_ventanilla), pointer :: current

        !current => self%head
        !do while (associated(current))
        !    if (current%id == id) then
         !       call append_image_stack(current%imagenes, image)
         !       print *, 'Imagen agregada a la ventanilla ', id
         !       return
         !   end if
        !    current => current%next
       ! end do
    !end subroutine append_image


    ! DE PRUEBAAAAAA

    !subroutine agregar_imagenes(self)
    !    class(lista_ventanillas), intent(inout) :: self
    !    type(nodo_ventanilla), pointer :: current
    !    integer :: i, num_imagenes
    !
    !    current => self%head
    !    do while (associated(current))
    !        num_imagenes = current%imagenes%get_size()
    !        do i = 1, num_imagenes
    !            call current%imagenes%push(i)
    !            print *, '-----------------------------------------------------'
    !            print *, 'Imagen ', i, ' agregada a la ventanilla ', current%id
    !            print *, '-----------------------------------------------------'
    !        end do
    !        current => current%next
    !        if (current == self%head) exit
    !    end do
    !end subroutine agregar_imagenes
    
    
    


end module lista_ventanillas_module
