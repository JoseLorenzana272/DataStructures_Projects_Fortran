module image_stack_module
    implicit none
    private
    public :: image_stack, image_node

    type, public :: image_node
        integer :: id
        type(image_node), pointer :: next => null()
    end type image_node

    type, public :: image_stack
        type(image_node), pointer :: top => null()
    contains
        procedure :: push
        procedure :: pop
        procedure :: peek
        procedure :: init_stack
        !procedure :: get_size
    end type image_stack

contains

    subroutine push(self, id)
        class(image_stack), intent(inout) :: self
        integer, intent(in) :: id
        type(image_node), pointer :: new_node

        allocate(new_node)
        new_node%id = id
        new_node%next => self%top
        self%top => new_node
    end subroutine push

    subroutine pop(self, id)
        class(image_stack), intent(inout) :: self
        integer, intent(out) :: id
        type(image_node), pointer :: temp_node

        if (.not. associated(self%top)) then
            print *, 'La pila de imágenes está vacía.'
            return
        end if

        temp_node => self%top
        id = temp_node%id
        self%top => temp_node%next
        deallocate(temp_node)
    end subroutine pop

    subroutine peek(self, id)
        class(image_stack), intent(in) :: self
        integer, intent(out) :: id

        if (.not. associated(self%top)) then
            print *, 'La pila de imágenes está vacía.'
            return
        end if

        id = self%top%id
    end subroutine peek

    subroutine init_stack(self)
        class(image_stack), intent(inout) :: self
        self%top => null()
    end subroutine init_stack

    !DE PRUEBA

    !integer function get_size(self)
    !class(image_stack), intent(in) :: self
    !integer :: size
    !get_size = self%size
    !end function get_size

end module image_stack_module
