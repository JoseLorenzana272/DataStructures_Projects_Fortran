module clientes_atendidos_module
    implicit none
    private
    public :: cliente_atendido, lista_clientes_atendidos

    type :: cliente_atendido
        character(len=:), allocatable :: nombre
        integer :: ventanilla
        integer :: num_imagenes
        type(cliente_atendido), pointer :: next => null()
    end type cliente_atendido

    type :: lista_clientes_atendidos
        type(cliente_atendido), pointer :: head => null()
    contains
        procedure :: agregar_cliente
        procedure :: imprimir_clientes
    end type lista_clientes_atendidos

contains

    subroutine agregar_cliente(this, nombre, ventanilla, num_imagenes)
        class(lista_clientes_atendidos), intent(inout) :: this
        character(len=*), intent(in) :: nombre
        integer, intent(in) :: ventanilla, num_imagenes
        type(cliente_atendido), pointer :: new_node, current

        allocate(new_node)
        new_node%nombre = nombre
        new_node%ventanilla = ventanilla
        new_node%num_imagenes = num_imagenes
        new_node%next => null()

        if (.not. associated(this%head)) then
            this%head => new_node
        else
            current => this%head
            do while (associated(current%next))
                current => current%next
            end do
            current%next => new_node
        end if
    end subroutine agregar_cliente

    subroutine imprimir_clientes(this)
        class(lista_clientes_atendidos), intent(in) :: this
        type(cliente_atendido), pointer :: current

        print *, 'Clientes Atendidos:'
        current => this%head
        do while (associated(current))
            print *, 'Nombre: ', current%nombre, ', Ventanilla: ', current%ventanilla, ', Num. Imágenes: ', current%num_imagenes
            current => current%next
        end do
    end subroutine imprimir_clientes

end module clientes_atendidos_module
