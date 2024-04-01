module cola_impresion_module
    implicit none
    private
    public :: cola_impresion, nodo_cola_impresion, agregar_a_cola, imprimir_cola

    ! Definición de la estructura de un nodo en la cola de impresión
    type :: nodo_cola_impresion
        type(image) :: imagen
        type(nodo_cola_impresion), pointer :: siguiente => null()
    end type nodo_cola_impresion

    ! Definición de la cola de impresión
    type :: cola_impresion
        type(nodo_cola_impresion), pointer :: frente => null()
        type(nodo_cola_impresion), pointer :: final => null()
    end type cola_impresion

contains

    ! Subrutina para agregar una imagen a la cola de impresión
    subroutine agregar_a_cola(cola, nueva_imagen)
        class(cola_impresion), intent(inout) :: cola
        type(image), intent(in) :: nueva_imagen
        type(nodo_cola_impresion), pointer :: nuevo_nodo

        ! Crear un nuevo nodo para la imagen
        allocate(nuevo_nodo)
        nuevo_nodo%imagen = nueva_imagen
        nuevo_nodo%siguiente => null()

        ! Si la cola está vacía, el nuevo nodo será tanto el frente como el final de la cola
        if (.not. associated(cola%frente)) then
            cola%frente => nuevo_nodo
            cola%final => nuevo_nodo
        else
            ! De lo contrario, agregar el nuevo nodo al final de la cola
            cola%final%siguiente => nuevo_nodo
            cola%final => nuevo_nodo
        end if
    end subroutine agregar_a_cola

    ! Subrutina para imprimir la cola de impresión
    subroutine imprimir_cola(cola)
        class(cola_impresion), intent(in) :: cola
        type(nodo_cola_impresion), pointer :: nodo_actual

        print *, 'Cola de impresión:'
        nodo_actual => cola%frente
        do while (associated(nodo_actual))
            print *, 'Imagen con ID ', nodo_actual%imagen%id
            nodo_actual => nodo_actual%siguiente
        end do
    end subroutine imprimir_cola

end module cola_impresion_module