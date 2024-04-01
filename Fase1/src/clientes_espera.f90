module lista_clientes_espera_module
    implicit none
    
    type :: cliente
        integer :: id
        character(len=:), allocatable :: nombre
        integer :: img_grande
        integer :: img_peque
        type(cliente), pointer :: prev => null()
        type(cliente), pointer :: next => null()
    end type cliente
    
    type :: lista_clientes_espera
        type(cliente), pointer :: head => null() 
        integer :: size 

    contains
        procedure :: append_cliente_espera
        procedure :: print_clientes_espera
        procedure :: graficar_clientes_espera
    end type lista_clientes_espera
    
contains

    subroutine append_cliente_espera(self, id, nombre, img_grande, img_peque)
        class(lista_clientes_espera), intent(inout) :: self
        integer, intent(in) :: id, img_grande, img_peque
        character(len=*), intent(in) :: nombre
        type(cliente), pointer :: new_client
        
        allocate(new_client)
        
        new_client%id = id
        new_client%nombre = nombre
        new_client%img_grande = img_grande
        new_client%img_peque = img_peque
        
        if (.not. associated(self%head)) then
            new_client%prev => new_client
            new_client%next => new_client
            self%head => new_client
        else
            new_client%prev => self%head%prev
            new_client%next => self%head
            self%head%prev%next => new_client
            self%head%prev => new_client
        end if
        
        self%size = self%size + 1
    end subroutine append_cliente_espera
    
    subroutine print_clientes_espera(self)
        class(lista_clientes_espera), intent(in) :: self
        type(cliente), pointer :: current
        
        if (.not. associated(self%head)) then
            print *, 'Lista de clientes en espera está vacía.'
            return
        end if
        
        print *, 'Lista de clientes en espera:'
        current => self%head
        do
            print *, 'ID:', current%id, ' | Nombre:', current%nombre, &
         ' | Imágenes grande:', current%img_grande, ' | Imágenes pequeñas:', current%img_peque
            current => current%next
            if (associated(current, self%head)) exit
        end do
    end subroutine print_clientes_espera

    subroutine graficar_clientes_espera(self)
        class(lista_clientes_espera), intent(in) :: self
        type(cliente), pointer :: current
        integer :: i
        character(len=100) :: filename
        character(len=500) :: command
    
        if (.not. associated(self%head)) then
            print *, 'La lista de clientes en espera está vacía.'
            return
        end if
    
        filename = 'clientes_espera.dot'
        open(unit=10, file=filename, status='replace')
    
        write(10, '(a)') 'digraph G {'
        write(10, '(a)') '    rankdir=LR;'
        write(10, '(a)') '    node [shape=record];'
    
        current => self%head
        i = 0
        do
            write(10, '(a,i0,a)') '    node', i, ' [label="'
            write(10, '(a,i0,a)') 'ID: ', current%id, '\\n'
            write(10, '(a,a,a)') 'Nombre: ', trim(current%nombre), '\\n'
            write(10, '(a,i0,a)') 'Imágenes grandes: ', current%img_grande, '\\n'
            write(10, '(a,i0,a)') 'Imágenes pequeñas: ', current%img_peque, '"];'
            if (i > 0) then
                write(10, '(a,i0,a,i0,a)') '    node', i-1, ' -> node', i, ';'
            end if
            current => current%next
            if (associated(current, self%head)) exit
            i = i + 1
        end do
    
        write(10, '(a)') '}'
        close(10)
    
        command = 'dot -Tpng ' // trim(filename) // ' -o clientes_espera.png'
        call system(command)
        print *, 'El gráfico de clientes en espera ha sido generado en el archivo clientes_espera.png.'
    end subroutine graficar_clientes_espera
    
    
    
    

end module lista_clientes_espera_module
