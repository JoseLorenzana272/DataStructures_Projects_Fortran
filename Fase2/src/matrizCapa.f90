module matrix_m_sparse
    implicit none
    private

    type :: node_val
        private
        logical :: exists = .false.
        character(len=7) :: color
    end type node_val

    type, public :: node_matrix

        integer :: i, j
        character(len=7) :: color
        type(node_matrix), pointer :: up => null()
        type(node_matrix), pointer :: down => null()
        type(node_matrix), pointer :: right => null()
        type(node_matrix), pointer :: left => null()
    end type node_matrix

    type, public :: matrix
        
        type(node_matrix), pointer :: root => null()
        integer :: width = 0
        integer :: height = 0
    contains
        procedure :: insert
        procedure :: insertRowHeader
        procedure :: insertColumnHeader
        procedure :: insertInRow
        procedure :: insertInColumn
        procedure :: searchRow
        procedure :: searchColumn
        procedure :: nodeExists
        procedure :: print
        procedure :: printColumnHeaders
        procedure :: getValue
        procedure :: clear
        ! procedure :: printRowHeaders
        procedure :: dot_matrix
        procedure :: html_matrix

    end type

contains
    subroutine insert(self, i, j, color) 
        class(matrix), intent(inout) :: self  
        integer, intent(in) :: i
        integer, intent(in) :: j
        character(len=7), intent(in) :: color

        type(node_matrix), pointer :: new
        type(node_matrix), pointer :: row
        type(node_matrix), pointer :: column

        allocate(new)
        new = node_matrix(i=i, j=j, color=color)

        if(.not. associated(self%root)) then
            allocate(self%root)
            self%root = node_matrix(i=-1, j=-1, color="")
        end if

        row => self%searchRow(i)
        column => self%searchColumn(j)

        if(j > self%width) self%width = j
        if(i > self%height) self%height = i

        if(.not. self%nodeExists(new)) then
            if(.not. associated(column)) then
                column => self%insertColumnHeader(j)
            end if

            if(.not. associated(row)) then
                row => self%insertRowHeader(i)
            end if
            call self%insertInColumn(new, row)
            call self%insertInRow(new, column)
        end if
    end subroutine insert

    function searchColumn(self, j) result(actual)
        class(matrix), intent(in) :: self
        integer, intent(in) :: j

        type(node_matrix), pointer :: actual
        actual => self%root

        do while(associated(actual))
            if(actual%j == j) return
            actual => actual%right
        end do
    end function searchColumn

    function searchRow(self, i) result(actual)
        class(matrix), intent(in) :: self
        integer, intent(in) :: i

        type(node_matrix), pointer :: actual
        actual => self%root

        do while(associated(actual))
            if(actual%i == i) return
            actual => actual%down
        end do
    end function searchRow

    function nodeExists(self, new) result(exists)
        class(matrix), intent(inout) :: self  
        type(node_matrix), pointer :: new
        
        logical :: exists
        type(node_matrix), pointer :: rowHeader
        type(node_matrix), pointer :: column
        rowHeader => self%root
        exists = .false.

        do while(associated(rowHeader))
            if(rowHeader%i == new%i) then
                column => rowHeader
                do while(associated(column))
                    if(column%j == new%j) then
                        column%color = new%color
                        exists = .true.
                        return
                    end if
                    column => column%right
                end do
                return
            end if
            rowHeader => rowHeader%down
        end do
        return
    end function nodeExists

    function insertRowHeader(self, i) result(newRowHeader)
        class(matrix), intent(inout) :: self  
        integer, intent(in) :: i

        type(node_matrix), pointer :: newRowHeader
        allocate(newRowHeader)

        newRowHeader = node_matrix(i=i, j=-1, color="")
        call self%insertInRow(newRowHeader, self%root)
    end function insertRowHeader

    subroutine insertInRow(self, new, rowHeader)
        class(matrix), intent(inout) :: self
        type(node_matrix), pointer :: new
        type(node_matrix), pointer :: rowHeader

        type(node_matrix), pointer :: actual
        actual => rowHeader

        do while(associated(actual%down))
            if(new%i < actual%down%i .and. new%i > actual%i) then
                new%down => actual%down
                new%up => actual
                actual%down%up => new
                actual%down => new
                exit
            end if
            actual => actual%down
        end do

        if(.not. associated(actual%down)) then
            actual%down => new
            new%up => actual
        end if
    end subroutine insertInRow

    function insertColumnHeader(self, j) result(newColumnHeader)
        class(matrix), intent(inout) :: self  
        integer, intent(in) :: j

        type(node_matrix), pointer :: newColumnHeader
        allocate(newColumnHeader)

        newColumnHeader = node_matrix(i=-1, j=j, color="")
        call self%insertInColumn(newColumnHeader, self%root)
    end function insertColumnHeader

    subroutine insertInColumn(self, new, columnHeader)
        class(matrix), intent(inout) :: self
        type(node_matrix), pointer :: new
        type(node_matrix), pointer :: columnHeader
        
        type(node_matrix), pointer :: actual
        actual => columnHeader
        do while(associated(actual%right))
            if(new%j < actual%right%j .and. new%j > actual%j) then
                new%right => actual%right
                new%left => actual
                actual%right%left => new
                actual%right => new
                exit
            end if
            actual => actual%right
        end do
        
        if(.not. associated(actual%right)) then
            actual%right => new
            new%left => actual
        end if
    end subroutine insertInColumn

    subroutine print(self)
        class(matrix), intent(inout) :: self  
        integer :: i
        integer :: j
        type(node_matrix), pointer :: aux
        type(node_val) :: val
        aux => self%root%down

        call self%printColumnHeaders()

        do i = 0, self%height
            print *, ""
            write(*, fmt='(I3)', advance='no') i
            do j = 0, self%width
                val = self%getValue(i,j)
                if(.not. val%exists) then
                    write(*, fmt='(I3)', advance='no') 0
                else
                    write(*, fmt='(L3)', advance='no') val%color
                end if
            end do
        end do
        print *, ""
    end subroutine print

    subroutine printColumnHeaders(self)
        class(matrix), intent(in) :: self
        integer :: j

        do j=-1, self%width
            write(*, fmt='(I3)', advance='no') j
        end do
    end subroutine printColumnHeaders

    function getValue(self, i, j) result(val)
        class(matrix), intent(in) :: self
        integer, intent(in) :: i
        integer, intent(in) :: j
        
        type(node_matrix), pointer :: rowHeader
        type(node_matrix), pointer :: column
        type(node_val) :: val
        rowHeader => self%root

        do while(associated(rowHeader))
            if(rowHeader%i == i) then
                column => rowHeader
                do while(associated(column))
                    if(column%j == j) then
                        val%color = column%color
                        val%exists = .true.
                        return
                    end if
                    column => column%right
                end do
                return
            end if
            rowHeader => rowHeader%down
        end do
    end function getValue

    subroutine clear(self)
        class(matrix), intent(inout) :: self
    
        type(node_matrix), pointer :: current_row
        type(node_matrix), pointer :: next_row
        type(node_matrix), pointer :: current_node
        type(node_matrix), pointer :: next_node
    
        current_row => self%root
        do while(associated(current_row))
            current_node => current_row%right
            do while(associated(current_node))
                next_node => current_node%right
                deallocate(current_node)
                current_node => next_node
            end do
            next_row => current_row%down
            deallocate(current_row)
            current_row => next_row
        end do
    
        self%width = 0
        self%height = 0
    end subroutine clear

    subroutine dot_matrix(this, filename)
        class(matrix), intent(in) :: this
        character(len=*), intent(in) :: filename
        integer :: unit
        integer :: j, i
        type(node_val) :: val
        open(unit, file=filename, status='replace')
        write(unit, '(A)') 'graph{'
        write(unit, '(A)') 'node [shape=box];'
        
        ! Nodo de donde va aa salir todo (para que puedan salir tanto las filas y columnas),,,, group(1) hace que se alinie verticalemente
        write(unit, *) '"Root" [label="-1", group=1];'
        ! Graficar encabezados (columna)
        do j=0, this%width
            ! aqui es group = j+2 porque ya iniciamos con 1
            write(unit, '(A,I0, A, I0, A, I0, A)') '"ColumnHeader', j, '" [label="', j, '", group=', j+2,'];'
            if (j == 0) then
                write(unit, '(A,I0, A, I0, A)') '"Root" -- "ColumnHeader', j, '";'
                write(unit, '(A,I0, A, I0, A)') '"ColumnHeader', j, '" -- "ColumnHeader', j+1, '";'
            else

                if (j /= this%width) then
                    write(unit, '(A,I0, A, I0, A)') '"ColumnHeader', j, '" -- "ColumnHeader', j+1, '";'
                end if
            end if
        end do
        write(unit, *) ' { rank=same; "Root";'

        do j=0, this%width
            write(unit, '(A,I0,A)', advance='no') ' "ColumnHeader', j, '";'
        end do

        write(unit, *) " }"
        ! Graficar encabezados (fila)
        do i=0, this%height
            write(unit, '(A,I0,A,I0,A)') '"RowHeader', i, '" [label="', i, '"];'
            if (i == 0) then
                write(unit, '(A,I0,A)') '"Root" -- "RowHeader', i, '";'
                write(unit, '(A,I0,A,I0,A)') '"RowHeader', i, '" -- "RowHeader', i+1, '";'
            else
                if (i /= this%height) then
                    write(unit, '(A,I0,A,I0,A)') '"RowHeader', i, '" -- "RowHeader', i+1, '";'
                end if
            end if
        end do
        ! Aqui se rellena la matriz
        do i = 0, this%height
            do j = 0, this%width
                val = this%getValue(i,j)
                if(.not. val%exists) then
                    write(unit, '(A,I0,A,I0,A,I0,A)') '"Cell_', i, '_', j, '" [label="", style=filled, fillcolor=white];'
                else
                    write(unit, '(A,I0,A,I0,A,I0,A)') '"Cell_', i, '_', j, '" [label="", style=filled, fillcolor="' // &
                    trim(val%color) // '"];'
                end if
                ! Logica para las conexiones
                if (i == 0 .and. j == 0) then
                    write(unit, '(A,I0,A,A,I0,A,I0,A)') '"RowHeader', i, '" -- ', '"Cell_', i, '_', j, '";'
                    write(unit, '(A,I0,A,A,I0,A,I0,A)') '"ColumnHeader', j, '" -- ', '"Cell_', i, '_', j, '";'
                else if (i == 0) then
                    write(unit, '(A,I0,A,A,I0,A,I0,A)') '"ColumnHeader', j, '" -- ', '"Cell_', i, '_', j, '";'
                else if (j == 0) then
                    write(unit, '(A,I0,A,A,I0,A,I0,A)') '"RowHeader', i, '" -- ', '"Cell_', i, '_', j, '";'
                end if
                if (j /= this%width) then
                    write(unit, '(A,I0,A,I0,A)') '"Cell_', i, '_', j, '" -- '
                    write(unit, '(A,I0,A,I0,A)') '"Cell_', i, '_', j+1, '";'
                end if
                
                if (i /= this%height) then
                    write(unit, '(A,I0,A,I0,A)') '"Cell_', i, '_', j, '" -- '
                    write(unit, '(A,I0,A,I0,A)') '"Cell_', i+1, '_', j, '";'
                end if
            end do

            write(unit, '(A,I0,A)') ' { rank=same; "RowHeader', i, '";'
            do j=0, this%width
                write(unit, '(A,I0,A,I0,A)', advance='no') ' "Cell_', i, '_', j, '";'
            end do
            write(unit, *) " }"
        end do


        
        print *, ""

        write(unit, '(A)') '}'
        close(unit)
        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')
        call system('start ' // trim(adjustl(filename)) // '.png')
    end subroutine dot_matrix

    
    subroutine html_matrix(this, filename)
        class(matrix), intent(in) :: this
        character(len=*), intent(in) :: filename
        integer :: unit
        integer :: j, i
        type(node_val) :: val
        open(unit, file=filename, status='replace')
        write(unit, '(A)') '<!DOCTYPE html>'
        write(unit, '(A)') '<html>'
        write(unit, '(A)') '<head>'
        write(unit, '(A)') '<style>'
        write(unit, '(A)') 'table {'
        write(unit, '(A)') '  border-collapse: collapse;'
        write(unit, '(A)') '  width: 100%;'
        write(unit, '(A)') '}'
        write(unit, '(A)') 'th, td {'
        write(unit, '(A)') '  border: 1px solid black;'
        write(unit, '(A)') '  text-align: center;'
        write(unit, '(A)') '  padding: 8px;'
        write(unit, '(A)') '}'
        write(unit, '(A)') 'th {'
        write(unit, '(A)') '  background-color: #f2f2f2;'
        write(unit, '(A)') '}'
        write(unit, '(A)') '</style>'
        write(unit, '(A)') '</head>'
        write(unit, '(A)') '<body>'
        write(unit, '(A)') '<table>'
        write(unit, '(A)') '  <tr>'
        write(unit, '(A)') '    <th></th>'
        do j=0, this%width
            write(unit, '(A,I0,A)', advance='no') '    <th>', j, '</th>'   
            
        end do
        write(unit, '(A)') '  </tr>'
        do i = 0, this%height
            write(unit, '(A)') '  <tr>'
            write(unit, '(A,I0,A)', advance='no') '    <th>', i, '</th>'
            do j = 0, this%width
                val = this%getValue(i,j)
                if(.not. val%exists) then
                    write(unit, '(A)') '    <td></td>'
                else
                    write(unit, '(A)') '    <td style="background-color:', trim(val%color), '"></td>'
                end if
            end do
            write(unit, '(A)') '  </tr>'
        end do
        write(unit, '(A)') '</table>'
        write(unit, '(A)') '</body>'
        write(unit, '(A)') '</html>'
        close(unit)
        call system('start ' // trim(filename))
    end subroutine html_matrix
    
end module matrix_m_sparse