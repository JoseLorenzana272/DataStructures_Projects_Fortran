module merkle_tree
    use sha256_module
    implicit none
    integer :: uid = 1
    type data
        integer :: uid
        character(:), allocatable :: id_origin, address_origin                
        character(:), allocatable :: id_destination, address_destination
        character(:), allocatable :: cost_between, hash_value
        type(data), pointer :: next => null()
    end type data

    type hash_node
        integer :: uid
        character(:), allocatable :: hash        
        type(hash_node), pointer :: left => null()
        type(hash_node), pointer :: right => null()
        type(data), pointer :: dataref => null()
    end type hash_node
    
    type merkle 
        type(hash_node), pointer :: top_hash => null()
        type(data), pointer :: data_head => null()
        type(data), pointer :: data_tail => null()
        integer :: pos = 0
    contains 
        procedure :: agregar_data
        procedure :: get_data
        procedure :: largo_info
        procedure :: crear_merckle
        procedure :: hashear
        procedure :: generar_merckle
        procedure :: dot_merckle 
        procedure :: dot_merckle_rec
        procedure :: primero_hasheo
    end type merkle

contains

    subroutine agregar_data(this, id_origin, address_origin, id_destination, address_destination, cost_between)
        class(merkle), intent(inout) :: this
        integer, intent(in) :: id_origin
        character(*), intent(in) :: address_origin
        integer, intent(in) :: id_destination
        character(*), intent(in) :: address_destination
        integer, intent(in) :: cost_between
        character(len=256) :: id_o_str, id_d_str, cost_str
        character(:), allocatable :: hash_value
        type(data), pointer :: new_data

        write(id_o_str, '(I10)') id_origin
        write(id_d_str, '(I10)') id_destination
        write(cost_str, '(I10)') cost_between

        hash_value = sha256(trim(id_o_str) // address_origin // trim(id_d_str) // address_destination // trim(cost_str))

        allocate(new_data)
        allocate(new_data%id_origin, source=id_o_str)
        allocate(new_data%address_origin, source=address_origin)
        allocate(new_data%id_destination, source=id_d_str)
        allocate(new_data%address_destination, source=address_destination)
        allocate(new_data%cost_between, source=cost_str)
        allocate(new_data%hash_value, source=hash_value)
        new_data%uid = uid
        uid = uid + 1

        if ( associated(this%data_head) ) then
            this%data_tail%next => new_data
            this%data_tail => new_data
        else 
            this%data_head => new_data
            this%data_tail => new_data
        end if

    end subroutine agregar_data

    function largo_info(this) result(res)
        class(merkle), intent(inout) :: this
        type(data), pointer :: tmp 
        integer :: res 
        res = 0
        tmp => this%data_head
        do while (associated(tmp))
            res = res + 1
            tmp => tmp%next
        end do        
    end function largo_info
    
    function get_data(this, pos) result(data_node)
        class(merkle), intent(inout) :: this
        integer, intent(inout) :: pos
        type(data), pointer :: data_node
        data_node => this%data_head       
        do while (associated(data_node))
            if ( pos == 0 ) then
                return
            end if
            pos = pos - 1
            data_node => data_node%next
        end do
    end function get_data

    subroutine crear_merckle(this, node, expo)
        class(merkle), intent(inout) :: this 
        type(hash_node), pointer, intent(inout) :: node
        integer, intent(in) :: expo
        node%uid = uid
        uid = uid + 1

        if ( expo > 0 ) then
            allocate(node%left)
            allocate(node%right)
            call this%crear_merckle(node%left, expo - 1)
            call this%crear_merckle(node%right, expo - 1)
        end if    
        
    end subroutine crear_merckle

    subroutine hashear(this,  node, pow)
        class(merkle), intent(inout) :: this
        type(hash_node), pointer, intent(inout) :: node        
        character(:), allocatable :: hash
        integer, intent(in) :: pow
        integer :: tmp 
        
        if ( associated(node) ) then
            call this%hashear(node%left, pow)
            call this%hashear(node%right, pow)
            if ( .NOT. associated(node%left) .AND. .NOT. associated(node%right) ) then
                tmp = pow - this%pos
                node%dataref => this%get_data(tmp)
                this%pos = this%pos - 1
                hash = node%dataref%hash_value
                node%hash = hash
            else 
                hash = sha256(node%left%hash // node%right%hash)
                node%hash = hash
            end if
        end if
        
    end subroutine hashear

    function primero_hasheo(this) result(hash_value)
        class(merkle), intent(in) :: this
        character(:), allocatable :: hash_value
        
        if (.not. associated(this%top_hash)) then
            write(*, '(A)') "Error: El árbol Merkle no ha sido generado."
            hash_value = "ERROR"
            return
        else
            hash_value = this%top_hash%hash
        endif
    end function primero_hasheo

    subroutine generar_merckle(this)
        class(merkle), intent(inout) :: this
        integer :: expo, i, pow         
        expo = 1

        do while (2 ** expo < this%largo_info() )
            expo = expo + 1
        end do

        pow = 2 ** expo
        this%pos = pow 
        i = this%largo_info()

        do while (i < pow)
            call this%agregar_data(-1, "null", -1, "null", -1)
            i = i + 1
        end do
        allocate(this%top_hash)
        call this%crear_merckle(this%top_hash, expo)
        call this%hashear(this%top_hash, pow)
    end subroutine generar_merckle

    subroutine dot_merckle(this)
        class(merkle), intent(inout) :: this        
        open(69, file='merkle.dot', status='replace')
        write(69, '(A)') 'digraph Merkle_tree {'
        write(69, '(A)') 'node [shape=record, fontname=Arial, fontsize=12];'
        call this%dot_merckle_rec(this%top_hash, 69)
        write(69, '(A)') '}'
        close(69)
        call execute_command_line('dot -Tpng merkle.dot -o merkle.png')
    end subroutine dot_merckle

    subroutine dot_merckle_rec(this,  tmp, unit)
        class(merkle), intent(inout) :: this
        class(hash_node), pointer, intent(in) :: tmp
        integer, intent(in) :: unit        
        if ( .NOT. associated(tmp) ) then
            return
        end if
        write(unit, '(I0, A, A, A)') tmp%uid, ' [label="', tmp%hash, '"];'
        if ( associated(tmp%left) ) then
            write(unit, '(I0, A, I0, A)') tmp%uid, ' -> ', tmp%left%uid, ';'            
        end if
        if ( associated(tmp%right) ) then
            write(unit, '(I0, A, I0, A)') tmp%uid, ' -> ', tmp%right%uid, ';'
        end if
        call this%dot_merckle_rec(tmp%left, unit)
        call this%dot_merckle_rec(tmp%right, unit)
        if ( associated(tmp%dataref) ) then            
            write(unit, '(I0, A)') tmp%dataref%uid, ' [label=<<TABLE><TR>'
            write(unit, '(A, A, A)') '<TD>id_origin: ', trim(tmp%dataref%id_origin), '</TD>'
            write(unit, '(A, A, A)') '<TD>address_origin: ', trim(tmp%dataref%address_origin), '</TD></TR>'
            write(unit, '(A, A, A)') '<TR><TD>id_destination: ', trim(tmp%dataref%id_destination), '</TD>'
            write(unit, '(A, A, A)') '<TD>address_destination: ', trim(tmp%dataref%address_destination), '</TD></TR>'
            write(unit, '(A, A, A)') '<TR><TD>cost_between: ', trim(tmp%dataref%cost_between), '</TD></TR>'
            write(unit, '(A)') '</TABLE>>];'
            write(unit, '(I0, A, I0, A)') tmp%uid, ' -> ', tmp%dataref%uid, ';'
        end if
    end subroutine dot_merckle_rec

    
end module merkle_tree