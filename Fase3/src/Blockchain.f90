module block_chain
    use routes, only: result_list, result
    use merkle_tree
    use Avl_Tree
    implicit none
    integer :: block_id = 0
    type block 
        integer :: index
        integer :: nonce
        character(:), allocatable :: timestamp
        type(result_list) :: data
        character(len=256) :: previous_hash = '0000'
        character(len=256) :: root_merkle = '0000'
        character(len=256) :: hash = '0000'
        type(block), pointer :: next => null()
        type(Tree_t) :: branches
    contains
        procedure :: generar_bloque
        procedure :: imprimir_info
    end type block
    type chainer
        type(block), pointer :: head => null()
        type(block), pointer :: tail => null()
    contains 
        procedure :: add_block
        procedure :: imprimir_cad
    end type chainer
contains
    subroutine generar_bloque(this, new_data, new_branches, printers)
    class(block), intent(inout) :: this
    type(result_list) :: new_data
    type(Tree_t) :: new_branches
    type(result), pointer :: current
    type(Node_t), pointer :: b_origin, b_destination
    type(merkle) :: new_merkle
    integer, dimension(8) :: values 
    character(20) :: timestamp
    logical, intent(in) :: printers
    this%index = block_id
    block_id = block_id + 1
    call date_and_time(values=values)
    write(timestamp, '(I0, A, I0, A, I0, A, I0, A, I0, A, I0)') values(3), '-', values(2) &
        , '-', values(1), '::', values(5), ':', values(6), ':', values(7)
    this%nonce = 4560
    this%timestamp = trim(timestamp)
    this%data = new_data
    this%branches = new_branches
    current => new_data%head
    do while ( associated(current) )
        b_origin => new_branches%searchBranch(current%id)
        if ( associated(b_origin) ) then
            if ( associated(current%next) ) then
                b_destination => new_branches%searchBranch(current%next%id)
                if ( associated(b_destination) ) then
                    if (printers) then
                        call new_merkle%agregar_data(b_origin%id, b_origin%dept, b_destination%id,&
                        b_destination%dept, current%next%printers*80)
                    else
                        call new_merkle%agregar_data(b_origin%id, b_origin%dept, b_destination%id,&
                        b_destination%dept, current%next%weight*80)
                    end if

                end if
            end if
        end if
        current => current%next
    end do
    call new_merkle%generar_merckle()
    call new_merkle%dot_merckle()
    this%root_merkle = new_merkle%primero_hasheo()
    end subroutine generar_bloque

    subroutine add_block(this,  new_block)
        class(chainer), intent(inout) :: this
        type(block), pointer, intent(inout) :: new_block
        character(len=100) :: index, nonce 

        write(index, '(i0)') new_block%index
        write(nonce, '(i0)') new_block%nonce
        if ( .not. associated(this%head) ) then
            this%head => new_block
            new_block%hash = sha256(trim(index)//new_block%timestamp//trim(nonce)&
            //new_block%previous_hash//new_block%root_merkle)
            this%tail => new_block
        else
            this%tail%next => new_block
            new_block%previous_hash = this%tail%hash
            new_block%hash = sha256(trim(index)//new_block%timestamp//trim(nonce)&
            //new_block%previous_hash//new_block%root_merkle)
            this%tail => new_block
        end if
    end subroutine add_block

    subroutine imprimir_info(this)
        class(block), intent(in) :: this
        
        print *, 'Index: ', this%index
        print *, 'Timestamp: ', this%timestamp
        print *, 'Nonce: ', this%nonce
        call this%data%print()
        print *, 'Previous Hash: ', this%previous_hash
        print *, 'Root Merkle: ', this%root_merkle
        print *, 'Hash: ', this%hash

    end subroutine imprimir_info

    subroutine imprimir_cad(this)
        class(chainer), intent(inout) :: this
        type(block), pointer :: current

        current => this%head
        do while ( associated(current) )
            call current%imprimir_info()
            current => current%next
        end do
        
    end subroutine imprimir_cad
end module block_chain