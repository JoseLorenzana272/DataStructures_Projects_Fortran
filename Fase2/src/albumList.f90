module AlbumModule
    implicit none

    type node_image
        integer :: id
        type(node_image), pointer :: next => null()
    end type node_image

    type node_album
        character(len=:), allocatable :: name
        integer :: images
        type(node_album), pointer :: next => null()
        type(node_image), pointer :: image_list => null()
        type(node_album), pointer :: prev => null()
    end type node_album

    type album
        type(node_album), pointer :: head => null()
        type(node_album), pointer :: tail => null()
    contains
        procedure :: add_album
        procedure :: add_image
        procedure :: generate_dot_file
        procedure :: count_albums
    end type album

contains
    
    subroutine add_album(this, name)
        class(album), intent(inout) :: this
        character(len=:), allocatable, intent(in) :: name
        type(node_album), pointer :: new_album
        type(node_album), pointer :: current

        allocate(new_album)
        new_album%name = name
        new_album%images = 0

        if (associated(this%head)) then
            current => this%head
            do while (associated(current%next))
                current => current%next
            end do
            current%next => new_album
            new_album%prev => current
            this%tail => new_album
        else
            this%head => new_album
            this%tail => new_album
        end if

        print *, '-----------------------'
        print *, 'Album agregado: ', new_album%name
    end subroutine add_album

    subroutine add_image(this, album_name, image_id)
        class(album), intent(inout) :: this
        character(len=:), allocatable, intent(in) :: album_name
        integer, intent(in) :: image_id
        type(node_album), pointer :: current_album
        type(node_image), pointer :: new_image
        type(node_image), pointer :: current_image

        current_album => this%head
        do while (associated(current_album))
            if (trim(current_album%name) == trim(album_name)) then
                allocate(new_image)
                new_image%id = image_id
                new_image%next => null()

                if (.not. associated(current_album%image_list)) then
                    current_album%image_list => new_image
                else
                    current_image => current_album%image_list
                    do while (associated(current_image%next))
                        current_image => current_image%next
                    end do
                    current_image%next => new_image
                end if

                current_album%images = current_album%images + 1
                print *, 'Imagen ', image_id, ' agregada al album ', trim(album_name)
                return
            end if
            current_album => current_album%next
        end do
        
        print *, 'Álbum no encontrado: ', album_name
    end subroutine add_image

    subroutine generate_dot_file(this, filename)
        class(album), intent(in) :: this
        character(len=*), intent(in) :: filename
        integer :: unit, i
        type(node_album), pointer :: current_album
        type(node_image), pointer :: current_image
    
        open(newunit=unit, file=filename, status='replace')
        write(unit, '(A)') 'digraph{'
        write(unit, '(A)') 'rankdir=LR;'
        
        current_album => this%head
        do while (associated(current_album))
            current_image => current_album%image_list
            do i=0, current_album%images-1
                if (associated(current_image)) then
                    
                    write(unit, '(A)') '"Image' // int2str(current_image%id) // trim(current_album%name) &
                    // '" [shape=box, label="'// int2str(current_image%id) // '"];'
                    current_image => current_image%next
                end if
            end do
            write(unit, '(A)') '"' // trim(current_album%name) // '" [shape=box];'
            current_image => current_album%image_list
            do i=0, current_album%images-1
                if (associated(current_image)) then
                    
                    if (i == 0) then
                        write(unit, '(A)') '"' // trim(current_album%name) // '" -> "Image' // &
                        int2str(current_image%id) // trim(current_album%name) // '" [color=blue];'
                        
                        if (associated(current_image%next)) then
                            write(unit, '(A)') '"Image' // int2str(current_image%id) // trim(current_album%name) &
                            // '" -> "Image' // &
                            int2str(current_image%next%id) // trim(current_album%name) // '" [color=blue];'
                        end if
                    else if (associated(current_image%next)) then
                        write(unit, '(A)') '"Image' // int2str(current_image%id) // &
                        trim(current_album%name) // '" -> "Image' // &
                        int2str(current_image%next%id) // trim(current_album%name) // '" [color=blue];'
                        if (.not. associated(current_image%next%next)) then
                            exit
                        end if
                    end if
                    current_image => current_image%next
                end if
            end do
            write(unit, '(A)') '{rank=same; "' // trim(current_album%name) // '";'
            current_image => current_album%image_list
            do i=0, current_album%images-1
                if (associated(current_image)) then
                    
                    write(unit, '(A)') '"Image' // int2str(current_image%id) // trim(current_album%name) // '" [shape=box];'
                    current_image => current_image%next
                end if
            end do
            write(unit, '(A)') '}'
            if (associated(current_album%next)) then
                write(unit, '(A)') '"' // trim(current_album%name) // '" -> "' // &
                trim(current_album%next%name) // '" [color=blue];'
                write(unit, '(A)') '"' // trim(current_album%next%name) // '" -> "' // &
                trim(current_album%name) // '" [color=blue];'
            end if
            current_album => current_album%next
        end do
    
        write(unit, '(A)') '}'
        close(unit)
        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')
        call system('start ' // trim(adjustl(filename)) // '.png')
    end subroutine generate_dot_file
    
    subroutine count_albums(this, total_albums)
        class(album), intent(in) :: this
        integer, intent(out) :: total_albums
        type(node_album), pointer :: current_album
        
        total_albums = 0
        current_album => this%head
        do while (associated(current_album))
            total_albums = total_albums + 1
            current_album => current_album%next
        end do
    end subroutine count_albums
    
    function int2str(i) result(res)
        integer, intent(in) :: i
        character(len=32) :: res
        write(res, '(I0)') i
        res = adjustl(res)
    end function int2str
    

end module AlbumModule
