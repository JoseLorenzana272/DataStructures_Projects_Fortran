module ejemploEDD
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, ejemploEDD!"
  end subroutine say_hello
end module ejemploEDD
