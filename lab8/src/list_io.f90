module ll_io
   use environment
   implicit none

   integer(I_),  parameter :: last_name_length =10
   integer(I_),  parameter :: record_length    = last_name_length * CH_ + I_
   !character(*), allocatable :: fmt    = '(a, 1x, i4)'
   ! Структура данных для узла в списке людей.(P1)
   type person
      character(last_name_length, kind=CH_) :: Surname  = ''
      integer(I_)                           :: birth_year = 0
   end type person

   type, extends(person) :: person_node !расширяет (унастледует тип node)
      type(person_node), allocatable :: next
   end type person_node
   
   ! person node унаследованно от person => имеет поле pers_name, birth_year, и добаляет next

contains
   ! Чтение списка.
   function read_list(input_file, records_num) result(list)
      type(person_node), allocatable   :: list
      character(*),        intent(in)  :: input_file
      integer(I_)                      :: in
      integer, intent(inout)           :: records_num

      open(file=input_file, newunit=in, encoding=E_)
      call read_value(in, list, records_num)
      close(in)
   end function read_list

   ! Чтение следующего значения.
   recursive subroutine read_value(in, elem, records_num)
      type(person_node),   allocatable :: elem
      integer,             intent(in)  :: in
      integer                          :: io

      integer, intent(inout)           :: records_num
      character(:), allocatable :: fmt
      
      fmt = "(a, 1x, i4)"
      allocate(elem)
      read(in, fmt, iostat=io) &
         elem%Surname, elem%birth_year
      call handle_io_status(io, "reading value from file")
      if(io == 0) then
         records_num = records_num + 1
         call read_value(in, elem%next, records_num)
      else
         deallocate(elem)
      end if
   end subroutine read_value

! Создание неформатированного файла.
   subroutine create_data_file(list, data_file)
      character(*),        intent(in)  :: data_file
      type(person_node), allocatable :: list
      integer(I_)                      :: out

      open(file=data_file, form='unformatted', newunit=out, access='direct', recl=record_length)
      call write_element(out, list, 1)
      close(out)
   end subroutine create_data_file

   recursive subroutine write_element(out, list, i)
      integer,             intent(in) :: out, i
      type(person_node), intent(in) :: list
      integer(I_)                     :: io

      write(out, iostat=io, rec=i) list%Surname, list%birth_year
      call handle_io_status(io, "writing data")
      if(allocated(list%next)) &
         call write_element(out, list%next, i+1)
   end subroutine write_element
!!!!!!!!!   ! Создание неформатированного файла. и запись в него данных
 !  subroutine create_data_file(list, data_file)
! !~     character(*),        intent(in)  :: data_file
!      type(person_node), allocatable   :: list
!      integer(I_)                      :: out
!
!      call write_element(out, list, 1)
!      open(file=data_file, form='unformatted', newunit=out, access='direct', recl=record_length)
!      !call write_element(out, list)
!      call write_element(out, list, 1)
!      close(out)
!   end subroutine create_data_file
!
!   recursive subroutine write_element(out, list, i)
!      integer,             intent(in) :: out
!      type(person_node), intent(in) :: list
!      integer(I_)                     :: io,i
!
  !    write(out, iostat=io, rec=i) list%Surname, list%birth_year !rec используется для указания номера записи 
  !    call handle_io_status(io, "writing data")
  !    if(allocated(list%next)) &
  !       call write_element(out, list%next,i)
  ! end subroutine write_element
!!!!!!!!!!!!!!!!!!!!!!!
    !Вывод списка.
   subroutine output(output_file, data_file, list_name, records_num)
      character(*), intent(in) :: output_file, data_file, list_name
      type(person)             :: elem
      integer(I_)              :: in, out, io, size, i

      integer(I_), intent(in)       :: records_num
      
      character(:), allocatable :: fmt
      
      fmt = "(a, 1x, i4)"
      open(file=data_file, form='unformatted', newunit=in, access='stream')
      inquire(unit=in, size=size)  ! Получаем размер файла.

      open(file=output_file, encoding=E_, position='rewind', newunit=out)
      write(out, '(a/)') list_name

      do i=1, size / record_length ! получаем колическтво строк, которые нужно вывести
         read(in, iostat=io) elem
         call handle_io_status(io, "reading line from the file")
         write(out, fmt, iostat=io) elem
         call handle_io_status(io, "writing line into the file")
      end do

      close(out)
      close(in)
   end subroutine output

   recursive subroutine deallocate_list(list)
      type(person_node), allocatable :: list

      if(allocated(list%next)) &
        call deallocate_list(list%next)
      deallocate(list)
    end subroutine deallocate_list
end module ll_io
