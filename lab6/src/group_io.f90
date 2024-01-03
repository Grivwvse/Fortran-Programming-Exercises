module Group_IO
   use Environment

   implicit none
   integer, parameter :: PEOPLE_AMOUNT   = 500
   integer, parameter :: SURNAME_LEN   = 15
   integer, parameter :: INITIALS_LEN  = 5
   !character()       :: format = '(4(a, 1x), f5.2)'

   ! Структура данных для хранения данных о человеке.
   type person
      character(SURNAME_LEN, kind=CH_)    :: Surname              = ""
      character(INITIALS_LEN, kind=CH_)   :: Initials             = ""
      character(kind=CH_)                 :: Gender               = ""
      character(kind=CH_)                 :: Registration         = ""
      real(R_)                            :: avg_mark             = 0
      type(person), pointer               :: next                 =>Null()
   end type person
   
!contains
contains
   ! Чтение списка класса: фамилии, инициалы, полы и оценки.
   function Read_group_list(Input_File) result(Group_List)
      type(person), pointer     :: Group_List
      character(*), intent(in)   :: Input_File
      integer  In

      open (file=Input_File, encoding=E_, newunit=In)
         Group_List => Read_person(In)
      close (In)
   end function Read_group_list

   !Создаем повязанного студента, у когорого поле next будет ссылаться на дргого и затем этого студента
   !Со всеми есго связями возвращаем (Однонаправленный динамический список)
   ! Чтение следующего человека.
   recursive function Read_person(In) result(pers)
      type(person), pointer  :: pers
      integer, intent(in)     :: In
      integer  IO
      character(:), allocatable  :: format

      allocate (pers)
      format = '(4(a, 1x), f5.2)'
      read (In, format, iostat=IO) pers%Surname, pers%Initials, pers%Gender, pers%Registration, &
         pers%avg_mark
      call Handle_IO_status(IO, "reading line from file")
      if (IO == 0) then
          pers%next => Read_person(In)
      else
         deallocate (pers)
      end if
   end function Read_person
! Вывод списка
   subroutine Output_group_list(Output_File, Group_List, NameOfList, Position)
      character(*), intent(in)   :: Output_File, Position, NameOfList
      type(person), intent(in)  :: Group_List                                 !Обьект
      integer  :: Out

      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
         write (out, '(/a)') NameOfList
         call Output_person(Out, Group_List)
      close (Out)
   end subroutine Output_group_list
   
   recursive subroutine Output_person(Out, pers)
      integer, intent(in)        :: Out
      type(person), intent(in)  :: pers

      integer  :: IO
      character(:), allocatable  :: format

      format = '(4(a, 1x), f5.2)'
      write (Out, format, iostat=IO) pers%Surname, pers%Initials, pers%Gender, pers%Registration, &
         pers%avg_mark
      call Handle_IO_status(IO, "writing person")
      if (Associated(pers%next)) &        !Если еще остались люди, то продолжаем 
         call Output_person(Out, pers%next)
   end subroutine Output_person

end module Group_IO 
