module Group_IO
   use Environment

   implicit none
   integer, parameter :: PEOPLE_AMOUNT   = 8
   integer, parameter :: SURNAME_LEN   = 15
   integer, parameter :: INITIALS_LEN  = 5

   ! Структура данных для хранения данных о человеке.
   type person
      character(SURNAME_LEN, kind=CH_)    :: Surname(PEOPLE_AMOUNT)    = ""
      character(INITIALS_LEN, kind=CH_)   :: Initials(PEOPLE_AMOUNT)   = ""
      character(kind=CH_)                 :: Gender(PEOPLE_AMOUNT)               = ""
      character(kind=CH_)                 :: Registration(PEOPLE_AMOUNT)         = ""
      real(R_)                            :: avg_mark(PEOPLE_AMOUNT)             = 0
   end type person
   
contains
   ! Создание неформатированного файла данных.
   subroutine Create_data_file(Input_File, Data_File)
      character(*), intent(in)   :: Input_File, data_file
     !Структура массивов 
      type(person)               :: pers
      integer                    :: In, Out, IO, i
      character(:), allocatable  :: format
      
      open (file=Input_File, encoding=E_, newunit=In)
      open (file=Data_File, form='unformatted', newunit=Out, access='stream')
         format = '(4(a, 1x), f5.2)'
         read (In, format, iostat=IO) (pers%Surname(i), pers%Initials(i), & 
            pers%Gender(i), pers%Registration(i), pers%avg_mark(i), i = 1, PEOPLE_AMOUNT)
            call Handle_IO_status(IO, "reading formatted list, line " // i)
            
            write (Out, iostat=IO) pers%Surname, pers%Initials, pers%Gender, pers%Registration, pers%avg_mark
            call Handle_IO_status(IO, "creating unformatted file with list, record " // i)
         !end do
      close (In)
      close (Out)
   end subroutine Create_data_file

   ! Чтение списка класса: фамилии, инициалы, пол регистрация оценки.
   function Read_group_list(Data_File) result(Group)
      type(person)                 Group
      character(*), intent(in)   :: Data_File

      integer In, IO
      
      open (file=Data_File, form='unformatted', newunit=In, access='stream')
         read (In, iostat=IO) Group
         call Handle_IO_status(IO, "reading unformatted list")
      close (In)
   end function Read_group_list
 
   ! Вывод списка
   subroutine Output_group_list(Output_File, Group, List_name, Position, Amount)
      character(*), intent(in)   :: Output_File, Position, List_name
      type(person), intent(in)  :: Group
      integer, intent(in)       :: Amount

      integer                    :: Out, IO, i
      character(:), allocatable  :: format
      
      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
         write (out, '(/a)') List_name
         format = '(4(a, 1x), f5.2)'
         write (Out,format, iostat=IO) (Group%Surname(i), Group%Initials(i), Group%Gender(i), Group%Registration(i), & 
            Group%avg_mark(i), i = 1, Amount)
         call Handle_IO_status(IO, "writing " // List_name)
      close (Out)
   end subroutine Output_group_list
end module Group_IO 
