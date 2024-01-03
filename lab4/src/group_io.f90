module Group_IO
   use Environment

   implicit none
   integer, parameter :: PEOPLE_AMOUNT   = 500
   integer, parameter :: SURNAME_LEN   = 15
   integer, parameter :: INITIALS_LEN  = 5

   ! Структура данных для хранения данных о человеке.
   type person
      character(SURNAME_LEN, kind=CH_)    :: Surname              = ""
      character(INITIALS_LEN, kind=CH_)   :: Initials             = ""
      character(kind=CH_)                 :: Gender               = ""
      character(kind=CH_)                 :: Registration         = ""
      real(R_)                            :: avg_mark             = 0
   end type person
   
contains
   ! Создание неформатированного файла данных.
   subroutine Create_data_file(Input_File, Data_File)
      character(*), intent(in)   :: Input_File, data_file
      
      type(person)               :: pers
      integer                    :: In, Out, IO, i, recl
      character(:), allocatable  :: format
      
      open (file=Input_File, encoding=E_, newunit=In)
      recl = (SURNAME_LEN + INITIALS_LEN + 2)*CH_ + R_
      !Создаем файл неформатированный (двоичный) direct - прямой доступ => требуется указывать recl()
      !Recording length длина записи (22*4 + 4 = 48)
      open (file=Data_File, form='unformatted', newunit=Out, access='direct', recl=recl)
         format = '(4(a, 1x), f5.2)'
         do i = 1, PEOPLE_AMOUNT
            read (In, format, iostat=IO) pers 
            call Handle_IO_status(IO, "reading formatted list, line " // i)
            
            write (Out, iostat=IO, rec=i) pers
            call Handle_IO_status(IO, "creating unformatted file with list, record " // i)
         end do
      close (In)
      close (Out)
   end subroutine Create_data_file

   ! Чтение списка класса: фамилии, инициалы, пол регистрация оценки.
   function Read_group_list(Data_File) result(Group)
      type(person)                 Group(PEOPLE_AMOUNT)
      character(*), intent(in)   :: Data_File

      integer In, IO, recl
      
      recl = ((SURNAME_LEN + INITIALS_LEN + 2)*CH_ + R_) * PEOPLE_AMOUNT
      open (file=Data_File, form='unformatted', newunit=In, access='direct', recl=recl)
         read (In, iostat=IO, rec=1) Group
         call Handle_IO_status(IO, "reading unformatted list")
      close (In)
   end function Read_group_list
 
   ! Вывод списка
   subroutine Output_group_list(Output_File, Group, List_name, Position)
      character(*), intent(in)   :: Output_File, Position, List_name
      type(person), intent(in)  :: Group(:)

      integer                    :: Out, IO
      character(:), allocatable  :: format
      
      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
         write (out, '(/a)') List_name
         format = '(4(a, 1x), f5.2)'
         write (Out, format, iostat=IO) Group
         call Handle_IO_status(IO, "writing " // List_name)
      close (Out)
   end subroutine Output_group_list
end module Group_IO 
