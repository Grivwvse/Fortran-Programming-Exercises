program lab_2_1
   use Environment

   implicit none
   integer(I_), parameter               :: PEOPLE_AMOUNT = 2000, SURNAME_LEN = 15, INITIALS_LEN = 5
   character(kind=CH_), parameter   :: Native = Char(1055, CH_) !CH__"\u1052" CH__"М" П
   real                             :: start, finish

   character(:), allocatable  :: input_file, output_file, format

   ! Массивы фамилий, инициалов, полов, прописки и средних оценов и временные 
   ! переменные для обменов при сортировке.
   ! Массив Фамилий
   character(SURNAME_LEN, kind=CH_)                :: Surnames(PEOPLE_AMOUNT) = ""
   character(SURNAME_LEN, kind=CH_), allocatable   :: guest_surnames(:), native_surnames(:)
   
   !Массив инициалов
   character(INITIALS_LEN, kind=CH_)               :: Initials(PEOPLE_AMOUNT) = ""
   character(INITIALS_LEN, kind=CH_), allocatable  :: guest_initials(:), native_initials(:)
   
   !Массив пола
   character(kind=CH_)                             :: Gender(PEOPLE_AMOUNT) = ""
   character(kind=CH_), allocatable                        :: guest_gender(:), native_gender(:)
   !Массив прописки
   character(kind=CH_)                             :: Registration(PEOPLE_AMOUNT) = ""
  
   integer, allocatable                            :: Native_Position(:), Guest_Position(:)

   !массив среднего балла
   real(R_)                                        :: Avg_Marks(PEOPLE_AMOUNT) = 0
   real(R_), allocatable                           :: guest_avg_mark(:), native_avg_mark(:)

   !Массив маска
   logical, allocatable                            :: Is_Guest(:), Is_Native(:)
   integer                                         :: guest_amount = 0, native_amount = 0

   integer :: In, Out, IO, i
   integer, parameter                              :: INDEXES(*) = [(i, i = 1, PEOPLE_AMOUNT)]

   input_file = "../data/class.txt"
   output_file = "output.txt"
   ! Чтение списка людей: фамилии, инициалы, полы, прописки и средний.
   open (file=input_file, encoding=E_, newunit=In)
      format = '(4(a, 1x), f5.2)'
      read (In, format, iostat=IO) (Surnames(i), Initials(i), Gender(i), Registration(i), Avg_Marks(i), i = 1, PEOPLE_AMOUNT)
   close (In)
   
   ! Обработка статуса чтения.
   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(IO)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while reading people list."
      case(1:)
         write (Out, '(a)') "Error while reading people list: ", IO
      case default
         write (Out, '(a)') "Undetermined error has been reached while reading class list: ", io
   end select

   ! Вывод списка людей.
   open (file=output_file, encoding=E_, newunit=Out)
      write (out, '(a)') "Исходный список:"
      write (Out, format, iostat=IO) (Surnames(i), Initials(i), Gender(i), Registration(i), Avg_Marks(i), i = 1, PEOPLE_AMOUNT)
   close (Out)
   
   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while writing people list."
      case(1:)
         write (Out, '(a)') "Error while writing people list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while writing people list: ", io
   end select

   call cpu_time(start)
   !Логическая маска для определения петербуржцев
   Is_Native    = Registration == Native
   !Подсчитываем количесто петербуржцев
   native_amount    = Count(Is_Native)
   
   ! Получение массивов, связынных с Петеруржцами.
   Native_Position = Pack(INDEXES, Is_Native)
   allocate (native_surnames(native_amount), native_initials(native_amount), &
      native_gender(native_amount), native_avg_mark(native_amount))
   do concurrent (i = 1:native_amount)
      ! Получение списков петербурцев.
      native_surnames(i)  = Surnames(Native_Position(i))
      native_initials(i)  = Initials(Native_Position(i))
      native_gender(i)    = Gender(Native_Position(i))
      native_avg_mark(i)  = Avg_Marks(Native_Position(i))
   end do
   
   call SortArray (native_amount, native_avg_mark, native_surnames, native_initials, native_gender)

   Is_Guest      = .not. Is_Native
   guest_amount   = PEOPLE_AMOUNT - native_amount
   
   ! Получение массивов, связынных с гостями.
   Guest_Position   = Pack(INDEXES, Is_Guest) 
   allocate (guest_surnames(guest_amount), guest_initials(guest_amount), &
      guest_gender(guest_amount), guest_avg_mark(guest_amount))
   do concurrent (i = 1:guest_amount)
      ! Получение списков гостей
      guest_surnames(i)  = Surnames(Guest_Position(i))
      guest_initials(i)  = Initials(Guest_Position(i))
      guest_gender(i)    = Gender(Guest_Position(i))
      guest_avg_mark(i)  = Avg_Marks(Guest_Position(i))
   end do
   call SortArray (guest_amount, guest_avg_mark, guest_surnames, guest_initials, guest_gender)
   
   call cpu_time(finish)
   print'("Time =",f10.7)',finish-start
   ! Вывод отсортированного списка петербурожцев
   open (file=output_file, encoding=E_, position='append', newunit=Out)
      write (out, '(/a)') "Успеваемость Петербуржцев:"
      write (Out, format, iostat=IO) &
         (native_surnames(i), native_initials(i), native_gender(i), "П", native_Avg_mark(i), i = 1, native_amount)
   close (Out)
   ! Обработка статуса записи.
   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while writing sorted Native list."
      case(1:)
         write (Out, '(a)') "Error while writing sorted Native list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while writing sorted Native list: ", io
   end select
   
      ! Вывод отсортированного списка гостей
   open (file=output_file, encoding=E_, position='append', newunit=Out)
      write (out, '(/a)') "Успеваемость гостей Санкт-Петербурга:"
      write (Out, format, iostat=IO) (guest_surnames(i), guest_initials(i), guest_gender(i), "С", &
         guest_avg_mark(i), i = 1, guest_amount)
   close (Out)
   ! Обработка статуса записи.
   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while writing sorted Guest list."
      case(1:)
         write (Out, '(a)') "Error while writing sorted Guest list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while writing sorted Guest list: ", io
   end select

contains
   pure subroutine SortArray(array_amount, array_avg_mark, array_surnames, array_initials, array_gender)
   character(INITIALS_LEN, kind=CH_), intent(inout)  :: array_initials(:)
   character(SURNAME_LEN, kind=CH_), intent(inout)   :: array_surnames(:)
   character(kind=CH_), intent(inout)                :: array_gender(:)
   real(R_), intent(inout)                            :: array_avg_mark(:)
   integer, intent(inout)                               :: array_amount

   character(SURNAME_LEN,kind=CH_) :: surname_temp
   character(INITIALS_LEN,kind=CH_) :: initials_temp
   character(kind=CH_) :: gender_temp
   real(R_) :: avg_mark_temp
   integer i,j
   logical :: Swap

   i=0
   j=0

   do i = array_amount, 2, -1
      ! Просматриваем список с начала, ставя в конец менее успешного.
      do j = 1, i-1
         Swap = .false.
         ! Проверка на то, стоит ли менять людей местами.
         if (array_avg_mark(j) < array_avg_mark(j+1)) then
            Swap = .true.
         else if (array_avg_mark(j) == array_avg_mark(j+1)) then
            if (array_surnames(j) > array_surnames(j+1)) then
               Swap = .true.
            else if (array_surnames(j)==array_surnames(j+1) .and. array_initials(j)>array_initials(j+1)) then 
               Swap = .true.
            end if
         end if

         if (Swap) then
            surname_temp          = array_surnames(j+1)
            array_surnames(j+1)  = array_surnames(j)
            array_surnames(j)    = surname_temp

            initials_temp        = array_initials(j+1)
            array_initials(j+1)  = array_initials(j)
            array_initials(j)    = initials_temp

            gender_temp          = array_gender(j+1)
            array_gender(j+1)  = array_gender(j)
            array_gender(j)    = gender_temp

            avg_mark_temp        = array_avg_mark(j+1)
            array_avg_mark(j+1)  = array_avg_mark(j)
            array_avg_mark(j)   = avg_mark_temp
         end if
      end do
   end do
   end subroutine SortArray
end program lab_2_1
