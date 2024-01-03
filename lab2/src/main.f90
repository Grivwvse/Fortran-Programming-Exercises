  
program lab_1_2var2
   use Environment
   use Group_Process
   use Group_IO

   implicit none
   !use PEOPLE_AMOUNT = 8, SURNAME_LEN = 15, INITIALS_LEN = 5
   character(:), allocatable        :: input_file, output_file, data_file
   character(kind=CH_), parameter   :: Native = Char(1055, CH_), Guest = Char(1057, CH_)
     character(kind=CH_) :: Array(SymbolLength)
   !
! Массивы фамилий, инициалов, полов, регистрации и средних оценов. Хранение по столбцам!
   character(kind=CH_)  :: Surnames(SURNAME_LEN, PEOPLE_AMOUNT)  = "", &
                           Initials(INITIALS_LEN, PEOPLE_AMOUNT) = "", &
                           Genders(PEOPLE_AMOUNT), Registration(PEOPLE_AMOUNT)  = ""
  ! character(kind=CH_)  :: Surnames(PEOPLE_AMOUNT, SURNAME_LEN)  = "", &
  !                         Initials(PEOPLE_AMOUNT, INITIALS_LEN) = "", &
  !                         Genders(PEOPLE_AMOUNT), Registration(PEOPLE_AMOUNT)  = ""
     character(kind=CH_) :: Array(SymbolLength)
   character(kind=CH_), allocatable :: Native_Surnames(:, :), Guest_Surnames(:, :)
   character(kind=CH_), allocatable :: Native_Initials(:, :), Guest_Initials(:, :)
   character(kind=CH_), allocatable :: Native_Gender(:), Guest_Gender(:)
   integer               :: i = 0
   real(R_)             :: Aver_Marks(PEOPLE_AMOUNT) = 0
   real(R_),allocatable :: Native_Aver_Marks(:), Guest_Aver_Marks(:)
!|Л| |е| |ш| |а|
!|П| |е| |т| |я|

!|Л| |П|
!|е| |е|
!|ш| |т|
!|а| |я|

   !type(student)              :: Group(STUD_AMOUNT)
   !type(student), allocatable :: Boys(:), Girls(:)
   !integer                    :: i ! По стандарту можно заводить прямо в do concurrent.

   input_file  = "../data/class.txt"
   output_file = "output.txt"
   !data_file   = "class.dat"
   
   call Read_people_list(input_file, Surnames, Initials, Genders, Registration, Aver_Marks)
   !вывогдим содержимое файла
   call Output_people_list(output_file, Surnames, Initials, Genders, Registration, Aver_Marks, &
      "Исходный список:", "rewind")
   !Получаем массив петербуржцев
   call Get_list_by_reg(Surnames, Initials, Genders, Registration, Native_Surnames, Aver_Marks, &
      Native_Initials, Native_Gender, Native_Aver_Marks, Native)
   !Получаем массив Гостей
   call Get_list_by_reg(Surnames, Initials, Genders, Registration, Guest_Surnames, Aver_Marks, &
      Guest_Initials,Guest_Gender, Guest_Aver_Marks, Guest)
   !Сортируем петербуржцев по среднему баллу
   call Sort_people_list(Native_Surnames, Native_Initials, Native_Gender, Native_Aver_Marks)
   !Сортируем Гостей по среднему баллу
   call Sort_people_list(Guest_Surnames, Guest_Initials, Guest_Gender, Guest_Aver_Marks)
   !Выводим отсортированный массив петербуржцев
   call Output_people_list(output_file, Native_Surnames, Native_Initials, Native_Gender, &
      [(Native, i = 1, Size(Native_Aver_Marks))],&
      Native_Aver_Marks, "Успеваемость жителей Санкт-Петербурга:", "append")
   !Выводим отсортированный массив Гостейц
   call Output_people_list(output_file, Guest_Surnames, Guest_Initials, Guest_Gender, [(Guest, i = 1, Size(Guest_Aver_Marks))], &
      Guest_Aver_Marks, "Успеваемость гостей Санкт-Петербурга:", "append")

   !call Create_data_file(input_file, data_file)
   
   !Group = Read_class_list(data_file)

   !call Output_class_list(output_file, Group, "Исходный список:", "rewind")

   !Boys  = Pack(Group, Group%Sex == Native)
   !Girls = Pack(Group, Group%Sex == Guest)

   ! Вычисление средней оценки для каждого юноши.
   !do concurrent (i = 1:Size(Boys))
   !   Boys(i)%Aver_mark = Sum(Boys(i)%Marks) / Real(MARKS_AMOUNT, R_)
   !end do
   ! нельзя: Boys%Aver_mark = Sum(Boys%Marks) / Real(MARKS_AMOUNT, R_)
   ! 
   ! Переменную i можно заводить прямо тут.
   !do concurrent (integer :: i = 1:Size(Boys))
   
   ! Вычисление средней оценки для каждой левушки.
   !do concurrent (i = 1:Size(Girls))
   !   Girls(i)%Aver_mark = Real(Sum(Girls(i)%Marks), R_) / MARKS_AMOUNT
   !end do

   !call Sort_class_list(Boys)
   !call Sort_class_list(Girls)

   !call Output_class_list(output_file, Boys, "Успеваемость юношей:", "append")
   !call Output_class_list(output_file, Girls, "Успеваемость девушек:", "append")

end program lab_1_2var2
