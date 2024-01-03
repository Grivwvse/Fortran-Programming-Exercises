program lab_1_3
   use Environment
   use Group_Process
   use Group_IO

   implicit none
   character(:), allocatable :: input_file, output_file, data_file
   character(kind=CH_), parameter   :: native = Char(1055, CH_), guest = Char(1057, CH_)
  
   !Массив структур 
   type(person)              :: Group(PEOPLE_AMOUNT)
   type(person), allocatable :: NativeArray(:), GuestArray(:)
   real :: start, finish

   input_file  = "../data/class.txt"
   output_file = "output.txt"
   data_file   = "list.dat"
   
   call Create_data_file(input_file, data_file)
   
   Group = Read_group_list(data_file)

   call Output_group_list(output_file, Group, "Исходный список:", "rewind")
   print"(a, 1x)", Group%Surname

   call cpu_time(start)
   NativeArray  = Pack(Group, Group%Registration == native)
   GuestArray = Pack(Group, Group%Registration == guest)

   call Sort_group_list(NativeArray)
   call Sort_group_list(GuestArray)
   call cpu_time(finish)
   print'("time", f6.3)', finish-start

   !call Output_group_list(output_file, NativeArray, "Успеваемость жителей Санкт-Петербурга:", &
   !   "append")
   !call Output_group_list(output_file, GuestArray, "Успеваемость гостей Санкт-Петербурга:", &
   !   "append")

end program lab_1_3
