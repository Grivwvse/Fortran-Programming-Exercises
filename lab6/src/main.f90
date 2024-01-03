program lab_1_6
   use Environment
   use Group_Process
   use Group_IO

   implicit none
   character(:), allocatable :: input_file, output_file
   character(kind=CH_), parameter   :: native = Char(1055, CH_), guest = Char(1057, CH_)
  real :: start, finish
   !Зануляем ссылки
   type(person), pointer     :: Group_list => Null(), Native_list => Null(), Guest_list => Null()
   integer(I_)               :: Native_Amount = 0, Guest_Amount = 0
  
   input_file  = "../data/class.txt"
   output_file = "output.txt"
   
   Group_list => Read_group_list(input_file)
   
   if (Associated(Group_list)) then
      call Output_group_list(output_file, Group_List, "Исходный список:", "rewind")
      call cpu_time(start)
      call Get_list_by_registration(Group_list, Native_list, Native_Amount, native)
      call Get_list_by_registration(Group_list, Guest_list,Guest_Amount, guest)
      
      call Sort_group_list(Native_List, Native_Amount)
      call Sort_group_list(Guest_List, Guest_Amount)
call cpu_time(finish)
print '("Time = ",f6.3," seconds.")',finish-start
      call Output_group_list(output_file, Native_List, "Успеваемость жителей Санкт-Петербурга:", &
      "append")
       call Output_group_list(output_file, Guest_List, "Успеваемость гостей Санкт-Петербурга:", &
      "append")
   end if
end program lab_1_6
