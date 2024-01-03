program lab_1_3
   use Environment
   use Group_Process
   use Group_IO

   implicit none
   character(:), allocatable :: input_file, output_file, data_file
   character(kind=CH_), parameter   :: native = Char(1055, CH_), guest = Char(1057, CH_)
   integer     :: native_amount, guest_amount
  
   !Массив структур 
   type(person)              :: Group
   type(person)              :: NativeArray, GuestArray

   input_file  = "../data/list.txt"
   output_file = "output.txt"
   data_file   = "list.dat"
   
   call Create_data_file(input_file, data_file)
   
   Group = Read_group_list(data_file)

   call Output_group_list(output_file, Group, "Исходный список:", "rewind", PEOPLE_AMOUNT)
   call Get_list_by_reg(Group, NativeArray, native, native_amount)
   call Get_list_by_reg(Group, GuestArray, guest, guest_amount)

   call Sort_group_list(NativeArray,native_amount)
   call Sort_group_list(GuestArray,guest_amount)

   call Output_group_list(output_file, NativeArray, "Успеваемость жителей Санкт-Петербурга:", &
      "append", native_amount)
   call Output_group_list(output_file, GuestArray, "Успеваемость гостей Санкт-Петербурга:", &
      "append",guest_amount)

end program lab_1_3
