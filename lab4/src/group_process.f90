module Group_Process
   use Environment
   use Group_IO

   implicit none
   
contains
 !Сортировка списка людей по среднему баллу рекурсивно методом пузырька.
   pure recursive subroutine Sort_group_list(Group, N)
      type(person), intent(inout)  :: Group(:)
      integer, intent(in)           :: N

      ! Работаем только с первыми N элементами: помещаем в ИХ конец менее успешного.
      call Drop_down(Group, 1, N-1)

      ! Если необходимо, делаем то же с последними N-1 элементами.
      if (N >= 3) &
         call Sort_group_list(Group, N-1)
   end subroutine Sort_group_list

   ! Помещаем c j-ой на N-ую позицию менее успешного, поочерёдно сравнивая.
   pure recursive subroutine Drop_down(Group, j, N)
      type(person), intent(inout)  :: Group(:)
      integer, intent(in)           :: j, N

      type(person)  :: tmp_person

      ! Если требуется, то меняем людей  местами В данном случае больше подходит строуктура массивов,
      ! поскольку мы не работаем с отдельными переменными экземпляра, а со всем экземпляорм(обьектом)
      if (Swap(Group, j)) then
         tmp_person = Group(j+1)
         Group(j+1) = Group(j)
         Group(j) = tmp_person
      end if
      if (j < N) &
         call Drop_down(Group, j+1, N)
   end subroutine Drop_down

   ! Проверка того, стоит ли менять местами текущего человека со следующим.
   pure logical function Swap(Group, j)
      type(person), intent(in)  :: Group(:)
      integer, intent(in)        :: j

      Swap = .false.
      if (Group(j)%avg_mark < Group(j+1)%avg_mark) then
         Swap = .true.
      else if (Group(j)%avg_mark == Group(j+1)%avg_mark) then
         if (Group(j)%Surname > Group(j+1)%Surname) then
            Swap = .true.
         else if (Group(j)%Surname==Group(j+1)%Surname .and. Group(j)%Initials>Group(j+1)%Initials) then
            Swap = .true.
         end if
      end if
   end function Swap
end module group_process
