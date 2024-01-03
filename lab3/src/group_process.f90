module Group_Process
   use Environment
   use Group_IO

   implicit none
   
contains
   ! Сортировка списка по среднему баллу.
   pure subroutine Sort_group_list(Group)
      type(person), intent(inout)  :: Group(:)

      integer        :: i, j
      type(person)  :: tmp_person

      ! Сортировка списка по среднему баллу методом пузырька.
      do i = Size(Group), 2, -1
         ! Просматриваем список с начала, ставя в конец менее успешного.
         do j = 1, i-1
            ! Проверка на то, стоит ли менять людей  местами.
            if (Swap(Group, j)) then
               tmp_person = Group(j+1)
               Group(j+1) = Group(j)
               Group(j) = tmp_person
            end if
         end do
      end do
   end subroutine Sort_group_list

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
