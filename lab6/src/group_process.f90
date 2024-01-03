module Group_Process
   use Environment
   use Group_IO

   implicit none
   
contains
   ! Получение списков по регистрации.
   pure recursive subroutine Get_list_by_Registration(pers, List, Amount, Reg) !Рекурсия никогда не работает со всем списком, а
     ! только с конкретным обьеуктом
      type(person), intent(in)        :: pers
      type(person), pointer           :: List            !Обьект чтобы не испортить данные тк мы будем только копировать
      integer(I_), intent(inout)       :: Amount
      character(kind=CH_), intent(in)  :: Reg

      ! Если найден человек с подходящей регистрацией, то размещаем в новом списке элемент и копируем его данные.
      if (pers%Registration == Reg) then
         allocate (List, source=pers)
         Amount = Amount + 1
         List%next => Null() ! зануляем
         ! Если ещё остались люди, сканируем дальше, а в создаваемом списке передаём место СОСЕДА.
         if (Associated(pers%next)) &
            call Get_list_by_Registration(pers%next, List%next, Amount, Reg)
      ! Если ещё остались студенты, сканируем дальше, а в создаваемом списке передаём ПРЕЖНЕЕ место.
      else if (Associated(pers%next)) then
         call Get_list_by_Registration(pers%next, List, Amount, Reg)
          else
             List => Null() ! условие из-за него, предыдущий может не выполнится, если к примеру гостей нет
      end if
   end subroutine Get_list_by_Registration

   ! Сортировка списка по среднему баллу рекурсивно методом пузырька.
   pure recursive subroutine Sort_group_list(GroupList, N)
      type(person), pointer, intent(inout)  :: GroupList   !Ссылка тк нам надо достучатся до ссылки на первый обьект, чтобы он тоже
      !мог перемещатся
      integer, intent(in)                    :: N

      ! Работаем только с первыми N элементами: помещаем в ИХ конец менее успешного.
      call Drop_down(GroupList, 1, N-1)

      ! Если необходимо, делаем то же с первыми N-1 элементами.
      if (N >= 3) &
         call Sort_group_list(GroupList, N-1)
   end subroutine Sort_group_list

   ! Помещаем c j-ой на N-ую позицию менее успешного, поочерёдно сравнивая.
   pure recursive subroutine Drop_down(GroupList, j, N)
      type(person), pointer  :: GroupList
      integer, intent(in)                    :: j, N

      ! Если требуется, то меняем местами текущего человека со следующим.
      if (Swap(GroupList)) &
         call Swap_from_current(GroupList)
      if (j < N) &
         call Drop_down(GroupList%next, j+1, N)
   end subroutine Drop_down

   ! Проверка того, стоит ли менять местами текущего человека со следующим.
   pure logical function Swap(Current)
      type(person), intent(in)  :: Current

      Swap = .false.
      if (Current%Avg_Mark < Current%next%Avg_Mark) then
         Swap = .true.
      else if (Current%Avg_Mark == Current%next%Avg_Mark) then
         if (Current%Surname > Current%next%Surname) then
            Swap = .true.
         else if (Current%Surname==Current%next%Surname .and. Current%Initials>Current%next%Initials) then
            Swap = .true.
         end if
      end if
   end function Swap
 ! Перестановка местами двух эелементов списка, начиная с текущего.
   pure subroutine Swap_from_current(Current)
      type(person), pointer  :: Current ! изменяем ссылочную природу + чтобы поменять и первый обьект с каким-нгибудь 

      type(person), pointer  :: tmp_person

      tmp_person       => Current%next
      Current%next   => Current%next%next
      tmp_person%next  => Current
      Current        => tmp_person
   end subroutine Swap_from_current
end module group_process
