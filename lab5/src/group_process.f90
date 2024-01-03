module Group_Process
   use Environment
   use Group_IO

   implicit none
   
contains
   pure subroutine Get_list_by_reg(Group, reg_array, Mask, Reg_Amount)
      type(person), intent(in) :: Group
      type(person) :: reg_array 
      integer :: Reg_Amount
      intent(inout) :: reg_array, Reg_Amount
      character(kind=CH_), intent(in) :: Mask

      logical, allocatable :: Is_A_Reg(:)
      integer :: i
      integer, allocatable :: Reg_Pos(:)
      integer, parameter   :: INDEXES(*) = [(i, i = 1, PEOPLE_AMOUNT)] 

      Is_A_Reg = (Group%Registration == Mask)
      Reg_Amount = count(Is_A_Reg)
      Reg_Pos = Pack(INDEXES, Is_A_Reg)
      do concurrent (i = 1:Reg_Amount)
         reg_array%Surname(i) = Group%Surname(Reg_Pos(i))
         reg_array%Initials(i) = Group%Initial(Reg_Pos(i))
         reg_array%Gender(i) = Group%Gender(Reg_Pos(i))
         reg_array%Registration(i) = Group%Registration(Reg_Pos(i))
         reg_array%avg_mark(i) = Group%avg_mark(Reg_Pos(i))
      end do
   end subroutine Get_list_by_reg
   ! Сортировка списка по среднему баллу.
   pure subroutine Sort_group_list(Group, reg_amount)
      type(person), intent(inout)  :: Group
      integer, intent(in)     :: reg_amount

      integer        :: i, j
      type(person)  :: tmp_person

      i = 1
      ! Сортировка списка по среднему баллу методом пузырька.
      do i = reg_amount, 2, -1
         ! Просматриваем список с начала, ставя в конец менее успешного.
         do j = 1, i-1
            ! Проверка на то, стоит ли менять людей  местами.
            if (Swap(Group, j)) then
               tmp_person%Surname = Group%Surname(j+1)
               Group%Surname(j+1) = Group%Surname(j)
               Group%Surname(j) = tmp_person%Surname(1)

               tmp_person%Initials = Group%Initials(j+1)
               Group%Initials(j+1) = Group%Initials(j)
               Group%Initials(j) = tmp_person%Initials(1)

               tmp_person%Gender = Group%Gender(j+1)
               Group%Gender(j+1) = Group%Gender(j)
               Group%Gender(j) = tmp_person%Gender(1)

               tmp_person%avg_mark = Group%avg_mark(j+1)
               Group%avg_mark(j+1) = Group%avg_mark(j)
               Group%avg_mark(j) = tmp_person%avg_mark(1)
            end if
         end do
      end do
   end subroutine Sort_group_list

   pure subroutine Sort_group_list2(Group, reg_amount)
      type(person), intent(inout)  :: Group
      integer, intent(in)     :: reg_amount

      integer        :: i, j
      character(SURNAME_LEN, kind=CH_) :: tmp_surname
      character(INITIALS_LEN, kind=CH_) :: tmp_initials
      character(kind=CH_)               :: tmp_gender
      real(R_)    :: tmp_avg_mark

      i = 1
      ! Сортировка списка по среднему баллу методом пузырька.
      do i = reg_amount, 2, -1
         ! Просматриваем список с начала, ставя в конец менее успешного.
         do j = 1, i-1
            ! Проверка на то, стоит ли менять людей  местами.
            if (Swap(Group, j)) then
               tmp_surname = Group%Surname(j+1)
               Group%Surname(j+1) = Group%Surname(j)
               Group%Surname(j) = tmp_surname

               tmp_initials = Group%Initials(j+1)
               Group%Initials(j+1) = Group%Initials(j)
               Group%Initials(j) = tmp_initials

               tmp_gender = Group%Gender(j+1)
               Group%Gender(j+1) = Group%Gender(j)
               Group%Gender(j) = tmp_gender

               tmp_avg_mark = Group%avg_mark(j+1)
               Group%avg_mark(j+1) = Group%avg_mark(j)
               Group%avg_mark(j) = tmp_avg_mark
            end if
         end do
      end do
   end subroutine Sort_group_list2
   ! Проверка того, стоит ли менять местами текущего человека со следующим.
   pure logical function Swap(Group, j)
      type(person), intent(in)  :: Group
      integer, intent(in)        :: j

      Swap = .false.
      if (Group%avg_mark(j) < Group%avg_mark(j+1)) then
         Swap = .true.
      else if (Group%avg_mark(j) == Group%avg_mark(j+1)) then
         if (Group%Surname(j) > Group%Surname(j+1)) then
            Swap = .true.
         else if (Group%Surname(j)==Group%Surname(j+1) .and. Group%Initials(j)>Group%Initials(j+1)) then
            Swap = .true.
         end if
      end if
   end function Swap
end module group_process
